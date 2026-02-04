##########################
### Goals and Progress ###
##########################

###
### Download Required Packages
### 
if(!require(tidyverse))  install.packages("tidyverse",  repos = "http://cran.us.r-project.org")
if(!require(Hmisc))  install.packages("Hmisc",  repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(googlesheets4)) install.packages("googlesheets4", repos = "http://cran.us.r-project.org")

########################
### Set Lifting Goal ###
########################

lift   <- "barbell_squat" 
        # "barbell_squat" | "barbell_bench_press" | "deadlift"
reps   <- "1_rep_max"     
        # "1_rep_max" | "3_rep_max" | "10_rep_max"
weight <- 400             
        # Goal weight in lbs (at 'reps' above)
date   <- as.Date("2026-07-31") 
        # Set goal date (YYYY-MM-DD)

###
### Add file locations
###

### Workout log excel file
# workout_log_xlsx <- "~/workout_log.xlsx" #~/workout_log.xlsx

### Workout log google sheet
# Use the sheet generated from responses to the following survey:
# https://docs.google.com/forms/d/e/1FAIpQLSeTVY4Df98JANqC5RlyLEft-xDz8GCLKLUVgGjqq-2UAhWkdQ/viewform?usp=sharing&ouid=110422957884271136845
gs4_deauth()

# Read directly from the google sheet URL share link
workout_log_sheet <- read_sheet("https://docs.google.com/spreadsheets/")

### Plot file output directory
output_dir <- "~/plots" # "~/plots" for plots folder

# Plot file name
file_name <- paste0(
  lift, "_", 
  reps, "_", 
  weight, "lb_", 
  format(date, "%Y-%m-%d"), "_goal_",
  format(Sys.Date(), "%Y-%m-%d"),
  ".png")

# Full file path
file_path <- file.path(output_dir, file_name)

###
### Data
###

# Read workout log from Excel
#workout_log_file <- read_excel(workout_log_xlsx, sheet = "lifting")

#stopifnot(file.exists(path.expand(workout_log_xlsx)))

#required_cols <- c("lift","reps","weight_lbs","date")
#missing <- setdiff(required_cols, names(workout_log_file))
#if (length(missing)) stop("Missing columns in 'lifting' sheet: ", paste(missing, collapse=", "))

#workout_log_file$date       <- as.Date(workout_log_file$date)

#workout_log_file$weight_lbs <- suppressWarnings(as.numeric(workout_log_file$weight_lbs))

# Use Google Sheet Log
workout_log_sheet <- workout_log_sheet %>%
  rename(
    timestamp   = Timestamp,
    date        = `Date and Time of Workout`,
    lift        = `Select the Type of Lift/Exercise`,
    reps        = `Number of Reps`,
    weight_lbs  = `Weight Lifted (in lbs)`
  )

workout_log_sheet <- workout_log_sheet %>%
  mutate(
    lift = tolower(lift) %>% gsub("[^a-z0-9]+", "_", .),
    reps = tolower(reps) %>% gsub("[^a-z0-9]+", "_", .),
    date = as.Date(date),
    weight_lbs = as.numeric(weight_lbs)
  )

workout_log <- workout_log_sheet

# Epley conversions
est_1rm  <- function(w, reps) w * (1 + reps/30)             # set -> 1RM
est_nrm  <- function(one_rm, reps) one_rm / (1 + reps/30)   # 1RM -> nRM

# Map reps label or numeric to integer reps
reps_to_n <- function(r) {
  if (length(r) != 1 || is.na(r)) stop("Reps must be a single, non-NA value.")
  
  # if numeric input: 3, 10, etc.
  if (is.numeric(r)) {
    rn <- as.integer(round(r))
    if (rn > 0) return(rn)
    stop("Numeric reps must be a positive integer.")
  }
  
  # if character input: "1_rep_max", "1RM", " 10 rm ", "3", etc.
  if (is.character(r)) {
    s <- trimws(tolower(r))
    s <- gsub("rep_max|rm|reps|max", "", s)   # strip words
    s <- gsub("[^0-9]", "", s)                # keep digits only
    if (nchar(s) > 0) {
      rn <- suppressWarnings(as.integer(s))
      if (is.finite(rn) && rn > 0) return(rn)
    }
    stop("Unknown reps spec: ", r,
         ". Try one of: 1, 3, 10, '1_rep_max', '3RM', '10RM'.")
  }
  
  stop("Unsupported reps type: ", typeof(r))
}

# Normalize goal to a 1RM
goal_input_reps <- reps_to_n(reps)
goal_1rm_anchor <- if (goal_input_reps == 1) weight else est_1rm(weight, goal_input_reps)

# Capital name for plotting
plot_name <- function(x) {
  x_clean <- gsub("^barbell_", "Barbell ", x)   # Replace prefix
  x_clean <- gsub("_", " ", x_clean)            # Replace underscores with spaces
  tools::toTitleCase(tolower(x_clean))          # Capitalize Each Word
}

# Compact builder for 1/3/10RM blocks 
# - safer logs 
# - downweighted goal row
# - sparse-history 
# - robust option

build_rep_block <- function(wlog, lift_id, reps_n, goal_1rm, goal_date) {
  reps_lab <- paste0(reps_n, "_rep_max")
  
  # historical actuals
  actual <- wlog[wlog$lift == lift_id & wlog$reps == reps_lab,
                 c("lift","reps","weight_lbs","date")]
  actual$date <- as.Date(actual$date)
  actual <- actual[order(actual$date), ]
  n_actual <- sum(is.finite(actual$weight_lbs))
  
  # today placeholder
  # today placeholder
  # If we already have an observed lift for today, do NOT add the placeholder row.
  has_today_observed <- any(is.finite(actual$weight_lbs) & actual$date == Sys.Date())
  
  if (has_today_observed) {
    block <- actual
  } else {
    today_row <- data.frame(lift = lift_id,
                            reps = reps_lab,
                            weight_lbs = NA_real_,
                            date = Sys.Date())
    block <- rbind(actual, today_row)
  }
  
  
  # lags & engineered features
  block$weight_lag1   <- Lag(block$weight_lbs, +1)
  block$weight_lag2   <- Lag(block$weight_lbs, +2)
  block$trend         <- seq_len(nrow(block))
  block$trend_sq      <- block$trend^2
  
  # goal row (planning): 1RM uses goal_1rm; others via Epley down-conversion
  goal_wt <- if (reps_n == 1) goal_1rm else est_nrm(goal_1rm, reps_n)
  goal_row <- data.frame(lift = lift_id,
                         reps = reps_lab,
                         weight_lbs = goal_wt,
                         date = as.Date(goal_date),
                         weight_lag1 = goal_wt,
                         weight_lag2 = goal_wt,
                         trend = nrow(block) + 1,
                         trend_sq = (nrow(block) + 1)^2)
  block <- rbind(block, goal_row)
  block$date <- as.Date(block$date)
  
  # SAFE LOGS: prevent -Inf/NA by flooring lags at eps
  eps <- 1e-6
  block$w1 <- ifelse(is.finite(block$weight_lag1) & block$weight_lag1 > 0, block$weight_lag1, eps)
  block$w2 <- ifelse(is.finite(block$weight_lag2) & block$weight_lag2 > 0, block$weight_lag2, eps)
  
  # WEIGHTS: emphasize recent lifts, de-emphasize goal
  wt <- rep(1, nrow(block))
  
  # downweight the goal row so it steers but doesn’t dominate
  wt[nrow(block)] <- 0.5
  
  # upweight the last and second-to-last actual lifts (before today)
  actual_idx <- which(is.finite(block$weight_lbs) & block$date < Sys.Date())
  
  if (length(actual_idx) >= 1) {
    last_actual_idx <- tail(actual_idx, 1)
    wt[last_actual_idx] <- 1.8   # most recent lift
  }
  
  if (length(actual_idx) >= 2) {
    second_last_idx <- tail(actual_idx, 2)[1]
    wt[second_last_idx] <- 1.3  # second-most recent lift
  }
  
  # Sparse-history fallback + robust regression
  fit <- NULL
  have_mass <- requireNamespace("MASS", quietly = TRUE)
  
  if (n_actual >= 3) {
    fit <- if (have_mass) try(
      MASS::rlm(weight_lbs ~ date + trend_sq + log(w1) + log(w2),
                data = block, weights = wt),
      silent = TRUE) else NULL
    if (inherits(fit, "try-error") || is.null(fit)) {
      fit <- lm(weight_lbs ~ date + trend_sq + log(w1) + log(w2),
                data = block, weights = wt)
    }
    block$predicted <- as.numeric(predict(fit, block))
  } else if (n_actual == 2) {
    fit <- if (have_mass) try(
      MASS::rlm(weight_lbs ~ date + trend_sq,
                data = block, weights = wt, psi = MASS::psi.huber),
      silent = TRUE) else NULL
    if (inherits(fit, "try-error") || is.null(fit)) {
      fit <- lm(weight_lbs ~ date + trend_sq, data = block, weights = wt)
    }
    block$predicted <- as.numeric(predict(fit, block))
  } else {
    # ≤ 1 actual: linear bridge prescription from last actual to goal
    w0 <- if (n_actual == 1) tail(na.omit(actual$weight_lbs), 1) else NA_real_
    d0 <- if (n_actual == 1) max(actual$date, na.rm = TRUE) else min(block$date, na.rm = TRUE)
    dg <- as.Date(goal_date); wg <- goal_wt
    frac <- as.numeric(block$date - d0) / max(1, as.numeric(dg - d0))
    frac <- pmin(pmax(frac, 0), 1)
    block$predicted <- if (is.finite(w0)) w0 + (wg - w0) * frac else wg * frac
    fit <- NULL
  }
  
  list(data = block, model = fit)
}

# Monotone clamp applied only to today's projection
# - k: number of past increments to look back
# - mult: cap factor relative to median of recent increments
# - max_step_abs: hard ceiling on today’s step (lbs)
# - min_step: floor on today’s step (avoid flatline)
# - step_round: set to 2.5 to snap to plates, NULL to skip
adjust_projection <- function(block, goal_wt,
                              k_actual = 3, 
                              mult = 0.85,
                              max_step_abs = 25,
                              min_step = 0,
                              step_round = NULL,
                              today_date = Sys.Date()) {
  if (!all(c("date","predicted","weight_lbs") %in% names(block))) return(block)
  
  # Order by date; pull vectors
  o <- order(block$date)
  d <- as.Date(block$date[o])
  x <- as.numeric(block$predicted[o])    # projections (used only to adjust today's value)
  y <- as.numeric(block$weight_lbs[o])   # actuals (used to compute baseline step)
  
  if (length(x) < 2 || all(!is.finite(x))) { block$predicted[o] <- x; return(block) }
  
  # Find today's row: exact match preferred, else latest <= today
  idx_today <- which(d == today_date)
  if (length(idx_today) == 0) idx_today <- suppressWarnings(max(which(d <= today_date), na.rm = TRUE))
  if (!length(idx_today) || !is.finite(idx_today) || idx_today <= 1) {
    block$predicted[o] <- x
    return(block)
  }
  i <- idx_today
  
  # Baseline step from last up-to-3 actual lifts (observed only)
  act_idx <- which(is.finite(y) & d <= today_date)
  if (length(act_idx) >= 2) {
    act_idx_recent <- tail(act_idx, k_actual)
    act_diff <- diff(y[act_idx_recent])
    base <- stats::median(act_diff[act_diff > 0], na.rm = TRUE)
    if (!is.finite(base) || base <= 0) {
      # fallback: use median absolute change if all diffs <=0/NA, else small default
      base <- stats::median(abs(act_diff), na.rm = TRUE)
      if (!is.finite(base) || base <= 0) base <- 2.5
    }
  } else {
    base <- 2.5  # not enough actuals: sensible default
  }
  
  # Proposed raw increment for today (relative to previous *predicted* point)
  raw_inc <- x[i] - x[i - 1]
  
  # Cap today’s step using baseline from actuals
  cap <- mult * base
  cap <- min(cap, max_step_abs)
  if (is.finite(goal_wt)) cap <- min(cap, goal_wt - x[i - 1])
  
  inc <- max(min(raw_inc, cap), min_step)
  x[i] <- x[i - 1] + inc
  
  # Optional plate rounding
  if (!is.null(step_round) && is.finite(step_round) && step_round > 0) {
    x[i] <- ceiling(x[i] / step_round) * step_round
    if (is.finite(goal_wt)) x[i] <- min(x[i], goal_wt)
    x[i] <- max(x[i], x[i - 1])  # keep monotone
  }
  
  block$predicted[o] <- x
  block
}

# All goals for the SELECTED lift (always generate 1/3/10RM projections)
res1  <- build_rep_block(workout_log, lift, 1,  goal_1rm_anchor, date)
res3  <- build_rep_block(workout_log, lift, 3,  goal_1rm_anchor, date)
res10 <- build_rep_block(workout_log, lift, 10, goal_1rm_anchor, date)

# Estimates
lift_max1        <- res1$data;   lift_max1_model  <- res1$model
lift_max3        <- res3$data;   lift_max3_model  <- res3$model
lift_max10       <- res10$data;  lift_max10_model <- res10$model

# actual (for legend/lines)
lift_max1_actual  <- workout_log[workout_log$lift == lift & workout_log$reps == "1_rep_max", ]
lift_max3_actual  <- workout_log[workout_log$lift == lift & workout_log$reps == "3_rep_max", ]
lift_max10_actual <- workout_log[workout_log$lift == lift & workout_log$reps == "10_rep_max", ]

# If there is observed data today, do not show the red projected "today" points
has_today_1  <- any(is.finite(lift_max1_actual$weight_lbs)  & as.Date(lift_max1_actual$date)  == Sys.Date())
has_today_3  <- any(is.finite(lift_max3_actual$weight_lbs)  & as.Date(lift_max3_actual$date)  == Sys.Date())
has_today_10 <- any(is.finite(lift_max10_actual$weight_lbs) & as.Date(lift_max10_actual$date) == Sys.Date())

show_today_projection <- !(has_today_1 | has_today_3 | has_today_10)

###
### Current Projection
###
today <- Sys.Date()
gdate <- date

# derive goal weights for each rep from the 1RM anchor
g1  <- goal_1rm_anchor
g3  <- est_nrm(g1, 3)
g10 <- est_nrm(g1, 10)

# Monotone, capped, and plate-rounded adjustment to today's projection
lift_max1  <- adjust_projection(lift_max1,  goal_wt = g1,  k_actual = 3, mult = 0.75, min_step = 0, step_round = NULL)
lift_max3  <- adjust_projection(lift_max3,  goal_wt = g3,  k_actual = 3, mult = 0.75, min_step = 0, step_round = NULL)
lift_max10 <- adjust_projection(lift_max10, goal_wt = g10, k_actual = 3, mult = 0.75, min_step = 0, step_round = NULL)


# grab today's projections (fallback to nearest prior date if today not present)
get_today <- function(df) {
  # If we are not showing projections for today, return NA (so no red point)
  if (!show_today_projection) return(NA_real_)
  
  # find rows matching today's date
  idx <- which(as.Date(df$date) == today)
  # if no exact match, use the latest date before today
  if (length(idx) == 0) {
    idx <- max(which(as.Date(df$date) <= today), na.rm = TRUE)
  } else {
    idx <- max(idx, na.rm = TRUE)  # in case multiple matches
  }
  # if still invalid (no rows at all)
  if (length(idx) == 0 || !is.finite(idx)) return(NA_real_)
  
  as.numeric(df$predicted[idx])
}


pt1  <- get_today(lift_max1)
pt3  <- get_today(lift_max3)
pt10 <- get_today(lift_max10)

###
### Plot
###

###
### Plot predicted vs Actual for selected lift
###
ymin <- min(c(lift_max1$weight_lbs, lift_max3$weight_lbs, lift_max10$weight_lbs,
              lift_max1$predicted,  lift_max3$predicted,  lift_max10$predicted), na.rm = TRUE) - 50
ymax <- max(c(g1, lift_max1$predicted, lift_max3$predicted, lift_max10$predicted), na.rm = TRUE) + 50

plot(lift_max1$date, lift_max1$weight_lbs, 
     col = "white",
     ylim = c(ymin, ymax),
     main = paste0(plot_name(lift), " Goal Progression: Actual vs Projected"),
     xlab = "Date", ylab = "Weight lbs")

grid(nx = NULL, ny = NULL, lty = 1, col = "gray", lwd = .5)

lines(lift_max1$date,  lift_max1$predicted,  type = "l", lty = 1, lwd = 1, pch = 19, cex = .5, col = "darkgray")
lines(lift_max3$date,  lift_max3$predicted,  type = "l", lty = 5, lwd = 1, pch = 19, cex = .5, col = "darkgray")
lines(lift_max10$date, lift_max10$predicted, type = "l", lty = 3, lwd = 1, pch = 19, cex = .5, col = "darkgray")

lines(lift_max1_actual$date,  lift_max1_actual$weight_lbs,  type = "o", lty = 1, lwd = 2, pch = 19, cex = .5, col = "black")
lines(lift_max3_actual$date,  lift_max3_actual$weight_lbs,  type = "o", lty = 5, lwd = 2, pch = 19, cex = .5, col = "black")
lines(lift_max10_actual$date, lift_max10_actual$weight_lbs, type = "o", lty = 3, lwd = 2, pch = 19, cex = .5, col = "black")

if (show_today_projection) {
  points(today, pt1,  pch = 19, cex = 1, col = "firebrick")
  points(today, pt3,  pch = 19, cex = 1, col = "firebrick")
  points(today, pt10, pch = 19, cex = 1, col = "firebrick")
}

points(gdate, g1,   pch = 15, cex = 1, col = "forestgreen")
points(gdate, g3,   pch = 15, cex = 1, col = "forestgreen")
points(gdate, g10,  pch = 15, cex = 1, col = "forestgreen")

# Days remaining and Days Since Last Recorded Workout legend
days_remaining    <- as.numeric(as.Date(date) - Sys.Date())
last_workout_date <- max(workout_log[workout_log$lift == lift, "date", drop = TRUE], na.rm = TRUE)
days_since_last   <- as.numeric(Sys.Date() - as.Date(last_workout_date))

days_since_label <- if (!is.finite(days_since_last)) {
  "NA"
} else if (days_since_last == 0) {
  "Today"
} else {
  as.character(days_since_last)
}

legend("topleft",
       legend = c(paste0("Days Since Last Recorded Workout: ", days_since_label),
                  paste0("Days Until Goal: ",       days_remaining)),
       text.col = "black", cex = 0.9, bty = "n")

# last, today (observed OR projected), and goal weights (rounded up to nearest 2.5 lb)
round_up_2p5 <- function(x) {
  ifelse(is.na(x), NA, ceiling(x / 2.5) * 2.5)
}

# Helper: last observed value strictly BEFORE today
get_last_before_today <- function(df) {
  df$date <- as.Date(df$date)
  df <- df[is.finite(df$weight_lbs) & df$date < today, ]
  if (nrow(df) == 0) return(NA_real_)
  df <- df[order(df$date), ]
  tail(df$weight_lbs, 1)
}

# Helper: today's observed value (if any). If multiple today, take the max.
get_observed_today <- function(df) {
  df$date <- as.Date(df$date)
  vals <- df$weight_lbs[is.finite(df$weight_lbs) & df$date == today]
  if (!length(vals)) return(NA_real_)
  max(vals, na.rm = TRUE)
}

# "Last" (previous workout, not today)
last1  <- get_last_before_today(lift_max1_actual)
last3  <- get_last_before_today(lift_max3_actual)
last10 <- get_last_before_today(lift_max10_actual)

# "Today" = observed today if present, else projected today
obs1  <- get_observed_today(lift_max1_actual)
obs3  <- get_observed_today(lift_max3_actual)
obs10 <- get_observed_today(lift_max10_actual)

today1  <- if (is.finite(obs1))  obs1  else pt1
today3  <- if (is.finite(obs3))  obs3  else pt3
today10 <- if (is.finite(obs10)) obs10 else pt10

legend_table <- data.frame(
  "Last Lift (lb)" = c(round_up_2p5(last1),  round_up_2p5(last3),  round_up_2p5(last10)),
  "Today (lb)"     = c(round_up_2p5(today1), round_up_2p5(today3), round_up_2p5(today10)),
  "Goal (lb)"      = c(round_up_2p5(g1),     round_up_2p5(g3),     round_up_2p5(g10))
)

# Format numbers so whole numbers show as "400.0" (always 1 decimal)
fmt_1dp <- function(x) {
  ifelse(is.na(x), "NA", formatC(as.numeric(x), format = "f", digits = 1))
}

legend_labels <- apply(legend_table, 1, function(row)
  sprintf("%8s  %8s  %8s", fmt_1dp(row[1]), fmt_1dp(row[2]), fmt_1dp(row[3]))
)


legend("bottomright",
       legend = c("    Last:    Today:     Goal:  ", legend_labels),
       text.col = "black", cex = 0.9, bty = "n")

legend("bottom",
       legend = c("  1 Rep:", "  3 Rep:", " 10 Rep:"),
       col    = rep("black", 3),
       lty    = c(1,5,3), lwd = rep(1, 3),
       cex = 0.9, bty = "n")

# Open a PNG graphics device
 png(filename = file_path, width = 1200, height = 960, res = 180)

###
### Plot predicted vs Actual for selected lift
###
 ymin <- min(c(lift_max1$weight_lbs, lift_max3$weight_lbs, lift_max10$weight_lbs,
               lift_max1$predicted,  lift_max3$predicted,  lift_max10$predicted), na.rm = TRUE) - 50
 ymax <- max(c(g1, lift_max1$predicted, lift_max3$predicted, lift_max10$predicted), na.rm = TRUE) + 50
 
 plot(lift_max1$date, lift_max1$weight_lbs, 
      col = "white",
      ylim = c(ymin, ymax),
      main = paste0(plot_name(lift), " Goal Progression: Actual vs Projected"),
      xlab = "Date", ylab = "Weight lbs")
 
 grid(nx = NULL, ny = NULL, lty = 1, col = "gray", lwd = .5)
 
 lines(lift_max1$date,  lift_max1$predicted,  type = "l", lty = 1, lwd = 1, pch = 19, cex = .5, col = "darkgray")
 lines(lift_max3$date,  lift_max3$predicted,  type = "l", lty = 5, lwd = 1, pch = 19, cex = .5, col = "darkgray")
 lines(lift_max10$date, lift_max10$predicted, type = "l", lty = 3, lwd = 1, pch = 19, cex = .5, col = "darkgray")
 
 lines(lift_max1_actual$date,  lift_max1_actual$weight_lbs,  type = "o", lty = 1, lwd = 2, pch = 19, cex = .5, col = "black")
 lines(lift_max3_actual$date,  lift_max3_actual$weight_lbs,  type = "o", lty = 5, lwd = 2, pch = 19, cex = .5, col = "black")
 lines(lift_max10_actual$date, lift_max10_actual$weight_lbs, type = "o", lty = 3, lwd = 2, pch = 19, cex = .5, col = "black")
 
 if (show_today_projection) {
   points(today, pt1,  pch = 19, cex = 1, col = "firebrick")
   points(today, pt3,  pch = 19, cex = 1, col = "firebrick")
   points(today, pt10, pch = 19, cex = 1, col = "firebrick")
 }
 
 points(gdate, g1,   pch = 15, cex = 1, col = "forestgreen")
 points(gdate, g3,   pch = 15, cex = 1, col = "forestgreen")
 points(gdate, g10,  pch = 15, cex = 1, col = "forestgreen")
 
 # Days remaining and Days Since Last Recorded Workout legend
 days_remaining    <- as.numeric(as.Date(date) - Sys.Date())
 last_workout_date <- max(workout_log[workout_log$lift == lift, "date", drop = TRUE], na.rm = TRUE)
 days_since_last   <- as.numeric(Sys.Date() - as.Date(last_workout_date))
 
 days_since_label <- if (!is.finite(days_since_last)) {
   "NA"
 } else if (days_since_last == 0) {
   "Today"
 } else {
   as.character(days_since_last)
 }
 
 legend("topleft",
        legend = c(paste0("Days Since Last Recorded Workout: ", days_since_label),
                   paste0("Days Until Goal: ",       days_remaining)),
        text.col = "black", cex = 0.9, bty = "n")
 
 # last, today (observed OR projected), and goal weights (rounded up to nearest 2.5 lb)
 round_up_2p5 <- function(x) {
   ifelse(is.na(x), NA, ceiling(x / 2.5) * 2.5)
 }
 
 # Helper: last observed value strictly BEFORE today
 get_last_before_today <- function(df) {
   df$date <- as.Date(df$date)
   df <- df[is.finite(df$weight_lbs) & df$date < today, ]
   if (nrow(df) == 0) return(NA_real_)
   df <- df[order(df$date), ]
   tail(df$weight_lbs, 1)
 }
 
 # Helper: today's observed value (if any). If multiple today, take the max.
 get_observed_today <- function(df) {
   df$date <- as.Date(df$date)
   vals <- df$weight_lbs[is.finite(df$weight_lbs) & df$date == today]
   if (!length(vals)) return(NA_real_)
   max(vals, na.rm = TRUE)
 }
 
 # "Last" (previous workout, not today)
 last1  <- get_last_before_today(lift_max1_actual)
 last3  <- get_last_before_today(lift_max3_actual)
 last10 <- get_last_before_today(lift_max10_actual)
 
 # "Today" = observed today if present, else projected today
 obs1  <- get_observed_today(lift_max1_actual)
 obs3  <- get_observed_today(lift_max3_actual)
 obs10 <- get_observed_today(lift_max10_actual)
 
 today1  <- if (is.finite(obs1))  obs1  else pt1
 today3  <- if (is.finite(obs3))  obs3  else pt3
 today10 <- if (is.finite(obs10)) obs10 else pt10
 
 legend_table <- data.frame(
   "Last Lift (lb)" = c(round_up_2p5(last1),  round_up_2p5(last3),  round_up_2p5(last10)),
   "Today (lb)"     = c(round_up_2p5(today1), round_up_2p5(today3), round_up_2p5(today10)),
   "Goal (lb)"      = c(round_up_2p5(g1),     round_up_2p5(g3),     round_up_2p5(g10))
 )
 
 legend_labels <- apply(legend_table, 1, function(row)
   sprintf("%8s  %8s  %8s", row[1], row[2], row[3])
 )
 
 legend("bottomright",
        legend = c("    Last:   Today:    Goal:  ", legend_labels),
        text.col = "black", cex = 0.9, bty = "n")
 
 legend("bottom",
        legend = c("  1 Rep:", "  3 Rep:", " 10 Rep:"),
        col    = rep("black", 3),
        lty    = c(1,5,3), lwd = rep(1, 3),
        cex = 0.9, bty = "n")
 
dev.off()

