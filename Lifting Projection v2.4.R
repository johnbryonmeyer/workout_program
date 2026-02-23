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

############################
### Helpers / Conversions ###
############################

# Epley conversions
est_1rm <- function(w, reps) w * (1 + reps / 30)           # set -> 1RM
est_nrm <- function(one_rm, reps) one_rm / (1 + reps / 30) # 1RM -> nRM

# Parse reps label to integer
reps_to_n <- function(r) {
  if (length(r) != 1 || is.na(r)) stop("Reps must be a single, non-NA value.")
  
  if (is.numeric(r)) {
    rn <- as.integer(round(r))
    if (rn > 0) return(rn)
    stop("Numeric reps must be a positive integer.")
  }
  
  if (is.character(r)) {
    s <- trimws(tolower(r))
    s <- gsub("rep_max|rm|reps|max", "", s)
    s <- gsub("[^0-9]", "", s)
    if (nchar(s) > 0) {
      rn <- suppressWarnings(as.integer(s))
      if (is.finite(rn) && rn > 0) return(rn)
    }
    stop("Unknown reps spec: ", r)
  }
  
  stop("Unsupported reps type: ", typeof(r))
}

# Normalize goal to a 1RM anchor
goal_input_reps <- reps_to_n(reps)
goal_1rm_anchor <- if (goal_input_reps == 1) weight else est_1rm(weight, goal_input_reps)

# Pretty lift name for plot title
plot_name <- function(x) {
  x_clean <- gsub("^barbell_", "Barbell ", x)
  x_clean <- gsub("_", " ", x_clean)
  tools::toTitleCase(tolower(x_clean))
}

# Rounding used ONLY for legend display:
round_to_2p5  <- function(x) ifelse(is.na(x), NA_real_, round(x / 2.5) * 2.5)   # displayed value
round_to_1p25 <- function(x) ifelse(is.na(x), NA_real_, round(x / 1.25) * 1.25) # displayed CI/moe

#####################################
### Build series + fit projections ###
#####################################

build_rep_block <- function(wlog, lift_id, reps_n, goal_1rm, goal_date) {
  reps_lab <- paste0(reps_n, "_rep_max")
  
  actual <- wlog[wlog$lift == lift_id & wlog$reps == reps_lab, c("lift","reps","weight_lbs","date")]
  actual$date <- as.Date(actual$date)
  actual <- actual[order(actual$date), ]
  n_actual <- sum(is.finite(actual$weight_lbs))
  
  # Add today's placeholder only if not observed today
  has_today_observed <- any(is.finite(actual$weight_lbs) & actual$date == Sys.Date())
  if (has_today_observed) {
    block <- actual
  } else {
    block <- rbind(actual, data.frame(lift = lift_id, reps = reps_lab, weight_lbs = NA_real_, date = Sys.Date()))
  }
  
  # features
  block$weight_lag1 <- Lag(block$weight_lbs, +1)
  block$weight_lag2 <- Lag(block$weight_lbs, +2)
  block$trend       <- seq_len(nrow(block))
  block$trend_sq    <- block$trend^2
  
  # goal row
  goal_wt <- if (reps_n == 1) goal_1rm else est_nrm(goal_1rm, reps_n)
  goal_row <- data.frame(
    lift = lift_id,
    reps = reps_lab,
    weight_lbs = goal_wt,
    date = as.Date(goal_date),
    weight_lag1 = goal_wt,
    weight_lag2 = goal_wt,
    trend = nrow(block) + 1,
    trend_sq = (nrow(block) + 1)^2
  )
  block <- rbind(block, goal_row)
  block$date <- as.Date(block$date)
  
  # safe logs
  eps <- 1e-6
  block$w1 <- ifelse(is.finite(block$weight_lag1) & block$weight_lag1 > 0, block$weight_lag1, eps)
  block$w2 <- ifelse(is.finite(block$weight_lag2) & block$weight_lag2 > 0, block$weight_lag2, eps)
  
  # weights
  wt <- rep(1, nrow(block))
  wt[nrow(block)] <- 0.5  # downweight goal row
  
  actual_idx <- which(is.finite(block$weight_lbs) & block$date < Sys.Date())
  if (length(actual_idx) >= 1) wt[tail(actual_idx, 1)] <- 1.8
  if (length(actual_idx) >= 2) wt[tail(actual_idx, 2)[1]] <- 1.3
  
  # fit
  fit <- NULL
  have_mass <- requireNamespace("MASS", quietly = TRUE)
  
  if (n_actual >= 3) {
    fit <- if (have_mass) try(
      MASS::rlm(weight_lbs ~ date + trend_sq + log(w1) + log(w2), data = block, weights = wt),
      silent = TRUE
    ) else NULL
    if (inherits(fit, "try-error") || is.null(fit)) {
      fit <- lm(weight_lbs ~ date + trend_sq + log(w1) + log(w2), data = block, weights = wt)
    }
    block$predicted <- as.numeric(predict(fit, block))
    
  } else if (n_actual == 2) {
    fit <- if (have_mass) try(
      MASS::rlm(weight_lbs ~ date + trend_sq, data = block, weights = wt, psi = MASS::psi.huber),
      silent = TRUE
    ) else NULL
    if (inherits(fit, "try-error") || is.null(fit)) {
      fit <- lm(weight_lbs ~ date + trend_sq, data = block, weights = wt)
    }
    block$predicted <- as.numeric(predict(fit, block))
    
  } else {
    # bridge to goal
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

# Adjust ONLY today's projection (keeps model trajectory; snaps today's point to plates)
adjust_projection <- function(block, goal_wt,
                              k_actual = 3,
                              mult = 0.5,
                              max_step_abs = 25,
                              min_step = 0,
                              step_round = 2.5,
                              today_date = Sys.Date()) {
  
  if (!all(c("date","predicted","weight_lbs") %in% names(block))) return(block)
  
  o <- order(block$date)
  d <- as.Date(block$date[o])
  x <- as.numeric(block$predicted[o])
  y <- as.numeric(block$weight_lbs[o])
  
  if (length(x) < 2 || all(!is.finite(x))) { block$predicted[o] <- x; return(block) }
  
  idx_today <- which(d == today_date)
  if (length(idx_today) == 0) idx_today <- suppressWarnings(max(which(d <= today_date), na.rm = TRUE))
  if (!length(idx_today) || !is.finite(idx_today) || idx_today <= 1) { block$predicted[o] <- x; return(block) }
  i <- idx_today
  
  act_idx <- which(is.finite(y) & d <= today_date)
  if (length(act_idx) >= 2) {
    act_idx_recent <- tail(act_idx, k_actual)
    act_diff <- diff(y[act_idx_recent])
    base <- stats::median(act_diff[act_diff > 0], na.rm = TRUE)
    if (!is.finite(base) || base <= 0) {
      base <- stats::median(abs(act_diff), na.rm = TRUE)
      if (!is.finite(base) || base <= 0) base <- 2.5
    }
  } else {
    base <- 2.5
  }
  
  raw_inc <- x[i] - x[i - 1]
  cap <- min(mult * base, max_step_abs)
  if (is.finite(goal_wt)) cap <- min(cap, goal_wt - x[i - 1])
  
  inc <- max(min(raw_inc, cap), min_step)
  x[i] <- x[i - 1] + inc
  
  # snap today's point to plates (ONLY for the today row)
  if (!is.null(step_round) && is.finite(step_round) && step_round > 0) {
    x[i] <- ceiling(x[i] / step_round) * step_round
    if (is.finite(goal_wt)) x[i] <- min(x[i], goal_wt)
    x[i] <- max(x[i], x[i - 1])
  }
  
  block$predicted[o] <- x
  block
}

#############################
### Build 1/3/10RM series ###
#############################

res1  <- build_rep_block(workout_log, lift, 1,  goal_1rm_anchor, date)
res3  <- build_rep_block(workout_log, lift, 3,  goal_1rm_anchor, date)
res10 <- build_rep_block(workout_log, lift, 10, goal_1rm_anchor, date)

lift_max1 <- res1$data;  lift_max1_model  <- res1$model
lift_max3 <- res3$data;  lift_max3_model  <- res3$model
lift_max10 <- res10$data; lift_max10_model <- res10$model

lift_max1_actual  <- workout_log[workout_log$lift == lift & workout_log$reps == "1_rep_max", ]
lift_max3_actual  <- workout_log[workout_log$lift == lift & workout_log$reps == "3_rep_max", ]
lift_max10_actual <- workout_log[workout_log$lift == lift & workout_log$reps == "10_rep_max", ]

has_today_1  <- any(is.finite(lift_max1_actual$weight_lbs)  & as.Date(lift_max1_actual$date)  == Sys.Date())
has_today_3  <- any(is.finite(lift_max3_actual$weight_lbs)  & as.Date(lift_max3_actual$date)  == Sys.Date())
has_today_10 <- any(is.finite(lift_max10_actual$weight_lbs) & as.Date(lift_max10_actual$date) == Sys.Date())
show_today_projection <- !(has_today_1 | has_today_3 | has_today_10)

today <- Sys.Date()
gdate <- date

g1  <- goal_1rm_anchor
g3  <- est_nrm(g1, 3)
g10 <- est_nrm(g1, 10)

# days-since-last + recovery multiplier (unchanged logic)
get_days_since_last_observed <- function(df_actual, today = Sys.Date()) {
  d <- as.Date(df_actual$date)
  y <- as.numeric(df_actual$weight_lbs)
  ok <- is.finite(y) & is.finite(d) & d < today
  if (!any(ok)) return(Inf)
  as.numeric(today - max(d[ok], na.rm = TRUE))
}

recovery_multiplier <- function(days_since_last,
                                mult_min = 0.25,
                                mult_max = 0.70,
                                peak_day = 6,
                                left_width = 2,
                                right_width = 6) {
  if (!is.finite(days_since_last)) return(mult_min)
  d <- max(0, as.numeric(days_since_last))
  width <- if (d <= peak_day) left_width else right_width
  score <- exp(-((d - peak_day) / width)^2)
  mult <- mult_min + (mult_max - mult_min) * score
  if (d >= 14) mult <- max(mult_min, mult * 0.85)
  mult
}

m1  <- recovery_multiplier(get_days_since_last_observed(lift_max1_actual,  today))
m3  <- recovery_multiplier(get_days_since_last_observed(lift_max3_actual,  today))
m10 <- recovery_multiplier(get_days_since_last_observed(lift_max10_actual, today))

# Apply today's adjustment (model values remain numeric; today point snaps to 2.5)
lift_max1  <- adjust_projection(lift_max1,  goal_wt = g1,  k_actual = 3, mult = m1,  step_round = 2.5)
lift_max3  <- adjust_projection(lift_max3,  goal_wt = g3,  k_actual = 3, mult = m3,  step_round = 2.5)
lift_max10 <- adjust_projection(lift_max10, goal_wt = g10, k_actual = 3, mult = m10, step_round = 2.5)

# get today's point (for plotting)
get_today <- function(df) {
  if (!show_today_projection) return(NA_real_)
  idx <- which(as.Date(df$date) == today)
  if (length(idx) == 0) idx <- max(which(as.Date(df$date) <= today), na.rm = TRUE)
  if (!length(idx) || !is.finite(idx)) return(NA_real_)
  as.numeric(df$predicted[max(idx, na.rm = TRUE)])
}

pt1  <- get_today(lift_max1)
pt3  <- get_today(lift_max3)
pt10 <- get_today(lift_max10)

#####################################
### Projection confidence (95% CI) ###
#####################################

add_ci_cols <- function(df, fit, level = 0.95) {
  alpha <- 1 - level
  df$pred_se <- NA_real_
  df$pred_lo <- NA_real_
  df$pred_hi <- NA_real_
  
  if (is.null(fit)) return(df)
  
  tt <- stats::terms(fit)
  X  <- stats::model.matrix(tt, df)
  
  V <- try(stats::vcov(fit), silent = TRUE)
  if (inherits(V, "try-error") || any(!is.finite(V))) return(df)
  
  se <- sqrt(pmax(0, rowSums((X %*% V) * X)))
  
  crit <- if (inherits(fit, "lm")) stats::qt(1 - alpha / 2, df = fit$df.residual) else stats::qnorm(1 - alpha / 2)
  if (!is.finite(crit)) crit <- stats::qnorm(1 - alpha / 2)
  
  mu <- as.numeric(df$predicted)
  mu_fallback <- try(as.numeric(stats::predict(fit, newdata = df)), silent = TRUE)
  if (!inherits(mu_fallback, "try-error")) {
    need <- !is.finite(mu) & is.finite(mu_fallback)
    mu[need] <- mu_fallback[need]
  }
  
  ok <- is.finite(se) & is.finite(mu)
  df$pred_se[ok] <- se[ok]
  df$pred_lo[ok] <- mu[ok] - crit * se[ok]
  df$pred_hi[ok] <- mu[ok] + crit * se[ok]
  df
}

lift_max1  <- add_ci_cols(lift_max1,  lift_max1_model,  level = 0.95)
lift_max3  <- add_ci_cols(lift_max3,  lift_max3_model,  level = 0.95)
lift_max10 <- add_ci_cols(lift_max10, lift_max10_model, level = 0.95)

get_today_idx <- function(df, today = Sys.Date()) {
  idx <- which(as.Date(df$date) == today)
  if (length(idx) == 0) idx <- max(which(as.Date(df$date) <= today), na.rm = TRUE)
  if (!length(idx) || !is.finite(idx)) return(NA_integer_)
  max(idx, na.rm = TRUE)
}

# For PLOT: use exact model CI (no rounding).
# For LEGEND: round displayed mean to 2.5 and MOE to 1.25.
today_ci_plot <- function(df) {
  i <- get_today_idx(df, today = today)
  if (!is.finite(i)) return(list(lo = NA_real_, hi = NA_real_))
  list(lo = df$pred_lo[i], hi = df$pred_hi[i])
}

today_ci_legend <- function(df) {
  i <- get_today_idx(df, today = today)
  if (!is.finite(i)) return(list(mu = NA_real_, moe = NA_real_))
  lo <- df$pred_lo[i]; hi <- df$pred_hi[i]
  moe <- if (is.finite(lo) && is.finite(hi)) (hi - lo) / 2 else NA_real_
  list(mu = round_to_2p5(df$predicted[i]), moe = round_to_1p25(moe))
}

ci1_plot  <- today_ci_plot(lift_max1)
ci3_plot  <- today_ci_plot(lift_max3)
ci10_plot <- today_ci_plot(lift_max10)

ci1_leg  <- today_ci_legend(lift_max1)
ci3_leg  <- today_ci_legend(lift_max3)
ci10_leg <- today_ci_legend(lift_max10)

####################################
### Diagnostic Checks (optional) ###
####################################

diag_ci <- function(df, fit, label) {
  n_obs <- sum(is.finite(df$weight_lbs) & as.Date(df$date) < Sys.Date())
  i <- get_today_idx(df, today = Sys.Date())
  
  # mean-pred SE at today (what you are using)
  se_today <- if (is.finite(i)) df$pred_se[i] else NA_real_
  lo <- if (is.finite(i)) df$pred_lo[i] else NA_real_
  hi <- if (is.finite(i)) df$pred_hi[i] else NA_real_
  moe <- if (is.finite(lo) && is.finite(hi)) (hi - lo)/2 else NA_real_
  
  # model residual scale
  sigma_like <- NA_real_
  if (inherits(fit, "lm")) sigma_like <- summary(fit)$sigma
  if (inherits(fit, "rlm")) {
    # MASS::rlm stores scale; it’s not exactly lm sigma but good context
    sigma_like <- fit$s
  }
  
  data.frame(
    series = label,
    n_observed = n_obs,
    sigma_like = sigma_like,
    se_today = se_today,
    moe_95_mean = moe
  )
}

rbind(
  diag_ci(lift_max1,  lift_max1_model,  "1RM"),
  diag_ci(lift_max3,  lift_max3_model,  "3RM"),
  diag_ci(lift_max10, lift_max10_model, "10RM")
)

########################
### Smoothing helper ###
########################

smooth_pred_path <- function(df, n = 250, mono = TRUE) {
  x <- as.numeric(as.Date(df$date))
  y <- as.numeric(df$predicted)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  
  if (length(x) < 4) return(list(x = as.Date(x, origin = "1970-01-01"), y = y))
  
  ord <- order(x)
  x <- x[ord]; y <- y[ord]
  keep <- !duplicated(x)
  x <- x[keep]; y <- y[keep]
  
  x_grid <- seq(min(x), max(x), length.out = n)
  
  if (mono) {
    f <- stats::splinefun(x, y, method = "monoH.FC")
    y_grid <- f(x_grid)
  } else {
    fit <- stats::smooth.spline(x, y)
    y_grid <- stats::predict(fit, x_grid)$y
  }
  
  list(x = as.Date(x_grid, origin = "1970-01-01"), y = y_grid)
}

############
### Plot ###
############

plot_goal_progress <- function(file_path = NULL, width = 1200, height = 960, res = 180) {
  if (!is.null(file_path)) {
    dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
    png(filename = file_path, width = width, height = height, res = res)
    on.exit(dev.off(), add = TRUE)
  }
  
  ymin <- min(c(lift_max1$weight_lbs, lift_max3$weight_lbs, lift_max10$weight_lbs,
                lift_max1$predicted,  lift_max3$predicted,  lift_max10$predicted), na.rm = TRUE) - 50
  
  ymax <- max(c(g1, lift_max1$predicted, lift_max3$predicted, lift_max10$predicted), na.rm = TRUE) + 50
  
  plot(lift_max1$date, lift_max1$weight_lbs,
       col = "white",
       ylim = c(ymin, ymax),
       main = paste0(plot_name(lift), " Goal Progression: Actual vs Projected"),
       xlab = "Date", ylab = "Weight lbs")
  
  grid(nx = NULL, ny = NULL, lty = 1, col = "gray", lwd = .5)
  
  p1  <- smooth_pred_path(lift_max1,  n = 250, mono = TRUE)
  p3  <- smooth_pred_path(lift_max3,  n = 250, mono = TRUE)
  p10 <- smooth_pred_path(lift_max10, n = 250, mono = TRUE)
  
  lines(p1$x,  p1$y,  lty = 1, lwd = 1, col = "darkgray")
  lines(p3$x,  p3$y,  lty = 5, lwd = 1, col = "darkgray")
  lines(p10$x, p10$y, lty = 3, lwd = 1, col = "darkgray")
  
  lines(lift_max1_actual$date,  lift_max1_actual$weight_lbs,  type = "o", lty = 1, lwd = 2, pch = 19, cex = .5, col = "black")
  lines(lift_max3_actual$date,  lift_max3_actual$weight_lbs,  type = "o", lty = 5, lwd = 2, pch = 19, cex = .5, col = "black")
  lines(lift_max10_actual$date, lift_max10_actual$weight_lbs, type = "o", lty = 3, lwd = 2, pch = 19, cex = .5, col = "black")
  
  # T-bar for plot: uses exact model CI (no rounding)
  draw_tbar <- function(x, y, lo, hi, col = "darkgrey", lwd = 2, cap = 0.35) {
    if (!is.finite(y) || !is.finite(lo) || !is.finite(hi)) return(invisible(NULL))
    segments(x, lo, x, hi, col = col, lwd = lwd)
    segments(x - cap, lo, x + cap, lo, col = col, lwd = lwd)
    segments(x - cap, hi, x + cap, hi, col = col, lwd = lwd)
  }
  
  if (show_today_projection) {
    
    draw_tbar(today, pt1,  ci1_plot$lo,  ci1_plot$hi)
    draw_tbar(today, pt3,  ci3_plot$lo,  ci3_plot$hi)
    draw_tbar(today, pt10, ci10_plot$lo, ci10_plot$hi)
    
    points(today, pt1,  pch = 19, cex = 1, col = "firebrick")
    points(today, pt3,  pch = 19, cex = 1, col = "firebrick")
    points(today, pt10, pch = 19, cex = 1, col = "firebrick")
  
  }
  
  points(gdate, g1,  pch = 15, cex = 1, col = "forestgreen")
  points(gdate, g3,  pch = 15, cex = 1, col = "forestgreen")
  points(gdate, g10, pch = 15, cex = 1, col = "forestgreen")
  
  # Top-left legend: days
  days_remaining    <- as.numeric(as.Date(date) - Sys.Date())
  last_workout_date <- max(workout_log[workout_log$lift == lift, "date", drop = TRUE], na.rm = TRUE)
  days_since_last   <- as.numeric(Sys.Date() - as.Date(last_workout_date))
  days_since_label <- if (!is.finite(days_since_last)) "NA" else if (days_since_last == 0) "Today" else as.character(days_since_last)
  
  legend("topleft",
         legend = c(paste0("Days Since Last Recorded Workout: ", days_since_label),
                    paste0("Days Until Goal: ", days_remaining)),
         text.col = "black", cex = 0.9, bty = "n")
  
  # Bottom-right legend table values
  get_last_before_today <- function(df) {
    df$date <- as.Date(df$date)
    df <- df[is.finite(df$weight_lbs) & df$date < today, ]
    if (nrow(df) == 0) return(NA_real_)
    df <- df[order(df$date), ]
    tail(df$weight_lbs, 1)
  }
  
  get_observed_today <- function(df) {
    df$date <- as.Date(df$date)
    vals <- df$weight_lbs[is.finite(df$weight_lbs) & df$date == today]
    if (!length(vals)) return(NA_real_)
    max(vals, na.rm = TRUE)
  }
  
  last1  <- get_last_before_today(lift_max1_actual)
  last3  <- get_last_before_today(lift_max3_actual)
  last10 <- get_last_before_today(lift_max10_actual)
  
  obs1  <- get_observed_today(lift_max1_actual)
  obs3  <- get_observed_today(lift_max3_actual)
  obs10 <- get_observed_today(lift_max10_actual)
  
  # Formatting for legend:
  # - weights shown with 1 decimal always
  # - MOE shown with 2 decimals always
  fmt_wt_1dp <- function(x) {
    nx <- suppressWarnings(as.numeric(x))
    if (!is.finite(nx)) return("NA")
    formatC(nx, format = "f", digits = 1)
  }
  fmt_moe_2dp <- function(x) {
    nx <- suppressWarnings(as.numeric(x))
    if (!is.finite(nx)) return("NA")
    formatC(nx, format = "f", digits = 2)
  }
  
  today_cell <- function(obs, ci_leg) {
    if (is.finite(obs)) {
      fmt_wt_1dp(round_to_2p5(obs))
    } else {
      paste0(fmt_wt_1dp(ci_leg$mu), " \u00B1 ", fmt_moe_2dp(ci_leg$moe))
    }
  }
  
  legend_table <- data.frame(
    "Last Lift (lb)" = c(fmt_wt_1dp(round_to_2p5(last1)),  fmt_wt_1dp(round_to_2p5(last3)),  fmt_wt_1dp(round_to_2p5(last10))),
    "Today (lb)"     = c(today_cell(obs1, ci1_leg),        today_cell(obs3, ci3_leg),        today_cell(obs10, ci10_leg)),
    "Goal (lb)"      = c(fmt_wt_1dp(round_to_2p5(g1)),     fmt_wt_1dp(round_to_2p5(g3)),     fmt_wt_1dp(round_to_2p5(g10))),
    check.names = FALSE
  )
  
  legend_labels <- apply(legend_table, 1, function(row)
    sprintf("%8s  %18s  %8s", row[1], row[2], row[3])
  )
  
  legend("bottomright",
         legend = c("   Last:       Today:               Goal: ", legend_labels),
         text.col = "black", cex = 0.9, bty = "n")
  
  legend("bottom",
         legend = c("  1 Rep:  ", "  3 Rep:  ", " 10 Rep:  "),
         col    = rep("black", 3),
         lty    = c(1, 5, 3), lwd = rep(1, 3),
         cex = 0.9, bty = "n")
  
  invisible(NULL)
}

# Display in R plot window
plot_goal_progress()

# Save to file
plot_goal_progress(file_path = file_path)
