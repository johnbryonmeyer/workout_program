##################################
### Randomized Workout Program ###
##################################


###
### Download Required Packages
###

if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(stargazer)) install.packages("stargazer", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")

######################
### Select Workout ###
######################
available_equipment <- c("barbell","bench","dumbbell","rack","none",
                         "kettlebell","battle rope","jump rope","heavy rope",
                         "gloves","bike","rowing machine","ab_wheel",
                         "dip_bars","machine","ez_bar","tricep_bar","cable",
                         "bands","vest","belt","pull-up bar","trap_bar")
                 #From:  battle rope, jump rope, heavy rope, gloves, bike, 
                         #rowing machine, ab_wheel, bar, dip_bars, machine, 
                         #ez_bar, tricep_bar, cable, band, plate, vest, belt,
                         #pull-up bar, trap_bar

target_type         <- c("Chest") 
                 #From:  Chest, Back, Legs, Shoulders, Bicep, Arms, Push, 
                         #Pull, Full Body

intensity_name      <- "Medium"
                 #From: Easy, Medium, Hard, Heavy, Light, Quick

# Workout table file destination
output_dir <-  "~/downloads"

# Table file name
file_name <- paste0(
  "Workout_",
  target_type, "_", 
  intensity_name, "_", 
  format(Sys.Date(), "%Y-%m-%d"),
  ".html")

# Full file path
table_destination_html <- file.path(output_dir, file_name)

###
### Type of Workout
###
exercise_lib <- tribble(
  ~exercise,                     ~type,                                         ~role,             ~equipment,                                  ~category,
  
  # Warmup / Conditioning
  "Battle Rope",                 list("Conditioning"),                          list("multi"),     list("battle rope"),                          list("warmup","power"),
  "Speed Rope / Double Unders",  list("Conditioning"),                          list("multi"),     list("jump rope"),                            list("warmup","cardio"),
  "Heavy Jump Rope",             list("Conditioning"),                          list("multi"),     list("heavy rope"),                           list("warmup","cardio"),
  "Burpees",                     list("Conditioning"),                          list("multi"),     list("none"),                                 list("warmup","cardio"),
  "Boxing",                      list("Conditioning"),                          list("multi"),     list("gloves"),                               list("cardio","power"),
  "Biking",                      list("Conditioning"),                          list("multi"),     list("bike"),                                 list("cardio","endurance"),
  "Rowing",                      list("Conditioning"),                          list("multi"),     list("rowing machine"),                       list("cardio","endurance"),
  "Jogging",                     list("Conditioning"),                          list("multi"),     list("none"),                                 list("cardio","endurance"),
  "Running",                     list("Conditioning"),                          list("multi"),     list("none"),                                 list("cardio","endurance"),
  
  # Core
  "Ab Rollouts",                 list("Core"),                                  list("primary"),   list("ab_wheel","barbell"),                   list("core","anti_extension"),
  "Low to High Plank",           list("Core"),                                  list("primary"),   list("none"),                                 list("core","stability"),
  "Side-to-Side Crunches",       list("Core"),                                  list("secondary"), list("none"),                                 list("core","isolation"),
  "Sit Ups",                     list("Core"),                                  list("secondary"), list("none"),                                 list("core","isolation"),
  "V Sit Ups",                   list("Core"),                                  list("secondary"), list("none"),                                 list("core","isolation"),
  "Bicycles",                    list("Core"),                                  list("secondary"), list("none"),                                 list("core","isolation"),
  "Mountain Climbers",           list("Core"),                                  list("multi"),     list("none"),                                 list("core","cardio"),
  "Kettlebell Russian Twists",   list("Core"),                                  list("secondary"), list("kettlebell","dumbbell"),                list("core","rotation"),
  "Wood Splitters",              list("Core"),                                  list("secondary"), list("cable","bands"),                         list("core","rotation"),
  "Reverse Crunches",            list("Core"),                                  list("secondary"), list("none"),                                 list("core","isolation"),
  "Hanging Leg Raises",          list("Core"),                                  list("primary"),   list("bar"),                                  list("core","hip_flexion"),
  "Leg Lifts to Flutter Kicks",  list("Core"),                                  list("secondary"), list("none"),                                 list("core","isolation"),
  "Kettlebell Swings",           list("Full Body","Back","Pull"),               list("compound"),  list("kettlebell"),                           list("power","conditioning"),
  
  # Chest / Push
  "Barbell Bench Press",         list("Chest","Push"),                          list("compound"),  list("barbell","bench"),                     list("strength"),
  "Dumbbell Bench Press",        list("Chest","Push"),                          list("compound"),  list("dumbbell","bench"),                    list("strength"),
  "Incline Bench Press",         list("Chest","Push"),                          list("primary"),   list("barbell","bench"),                     list("strength","hypertrophy"),
  "Incline Dumbbell Press",      list("Chest","Push"),                          list("primary"),   list("dumbbell","bench"),                    list("hypertrophy"),
  "Close Grip Bench Press",      list("Chest","Push","Arms"),                   list("primary"),   list("barbell","bench"),                     list("strength","triceps_focus"),
  "Decline Bench Press",         list("Chest","Push"),                          list("primary"),   list("barbell","bench"),                     list("strength"),
  "Push Ups",                    list("Chest","Push"),                          list("secondary"), list("none"),                                list("conditioning"),
  "Weighted Push Ups",           list("Chest","Push"),                          list("primary"),   list("none","vest"),                         list("strength"),
  "High Cable Flies",            list("Chest","Push"),                          list("secondary"), list("cable"),                               list("isolation"),
  "Low Cable Flies",             list("Chest","Push"),                          list("secondary"), list("cable"),                               list("isolation"),
  "Flat Dumbbell Flies",         list("Chest","Push"),                          list("secondary"), list("dumbbell","bench"),                    list("isolation"),
  "Incline Dumbbell Flies",      list("Chest","Push"),                          list("secondary"), list("dumbbell","bench"),                    list("isolation"),
  "High Hex Press",              list("Chest","Push"),                          list("secondary"), list("dumbbell","bench"),                    list("isolation"),
  "Chest Dips",                  list("Chest","Push","Arms"),                   list("primary"),   list("dip_bars","none"),                     list("strength"),
  
  # Shoulders / Push
  "Shoulder Press",              list("Shoulders","Push"),                      list("compound"),  list("barbell","dumbbell","machine"),        list("strength"),
  "Overhead Press",              list("Shoulders","Push"),                      list("compound"),  list("barbell","dumbbell"),                  list("strength"),
  "Arnold Press",                list("Shoulders","Push"),                      list("primary"),   list("dumbbell"),                            list("hypertrophy"),
  "Alternate Dumbbell Front Raise", list("Shoulders","Push"),                   list("secondary"), list("dumbbell"),                            list("isolation"),
  "Lateral Raise",               list("Shoulders","Push"),                      list("secondary"), list("dumbbell","cable"),                    list("isolation"),
  "Upright Kettlebell Row",      list("Shoulders","Push","Back","Pull"),        list("primary"),   list("kettlebell"),                          list("hypertrophy"),
  "Cable Face Pulls",            list("Back","Shoulders","Pull"),               list("secondary"), list("cable","rope"),                        list("rear_delts","isolation"),
  "Back Barbell Shoulder Press", list("Shoulders","Push"),                      list("secondary"), list("barbell"),                             list("strength"),
  
  # Back / Pull
  "Barbell Row",                 list("Back","Pull"),                           list("compound"),  list("barbell"),                             list("strength"),
  "Single Arm Dumbbell Row",     list("Back","Pull"),                           list("primary"),   list("dumbbell","bench"),                    list("hypertrophy"),
  "Lat Pulldown",                list("Back","Pull"),                           list("primary"),   list("machine","cable"),                     list("hypertrophy"),
  "Pull Ups",                    list("Back","Pull"),                           list("compound"),  list("none","pull-up bar"),                  list("strength"),
  "Chin Ups",                    list("Back","Pull","Arms"),                    list("primary"),   list("none","pull-up bar"),                  list("strength"),
  "Cable Rows",                  list("Back","Pull"),                           list("primary"),   list("cable","machine"),                     list("hypertrophy"),
  "Close Grip Rows",             list("Back","Pull"),                           list("secondary"), list("cable","machine"),                     list("hypertrophy"),
  "Dumbbell Shrugs",             list("Back","Shoulders"),                      list("secondary"), list("dumbbell"),                            list("traps","isolation"),
  
  # Arms (Biceps/Triceps)
  "Barbell Curls",               list("Arms","Bicep","Pull"),                   list("primary"),   list("barbell"),                              list("hypertrophy"),
  "EZ Bar Curls",                list("Arms","Bicep","Pull"),                   list("primary"),   list("ez_bar"),                               list("hypertrophy"),
  "Wide EZ Bar Curls",           list("Arms","Bicep","Pull"),                   list("primary"),   list("ez_bar"),                               list("hypertrophy"),
  "Dumbbell Hammer Curls",       list("Arms","Bicep","Pull"),                   list("primary"),   list("dumbbell"),                             list("hypertrophy"),
  "EZ Bar Preacher Curls",       list("Arms","Bicep","Pull"),                   list("primary"),   list("ez_bar","bench"),                       list("hypertrophy"),
  "Close EZ Bar Curls",          list("Arms","Bicep","Pull"),                   list("secondary"), list("ez_bar"),                               list("hypertrophy"),
  "Reverse Barbell Curls",       list("Arms","Bicep","Pull","Forearms"),        list("secondary"), list("barbell"),                              list("hypertrophy"),
  "Forearm Front Barbell Curls", list("Arms","Forearms"),                       list("secondary"), list("barbell"),                              list("isolation"),
  "Forearm Front Dumbbell Curls",list("Arms","Forearms"),                       list("secondary"), list("dumbbell"),                             list("isolation"),
  "Barbell Skull Crushers",      list("Arms","Push","Chest"),                   list("secondary"), list("barbell","bench"),                      list("triceps","isolation"),
  "EZ Bar Skull Crushers",       list("Arms","Push","Chest"),                   list("secondary"), list("ez_bar","bench"),                       list("triceps","isolation"),
  "Tricep Bar Skull Crushers",   list("Arms","Push","Chest"),                   list("secondary"), list("tricep_bar","bench"),                   list("triceps","isolation"),
  "Cable Tricep Pulls",          list("Arms","Push"),                           list("secondary"), list("cable","rope"),                         list("triceps","isolation"),
  "Weighted Dips",               list("Arms","Chest","Push"),                   list("primary"),   list("dip_bars","belt","vest"),               list("strength"),
  "Diamond Pushups",             list("Arms","Chest","Push"),                   list("secondary"), list("none"),                                 list("triceps_focus"),
  
  # Legs
  "Squat",                       list("Legs","Push"),                           list("compound"),  list("barbell","rack"),                       list("strength"),
  "Deadlifts",                   list("Legs","Back","Pull","Full Body"),        list("compound"),  list("barbell"),                              list("strength"),
  "Barbell Hip-Thrusts",         list("Legs","Push"),                           list("compound"),  list("barbell","bench"),                      list("glutes","strength"),
  "Bulgarian Split Squat",       list("Legs","Push","Balance"),                 list("primary"),   list("dumbbell","none","bench"),              list("hypertrophy","balance"),
  "Single Dumbbell Cleans",       list("Full Body","Legs","Pull"),               list("primary"),   list("dumbbell"),                             list("power"),
  "Zercher Squat",               list("Legs","Push"),                           list("primary"),   list("barbell"),                              list("strength"),
  "Leg Extensions",              list("Legs","Push"),                           list("secondary"), list("machine"),                              list("isolation"),
  "Quad Curls",                  list("Legs","Push"),                           list("secondary"), list("machine"),                              list("isolation"),
  "Calf Raises",                 list("Legs"),                                  list("secondary"), list("machine","none","dumbbell"),            list("isolation"),
  "Farmer Carry",                list("Full Body","Grip"),                      list("multi"),     list("dumbbell","kettlebell","trap_bar"),     list("conditioning","strength"),
  "Kneel to Squat Jump",         list("Legs","Full Body"),                      list("secondary"), list("none"),                                 list("plyometric"),
  "Lunge Hinges",                list("Legs","Push"),                           list("primary"),   list("dumbbell","none"),                      list("hypertrophy"),
  "Goblet Squat",                list("Legs","Push"),                           list("secondary"), list("dumbbell","kettlebell"),                list("technique","hypertrophy"),
  "Squat Pulses",                list("Legs","Push"),                           list("secondary"), list("none"),                                 list("burnout"),
  "Pistol Squat",                list("Legs","Push","Balance"),                 list("primary"),   list("none"),                                 list("skill","balance"),
  "Dumbbell Deadlifts",          list("Legs","Back","Pull"),                    list("primary"),   list("dumbbell"),                             list("strength"),
  "Alternating Light Lunges",    list("Legs","Push"),                           list("secondary"), list("none","dumbbell"),                      list("conditioning"),
  "Dumbbell Squat Press",        list("Full Body","Push"),                      list("primary"),   list("dumbbell"),                             list("power","conditioning"),
  "Banded Squats",               list("Legs","Push"),                           list("secondary"), list("bands"),                                 list("activation"),
  
  # Push (additional)
  "Landmine Press",              list("Push","Shoulders","Chest"),              list("primary"),   list("barbell","landmine"),                   list("strength","shoulders"),
  "Close Dumbbell Press",        list("Chest","Push"),                          list("primary"),   list("dumbbell","bench"),                     list("triceps_focus"),
  
  # Pull (additional)
  "Upright Barbell Row",         list("Back","Shoulders","Pull"),               list("primary"),   list("barbell"),                              list("hypertrophy"),
  "Band Pulls",                  list("Back","Shoulders","Pull"),               list("secondary"), list("bands"),                                 list("activation"),
  
  # Full Body
  "Bench Press",                 list("Chest","Push","Full Body"),              list("compound"),  list("barbell","bench"),                      list("strength"),
  "Barbell Clean",               list("Full Body","Pull","Push"),               list("compound"),  list("barbell"),                              list("power","strength"),
  "Barbell Clean & Jerk",        list("Full Body","Pull","Push"),               list("compound"),  list("barbell"),                              list("power"),
  "Dumbbell Pushup Rows",        list("Full Body","Pull","Push"),               list("multi"),     list("dumbbell"),                             list("conditioning"),
  "Barbell Row to Shoulder Press", list("Full Body","Push","Pull"),             list("multi"),     list("barbell"),                              list("power"),
  "Dumbbell Curl to Shoulder Press", list("Full Body","Push","Arms"),           list("multi"),     list("dumbbell"),                             list("power")
)

###
### Intensity Definitions
###

Easy <- c("6:00",
          "8-12 X3",
          "8-12 X3","8-10 X3","8-10 X3",
          "Max X3",
          "1:00 X3","1:00 X3",
          "15:00")

Medium <- c("8:00",
            "10-12 X3",
            "10-12 X3","10-12 X3","10-12 X3",
            "10-12 X3","10-12 X3",
            "Max X3",
            "1:30 X3","1:30 X3",
            "20:00")

Hard <- c("10:00",
          "8-12 X4",
          "8-12 X4","8-12 X4","8-12 X4","8-12 X4",
          "8-12 X4","8-12 X4",
          "Max X4",
          "2:00 X4","2:00 X4",
          "25:00")

Heavy <- c("8:00",
           "8-10 X3",
           "8-10 X3","8-10 X3","8-10 X3",
           "8-10 X3",
           "Max X3",
           "1:00 X3","1:00 X3",
           "15:00")

Light <- c("10:00",
           "10-15 X4",
           "10-15 X4","10-15 X4","10-15 X4",
           "10-15 X4","10-15 X4",
           "Max X4",
           "2:30 X4","2:30 X4",
           "20:00")

Quick <- c("5:00",
           "8-10 X3",
           "8-10 X3","8-10 X3","8-10 X3",
           "Max X3",
           "1:00 X3","1:00 X3",
           "10:00")

intensity_list <- list(
  Easy = Easy,
  Medium = Medium,
  Hard = Hard,
  Heavy = Heavy,
  Light = Light,
  Quick = Quick
)

intensity_list <- list(Easy=Easy, Medium=Medium, Hard=Hard, Heavy=Heavy, Light=Light, Quick=Quick)

###########################################
### Workout builder using tagged library ###
###########################################
# Helper: sample 'n' unique items safely from a vector (≤ length)
sample_unique <- function(x, n) {
  x <- unique(stats::na.omit(x))
  if (length(x) == 0) return(character(0))
  x[sample.int(length(x), size = min(n, length(x)), replace = FALSE)]
}

Workout <- function(exercise_lib,
                    intensity_name,
                    target_type = "Chest",
                    available_equipment = character()) {
  
  # Build intensity vector
  if (!intensity_name %in% names(intensity_list)) {
    stop("Unknown intensity_name. Choose one of: ", paste(names(intensity_list), collapse = ", "))
  }
  intensity_vec <- intensity_list[[intensity_name]]
  N <- length(intensity_vec)
  
  # Normalize target_type (vector ok)
  target_type <- as.character(target_type)
  
  # Optional equipment filtering
  lib_filt <- exercise_lib
  if (length(available_equipment)) {
    lib_filt <- lib_filt %>%
      filter(purrr::map_lgl(equipment, ~ any(. %in% available_equipment)))
  }
  
  # Internal helper: pick by role + type, exclude already chosen
  pick_by <- function(role_tag, type_vec = target_type, n = 1, exclude = character()) {
    pool <- lib_filt %>%
      filter(purrr::map_lgl(role, ~ role_tag %in% .)) %>%
      filter(purrr::map_lgl(type, ~ any(type_vec %in% .))) %>%
      filter(!(exercise %in% exclude))
    sample_unique(pool$exercise, n)
  }
  
  workout <- rep(NA_character_, N)
  
  # Warmup (category contains "warmup")
  warmup_pool <- lib_filt %>% filter(purrr::map_lgl(category, ~ "warmup" %in% .))
  workout[1] <- sample_unique(warmup_pool$exercise, 1)
  
  # Initial compound lift
  chosen <- na.omit(workout)
  workout[2] <- pick_by("compound", n = 1, exclude = chosen)
  chosen <- na.omit(workout)
  
  # Superset pair 1: primary + secondary
  w3 <- pick_by("primary",  n = 1, exclude = chosen)
  chosen <- c(chosen, w3)
  w4 <- pick_by("secondary", n = 1, exclude = chosen)
  workout[3] <- w3; workout[4] <- w4
  chosen <- na.omit(workout)
  
  # Superset pair 2
  w5 <- pick_by("primary",  n = 1, exclude = chosen)
  chosen <- c(chosen, w5)
  w6 <- pick_by("secondary", n = 1, exclude = chosen)
  workout[5] <- w5; workout[6] <- w6
  chosen <- na.omit(workout)
  
  # Additional primaries if long session
  if (N > 10) {
    extra_n <- N - 10
    workout[7:(N-4)] <- pick_by("primary", n = extra_n, exclude = chosen)
    chosen <- na.omit(workout)
  }
  
  # Final compound
  workout[N-3] <- pick_by("compound", n = 1, exclude = chosen)
  chosen <- na.omit(workout)
  
  # Core (type includes "Core")
  core_pool <- lib_filt %>% filter(purrr::map_lgl(type, ~ "Core" %in% .)) %>% filter(!(exercise %in% chosen))
  workout[(N-2):(N-1)] <- sample_unique(core_pool$exercise, 2)
  chosen <- na.omit(workout)
  
  # Cardio (category contains "cardio")
  cardio_pool <- lib_filt %>% filter(purrr::map_lgl(category, ~ "cardio" %in% .)) %>% filter(!(exercise %in% chosen))
  workout[N] <- sample_unique(cardio_pool$exercise, 1)
  
  # Labels
  details <- c(
    "Warmup",
    "Initial Compound Lift",
    "Superset With Following Workout",
    "Superset With Above",
    "Superset With Following Workout",
    "Superset With Above",
    rep("Additional Workout", max(0, N - 10)),
    "Final Compound Lift (To Failure)",
    "Core One (Superset)",
    "Core Two (Superset)",
    "Conditioning/Warmdown"
  )
  
  tibble(
    Workout   = workout,
    `Time/Reps` = intensity_vec,
    Details   = details
  )
}

######################
### Generate workout
######################

Workout_output <- Workout(
  exercise_lib = exercise_lib,
  intensity_name = intensity_name,
  target_type = target_type,                 # can be vector, e.g., c("Chest","Push")
  available_equipment = available_equipment  # filters library by equipment tags
)

####################
### Create Table  ###
####################
stargazer(Workout_output, type = "text", title="Today's Workout", summary = FALSE, out = "Workout_table.txt")

dtable <- datatable(
  Workout_output,
  options = list(pageLength = length(Workout_output$Workout), autoWidth = TRUE)
)

htmlwidgets::saveWidget(dtable, table_destination_html)
cat("Saved to: ", table_destination_html, "\n")