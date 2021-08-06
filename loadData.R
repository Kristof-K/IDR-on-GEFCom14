library(lubridate)
library(dplyr)

# path to the directory containing the GEFCom14 data
PATH <- "D:\\Studium\\Semester6\\BachelorArbeit\\GEFCom2014_Data"
CSV <- ".csv"
SLASH <- "\\"
JOIN <- c("TIMESTAMP", "ZONEID")

# load data track of the given task (task must be a number bewtween 1 and 15)
# the return value is a named list containing the elements "LastTrain_TS" (time
# stamp belonging to the last test entry), "Zones" (list of Zones) and then the
# elements listed in "Zones" containing the actual data
loadSet <- function(track, task) {
  if (task < 1 || task > 15) {
    return(data.frame(ERROR=c("unknown task number:", task)))
  }
  
  # now find out which track has to be loaded
  data <- switch(track,
                "Solar" = loadSolar(task),
                "Wind" = loadWind(task),
                "Load" = loadLoad(task),
                "Price" = loadPrice(task),
                data.frame(ERROR=c("unknwon track", track)))
  return(data)
}


# LOAD SOLAR -------------------------------------------------------------------

loadSolar <- function(task) {
  track <- "Solar"
  subfolder <- paste("Task", task)
  zones <- c("Zone1", "Zone2", "Zone3")
  
  # file containing explanatory and response (during train and test) variables
  predictors <- paste0(subfolder, SLASH, "predictors", task, CSV)
  targetVariable <- paste0(subfolder, SLASH, "train", task, CSV)
  if (task < 15) {
    observation <- paste0("Task ", (task+1), SLASH, "train", (task+1), CSV)
  } else {
    observation <- "Solution\ to\ Task 15\\Solution\ to\ Task\ 15.csv"
  }
  # read all necessary data and transform timestamps into clear format
  X <- read.csv(paste(PATH, track, predictors, sep=SLASH))
  X$TIMESTAMP <- ymd_hm(X$TIMESTAMP)
  Y_train <- read.csv(paste(PATH, track, targetVariable, sep=SLASH))
  Y_train$TIMESTAMP <- ymd_hm(Y_train$TIMESTAMP)
  Y_test <- read.csv(paste(PATH, track, observation, sep=SLASH))
  Y_test$TIMESTAMP <- ymd_hm(Y_test$TIMESTAMP)
  # now wrangle data
  if (task == 15) {  # in task 15 X has own POWER column, but with less decimals
    X <- X[names(X) != "POWER"]
  }
  t_test <- interval(tail(Y_train$TIMESTAMP, 1), tail(X$TIMESTAMP, 1))
  output <- list("Zones" = zones)

  train <- X %>% right_join(Y_train, by=JOIN) %>% rename(TARGET=POWER) %>%
    group_by(ZONEID) %>% group_split(.keep=FALSE)
  test <- X %>% filter(TIMESTAMP %within% t_test) %>%
    left_join(Y_test, by=JOIN) %>% rename(TARGET=POWER) %>% group_by(ZONEID) %>%
    group_split(.keep=FALSE)

  for (i in 1:length(zones)) {
    output[[zones[i]]] <- list(Train = as.data.frame(train[[i]]),
                               Test = as.data.frame(test[[i]]))
  }
  return(output)
}


# LOAD WIND --------------------------------------------------------------------

loadWind <- function(task) {
  track <- "Wind"
  subfolder <-  paste0(PATH, SLASH, track, SLASH, "Task\ ", task, SLASH)
  zones <- paste0("Zone", 1:10)

  train_files <- paste0(subfolder, "Task", task, "_W_Zone1_10", SLASH, "Task",
                        task, "_W_Zone")
  testX_files <- paste0(subfolder, "TaskExpVars", task, "_W_Zone1_10", SLASH,
                        "TaskExpVars", task, "_W_Zone")
  testY_files <- paste0(PATH,SLASH, track,SLASH, "Task\ ", task+1,SLASH, "Task",
                        task+1, "_W_Zone1_10", SLASH, "Task", task+1, "_W_Zone")
  output <- list("Zones" = zones)
  # now fetch data
  for(z in 1:length(zones)) {
    train <-  read.csv(paste0(train_files, z, CSV))
    train$TIMESTAMP <- ymd_hm(train$TIMESTAMP)    # work with dates
    X_test <- read.csv(paste0(testX_files, z, CSV))
    X_test$TIMESTAMP <- ymd_hm(X_test$TIMESTAMP)
    if (task < 15) {
      y_test <- read.csv(paste0(testY_files, z, CSV))
    } else {
      y_test <- read.csv(paste(PATH, track,
                               "Solution\ to\ Task 15\\Solution15_W.csv",
                               sep=SLASH))
    }
    y_test$TIMESTAMP <- ymd_hm(y_test$TIMESTAMP)
    test <- X_test[JOIN] %>% left_join(y_test, by=JOIN)
    output[[zones[z]]] <- list(Train = select(train, -ZONEID),
                               Test = select(test, -ZONEID))
  }
  return(output)
}


# LOAD LOAD --------------------------------------------------------------------
loadLoad <- function(task) {
  track <- "Load"
  subfolder <-  paste0(PATH, SLASH, track, SLASH)
  zones <- "Zone1"
  # sadly dates are not unanambiguous (e.g. 11.01.10 and 01.11.10 are both
  # represented as 1112010) => hardcode start dates and generate rest
  start_dates <- c("2001-01-01 01:00", "2010-10-01 01:00", "2010-11-01 01:00",
                   "2010-12-01 01:00", "2011-01-01 01:00", "2011-02-01 01:00",
                   "2011-03-01 01:00", "2011-04-01 01:00", "2011-05-01 01:00",
                   "2011-06-01 01:00", "2011-07-01 01:00", "2011-08-01 01:00",
                   "2011-09-01 01:00", "2011-10-01 01:00", "2011-11-01 01:00",
                   "2011-12-01 01:00")
  if (task < 15) {
    test_start_ts <- start_dates[task + 1]
    test_file <- paste0(subfolder, "Task\ ", task+1, SLASH, "L", task+1,
                        "-train", CSV)
  } else {
    test_start_ts <- start_dates[16]
    test_file <- paste0(PATH, SLASH, track, SLASH, "Solution\ to\ Task\ 15",
                        SLASH, "solution15_L", CSV)
  }
  output <- list("Zones" = zones)
  train <- data.frame()
  # now fetch data
  for (i in 1:task) {
    train_file <- paste0(subfolder, "Task\ ", i, SLASH, "L", i, "-train", CSV)
    train_i <-  read.csv(train_file)
    train <- rbind(train, train_i)
  }
  train$TIMESTAMP <- ymd_hm(start_dates[1]) + hours(0:(nrow(train)-1))
  train <- train %>% select(-ZONEID) %>% rename(TARGET=LOAD)

  test <- select(read.csv(test_file), LOAD) %>% rename(TARGET=LOAD)
  test$TIMESTAMP <- ymd_hm(test_start_ts) + hours(0:(nrow(test)-1))

  output[[zones]] <- list(train=train, test=test)
  return(output)
}

# LOAD PRICE -------------------------------------------------------------------
loadPrice <- function(task) {
  track <- "Price"
  return(data.frame(X=c(track)))
}
