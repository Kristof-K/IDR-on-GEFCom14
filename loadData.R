library(lubridate)

# path to the directory containing the GEFCom14 data
PATH <- "D:\\Studium\\Semester6\\BachelorArbeit\\GEFCom2014_Data"
CSV <- ".csv"
SLASH <- "\\"

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
  zones <- c("ZONE1", "ZONE2", "ZONE3")
  
  # file containing explaining variables
  predictors <- paste0(subfolder, SLASH, "predictors", task, CSV)
  # file containing response variable during training period
  targetVariable <- paste0(subfolder, SLASH, "train", task, CSV)
  # file containing  response variable during testing period
  if (task < 15) {
    observation <- paste0("Task ", (task+1), SLASH, "train", (task+1), CSV)
  } else {
    observation <- "Solution\ to\ Task 15\\Solution\ to\ Task\ 15.csv"
  }
  
  # read all necessary data and transform timestamps into clear format
  # understood from functions in lubridate
  X <- read.table(paste(PATH, track, predictors, sep=SLASH), header=TRUE,
                  dec=".", sep=",")
  X$TIMESTAMP <- ymd_hm(X$TIMESTAMP)
  Y_train <- read.table(paste(PATH, track, targetVariable, sep=SLASH),
                        header=TRUE, dec=".", sep=",")
  Y_train$TIMESTAMP <- ymd_hm(Y_train$TIMESTAMP)
  Y_test <- read.table(paste(PATH, track, observation, sep=SLASH),
                       header=TRUE, dec=".", sep=",")
  Y_test$TIMESTAMP <- ymd_hm(Y_test$TIMESTAMP)
  # order data in a sensible format
  output <- list("LastTrain_TS" = Y_train$TIMESTAMP[length(Y_train$TIMESTAMP)],
                 "Zones" = zones)
  # and fill the data frames
  for (i in 1:length(zones)) {
    if (task < 15) {
      zoneData <- subset(X, ZONEID==i, select=-ZONEID)
    } else {   # in task 15 X has own POWER column. but with less decimals
      zoneData <- subset(X, ZONEID==i, select=c(-ZONEID, -POWER))
    }
    y <- c(subset(Y_train, ZONEID == i)$POWER,
          subset(Y_test, 
                 (ZONEID==i) & (TIMESTAMP>output[["LastTrain_TS"]]))$POWER)
    # first name is lastTest_TS, so start at 2
    output[[zones[i]]] <- cbind(zoneData, POWER=y)
  }
  
  return(output)
}


# LOAD WIND --------------------------------------------------------------------

loadWind <- function(task) {
  track <- "Wind"
  subfolder <-  paste0(PATH, SLASH, track, SLASH, "Task\ ", task, SLASH)
  zones <- paste0("ZONE", 1:10)

  train_files <- paste0(subfolder, "Task", task, "_W_Zone1_10", SLASH, "Task",
                        task, "_W_Zone")
  testX_files <- paste0(subfolder, "TaskExpVars", task, "_W_Zone1_10", SLASH,
                        "TaskExpVars", task, "_W_Zone")
  testY_files <- paste0(PATH,SLASH, track,SLASH, "Task\ ", task+1,SLASH, "Task",
                        task+1, "_W_Zone1_10", SLASH, "Task", task+1, "_W_Zone")
  output <- list("Zones" = zones)
  # now fetch data
  z <- 1
  for(zone in zones) {
    train <-  read.table(paste0(train_files, z, CSV), header=TRUE, dec=".",
                         sep=",")
    train$TIMESTAMP <- ymd_hm(train$TIMESTAMP)    # work with dates
    train <- train[(names(train) != "ZONEID")]    # get rid of ZONEID column
    X_test <- read.table(paste0(testX_files, z, CSV), header=TRUE, dec=".",
                         sep=",")
    X_test$TIMESTAMP <- ymd_hm(X_test$TIMESTAMP)
    X_test <- X_test[(names(X_test) != "ZONEID")]
    if (task < 15) {
      y_test <- read.table(paste0(testY_files, z, CSV), header=TRUE,
                           dec=".", sep=",")
    } else {
      y_test <- read.table(paste(PATH, track,
                                 "Solution\ to\ Task 15\\Solution15_W.csv",
                                 sep=SLASH),
                            header=TRUE, dec=".", sep=",")
      y_test <- subset(y_test, ZONEID==z, select=-ZONEID)
    }
    y_test$TIMESTAMP <- ymd_hm(y_test$TIMESTAMP)

    lastTrainTS <- train$TIMESTAMP[length(train$TIMESTAMP)]
    test <- cbind(X_test,
                  TARGETVAR=subset(y_test,TIMESTAMP>lastTrainTS)[["TARGETVAR"]])
    output[[zone]] <- rbind(train, test)
    names(output[[zone]])[names(output[[zone]])=="TARGETVAR"] <- "POWER"
    z <- z+1
  }
  output[["LastTrain_TS"]] <- lastTrainTS
  return(output)
}


# LOAD LOAD --------------------------------------------------------------------
loadLoad <- function(task) {
  track <- "Load"
  subfolder <-  paste0(PATH, SLASH, track, SLASH)
  zones <- "ZONE1"
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
                        SLASH, "solution15_L_temperature", CSV)
  }
  output <- list("Zones" = zones)
  train <- data.frame()
  # now fetch data
  for (i in 1:task) {
    train_file <- paste0(subfolder, "Task\ ", i, SLASH, "L", i, "-train", CSV)
    train_i <-  read.table(train_file, header=TRUE, dec=".", sep=",")
    train <- rbind(train, train_i)
  }
  train$TIMESTAMP <- ymd_hm(start_dates[1]) + hours(0:(nrow(train)-1))
  train <- train[(names(train) != "ZONEID")]    # get rid of ZONEID column

  test <- read.table(test_file, header=TRUE, dec=".", sep=",")
  if (task < 15) {
    test <- test[(names(test) != "ZONEID")]
  } else {
    test <- test[!(names(test) %in% c("date", "hour"))]
  }
  test$TIMESTAMP <- ymd_hm(test_start_ts) + hours(0:(nrow(test)-1))
  lastTrainTS <- train$TIMESTAMP[length(train$TIMESTAMP)]
  output[[zones]] <- rbind(train, test)

  output[["LastTrain_TS"]] <- lastTrainTS
  return(output)
}
loadLoad(15)

# LOAD PRICE -------------------------------------------------------------------
loadPrice <- function(task) {
  track <- "Price"
  return(data.frame(X=c(track)))
}
