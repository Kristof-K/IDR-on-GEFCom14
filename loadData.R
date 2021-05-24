library(lubridate)

# path to the directory containing the GEFCom14 data
path <- "D:\\Studium\\Semester6\\BachelorArbeit\\GEFCom2014_Data"

# function loading a specific track
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

# load solar track of the given task (task must be a number bewtween 1 and 15)
# the return value is a named list containing the elements "LastTrain_TS" 
# (time stamp belonging to the last test entry) and "ZONE1", "ZONE2", "ZONE3"
# (data frames containing data belonging to the respective zone)
loadSolar <- function(task) {
  track <- "Solar"
  subfolder <- paste("Task", task)
  csv <- ".csv"
  slash <- "\\"
  
  # file containing explaining variables
  predictors <- paste0(subfolder, slash, "predictors", task, csv)
  # file containing response variable during training period
  targetVariable <- paste0(subfolder, slash, "train", task, csv)
  # file containing  response variable during testing period
  if (task < 15) {
    observation <- paste0("Task ", (task+1), slash, "train", (task+1), csv)
  } else {
    observation <- "Solution\ to\ Task 15\\Solution\ to\ Task\ 15.csv"
  }
  
  # read all necessary data and transform timestamps into clear format
  # understood from functions in lubridate
  X <- read.table(paste(path, track, predictors, sep=slash), header=TRUE,
                     dec=".", sep=",")
  X$TIMESTAMP = ymd_hm(X$TIMESTAMP)
  Y_train <- read.table(paste(path, track, targetVariable, sep=slash),
                        header=TRUE, dec=".", sep=",")
  Y_train$TIMESTAMP = ymd_hm(Y_train$TIMESTAMP)
  Y_test <- read.table(paste(path, track, observation, sep=slash),
                       header=TRUE, dec=".", sep=",")
  Y_test$TIMESTAMP = ymd_hm(Y_test$TIMESTAMP)
  # order data in a sensible format
  output <- list("LastTrain_TS" = Y_train$TIMESTAMP[length(Y_train$TIMESTAMP)],
              "ZONE1" = data.frame(), "ZONE2" = data.frame(),
              "ZONE3" = data.frame())
  # and fill the data frames
  for (i in 1:3) {
    if (task < 15) {
      zone = subset(X, ZONEID==i, select=-ZONEID)
    } else {   # in task 15 X has own POWER column. but with less decimals
      zone = subset(X, ZONEID==i, select=c(-ZONEID, -POWER))
    }
    y = c(subset(Y_train, ZONEID == i)$POWER, 
          subset(Y_test, 
                 (ZONEID==i) & (TIMESTAMP>output[["LastTrain_TS"]]))$POWER)
    # first name is lastTest_TS, so start at 2
    output[[names(output)[i+1]]] = cbind(zone, POWER=y)
  }
  
  return(output)
}


# LOAD WIND --------------------------------------------------------------------
loadWind <- function(task) {
  track  <- "Wind"
  return(data.frame(X=c(track)))
}


# LOAD LOAD --------------------------------------------------------------------
loadLoad <- function(task) {
  track <- "Load"
  return(data.frame(X=c(track)))
}


# LOAD PRICE -------------------------------------------------------------------
loadPrice <- function(task) {
  track <- "Price"
  return(data.frame(X=c(track)))
}
