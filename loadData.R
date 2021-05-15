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
  
  # read all necessary data
  X <- read.table(paste(path, track, predictors, sep=slash), header=TRUE, 
                     dec=".", sep=",")
  Y_train <- read.table(paste(path, track, targetVariable, sep=slash), 
                        header=TRUE, dec=".", sep=",")
  Y_test <- read.table(paste(path, track, observation, sep=slash), 
                       header=TRUE, dec=".", sep=",")
  # assign data in a sensible format
  output <- data.frame()
  output$LastTestTimeStamp <- c(Y_train$TIMESTAMP[length(Y_train$TIMESTAMP)])
  zones = c("ZONE1", "ZONE2", "ZONE3")
  for (i in 1:3) {
    zone = subset(X, ZONEID==i, select=-ZONEID)
    y = c(subset(Y_train, ZONEID == i)$POWER, 
          subset(Y_test, 
                 (ZONEID==i) && (TIMESTAMP>output$LastTestTimeStamp))$POWER)
    output[i,] = cbind(zone, c("POWER" = y))
  }
  ZONE1 = subset(X, ZONEID == 1)
  ZONE2 = subset(X, ZONEID == 2)
  ZONE3 = subset(X, ZONEID == 3)
  
  X$TIMESTAMP[c(1, length(X$TIMESTAMP))]
  Y_train$TIMESTAMP[c(1, length(Y_train$TIMESTAMP))]
  Y_test$TIMESTAMP[c(1, length(Y_test$TIMESTAMP))]
  
  return(data.frame(X=c(track)))
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
