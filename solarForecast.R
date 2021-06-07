#setwd("D:/Studium/Semester6/BachelorArbeit/Code")

library(foreach)    # to parallel central task loop
library(doParallel)

source("loadData.R")
source("util.R")
source("solarIDR.R")

SOLAR_CSV <- "../solarResults.csv"

# TODO
# - deaccumulate solar energy
# - forecast trained on all zone
# - extended hour model
# - Loss-plot
# - general IDR application


# Routine conducting evaluation for a given prediciton and scoring method
# - predictionfct : function getting
#     - X_train (data.frame containing covariates)
#     - y_train (vector containing response variable)
#     - X_test (data.frame containing covariates for test data)
#     - id (specify intern modalities)
#     - init (if true print and return information, if false make forecast)
#   If init false return matrix of quantiles (1% up to 99%) of response variable
#   for testing data, i.e. every row in X_test leads to such q quantile row
# - scoringfct : function getting matrix as 1st and vector as 2nd argument,
#   whereby 1st list in rows predictions for 1% up to 99% quantiles and
#   2nd is vector of true observation. It should return an average score over
#   all quantiles. If print=TRUE is passed, the function should print some
#   explaining text
# - id : id (argument for predicitionfct)
evaluation <- function(predictionfct, scoringfct, id) {
  # in order to run the foeach loop in parallel
  cl <- makeCluster(3)
  registerDoParallel(cl)
  # use print / init functionality to output and store important information
  info <- predictionfct(NA, NA, NA, id, init=TRUE)
  scoringfct(NA, NA, print=TRUE)
  start_ts <- now()       # saving timestamp

  # since files could be accessed from multiple threads, read it sequentially
  dataList <- foreach(task=TASKS) %do% {
    loadSolar(task)
  }
  # run in parallel through tasks and store results in scoreList
  scoreList <- foreach(task=TASKS,
                       .export=c("goParallel", "predAndEval")) %dopar% {
    source("util.R") # source necessary files (thread starts in empty env)
    source("solarIDR.R")
    saveScores <- goParallel(predictionfct, scoringfct, dataList[[task]], id,
                             info$PBZ)
    saveScores      # store result in list
  }
  stopCluster(cl)   # due to parallel programming
  end_ts <- now()
  duration <- as.numeric(difftime(end_ts, start_ts, unit="mins"))
  outputAndLog(scoreList, duration, info)
}

goParallel <- function(predictionfct, scoringfct, data, id, goByZone) {
    lastTrainTS <- data$LastTrain_TS
    saveScores <- data.frame()
    z <- 1
    # distinguish whether predicitionfct wants all data or data by zone
    if (goByZone) {
      for (zone in SOLAR_ZONES) {
        newScores <- predAndEval(predictionfct, scoringfct, data[[zone]], id,
                                 lastTrainTS, zone=z)
        saveScores <- rbind(saveScores, newScores)
        z <- z+1
      }
    } else {
      unite_data <- data.frame()
      for (zone in SOLAR_ZONES) {
        unite_data <- rbind(unite_data, cbind(data[[zone]], ZONE=z))
        z <- z+1
      }
      saveScores <- predAndEval(predictionfct, scoringfct, unite_data, id,
                                lastTrainTS)
    }
  return(saveScores)
}

predAndEval <- function(predictionfct, scoringfct, data, id, lastTrainTS,
                        zone=NA) {
  lastTestTS <- data$TIMESTAMP[length(data$TIMESTAMP)]

  i_train <- (data$TIMESTAMP <= lastTrainTS)
  i_test <- (data$TIMESTAMP > lastTrainTS & data$TIMESTAMP <= lastTestTS)

  y <- subset(data, i_test)[["POWER"]]
  y_train <- subset(data, i_train)[["POWER"]]
  X_train <- subset(data, i_train, select=-POWER)
  X_test <- subset(data, i_test, select=-POWER)
  times <- subset(data, i_test)[["TIMESTAMP"]]

  # conduct prediction
  prediction <- predictionfct(X_train, y_train, X_test, id)
  # conduct scoring
  scores <- scoringfct(prediction, y)
  if (is.na(zone)) {
    zone <- X_test$ZONE     # in this case, assume data has already ZONE column
  }
  # get them into a data.frame and add to previous scores
  return(data.frame(TIMESTAMP=times, ZONE=zone, SCORE=scores))
}

outputAndLog <- function(scoreList, duration, info) {
  # average over scores
  averageScores <- sapply(scoreList, function(df) { return(mean(df$SCORE)) })
  results <- data.frame(rbind(averageScores))
  colnames(results) <- paste0("Task", TASKS)
  print(results)
  finalScore <- mean(as.numeric(results[1, FIRST_EVAL_TASK:length(TASKS)]))
  cat("\n[AVERAGED SCORE]:", finalScore, "\n")
  cat("[TIME]:", duration, "min\n")

  # Lastly save the results in log file by extending previous results
  results <- cbind(X = 0, Name = info$TIT, Vars = paste(info$VAR, collapse="_"),
                   Preprocess = info$PP, results,
                   Mean_A = finalScore, Minutes=duration)
  rownames(results)[1] <- 1
  if (file.exists(SOLAR_CSV)) {
    previous <- read.csv2(SOLAR_CSV)
    # make sure that new determined result receives the next row number
    rownames(results)[1] <- dim(previous)[1] + 1
    results <- rbind(previous, results)
  }
  # X (1st) column in results is dummy column and should not be saved in result
  write.csv2(results[-1], SOLAR_CSV)
}


#evaluation(trivialForecast, pinBallLoss, 2)
evaluation(benchmark, pinBallLoss, 1)
#evaluation(unleashIDR, pinBallLoss, c(1, 1, 1))
#evaluation(unleashIDR, pinBallLoss, c(1, 2, 1))
#evaluation(unleashIDR, pinBallLoss, c(2, 1, 1))
#evaluation(unleashIDR, pinBallLoss, c(2, 2, 1))
#evaluation(unleashIDR, pinBallLoss, c(2, 3, 1))
#evaluation(unleashIDR, pinBallLoss, c(2, 4, 1))
#evaluation(unleashIDR, pinBallLoss, c(2, 5, 1))