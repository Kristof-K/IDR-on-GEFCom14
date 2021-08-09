library(foreach)    # to parallel central task loop
library(doParallel)
library(dplyr)

source("loadData.R")
source("util.R")
source("applyIDR.R")
source("preprocess.R")

# TODO
# - wind speed linear interpolated for different heights?
# - Loss-plot


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
#   all quantiles. If init=TRUE is passed, the function should print some
#   explaining text
# - id : id (argument for predicitionfct)
# - preprocessfct : function getting data after it was loaded (i.e. it expects
#   list with zones referring to data.frames), preprocess and return it
#   If init=TRUE, then print text and return name of this preprocess method
evaluation <- function(predictionfct, scoringfct, id, preprocessfct=no_pp) {
  # in order to run the foeach loop in parallel
  cl <- makeCluster(3)
  registerDoParallel(cl)
  # use print / init functionality to output and store important information
  info <- predictionfct(NA, NA, NA, id, init=TRUE)
  info[["PP"]] <- preprocessfct(NA, init=TRUE)
  info[["SF"]] <- scoringfct(NA, NA, init=TRUE)
  start_ts <- now()       # saving timestamp

  # since files could be accessed from multiple threads, read it sequentially
  dataList <- foreach(task=TASKS) %do% {
    preprocessfct(loadSet(info[["TRACK"]], task))
  }
  # run in parallel through tasks and store results in scoreList
  scoreList <- foreach(task=TASKS,
                       .export=c("goParallel", "predAndEval")) %dopar% {
    source("util.R") # source necessary files (thread starts in empty env)
    source("applyIDR.R")
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
  df_list <- list()
  # When predicitionfct wants data of all zones at once, unite it
  if (!goByZone) {
    unite_train <- data.frame()
    unite_test <- data.frame()
    for (zone in data$Zones) {
      unite_train <- rbind(unite_train, data[[zone]]$Train)
      unite_test <- rbind(unite_test, data[[zone]]$Test)
    }
    df_list <- list("joint_data"=list(Train=unite_train, Test=unite_test))
  } else { # otherwise list all zone separately
    for(zone in data$Zones) {
      df_list[[zone]] <- data[[zone]]
    }
  }
  # apply predAndEval on every element of df_list and rbind it afterwards
  allScores <- do.call(rbind, lapply(df_list, predAndEval, predictionfct,
                                     scoringfct, id))
  return(allScores)
}

predAndEval <- function(data, predictionfct, scoringfct, id) {
  y <- data$Test$TARGET
  y_train <- data$Train$TARGET
  X_train <- select(data$Train, -TARGET)
  X_test <-  select(data$Test, -TARGET)
  times <- data$Test$TIMESTAMP
  zone <- data$Test$ZONEID
  # conduct prediction
  prediction <- predictionfct(X_train, y_train, X_test, id)
  # conduct scoring
  scores <- scoringfct(prediction, y)
  return(data.frame(TIMESTAMP=times, ZONEID=zone, SCORE=scores))
}

outputAndLog <- function(scoreList, duration, info) {
  logFile <- paste0("../Results", info$TRACK, ".csv")
  # average over scores
  averageScores <- sapply(scoreList, function(df) {
                                        return(mean(df$SCORE, na.rm=TRUE))
                                      })
  results <- data.frame(rbind(averageScores))
  colnames(results) <- paste0("Task", TASKS)
  print(results)
  finalScore <- mean(as.numeric(results[1, FIRST_EVAL_TASK:length(TASKS)]))
  cat("\n[AVERAGED SCORE]:", finalScore, "\n")
  cat("[TIME]:", duration, "min\n")

  # Lastly save the results in log file by extending previous results
  results <- cbind(X = 0, Name = info$TIT, Vars = paste(info$VAR, collapse="_"),
                   Order = info$OR, Scoringfct = info$SF, Preprocess = info$PP,
                   Grouping=info$GR, Id=info$ID, results, Mean_A = finalScore,
                   Minutes=duration)
  rownames(results)[1] <- 1
  if (file.exists(logFile)) {
    previous <- read.csv2(logFile)
    # make sure that new determined result receives the next row number
    rownames(results)[1] <- dim(previous)[1] + 1
    results <- rbind(previous, results)
  }
  # X (1st) column in results is dummy column and should not be saved in result
  write.csv2(results[-1], logFile)
}


#evaluation(trivialForecast, pinBallLoss, c(2,1))
#evaluation(benchmarkSolar, pinBallLoss, 1)
#evaluation(unleashSolIDR, pinBallLoss, c(3, 1))
#evaluation(unleashSolIDR, pinBallLoss, c(4, 1))
#evaluation(unleashSolIDR, pinBallLoss, c(5, 1, 2))
#evaluation(unleashSolIDR, pinBallLoss, c(2, 1, 2))
#evaluation(unleashSolIDR, pinBallLoss, c(3, 1, 2))
#evaluation(unleashSolIDR, pinBallLoss, c(4, 1, 2))
#evaluation(unleashSolIDR, pinBallLoss, c(5, 1, 2))
#evaluation(unleashSolIDR, pinBallLoss, c(1, 1, 2), preprocessfct=deaccumulateSol)
#evaluation(unleashSolIDR, pinBallLoss, c(1, 1, 2), preprocessfct=deaccumulateSol)
#evaluation(unleashSolIDR, pinBallLoss, c(7, 1, 2))
#evaluation(unleashSolIDR, pinBallLoss, c(9, 2, 2), preprocessfct=deaccumulateSol)
#evaluation(unleashSolIDR, pinBallLoss, c(1, 1, 2, 0.95), preprocessfct=deaccumulateSol)
#evaluation(unleashSolIDR, pinBallLoss, c(1, 1, 2, 0.9), preprocessfct=deaccumulateSol)
#evaluation(unleashSolIDR, pinBallLoss, c(1, 1, 2, 0.9), preprocessfct=deaccumulateSol)
#evaluation(unleashSolIDR, pinBallLoss, c(1, 1, 8), preprocessfct=deaccumulateSol)
#evaluation(unleashSolIDR, pinBallLoss, c(14, 1, 7, 0.9, 50, 0.5), preprocessfct=deaccumulateSol)

#evaluation(benchmarkWind, pinBallLoss, 1)
#evaluation(unleashWinIDR, pinBallLoss, c(2, 1, 2), preprocessfct=getWindAttributes)
evaluation(unleashWinIDR, pinBallLoss, c(2, 1), preprocessfct=getWindAttributes)
#evaluation(unleashWinIDR, pinBallLoss, c(2, 1, 6), preprocessfct=getWindAttributes)
#evaluation(unleashWinIDR, pinBallLoss, c(2, 1, 4), preprocessfct=getWindAttributes)
#evaluation(unleashWinIDR, pinBallLoss, c(2, 1, 6, 0.8), preprocessfct=getWindAttributes)

#evaluation(unleashLoaIDR, pinBallLoss, c(1, 1, 1))
#evaluation(unleashLoaIDR, pinBallLoss, c(1, 1, 3))
#evaluation(unleashLoaIDR, pinBallLoss, c(15, 1, 3), preprocessfct=addLoadMeans)
#evaluation(unleashLoaIDR, pinBallLoss, c(16, 1, 3), preprocessfct=addLoadMeans)
