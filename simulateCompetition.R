library(foreach)   # to parallel central task loop
library(doParallel)
library(dplyr)

source("loadData.R")
source("util.R")
source("applyIDR.R")
source("preprocess.R")

# TODO
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
# - tune : true to consider only the initial tuning phase in the competition,
#   false to use the preceding competition phase
# - ret : true return list of all single scores, false then not
# - write : write predictions, true observations and scores in extra log file
evaluation <- function(predictionfct, scoringfct, id, preprocessfct=no_pp,
                       tune=FALSE, ret=FALSE, write=FALSE) {
  # in order to run the foeach loop in parallel
  cl <- makeCluster(3)
  registerDoParallel(cl)
  # use print / init functionality to output and store important information
  info <- predictionfct(NA, NA, NA, id, init=TRUE)
  info[["PP"]] <- preprocessfct(NA, init=TRUE)
  info[["SF"]] <- scoringfct(NA, NA, init=TRUE)
  tasks <- if (!tune) TASKS else TUNE_TASKS
  start_ts <- now()       # saving timestamp

  # since files could be accessed from multiple threads, read it sequentially
  dataList <- foreach(task=tasks) %do% {
    preprocessfct(loadSet(info[["TRACK"]], task))
  }
  # add directory that contains predictions if they should be saved
  path <- NULL
  if (write) {
    path <- paste0("predictions/", info[["TRACK"]], "-",
                   paste(id, collapse="_"), "-", info[["PP"]])
    dir.create(path)
  }
  # run in parallel through tasks and store results in scoreList
  scoreList <- foreach(task=1:length(dataList),
                       .export=c("goParallel", "predAndEval")) %dopar% {
    source("util.R") # source necessary files (thread starts in empty env)
    source("applyIDR.R")
    saveScores <- goParallel(predictionfct, scoringfct, dataList[[task]], id,
                             info$PBZ, path, task)
    saveScores      # store result in list
  }
  stopCluster(cl)   # due to parallel programming
  end_ts <- now()
  duration <- as.numeric(difftime(end_ts, start_ts, unit="mins"))
  outputAndLog(scoreList, duration, info, tune)
  if (ret) return(scoreList)
}

goParallel <- function(predictionfct, scoringfct, data, id, goByZone, path,
                       task) {
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
                                     scoringfct, id, path, task))
  return(allScores)
}

predAndEval <- function(data, predictionfct, scoringfct, id, path, task) {
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
  if (!is.null(path)) {
    colnames(prediction) <- paste(1:99 * 0.01)
    write_df <- data.frame(time = paste(times), zoneid = zone, y = y,
                           prediction, score = scores)
    write.csv(write_df, paste0(path, "/Task", task, "_", zone[1], ".csv"))
  }
  return(data.frame(TIMESTAMP=times, ZONEID=zone, SCORE=scores))
}

outputAndLog <- function(scoreList, duration, info, tune) {
  logFile <- paste0("../Results", info$TRACK, ".csv")
  # average over scores
  averageScores <- sapply(scoreList, function(df) mean(df$SCORE, na.rm=TRUE))
  finalScore <- mean(averageScores)
  if (tune) averageScores <- c(averageScores, rep(NA, length(TASKS)))
      else averageScores <- c(rep(NA, length(TUNE_TASKS)), averageScores)
  results <- data.frame(rbind(averageScores))
  colnames(results) <- paste0("Task", c(TUNE_TASKS, TASKS))
  print(results)
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


#evaluation(trivialForecast, pinBallLoss)
#evaluation(LastVal, pinBallLoss, c(3,2))
#evaluation(unleashSolIDR, pinBallLoss, c(100001, 2, 9), preprocessfct=deaccuInvertSol, tune=TRUE)
#evaluation(unleashSolIDR, pinBallLoss, c(1, 1, 2, 0.95), preprocessfct=deaccuInvertSol)
#evaluation(unleashSolIDR, pinBallLoss, c(1, 1, 9), preprocessfct=deaccuInvertSol)
#evaluation(unleashSolIDR, pinBallLoss, c(100001, 1, 9, 1, 75, 0.25), preprocessfct=deaccuInvertSol)

#evaluation(benchmarkWind, pinBallLoss, 1)
#evaluation(unleashWinIDR, pinBallLoss, c(10, 1, 3), preprocessfct=getWindAttributes, tune=TRUE)
#evaluation(unleashWinIDR, pinBallLoss, c(100000001, 1, 6), preprocessfct=getWindAttributes, tune=TRUE)
#evaluation(unleashWinIDR, pinBallLoss, c(2, 1, 6, 0.8), preprocessfct=getWindAttributes)

#evaluation(unleashLoaIDR, pinBallLoss, c(1, 1, 1), preprocessfct=squared_lastTmp, tune=TRUE)
#evaluation(unleashLoaIDR, pinBallLoss, c(1, 1, 8), preprocessfct=invWin_meanTmp)
#evaluation(unleashLoaIDR, pinBallLoss, c(1, 1, 8, 1, 75, 0.25), preprocessfct=invWin_lwMean)
#evaluation(unleashLoaIDR, pinBallLoss, c(1, 1, 8, 1, 100, 0.5), preprocessfct=invWin_lwMean)
#evaluation(unleashLoaIDR, pinBallLoss, c(1, 1, 1), preprocessfct=squ_meanTmp, tune=TRUE, write=TRUE)

#evaluation(unleashPriIDR, pinBallLoss, c(110, 1, 16), tune=TRUE, preprocessfct=addPriceRegressors)
#evaluation(unleashPriIDR, pinBallLoss, c(10000010, 1, 17), tune=TRUE, preprocessfct=addPriceRegressors)
#evaluation(unleashPriIDR, pinBallLoss, c(110, 1, 18), tune=TRUE, preprocessfct=addPriceRegressors)
#evaluation(unleashPriIDR, pinBallLoss, c(10, 1, 8, 1, 75, 0.25))

checkAllLoad <- function() {
  max_exp <- c(9, 9, 4)

  for(i in 0:2) {
    for (j in 0:max_exp[i + 1]) {
      var <- i * 10^10 + 10^j
      evaluation(unleashLoaIDR, pinBallLoss, c(var, 1, 1), preprocessfct=squ_meanTmp, tune=TRUE)
    }
  }
}

gridSearch <- function() {
  # Try all variable combinations
  num <- 12      # number of avaulable variables
  max_c <- 3    # maximal number of variales to insepect at once

  getNextNum <- function(arr, num, max_c) {
    out <- numeric(0)
    if (length(arr) == max_c) return(sum(10^(arr-1)))     # got everything?
    if (max(arr) == num) return(numeric(0))         # no numbers left
    for (k in (max(arr)+1):num) {
      out <- c(out, getNextNum(c(arr, k), num, max_c))
    }
    return(out)
  }

  for(k in 3) {
    # get all 0-1-numbers with k ones
    for (first in 3:(num - k + 1)) {
      for(n in getNextNum(first, num, k)) {
        evaluation(unleashSolIDR, pinBallLoss, c(n, 1, 2),
                         preprocessfct=deaccumulateSol, tune=TRUE)
      }
    }
  }
}

printScoresByZone <- function() {
  for(g in c(1,10)) {
    scoreList <- evaluation(unleashWinIDR, pinBallLoss, c(g, 1, 10),
                            preprocessfct=getWindAttributes, tune=TRUE)
    scoreByZone <- do.call(rbind, scoreList) %>% group_by(ZONEID) %>%
      summarise(MeanScore = mean(SCORE, na.rm=TRUE), .groups = "drop")
    results <- data.frame(t(scoreByZone$MeanScore))
    colnames(results) <- scoreByZone$ZONEID
    print(g)
    print(results)
  }
}