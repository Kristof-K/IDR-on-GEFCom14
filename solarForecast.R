#setwd("D:/Studium/Semester6/BachelorArbeit/Code")

library(tidyverse)
library(isodistrreg)

source("loadData.R")
source("util.R")

# prediction model
# - trivial IDR on all data
# - IDR on every hour (three variable models)
# - IDR on every hour +- 1 hour (~extended probabilistoc climatological)
# - IDR on every astronomical hour (same sunposition)


# Routine conducting evaluation for a given prediciton and scoring method
# - predictionfct : function getting X_train (data.frame containing covariates)
#   y_train (vector containing response variable) and X_test (data.frame
#   containing covariates for test data) and returning matrix of response
#   variable quantiles (1% up to 99%) for testing data, i.e. every row in X_test
#   should lead to such q quantile row in the output
#   if print=TRUE is passed, the function should print some explaining text
# - scoringfct : function getting matrix as 1st and vector as 2nd argument,
#   whereby 1st list in rows predictions for 1% up to 99% quantiles and
#   2nd is vector of true observation. It should return an average score over
#   all quantiles. If print=TRUE is passed, the function should print some
#   explaining text
evaluation <- function(predictionfct, scoringfct) {
  # use the print functionality in order to get some explaining text
  predictionfct(NA, NA, NA, print=TRUE)
  scoringfct(NA, NA, print=TRUE)
  
  # in GEFCom14 the first 3 tasks weren't evaluated
  firstTaskToEvalaute <- 4
  # list containing for every task data.frames with timestamps, zones and scores
  scoreList <- list()
  averageScores <- rep(0, length(TASKS))

  for (task in TASKS) {
    data <- loadSolar(task)   # read data
    # task was to predict the next 24 hours one month long
    lastTrainTS <- data$LastTrain_TS
    lastTestTS <- lastTrainTS
    day(lastTestTS) <- day(lastTestTS) + 1  # Test period comprises 24 hours
    currentMonth <- month(lastTrainTS)
    # data.frame for all results and vector for average results
    saveScores <- data.frame()
    # last timestamp we have to predict is hour zero in the new month
    while (month(lastTrainTS) == currentMonth) {
      z <- 1
      for (zone in SOLAR_ZONES) {
        current <- data[[zone]]  # restrict focus to one zone each iteration
        i_train <- (current$TIMESTAMP <= lastTrainTS)
        i_test <- (current$TIMESTAMP > lastTrainTS &
          current$TIMESTAMP <= lastTestTS)
        # get true observations (y) and train and test covariates, response var
        y <- subset(current, i_test)[["POWER"]]
        y_train <- subset(current, i_train)[["POWER"]]
        X_train <- subset(current, i_train, select=-POWER)
        X_test <- subset(current, i_test, select=-POWER)
        times <- subset(current, i_test, select=TIMESTAMP)
        # conduct prediction
        prediction <- predictionfct(X_train, y_train, X_test)
        # conduct scoring
        scores <- scoringfct(prediction, y)
        # get them into a data.frame and add to previous scores
        newScores <- data.frame(TIMESTAMP=times, ZONE=z, SCORE=scores)
        saveScores <- rbind(saveScores, newScores)
        z <- z+1
      }
      # move scope to next day
      lastTrainTS <- lastTestTS
      day(lastTestTS) <- day(lastTestTS) + 1
    }
    scoreList[[paste0("Task", task)]] <- saveScores
    averageScores[task] <- mean(saveScores[["SCORE"]])
  }
  
  results <- data.frame(rbind(averageScores), row.names=c("Score"))
  colnames(results) <- paste0("Task", TASKS)
  results
  finalScore <- mean(as.numeric(results[1,firstTaskToEvalaute:length(TASKS)]))
  cat("\n[AVERAGED SCORE]:", finalScore, "\n")
}

# Method output consistently a specific forecasting method
# - name : name of the forecasting method
# - vars : vector of variable nems that are incorporated
# - description : short text descripbing the method further
outputForecastingMethod <- function(name, vars, description) {
  cat("\n\n=================================================================\n")
  cat(" ", name,"\n")
  cat("=================================================================\n")
  print(description)
  cat("[VARIABLES]:", vars, "\n")
}

# Make a trivial forecast by using the empirical qauntiles of past obervations
# belonging to the hour for which a forecast is issued
trivialForecast <- function(X_train, y_train, X_test, print=FALSE) {
  if (print) {
    outputForecastingMethod("trivial forecast", "",
                            "Calculate for every hour the empirical
                            quantiles and return them irrespective of any
                            variable values")
    return("")
  }
  # function: calculate all quantiles and return data.frame
  getAllQuantiles <- function(x) {
    return(data.frame(PROBS=QUANTILES, Q=quantile(x, QUANTILES)))
  }
  # get for every hour the 99 empirical quantiles which we will use as forecast
  quantiles_by_hour <- X_train %>%
    mutate(Y = y_train, HOUR = hour(TIMESTAMP)) %>%
    group_by(HOUR) %>% summarise(getAllQuantiles(Y)) %>%
    pivot_wider(names_from=PROBS, values_from=Q)

  hours <- hour(X_test$TIMESTAMP)
  # pick the respective forecast
  getForecast <- function(hour) {
    predicted_q <- quantiles_by_hour %>% ungroup() %>% filter(HOUR == hour) %>%
      select(-HOUR) %>% as.numeric()
    return(predicted_q)
  }
  joinedForecast <- sapply(hours, getForecast)
  # sapply puts outputs of getForecast in columns, but we want that in the rows
  return(t(joinedForecast))
}

evaluation(trivialForecast, pinBallLoss)