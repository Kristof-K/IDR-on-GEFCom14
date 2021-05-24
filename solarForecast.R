#setwd("D:/Studium/Semester6/BachelorArbeit/Code")

source("loadData.R")
source("util.R")

library(tidyverse)
library(isodistrreg)

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
  
  for (task in TASKS) {
    data <- loadSolar(task)   # read data
    # task was to predict the next 24 hours one month long
    lastTrainTS <- date$LastTrain_TS
    lastTestTS <- lastTrainTS
    day(lastTestTS) <- lastTrainTS + 1  # Test period comprises 24 hours
    currentMonth <- month(lastTrainTS)
    # data.frame for all results and vector for average results
    saveScores <- data.frame()
    averageScores <- rep(0, length(TASKS))
    # last timestamp we have to predict is hour zero in the new month
    while (month(lastTrainTS) == currentMonth) {
      z <- 1
      for (zone in SOLAR_ZONES) {
        current <- data[[zone]]  # restrict focus to one zone each iteration
        i_train <- (TIMESTAMP <= LastTestTS)
        i_test <- (TIMESTAMP > LastTestTS & TIMESTAMP <= LastTrainTS)
        # get true observations (y) and train and test covariates, response var
        y <- subset(current, i_test)[["POWER"]]
        y_train <- subset(current, i_train)[["POWER"]]
        X_train <- subset(current, i_train, select=-POWER)
        X_test <- subset(current, i_test, select=-POWER)
        times <- subset(current, i_test, select=TIMESTAMP)
        # conduct prediction
        prediction <- predictfct(X_train, y_train, X_test)
        # conduct scoring
        scores <- scoringfct(prediction, y)
        # get them into a data.frame and add to previous scores
        newScores <- data.frame(TIMESTAMP=times, ZONE=z, SCORE=scores)
        saveScores <- rbind(saveScores, newScores)
        z <- z+1
      }
      # move scope to next day
      lastTrainTS <- lastTestTS
      day(lastTestTS) <- day(lastTest_TS) + 1
    }
    scoreList[[paste0("Task", task)]] <- saveScores
    averageScores[task] <- mean(saveScores[["SCORE"]])
  }
  
  results <- data.frame(rbind(TASKS, averageScores),
                        row.names=c("Task", "Score"))
  colnames(results) <- paste0("Task", TASKS)
  results
  finalScore <- mean(subset(results, Task >= firstTaskToEvalaute)[2,])
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
  cat(description,"\n[VARIABLES]:", vars, "\n")
}

# Make a trivial forecast by using the empirical qauntiles of past obervations
# belonging to the hour for which a forecast is issued
trivialForecast <- function(X_train, y_train, X_test, print=FALSE) {
  if (print) {
    outputForecastingMethod("trivial forecast", "",
                            "Calculate for every hour the empirical
                            quantiles and return them irrespective of any
                            variable values")
    return(NULL)
  }
  quantiles_by_hour <- mutate(X_train, Y = y_train, hour = hour(TIMESTAMP)) %>%
    group_by(hour) %>% summarise(y_quantiles=quantile(Y, QUANTILES))
}