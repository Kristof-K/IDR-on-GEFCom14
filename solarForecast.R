#setwd("D:/Studium/Semester6/BachelorArbeit/Code")

library(tidyverse)      # in order to use dplyr

source("loadData.R")
source("util.R")

source("solarIDR.R")

SOLAR_CSV <- "../solarResults.csv"


# Routine conducting evaluation for a given prediciton and scoring method
# - predictionfct : function getting
#     - X_train (data.frame containing covariates)
#     - y_train (vector containing response variable)
#     - X_test (data.frame containing covariates for test data)
#     - version (only important if there are several versions)
#     - variableVersion (only important if there are several variable combis)
#     - print (if true only print information text, if false make forecast)
#   and returning matrix of quantiles (1% up to 99%) response variable for
#   testing data, i.e. every row in X_test leads to such q quantile row
# - scoringfct : function getting matrix as 1st and vector as 2nd argument,
#   whereby 1st list in rows predictions for 1% up to 99% quantiles and
#   2nd is vector of true observation. It should return an average score over
#   all quantiles. If print=TRUE is passed, the function should print some
#   explaining text
# - name : name to be logged in csv file storing the results
# - version : which version should be passed to the predictionfct
# - variableVersion : which variableVersion should be passed to predictionfct
evaluation <- function(predictionfct, scoringfct, name, version = 0,
                       variableVersion = 0) {
  # use the print functionality in order to get some explaining text
  predictionfct(NA, NA, NA, version, variableVersion, print=TRUE)
  scoringfct(NA, NA, print=TRUE)
  # saving timestamp
  start_ts <- now()
  # list containing for every task data.frames with timestamps, zones and scores
  scoreList <- list()
  averageScores <- rep(0, length(TASKS))

  for (task in TASKS) {
    data <- loadSolar(task)   # read data
    # task was to predict the next 24 hours one month long
    lastTrainTS <- data$LastTrain_TS

    # data.frame for all results and vector for average results
    saveScores <- data.frame()
    z <- 1
    for (zone in SOLAR_ZONES) {
      current <- data[[zone]]  # restrict focus to one zone each iteration
      lastTestTS <- current$TIMESTAMP[length(current$TIMESTAMP)]
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
      prediction <- predictionfct(X_train, y_train, X_test, version,
                                  variableVersion)
      # conduct scoring
      scores <- scoringfct(prediction, y)
      # get them into a data.frame and add to previous scores
      newScores <- data.frame(TIMESTAMP=times, ZONE=z, SCORE=scores)
      saveScores <- rbind(saveScores, newScores)
      z <- z+1
    }
    # save scores and calc average score
    scoreList[[paste0("Task", task)]] <- saveScores
    averageScores[task] <- mean(saveScores[["SCORE"]])
    cat("- Finished task", task, "\n")
  }
  # print results
  results <- data.frame(rbind(averageScores))
  colnames(results) <- paste0("Task", TASKS)
  print(results)
  finalScore <- mean(as.numeric(results[1, FIRST_EVAL_TASK:length(TASKS)]))
  cat("\n[AVERAGED SCORE]:", finalScore, "\n")

  end_ts <- now()
  duration <- as.numeric(difftime(end_ts, start_ts, unit="mins"))
  # Lastly save the results in log file by extending previous results
  results <- cbind(X = 0, Name = name, Version = version,
                   VariableVersion = variableVersion, results,
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

# Method output consistently a specific forecasting method
# - name : name of the forecasting method
# - vars : vector of variable nems that are incorporated
# - description : short text descripbing the method further (it should be a
#   vector of words in order to print correctly)
outputForecastingMethod <- function(name, vars, description) {
  cat("\n\n=================================================================\n")
  cat(" ", name,"\n")
  cat("=================================================================\n")
  cat(description, "\n", fill = PRINT_WIDTH)
  cat("[VARIABLES]:", vars, "\n")
}

# Make a trivial forecast by using the empirical qauntiles of past obervations
# belonging to the hour for which a forecast is issued
trivialForecast <- function(X_train, y_train, X_test, print=FALSE, version = 0,
                            variableVersion = 0) {
  if (print) {
    outputForecastingMethod("trivial forecast", "None",
                            c("Calculate", "for", "every", "hour", "the",
                            "empirical", "quantiles", "and", "return", "them",
                            "irrespective", "of", "any", "variable", "values"))
    return("")
  }
  # function: calculate all quantiles and return data.frame
  getAllQuantiles <- function(x) {
    return(data.frame(PROBS=QUANTILES, Q=quantile(x, QUANTILES)))
  }
  # get for every hour the 99 empirical quantiles which we will use as forecast
  # pivot_wider transforms the long data table into a wide one
  quantiles_by_hour <- X_train %>%
    mutate(Y = y_train, HOUR = hour(TIMESTAMP)) %>%
    group_by(HOUR) %>% summarise(getAllQuantiles(Y)) %>%
    pivot_wider(names_from=PROBS, values_from=Q)

  hours <- hour(X_test$TIMESTAMP)
  # pick the respective forecast
  getForecast <- function(hour) {
    # get according to the hour the respective quantiles (one row)
    predicted_q <- quantiles_by_hour %>% ungroup() %>% filter(HOUR == hour) %>%
      select(-HOUR) %>% as.numeric()
    return(predicted_q)
  }
  joinedForecast <- sapply(hours, getForecast)
  # sapply puts outputs of getForecast in columns, but we want them in rows
  return(t(joinedForecast))
}


# GEFCOM14 Benchmark forecast : predict for all quantiles (1% up to 99%) the
# power generation value of last year at exactly the same date
# Therefore train has to comprise the one year past of test
benchmark <- function(X_train, y_train, X_test, print=FALSE, version = 0,
                      variableVersion = 0) {
  if (print) {
    outputForecastingMethod("benchmark forecast", "None",
                            c("Issue", "for", "every", "timestamp", "the",
                              "power", "production", "of", "last", "year",
                              "ago", "as", "all", "quantiles", "(point-measure",
                            "on", "value", "last", "year", "ago"))
    return("")
  }
  forecast_in <- X_test$TIMESTAMP

  # predict for all quantiles the one year past power generation
  getLastYearVal <- function(date) {
    year(date) <- year(date) - 1
    power <- X_train %>% mutate(POWER = y_train) %>% filter(TIMESTAMP == date)
    # it could be that train doesn't contain last year's value, then return NA
    out <-if(!identical(power$POWER, numeric(0)))  power$POWER  else NA
    return(rep(out, length(QUANTILES)))
  }

  joinedForecast <- sapply(forecast_in, getLastYearVal)
  # sapply puts outputs of getLastYearVal in columns, we want that in rows
  return(t(joinedForecast))
}

#evaluation(trivialForecast, pinBallLoss, "empirical quantiles")
#evaluation(benchmark, pinBallLoss, "benchmark")
#evaluation(unleashIDR, pinBallLoss, "IDR", 1, 1)
#evaluation(unleashIDR, pinBallLoss, "IDR", 1, 2)
#evaluation(unleashIDR, pinBallLoss, "IDR", 2, 1)
#evaluation(unleashIDR, pinBallLoss, "IDR", 2, 2)
#evaluation(unleashIDR, pinBallLoss, "IDR", 2, 3)
#evaluation(unleashIDR, pinBallLoss, "IDR", 2, 4)