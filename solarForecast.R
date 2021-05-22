#setwd("D:/Studium/Semester6/BachelorArbeit/Code")

source("loadData.R")

library(isodistrreg)

# prediction model
# - trivial IDR on all data
# - IDR on every hour (three variable models)
# - IDR on every hour +- 1 hour (~extended probabilistoc climatological)
# - IDR on every astronomical hour (same sunposition)

tasks <- 1:15
zones <- c("ZONE1", "ZONE2", "ZONE3")


# Routine conducting evaluation for a given prediciton and scoring method
# - predictionfct : function getting X_train (data.frame containing covariates)
#   y_train (vector containing response variable) and X_test (data.frame
#   containing covariates for test data) and returning vector of response
#   variable for testing data
#   if print=TRUE is passed, the function should print some explaining text
# - scoringfct : function getting two vectors of size 99, whereby 1st
#   corresponds to predictions for all 1% quantiles and 2nd to true observation  
#   The meturns should return an average score over all quantiles
#   if print=TRUE is passed, the function should print some explaining text
evaluation <- function(predictionfct, scoringfct) {
  # use the print functionality in order to get some explaining text
  predictionfct(NA, NA, NA, print=TRUE)
  scoringfct(NA, NA, print=TRUE)
  
  # in GEFCom14 the first 3 tasks weren't evaluated
  firstTaskToEvalaute <- 4
  # matrix containing for every task and every zone the average score
  averageScore <- array(0, dim=c(length(tasks), length(zones)))
  
  for (task in tasks) {
    data <- loadSolar(task)   # read data
    
    z <- 1
    for (zone in zones) {
      current <- data[[zone]]  # restrict focus to one zone each iteration
      # get true observations (y) and train and test covariates, response var
      y <- subset(current, TIMESTAMP > data$LastTest_TS)[["POWER"]]
      y_train <- subset(current, TIMESTAMP <= data$LastTest_TS)[["POWER"]]
      X_train <- subset(current, TIMESTAMP <= data$LastTest_TS, 
                        select=c(-TIMESTAMP, -POWER))
      X_test <- subset(current, TIMESTAMP > data$LastTest_TS, 
                        select=c(-TIMESTAMP, -POWER))
      # conduct prediction
      prediction <- predictfct(X_train, y_train, X_test)
      # conduct scoring
      averageScore[task, z] <- scoringfct(prediction, y)
      
      z <- z+1
    }
  }
  results <- data.frame(Task = tasks, averageScore)
  colnames(results) <- zones
  results
  finalScore <- mean(as.matrix(subset(results, Task >= firstTaskToEvalaute, 
                                      select=-Task)))
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