library(lubridate)  # handling dates
library(dplyr)
library(tidyr)      # for pivot_wider

QUANTILES <- seq(0.01, 0.99, 0.01)
TASKS <- 1:15
FIRST_EVAL_TASK <- 4        # in GEFCom14 the first 3 tasks weren't evaluated

PRINT_WIDTH <- 70
NONE <- "None"

# Calculate the pearson, spearman and kendall correlation coefficient for every
# variable for every category in list
# - list : list of data.frames for every category
# - categroy : vector of names defining the categories
# return 3-dim array: 1st axis variables, 2nd axis correlation coefficient typ
# (pearson, spearman, kendall), 3rd axis categories
getCorrelationCoefficients <- function(list, categories, numOfVariables) {
  output <- array(NA, dim=c(numOfVariables, 3, length(categories)))
  c <- 1
  for (element in categories) {       # iterate over 3rd dimension
    for (var in 1:numOfVariables) {   # iterate over 1st dimension
      examine <- na.omit(list[[element]])
      if (sd(examine[["POWER"]]) != 0 && sd(examine[, var]) != 0) {
        output[var,1:3,c] <- c(cor(examine[,var], examine[["POWER"]],
                                  method="pearson"),
                              cor(examine[,var], examine[["POWER"]],
                                  method="spearman"),
                              cor(examine[,var], examine[["POWER"]],
                                  method="kendall"))
      }
    }
    c <- c+1
  }
  return(output)
}


# Calculate the pinball loss for
# - x : matrix containing prediction for the 1% up to 99% quantile in every row
# - y : vector containg the respective observations
# - print : output information if True otherwise calc score
pinBallLoss <- function(x, y, init=FALSE) {
  if (init) {
    outputScoringFunction(c("Pinball-Loss /", "asymmetric", "piecewise",
                          "linear", "scoring", "fct."))
    return("pinball-loss")
  }
  singlePinBallLoss <- function(vec) {
    # separate y and x again
    y <- vec[1]
    x <- vec[-1]
    scoreVec <- ((y < x) - QUANTILES) * (x - y)
    return(mean(scoreVec))
  }
  # in order to use apply more easily
  join <- cbind(y, x)
  # apply pinball loss on every row in the given x vector
  return(apply(join, 1, singlePinBallLoss))
}

# Method output description consistently for a specific scoring function
# - name : name of the forecasting method (it should be a vector of words in
#   order to be printed correctly)
outputScoringFunction <- function(name) {
  cat("\n[SCORING FUNCTION]:", name, "\n", fill = PRINT_WIDTH)
}

# Method output consistently a specific forecasting method
# - name : name of the forecasting method
# - description : short text descripbing the method further (it should be a
#   vector of words in order to print correctly)
# - vars : vector of variable nems that are incorporated
# - preprocess : vecotr of preprocess methods used
outputForecastingMethod <- function(name, description, vars=NONE) {
  cat("\n\n=================================================================\n")
  cat(" ", name,"\n")
  cat("=================================================================\n")
  cat(description, "\n", fill = PRINT_WIDTH)
  cat("[VARIABLES]:", vars, "\n")
}

# Make a trivial forecast by using the empirical qauntiles of past obervations
# belonging to the hour for which a forecast is issued
trivialForecast <- function(X_train, y_train, X_test, id=c(1, 1), init=FALSE) {
  track <- switch(id[1], "Solar", "Wind")
  PBZ <- (if(id[2] == 1) TRUE else FALSE)
  if (init) {
    outputForecastingMethod("trivial forecast",
                            c("Calculate", "for", "every", "hour", "the",
                            "empirical", "quantiles", "and", "return", "them",
                            "irrespective", "of", "any", "variable", "values"))
    return(list(TIT=paste0("empirical quantiles ", PBZ), VAR="None", OR="None",
                PBZ=PBZ, TRACK=track))
  }
  # function: calculate all quantiles and return data.frame
  getAllQuantiles <- function(x) {
    return(data.frame(PROBS=QUANTILES, Q=quantile(x, QUANTILES)))
  }
  # get for every hour the 99 empirical quantiles which we will use as forecast
  # pivot_wider transforms the long data table into a wide one
  quantiles_by_hour <- X_train %>%
    mutate(Y = y_train, HOUR = hour(TIMESTAMP)) %>% filter(!is.na(Y)) %>%
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


# GEFCOM14 solar benchmark forecast : predict for all quantiles (1% up to 99%)
# the power generation value of last year at exactly the same date
# Therefore train has to comprise the one year past of test
benchmarkSolar <- function(X_train, y_train, X_test, id=1, init=FALSE) {
  if (init) {
    outputForecastingMethod("solar benchmark forecast",
                            c("Issue", "for", "every", "timestamp", "the",
                              "power", "production", "of", "last", "year",
                              "ago", "as", "all", "quantiles", "(point-measure",
                            "on", "value", "one", "year", "ago)"))
    return(list(TIT="benchmark", VAR="None", OR="None", PBZ=TRUE,
                TRACK="Solar"))
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

# GEFCOM14 wind benchmark forecast : use empirical quantiles of training data
# as (climatological) prediction
benchmarkWind <- function(X_train, y_train, X_test, id=1, init=FALSE) {
  if (init) {
    outputForecastingMethod("wind benchmark forecast",
                            c("Issue", "for", "every", "timestamp", "the",
                              "empirical", "quantiles", "as", "quantile",
                              "predictions"))
    return(list(TIT="benchmark", VAR="None", OR="None", PBZ=TRUE, TRACK="Wind"))
  }
  quantiles <- quantile(y_train, probs=QUANTILES, na.rm=TRUE)
  return(matrix(rep(quantiles, each=nrow(X_test)), nrow=nrow(X_test)))
}



# GROUPING FUNCTIONS

# Group data in categories: these functions should return a vector of categories
# if getCategories is TRUE, otherwise return boolean vector assigning each
# timestamp a value inidicating whether it belongs to given group or not

getHours <- function(timestamps, hour, getCategories=FALSE) {
  hours <- 0:23
  if (getCategories) {
    return(hours)
  }
  return(hour(timestamps) == hour)
}

getMonths <- function(timestamps, month, getCategories=FALSE) {
  months <- 1:12
  if (getCategories) {
    return(months)
  }
  return(month(timestamps) == month)
}

getSeasons <- function(timestamps, season, getCategories=FALSE) {
  seasons <- c(1,2,3)
  if (getCategories) {
    return(seasons)
  }
  seasonized <- 1 * (month(timestamps) %in% c(12, 1, 2)) +
                2 * (month(timestamps) %in% c(3, 4, 5, 9, 10, 11)) +
                3 * (month(timestamps) %in% c(6, 7, 8))
  return(seasonized == season)
}