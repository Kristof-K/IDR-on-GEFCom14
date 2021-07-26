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
      exa <- na.omit(list[[element]])
      # get TARGET to the end
      examine <- exa[, c(names(exa)[names(exa) != "TARGET"], "TARGET")]
      if (sd(examine[["TARGET"]]) != 0 && sd(examine[, var]) != 0) {
        output[var,1:3,c] <- c(cor(examine[,var], examine[["TARGET"]],
                                  method="pearson"),
                              cor(examine[,var], examine[["TARGET"]],
                                  method="spearman"),
                              cor(examine[,var], examine[["TARGET"]],
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
                PBZ=PBZ, TRACK=track, ID=paste0(id, collapse="_",
                                                GR="no_grouping")))
  }
  # function: calculate all quantiles and return data.frame
  getAllQuantiles <- function(x) {
    return(data.frame(PROBS=QUANTILES, Q=quantile(x, QUANTILES)))
  }
  # get for every hour the 99 empirical quantiles which we will use as forecast
  # pivot_wider transforms the long data table into a wide one
  quantiles_by_hour <- X_train %>%
    mutate(Y = y_train, HOUR = hour(TIMESTAMP)) %>% filter(!is.na(Y)) %>%
    group_by(HOUR) %>% summarise(getAllQuantiles(Y), .groups="drop") %>%
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
                TRACK="Solar", ID="1", GR="no_grouping"))
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
    return(list(TIT="benchmark", VAR="None", OR="None", PBZ=TRUE, TRACK="Wind",
                ID="1", GR="no_grouping"))
  }
  quantiles <- quantile(y_train, probs=QUANTILES, na.rm=TRUE)
  return(matrix(rep(quantiles, each=nrow(X_test)), nrow=nrow(X_test)))
}



# GROUPING FUNCTIONS

# Group data in categories: these functions should return a vector of categories
# if getCategories is TRUE, otherwise return boolean vector assigning each
# timestamp a value inidicating whether it belongs to given group or not

getGroupingfct <- function(nr) {
  return(switch(nr, no_gr, getHours, getMonths, getSeasons, get4Seasons,
         getWind100Directions, getSeasonHours, getSeasonLargeHours,
         getSeasonDayTime))
}

no_gr <- function(data, group_nr, getCategories=FALSE, getGroupVar=FALSE,
                  getName=FALSE, test=FALSE) {
  if (getCategories) return(1)
  if (getGroupVar) return(NULL)
  if (getName) return("no_grouping")
  return(TRUE)
}

getHours <- function(data, hour, getCategories=FALSE, getGroupVar=FALSE,
                     getName=FALSE, test=FALSE) {
  hours <- 0:23
  if (getCategories) return(hours)
  if (getGroupVar) return("TIMESTAMP")
  if (getName) return("hours")
  timestamps <- data$TIMESTAMP
  return(hour(timestamps) == hour)
}

getMonths <- function(data, month, getCategories=FALSE, getGroupVar=FALSE,
                     getName=FALSE, test=FALSE) {
  months <- 1:12
  if (getCategories) return(months)
  if (getGroupVar) return("TIMESTAMP")
  if (getName) return("months")
  timestamps <- data$TIMESTAMP
  return(month(timestamps) == month)
}

getSeasons <- function(data, season, getCategories=FALSE, getGroupVar=FALSE,
                     getName=FALSE, test=FALSE) {
  seasons <- c(1,2,3)
  if (getCategories) return(seasons)
  if (getGroupVar) return("TIMESTAMP")
  if (getName) return("3seasons")
  timestamps <- data$TIMESTAMP
  m <- month(timestamps)
  seasonized <- 1 * (m %in% c(12, 1, 2)) + 2 * (m %in% 3:11) + 3 * (m %in% 6:8)
  return(seasonized == season)
}

get4Seasons <- function(data, season, getCategories=FALSE, getGroupVar=FALSE,
                        getName=FALSE, test=FALSE) {
  seasons <- c(1,2,3,4)
  if (getCategories) return(seasons)
  if (getGroupVar) return("TIMESTAMP")
  if (getName) return("4seasons")
  timestamps <- data$TIMESTAMP
  m <- month(timestamps)
  seasonized <- 1 * (m %in% c(12, 1, 2)) + 2 * (m %in% 3:5) + 3 * (m %in% 6:8) +
    4 * (m %in% 9:11)
  return(seasonized == season)
}

getWind100Directions <- function(data, direction, getCategories=FALSE,
                                 getGroupVar=FALSE, getName=FALSE,
                                 test=FALSE) {
  directions <- 1:12
  if (getCategories) return(directions)
  if (getGroupVar) return("A100")
  if (getName) return("wind_direction")
  bins <- length(directions)
  # bin wind directions (method from dplyr)
  binWinDir <- ceiling((data$A100 + 180) / 360 * bins)
  windDirections <- binWinDir + 1 * (binWinDir == 0) # forget
  return(windDirections == direction)
}

getSeasonHours <- function(data, group_nr, getCategories=FALSE, getGroupVar=FALSE,
                     getName=FALSE, test=FALSE) {
  groups <- 1:(4*24)
  if (getCategories) return(groups)
  if (getGroupVar) return("TIMESTAMP")
  if (getName) return("hours+4seasons")
  timestamps <- data$TIMESTAMP
  m <- month(timestamps)
  grouped <- 1 * (m %in% c(12, 1, 2)) + 25 * (m %in% 3:5) + 49 * (m %in% 6:8) +
    73 * (m %in% 9:11) + hour(timestamps)
  return(grouped == group_nr)
}

getSeasonLargeHours <- function(data, group_nr, getCategories=FALSE,
                                getGroupVar=FALSE, getName=FALSE, test=FALSE) {
  groups <- 1:(4*24)
  if (getCategories) return(groups)
  if (getGroupVar) return("TIMESTAMP")
  if (getName) return("extended_hours+4seasons")
  timestamps <- data$TIMESTAMP
  m <- month(timestamps)
  grouped <- 1 * (m %in% c(12, 1, 2)) + 25 * (m %in% 3:5) + 49 * (m %in% 6:8) +
    73 * (m %in% 9:11) + hour(timestamps)
  if (test) {
    return(grouped == group_nr)
  } else {
    prev <- if(group_nr %% 24 == 1) group_nr else group_nr - 1
    succ <- if(group_nr %% 24 == 0) group_nr else group_nr + 1
    return(grouped %in% c(prev, group_nr, succ))
  }
}

getSeasonDayTime <- function(data, group_nr, getCategories=FALSE,
                             getGroupVar=FALSE, getName=FALSE, test=FALSE) {
  groups <- 1:(4*6)
  if (getCategories) return(groups)
  if (getGroupVar) return("TIMESTAMP")
  if (getName) return("4hour_groups+4seasons")
  timestamps <- data$TIMESTAMP
  m <- month(timestamps)
  h <- hour(timestamps)
  seasonGroup <- 1 * (m %in% c(12, 1, 2)) + 7 * (m %in% c(3, 4, 5)) +
    13 * (m %in% c(6, 7, 8)) + 19 * (m %in% c(9, 10, 11))
  hourGroup <- 0 * (h %in% 0:3) + 1 * (h %in% 4:7) + 2 * (h %in% 8:11) +
    3 * (h %in% 12:15) + 4 * (h %in% 16:19) + 5 * (h %in% 20:23)

  return(group_nr == (seasonGroup + hourGroup))
}