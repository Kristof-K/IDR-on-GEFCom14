library(lubridate)  # handling dates
library(dplyr)
library(tidyr)      # for pivot_wider

QUANTILES <- seq(0.01, 0.99, 0.01)
TASKS <- 4:15
TUNE_TASKS <- 1:3

PRINT_WIDTH <- 70
NONE <- "None"

# Calculate the pearson, spearman and kendall correlation coefficient for every
# variable for every category in list
# - list : list of data.frames for every category
# - categroy : vector of names defining the categories
# return 3-dim array: 1st axis variables, 2nd axis correlation coefficient typ
# (pearson, spearman, kendall), 3rd axis categories
getCorrelationCoefficients <- function(list, categories, vars) {
  output <- array(NA, dim=c(length(vars), 3, length(categories)))
  c <- 1
  for (element in categories) {       # iterate over 3rd dimension
    v <- 1
    examine <- na.omit(list[[element]])
    for (var in vars) {   # iterate over 1st dimension
      if (sd(examine[["TARGET"]]) != 0 && sd(examine[[var]]) != 0) {
        output[v,1:3,c] <- c(cor(examine[[var]], examine[["TARGET"]],
                                  method="pearson"),
                              cor(examine[[var]], examine[["TARGET"]],
                                  method="spearman"),
                              cor(examine[[var]], examine[["TARGET"]],
                                  method="kendall"))
      }
      v <- v+1
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
                getSeasonDayTime, getWday, getMonthWday, getMonthDayTime,
                getMonthWdTimeHour, getWdayWithHolidays))
}

no_gr <- function(data, group_nr=NA, getCategories=FALSE, getGroupVar=FALSE,
                  getName=FALSE, test=FALSE) {
  if (getCategories) return(1)
  if (getGroupVar) return(NULL)
  if (getName) return("no_grouping")
  if(is.na(group_nr)) {
    return(1)
  }
  return(TRUE)
}

# Construct a grouping method with given labels and a method assigning numbers
# (or labels) to each row in data, which assign each row to a group
# groupVar should specify which columns in data are used for grouping and name
# is for logging only
constructGrouping <- function(labels, groupVar, name, method) {
  return(function(data, group_nr=NA, getCategories=FALSE, getGroupVar=FALSE,
                     getName=FALSE, test=FALSE, label=FALSE) {
    if (getCategories) return(1:length(labels))
    if (getGroupVar) return(groupVar)
    if (getName) return(name)
    grouped <- method(data)
    if(is.na(group_nr)) {
      if (label) {
        return(factor(labels[grouped], ordered=TRUE, levels=labels))
      }
      return(grouped)
    }
    return(grouped == group_nr)
  })
}

getHours <- constructGrouping(
  paste0(0:23, "h"), "TIMESTAMP", "hours",
  function(data) return(hour(data$TIMESTAMP) + 1)
)

getMonths <- constructGrouping(
  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
    "Dez"), "TIMESTAMP", "months",
  function(data) return(month(data$TIMESTAMP))
)

getSeasons <- constructGrouping(
  c("Dez,Jan,Feb", "Mar-May,Sep-Nov", "Jun,Jul,Aug"), "TIMESTAMP",
  "3seasons",
  function(data) {
    m <- month(data$TIMESTAMP)
    return(1 * (m %in% c(12,1,2)) + 2 * (m %in% c(3:5,9:11)) + 3 * (m %in% 6:8))
  }
)

get4Seasons <- constructGrouping(
  c("Dez,Jan,Feb", "Mar,Apr,May", "Jun,Jul,Aug", "Sep,Oct,Nov"),
  "TIMESTAMP","3seasons",
  function(data) {
    m <- month(data$TIMESTAMP)
    return(1 * (m %in% c(12, 1, 2)) + 2 * (m %in% 3:5) + 3 * (m %in% 6:8) +
             4 * (m %in% 9:11))
  }
)

getWind100Directions <- constructGrouping(
  paste(1:12), "A100", "12WindDir",
  function(data) {
    bins <- 12
    binWinDir <- ceiling((data$A100 + 180) / 360 * bins)
    return(binWinDir + 1 * (binWinDir == 0))    # remove 0 bin
  }
)

get6DayTime <- constructGrouping(
  paste(1:6), "TIMESTAMP", "6hours",
  function(data) return(hour(data$TIMESTAMP) %/% 4 + 1)
)

get4DayTime <- constructGrouping(
  paste(1:4), "TIMESTAMP", "4hours",
  function(data) {
    h <- hour(data$TIMESTAMP)
    return(1 * (h %in% 4:9) + 2 * (h %in% 10:15) + 3 * (h %in% 16:21) +
             4 * (h %in% c(22, 23, 0, 1, 2, 3)))
  }
)

getWday <- constructGrouping(
  c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), "TIMESTAMP",
  "wdays",
  function(data) return(wday(data$TIMESTAMP))
)

get2Wday <- constructGrouping(
  c("workweek", "weekend"), "TIMESTAMP", "2wdays",
  function(data) {
    w <- wday(data$TIMESTAMP)
    return(1 * (w %in% 2:6) + 2 * (w %in% c(7,1)))
  }
)

# construct product of two groupings (assume groups assign numbers 1,...,n and
# 1,...,m (with this method the set of grouping methods is a mathematical group)
productGrouping <- function(gr_one, gr_two) {
  return(function(data, group_nr=NA, getCategories=FALSE, getGroupVar=FALSE,
                  getName=FALSE, test=FALSE) {
    n <- length(gr_one(NA, getCategories=TRUE))
    m <- length(gr_two(NA, getCategories=TRUE))
    groups <- 1:(n*m)
    v <- unique(c(gr_one(NA, getGroupVar=TRUE), gr_two(NA, getGroupVar=TRUE)))
    if (getCategories) return(groups)
    if (getGroupVar) return(paste(v, collapse=","))
    if (getName) return(paste(gr_one(NA, getName=TRUE),
                              gr_two(NA, getName=TRUE), sep = "+"))
    grouped = (gr_one(data) - 1) * m + gr_two(data)
    if (is.na(group_nr)) return(grouped)
    return(grouped == group_nr)
  })
}

getSeasonHours <- productGrouping(get4Seasons, getHours)
getSeasonDayTime <- productGrouping(get4Seasons, get6DayTime)
getMonthWday <- productGrouping(getMonths, getWday)
getMonthWdTimeHour <- productGrouping(getMonths, get6DayTime)
getMonthWdTimeHour <- productGrouping(getMonths,
                                      productGrouping(get4DayTime, get2Wday))

getSeasonLargeHours <- function(data, group_nr=NA, getCategories=FALSE,
                                getGroupVar=FALSE, getName=FALSE, test=FALSE) {
  if (getCategories) return(getSeasonHours(NA, getCategories=TRUE))
  if (getGroupVar) return(getSeasonHours(NA, getGroupVar=TRUE))
  if (getName) return(paste0("extended_", getSeasonHours(NA, getName=TRUE)))
  grouped <- getSeasonHours(data)
  if (is.na(group_nr)) {
    return(grouped)
  }
  if (test) {
    return(grouped == group_nr)
  } else {
    # use group plus lower and upper neighboring group for training
    # attention: 24 blocks are cyclic
    prev <- if(group_nr %% 24 == 1) (group_nr %/% 24 + 1)*24 else group_nr - 1
    succ <- if(group_nr %% 24 == 0) (group_nr %/% 24 - 1)*24+1 else group_nr + 1
    return(grouped %in% c(prev, group_nr, succ))
  }
}

getWdayWithHolidays <- function(data, group_nr=NA, getCategories=FALSE,
                                getGroupVar=FALSE, getName=FALSE, label=FALSE,
                                test=FALSE) {
  groups <- 1:17
  labs <- c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa", "NewYear", "MLKing",
            "Washing", "Memorial", "Independence", "Labour", "Columbus",
            "Veterans", "Thanksgiving", "Christmas")
  if (getCategories) return(groups)
  if (getGroupVar) return("TIMESTAMP")
  if (getName) return("7wday+10Holidays")
  timestamps <- data$TIMESTAMP

  # help functions
  weekDay <- function(t) wday(t, label=TRUE)
  # some holidays are always the 3-th Monday of May ...
  isNthWdayOf <- function(t, mon, n, wday_label) {
    return(month(t) == mon & weekDay(t) == wday_label &
           day(t) >= 1 + 7*(n-1) & day(t) <= 7*n)
  }
  # others have specific dates, but are prolonged if this date is Sat or Sun
  dateExWeekEnd <- function(t, mon, d) {
    tar <- ymd(paste(year(t) + 1*(month(t)==12 & mon==1), mon, d, sep="-"))
    return(date(tar)==date(t) |
             (weekDay(tar)=="So" & date(t)==date(tar+days(1))) |
             (weekDay(tar)=="Sa" & date(t)==date(tar-days(1))))
  }
  # actual holiday indicator functions
  isNewYear <- function(t) dateExWeekEnd(t, 1, 1)
  isMLKing <- function(t) isNthWdayOf(t, 1, 3, "Mo")
  isWashing <- function(t) isNthWdayOf(t, 2, 3, "Mo")
  isMemorial <- function(t) {
    return(month(t) == 5 & weekDay(t) == "Mo" & day(t) >= 25)
  }
  isIndependence <- function(t) dateExWeekEnd(t, 7, 4)
  isLabour <- function(t) isNthWdayOf(t, 9, 1, "Mo")
  isColumbus <- function(t) isNthWdayOf(t, 10, 2, "Mo")
  isVeterans <- function(t) dateExWeekEnd(t, 11, 11)
  isThanksgiving <- function(t) isNthWdayOf(t, 11, 4, "Do")
  isChristmas <- function(t) dateExWeekEnd(t, 12, 25)

   grouped <- case_when(
    isNewYear(timestamps) ~ 8,
    isMLKing(timestamps) ~ 9,
    isWashing(timestamps) ~ 10,
    isMemorial(timestamps) ~ 11,
    isIndependence(timestamps) ~ 12,
    isLabour(timestamps) ~ 13,
    isColumbus(timestamps) ~ 14,
    isVeterans(timestamps) ~ 15,
    isThanksgiving(timestamps) ~ 16,
    isChristmas(timestamps) ~ 17,
    TRUE ~ wday(timestamps)
  )
  if (is.na(group_nr)) {
    if (label) {
      return(factor(labs[grouped], ordered=TRUE, levels=labs))
    }
    return(grouped)
  }
  return(grouped == group_nr)
}
