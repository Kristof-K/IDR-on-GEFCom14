# define preprocess methods getting X_train and X_test and applying data
# transformation on both

library(dplyr)    # for lag in deaccumulateSun and transmute in

source("util.R")

ACCUMULATED <- c("VAR169", "VAR178", "VAR175", "VAR228")

# Method output description consistently for a preprocessing function
# - name : name of the forecasting method (it should be a vector of words in
#   order to be printed correctly)
outputPreprocessing <- function(name) {
  cat("\n[PREPROCESSING]:", name, "\n", fill = PRINT_WIDTH)
}

no_pp <- function(data, init=FALSE) {
  name <- NONE
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  return(data)
}

rm_na <- function(data, init=FALSE) {
  name <- "RemoveNAs"
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  for(zone in data$Zones) {
    for (t in c("Train", "Test")) {
      data[[zone]][[t]] <- filter(data[[zone]][[t]], !is.na(TARGET))
    }
  }
  return(data)
}

# expect data to be of format definied in load.R
deaccumulateSol <- function(data, init=FALSE) {
  name <- paste("deacc", paste(ACCUMULATED, collapse="_"), sep="_")
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  subShifted <- function(col) {
    return(col - lag(col, default=0))
  }
  # assume that data is ordered in time and conitnuous and first entry starts
  # at 1:00, since there starts the daily sun accumulation
  for (zone in data$Zones) {
    for(var in ACCUMULATED) {
      for(t in c("Train", "Test")) {
        current <- data[[zone]][[t]][[var]]
        asMatrix <- matrix(current, nrow=24)    # every column contains one day
        # deaccumulate every day individually
        deacc <- apply(asMatrix, 2, subShifted)

        data[[zone]][[t]][[var]] <- as.vector(deacc)
      }
    }
  }
  return(data)
}

getWindAttributes <- function(data, init=FALSE) {
  name <- "CalcWindAttributes"
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  # determined as max of Spearman correlation between wind power prod. and
  # linearly approximated wind speed at height h (use subagging to stabilize)
  indivHeights <- setNames(c(81, 113, 107, 77, 106, 117, 83, 92, 83, 134),
                           paste0("Zone", 1:10))
  for(zone in data$Zones) {
    alpha <- (indivHeights[[zone]] - 10) / (100 - 10)
    for(t in c("Train", "Test")) {
      data[[zone]][[t]] <- mutate(data[[zone]][[t]], S10 = sqrt(U10^2 + V10^2),
                                  S100 = sqrt(U100^2 + V100^2),
                                  A10 = atan2(V10, U10) * 360 / (2*pi),
                                  A100 = atan2(V100, U100) * 360 / (2*pi),
                                  SX = (1-alpha) * S10 + alpha * S100) %>%
        relocate(TARGET, .after = last_col())
    }
  }
  return(data)
}

addPriceRegressors <- function(data, init=FALSE) {
  name <- "addPriceRegressors"
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  # electricity price means for the respective groupings
  hourVals <- c(34.41, 38.06, 52.11, 56.81, 61.65, 48.15)
  wdayVals <- c(43.11, 49.83, 49.57, 49.26, 51.77, 50.67, 45.52)
  wdayHourVals <- c(35.81, 32.58, 42.8, 46.15, 54.17, 47.17, 33.83,	39.76,
                    54.57, 58.11, 63.92, 48.77, 33.25, 39.49, 53.95, 58.71,
                    63.7, 48.31, 32.66,	38.97, 53.58, 59.47, 63.33, 47.54, 34.2,
                    40.4, 55.63, 63.14,	67.52, 49.73, 34.78, 40.51,	55.94,
                    61.71, 62.96, 48.12, 36.36,	34.73, 48.29, 50.38, 55.93,
                    47.41)
  wday2Vals <- c(50.22, 44.32)
  wday2HourVals <- c(33.74, 39.82, 54.73, 60.23, 64.28, 48.49,
                     36.09, 33.66, 45.55, 48.26, 55.05, 47.29)
  for(zone in data$Zones) {
    for(t in c("Train", "Test")) {
      d <- data[[zone]][[t]]
      wdays <- getWday(d)
      data[[zone]][[t]] <- mutate(d,
                                  WDAY = wdayVals[getWday(d)],
                                  HOUR6 = hourVals[get6DayTime(d)],
                                  WDAYHOUR6 = wdayHourVals[getWday6Hour(d)],
                                  WDAY4 = case_when(wdays %in% 2:4 ~  49.56,
                                                    wdays %in% 5:6 ~ 51.22,
                                                    wdays == 7 ~ 45.52,
                                                    wdays == 1 ~ 43.11),
                                  WDAY4_CAT = case_when(wdays %in% 2:4 ~  3,
                                                    wdays %in% 5:6 ~ 4,
                                                    wdays == 7 ~ 2,
                                                    wdays == 1 ~ 1),
                                  WDAY2 = wday2Vals[get2Wday(d)],
                                  WDAY2HOUR6 = wday2HourVals[get2Wday6Hour(d)])
    }
  }
  return(data)
}

constructTempGenerator <- function(fct, name) {

tempGenerator <- (function(data, init=FALSE) {
    if (init) {
      outputPreprocessing(name)
      return(name)
    }
    tCols <- paste0("w", 1:25)
    for(zone in data$Zones) {
      newW <- mutate(data[[zone]]$Train, h=hour(TIMESTAMP), d=day(TIMESTAMP),
                     m=month(TIMESTAMP)) %>%
        group_by(m, d, h) %>%
        summarise(across(all_of(tCols), ~fct(.)), .groups="drop")
      findRow <- function(t) {
        r <- which(newW[["m"]]==month(t) & newW[["d"]]==day(t) &
                   newW[["h"]]==hour(t))
        return(as.numeric(newW[r, tCols]))
      }
      # sapply packs individual results in columns => transpose it
      filtered <- t(sapply(data[[zone]]$Test$TIMESTAMP, findRow))
      colnames(filtered) <- tCols

      data[[zone]]$Test <- cbind(data[[zone]]$Test, data.frame(filtered))
      data[[zone]]$Train <- filter(data[[zone]]$Train, !is.na(TARGET))
    }
    return(data)
  })
  return(tempGenerator)
}

meanTemp <- constructTempGenerator(mean, "meanTemp")
lastTemp <- constructTempGenerator(function(x) return(last(x)),
                                   "lastTemp")
sampleTemp <- constructTempGenerator(function(x) sample(x, 1),
                                     "sampleTemp")
# mean linear weighted x, so that last x comcponent has double the weight of 1st
linWeight2 <- function(x) {
  n <- length(x)
  return(sum(x * (1:n * 2/(3*(n^2-n)) + 2/3 * (n-2)/(n^2-n))))
}
lwMeanTemp <- constructTempGenerator(linWeight2, "lwMeanTemp")

addLoadMeans <- function(data, init=FALSE) {
  name <- "AddLoadMeans"
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  data <- rm_na(data)
  for(zone in data$Zones) {
    for(t in c("Train", "Test")) {
      data[[zone]][[t]] <- mutate(data[[zone]][[t]],
                                  M3= rowMeans(cbind(w10, w13, w25)),
                                  M6 = rowMeans(cbind(w10, w13, w25, w24, w23,
                                                      w22)),
                                  Med3 = apply(cbind(w10, w13, w25), 1,
                                               median),
                                  Med6 = apply(cbind(w10, w13, w25, w24, w23, w22),
                                               1, median))
    }
  }
  return(data)
}