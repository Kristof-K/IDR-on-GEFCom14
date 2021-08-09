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
    data[[zone]] <- data[[zone]][!is.na(data[[zone]][["TARGET"]]),]
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
  for(zone in data$Zones) {
    for(t in c("Train", "Test")) {
      data[[zone]][[t]] <- mutate(data[[zone]][[t]], S10 = sqrt(U10^2 + V10^2),
                                  S100 = sqrt(U100^2 + V100^2),
                                  A10 = atan2(V10, U10) * 360 / (2*pi),
                                  A100 = atan2(V100, U100) * 360 / (2*pi)) %>%
        relocate(TARGET, .after = last_col())
    }
  }
  return(data)
}

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