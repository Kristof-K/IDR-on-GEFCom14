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
      current <- data[[zone]][[var]]
      asMatrix <- matrix(current, nrow=24)    # every column contains one day
      # deaccumulate every day individually
      deacc <- apply(asMatrix, 2, subShifted)

      data[[zone]][[var]] <- as.vector(deacc)
    }
  }
  return(data)
}

getWindAttributes <- function(data, init=FALSE) {
  name <- "CalcWindVelocities"
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  for(zone in data$Zones) {
    data[[zone]] <- mutate(data[[zone]], W10 = sqrt(U10^2 + V10^2),
                           W100 = sqrt(U100^2 + V100^2),
                           A10 = atan2(V10, U10) * 360 / (2*pi),
                           A100 = atan2(V100, U100) * 360 / (2*pi)) %>%
      relocate(POWER, .after = last_col())
  }
  return(data)
}