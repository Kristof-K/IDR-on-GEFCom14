#setwd("D:/Studium/Semester6/BachelorArbeit/Code")

source("loadData.R")
source("util.R")
source("plot.R")
source("preprocess.R")


# implement the whole process of examination solar track
examineHour <- function() {
  # data is a list containing for every string in zones a data frame
  data <- loadSolar(15)
  numOfVars <- length(variableNames)  # variableNames is defined in plot.R

  # EXAMINE DATA GROUPED BY HOUR
  cat("Examing data grouped by hour:\n")
  for (zone in data$Zones) {
    cat("\t-", zone, "\n")
    hours <- paste0(0:23)   # categories by which is grouped
    groupByHour <- list()       # list containing data.frame for every category
    # get the data
    for (h in hours) {
      indices <- hour(data[[zone]]$TIMESTAMP) == h
      groupByHour[[h]] <- subset(data[[zone]], indices, select=-TIMESTAMP)
    }
    min <- sapply(data[[zone]][-1], min) # get for every column min and max
    max <- sapply(data[[zone]][-1], max) # drop timestamp => -1
    
    # now plot
    scatterHours(groupByHour, hours, zone, min, max)
    scatterAllSingle(groupByHour, hours, zone, min, max, hour=TRUE)
    # and examine the correlation coefficients
    coeffs <- getCorrelationCoefficients(groupByHour, hours, numOfVars)
    correlationPlotHours(coeffs, hours, zone)
    # plot time series
    plotAgainstTime(data[[zone]], "2013-09-09 00:00:00 UTC",
                    "2013-09-19 00:00:00 UTC", zone)
  }
}

examineHourDeacc <- function() {
  # data is a list containing for every string in zones a data frame
  deacc <- deaccumulateSol(loadSolar(15))
  deacc_vars <- c(ACCUMULATED, "POWER")

  # EXAMINE DATA GROUPED BY HOUR
  cat("Examing deacccumulated data grouped by hour:\n")
  for (zone in deacc$Zones) {
    cat("\t-", zone, "\n")
    hours <- paste0(0:23)   # categories by which is grouped
    groupByHour <- list()       # list containing data.frame for every category
    # get the data
    for (h in hours) {
      indices <- hour(deacc[[zone]]$TIMESTAMP) == h
      groupByHour[[h]] <- subset(deacc[[zone]], indices, select=deacc_vars)
    }
    # consider deaccumulated
    plotAgainstTime(deacc[[zone]], "2013-09-09 00:00:00 UTC",
                    "2013-09-19 00:00:00 UTC", zone, suf="deacc")
    min <- sapply(deacc[[zone]][deacc_vars], min)
    max <- sapply(deacc[[zone]][deacc_vars], max)
    for(var in ACCUMULATED) {
      scatterSingleHours(groupByHour, hours, zone, min, max, var, suf="deacc")
    }
  }
}

examineMonth <- function() {
  # data is a list containing for every string in zones a data frame
  data <- loadSolar(15)
  numOfVars <- length(variableNames)  # variableNames is defined in plot.R
  # EXAMINE DATA GROUPED BY MONTH
  cat("Examing data grouped by month:\n")
  for (zone in data$Zones) {
    cat("\t-", zone, "\n")
    months <- paste0(1:12)   # categories by which is grouped
    groupByMonth <- list()       # list containing data.frame for every category
    # get the data
    for (m in months) {
      indices <- month(data[[zone]]$TIMESTAMP) == m
      groupByMonth[[m]] <- subset(data[[zone]], indices, select=-TIMESTAMP)
    }
    min <- sapply(data[[zone]][-1], min) # get for every column min and max
    max <- sapply(data[[zone]][-1], max)# drop timestamp => -1
    
    # now plot
    scatterMonths(groupByMonth, months, zone, min, max)
    scatterAllSingle(groupByMonth, months, zone, min, max, hour=FALSE)
    # and examine the correlation coefficients
    coeffs <- getCorrelationCoefficients(groupByMonth, months, numOfVars)
    correlationPlotMonths(coeffs, months, zone)
  }
}

examinePower <- function() {
  data <- loadSolar(15)

  for (zone in data$Zones) {
    plotPowerHeatMap(data[[zone]][c("TIMESTAMP", "POWER")], zone)
  }
}

examineSolar <- function() {
  examineHour()
  examineMonth()
  examineHourDeacc()
  examinePower()
}

examinePower()

# VISUAL RESULT:
# positive propotionality to Power
# [strong]  : surface solar rad down (VAR169), top net solar rad (VAR178)
# [weak]    : 2-meter temperature (VAR167)
# negative proportionality to Power
# [strong]  :
# [weak]    : relative humidity (VAR 157)
