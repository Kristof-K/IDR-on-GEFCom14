#setwd("D:/Studium/Semester6/BachelorArbeit/Code")

source("loadData.R")
source("util.R")
source("plot.R")
source("preprocess.R")


# implement the whole process of examination solar track
examineHour <- function(track, preprocess=no_pp) {
  # data is a list containing for every string in zones a data frame
  data <- preprocess(loadSet(track,15))
  numOfVars <- length(getVars(track))  # variableNames is defined in plot.R

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
    scatterHours(groupByHour, track, zone, min, max)
    scatterAllSingle(groupByHour, track, zone, min, max, hour=TRUE)
    # and examine the correlation coefficients
    coeffs <- getCorrelationCoefficients(groupByHour, hours, numOfVars)
    correlationPlotHours(coeffs, track, zone)
    # plot time series
    plotAgainstTime(data[[zone]], "2013-09-09 00:00:00 UTC",
                    "2013-09-19 00:00:00 UTC", track, zone)
  }
}

examineSolarHourDeacc <- function() {
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
                    "2013-09-19 00:00:00 UTC", track="Solar", zone,
                    suf="deacc")
    min <- sapply(deacc[[zone]][deacc_vars], min)
    max <- sapply(deacc[[zone]][deacc_vars], max)
    for(var in ACCUMULATED) {
      scatterSingleHours(groupByHour, "Solar", zone, min, max, var,
                         suf="deacc")
    }
  }
}

examineMonth <- function(track, preprocess=no_pp) {
  # data is a list containing for every string in zones a data frame
  data <- preprocess(loadSet(track,15))
  numOfVars <- length(getVars(track))  # variableNames is defined in plot.R
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
    scatterMonths(groupByMonth, track, zone, min, max)
    scatterAllSingle(groupByMonth, track, zone, min, max, hour=FALSE)
    # and examine the correlation coefficients
    coeffs <- getCorrelationCoefficients(groupByMonth, months, numOfVars)
    correlationPlotMonths(coeffs, track, zone)
  }
}

examinePower <- function(track) {
  data <- loadSet(track,15)

  for (zone in data$Zones) {
    plotPowerHeatMap(data[[zone]][c("TIMESTAMP", "POWER")], track, zone)
    plotPowerCurves(data[[zone]][c("TIMESTAMP", "POWER")], track, zone, e=TRUE)
    plotPowerCurves(data[[zone]][c("TIMESTAMP", "POWER")], track, zone, e=FALSE)
    plotPowerAreaCurves(data[[zone]][c("TIMESTAMP", "POWER")], track, zone)
  }
}

examineSolar <- function() {
  cat("Solar:\n")
  track <- "Solar"
  examineHour(track)
  examineMonth(track)
  examineSolarHourDeacc()
  examinePower(track)
}

#examineSolar()

# VISUAL RESULT:
# positive propotionality to Power
# [strong]  : surface solar rad down (VAR169), top net solar rad (VAR178)
# [weak]    : 2-meter temperature (VAR167)
# negative proportionality to Power
# [strong]  :
# [weak]    : relative humidity (VAR 157)


examineWindFeatures <- function() {
  data <- getWindAttributes(loadWind(15))

  for(zone in data$Zones) {
     plotTimeSeries(data[[zone]], "2012-09-09 00:00:00 UTC",
                    "2012-09-19 00:00:00 UTC", "Wind",
                    name=paste0("TimeSeries_Wind_", zone, ".png"))
    for (n in c(NA, 4, 8, 12)) {
      scatterWindPower(data[[zone]], "Wind", zone, bins=n)
      estimatePowerDistribution(data[[zone]], "Wind", zone, bins=n)
    }
  }
}

examineWind <- function() {
  cat("Wind:\n")
  track <- "Wind"
  examineHour(track, preprocess=getWindAttributes)
  examineMonth(track, preprocess=getWindAttributes)
  examineWindFeatures()
  examinePower(track)
}

examineWindFeatures()