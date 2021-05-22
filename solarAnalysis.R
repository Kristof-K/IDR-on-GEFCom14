setwd("D:/Studium/Semester6/BachelorArbeit/Code")

source("loadData.R")
source("util.R")
source("plot.R")

zones <- c("ZONE1", "ZONE2", "ZONE3")


# implement the whole process of examination solar track
examineSolar <- function() {
  # data is a list containing for every string in zones a data frame
  data <- loadSolar(15)
  numOfVars <- length(variableNames)  # variableNames is defined in plot.R
  
  # EXAMINE DATA GROUPED BY HOUR
  for (zone in zones) {
    hours <- paste0("", 0:23)   # categories by which is grouped
    groupByHour <- list()       # list containing data.frame for every category
    # get the data
    for (hour in hours) {
      indices <- belongsToHour(data[[zone]]$TIMESTAMP, hour)
      groupByHour[[hour]] <- subset(data[[zone]], indices, select=-TIMESTAMP)
    }
    min <- sapply(data[[zone]][-1], min) # get for every column min and max
    max <- sapply(data[[zone]][-1], max)# drop timestamp => -1
    
    # now plot
    scatterHours(groupByHour, hours, zone, min, max)
    scatterAllSingle(groupByHour, hours, zone, min, max, hour=TRUE)
    # and examine the correlation coefficients
    coeffs <- getCorrelationCoefficients(groupByHour, hours, numOfVars)
    correlationPlotHours(coeffs, hours, zone)
  }
  
  # EXAMINE DATA GROUPED BY MONTH
  for (zone in zones) {
    months <- paste0("", 1:12)   # categories by which is grouped
    groupByMonth <- list()       # list containing data.frame for every category
    # get the data
    for (month in months) {
      indices <- belongsToMonth(data[[zone]]$TIMESTAMP, month)
      groupByMonth[[month]] <- subset(data[[zone]], indices, select=-TIMESTAMP)
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


# VISUAL RESULT:
# positive propotionality to Power
# [strong]  : surface solar rad down (VAR169), top net solar rad (VAR178)
# [weak]    : 2-meter temperature (VAR167)
# negative proportionality to Power
# [strong]  :
# [weak]    : relative humidity (VAR 157)
# 