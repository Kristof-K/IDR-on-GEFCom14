#setwd("D:/Studium/Semester6/BachelorArbeit/Code")

source("loadData.R")
source("util.R")
source("plot.R")
source("preprocess.R")


# for time series plots we need start and end dates for the different tracks
getStartDate <- function(track) {
  return(switch(track, "Solar"="2013-09-09 00:00:00 UTC",
                "Wind"="2013-09-09 00:00:00 UTC",
                "Load"="2011-07-01 00:00:00 UTC",
                "Price"="2013-05-01 00:00:00 UTC"))
}

getEndDate <- function(track) {
  return(switch(track, "Solar"="2013-09-19 00:00:00 UTC",
                "Wind"="2013-09-19 00:00:00 UTC",
                "Load"="2011-07-11 00:00:00 UTC",
                "Price"="2013-05-11 00:00:00 UTC"))
}


# implement the whole process of examination solar track
examineHour <- function(track, preprocess=no_pp) {
  # data is a list containing for every string in zones a data frame
  data_all <- preprocess(loadSet(track,15))
  vars <- names(getVars(track))

  # EXAMINE DATA GROUPED BY HOUR
  cat("Examing data grouped by hour:\n")
  for (zone in data_all$Zones) {
    cat("\t-", zone, "\n")
    hours <- paste0(0:23)   # categories by which is grouped
    groupByHour <- list()       # list containing data.frame for every category
    if (track != "Load") {
      data <- rbind(data_all[[zone]]$Train, data_all[[zone]]$Test)
    } else {
      data <- data_all[[zone]]$Train
    }
    # get the data
    for (h in hours) {
      indices <- hour(data$TIMESTAMP) == h
      groupByHour[[h]] <- subset(data, indices, select=-TIMESTAMP)
    }
    min <- sapply(data[vars], min) # get for every column min and max
    max <- sapply(data[vars], max) # drop timestamp => -1
    
    # now plot
    scatterHours(groupByHour, track, zone, min, max)
    scatterAllSingle(groupByHour, track, zone, min, max, hour=TRUE)
    # and examine the correlation coefficients
    coeffs <- getCorrelationCoefficients(groupByHour, hours, vars)
    correlationPlotHours(coeffs, track, zone)
    # plot time series
    plotAgainstTime(data, getStartDate(track), getEndDate(track), track,
                    zone)
  }
}

examineSolarHourDeacc <- function() {
  track <- "Solar"
  # data is a list containing for every string in zones a data frame
  data_all <- deaccumulateSol(loadSolar(15))
  deacc_vars <- c(ACCUMULATED, "TARGET")

  # EXAMINE DATA GROUPED BY HOUR
  cat("Examing deacccumulated data grouped by hour:\n")
  for (zone in data_all$Zones) {
    cat("\t-", zone, "\n")
    hours <- paste0(0:23)   # categories by which is grouped
    groupByHour <- list()       # list containing data.frame for every category
    data <- rbind(data_all[[zone]]$Train, data_all[[zone]]$Test)
    # get the data
    for (h in hours) {
      indices <- hour(data$TIMESTAMP) == h
      groupByHour[[h]] <- subset(data, indices, select=deacc_vars)
    }
    # consider deaccumulated
    plotAgainstTime(data, getStartDate(track), getEndDate(track),
                    track=track, zone, suf="deacc")
    min <- sapply(data[deacc_vars], min)
    max <- sapply(data[deacc_vars], max)
    for(var in ACCUMULATED) {
      scatterSingleHours(groupByHour, track, zone, min, max, var, suf="deacc")
    }
  }
}

examineMonth <- function(track, preprocess=no_pp) {
  # data is a list containing for every string in zones a data frame
  data_all <- preprocess(loadSet(track,15))
  vars <- names(getVars(track))
  # EXAMINE DATA GROUPED BY MONTH
  cat("Examing data grouped by month:\n")
  for (zone in data_all$Zones) {
    cat("\t-", zone, "\n")
    months <- paste0(1:12)   # categories by which is grouped
    groupByMonth <- list()       # list containing data.frame for every category
    if (track != "Load") {
      data <- rbind(data_all[[zone]]$Train, data_all[[zone]]$Test)
    } else {
      data <- data_all[[zone]]$Train
    }
    # get the data
    for (m in months) {
      indices <- month(data$TIMESTAMP) == m
      groupByMonth[[m]] <- subset(data, indices, select=-TIMESTAMP)
    }
    min <- sapply(data[vars], min) # get for every column min and max
    max <- sapply(data[vars], max)# drop timestamp => -1
    
    # now plot
    scatterMonths(groupByMonth, track, zone, min, max)
    scatterAllSingle(groupByMonth, track, zone, min, max, hour=FALSE)
    # and examine the correlation coefficients
    coeffs <- getCorrelationCoefficients(groupByMonth, months, vars)
    correlationPlotMonths(coeffs, track, zone)
  }
}

examineTarget <- function(track, preprocess=no_pp) {
  data_all <- preprocess(loadSet(track,15))
  cat("Examine target variable")

  for (zone in data_all$Zones) {
    if (track != "Load") {
      data <- rbind(data_all[[zone]]$Train, data_all[[zone]]$Test)
    } else {
      data <- data_all[[zone]]$Train
    }
    plotHeatMap(data[c("TIMESTAMP", "TARGET")], track, zone)
    plotAllTargetCurves(data[c("TIMESTAMP", "TARGET")], track, zone)
    if (track %in% c("Load", "Price")) {
      plotAllTargetCurves(data[c("TIMESTAMP", "TARGET")], track, zone,
                          groupingfct=getWday)
    }
    plotAreaCurves(data[c("TIMESTAMP", "TARGET")], track, zone)
  }
}

examineSolar <- function() {
  cat("Solar:\n")
  track <- "Solar"
  examineHour(track)
  examineMonth(track)
  examineSolarHourDeacc()
  examineTarget(track)
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
  data_all <- getWindAttributes(loadWind(15))

  for(zone in data_all$Zones) {
    data <- rbind(data_all[[zone]]$Train, data_all[[zone]]$Test)
    plotTimeSeries(data, "2012-09-09 00:00:00 UTC",
                    "2012-09-19 00:00:00 UTC", "Wind",
                    name=paste0("TimeSeries_Wind_", zone, ".png"))
    plotHistograms(data, "Wind", zone)
    for (n in c(NA, 4, 8, 12)) {
      scatterWindPower(data, "Wind", zone, bins=n)
      estimatePowerDistribution(data, "Wind", zone, bins=n)
    }
  }
}

examineWind <- function() {
  cat("Wind:\n")
  track <- "Wind"
  examineHour(track, preprocess=getWindAttributes)
  examineMonth(track, preprocess=getWindAttributes)
  examineWindFeatures()
  examineTarget(track)
}

examineLoad <- function() {
  cat("Load:\n")
  track <- "Load"
  examineHour(track, preprocess=rm_na)
  examineMonth(track, preprocess=rm_na)
  examineTarget(track, preprocess=rm_na)
  data_all <- rm_na(loadSet(track, 15))
  for (z in data_all$Zones) {
    data <- data_all[[z]]$Train
    plotHistograms(data, track, z)
    plotTimeSeries(data, "2011-09-09 00:00:00 UTC",
                    "2011-09-19 00:00:00 UTC", track,
                    name=paste0("TimeSeries_Load_Sep_", z, ".png"))
    plotTimeSeries(data, "2011-02-09 00:00:00 UTC",
                    "2011-02-19 00:00:00 UTC", track,
                    name=paste0("TimeSeries_Load_Feb_", z, ".png"))
    plotRanges(data, track, z, getWdayWithHolidays)
    plotHolidays(data, track, z)
    plotHolidays(data, track, z, e=FALSE)

    data %>% filter(month(TIMESTAMP) ==4) %>% mutate(X=day(TIMESTAMP)) %>%
    select(-TIMESTAMP) %>%
    pivot_longer(cols=c(-TARGET, -X), names_to="Temperature") %>%
    ggplot(aes(x=value, y=TARGET, color=factor(X))) +
    facet_wrap(~Temperature) +
    geom_point()
  }
  data <- loadLoad(15)
  data$Zone1$Train %>% mutate(Hour = hour(TIMESTAMP), Year = year(TIMESTAMP),
                              Month = month(TIMESTAMP)) %>%
    group_by(Hour, Year, Month) %>% summarise(averageTmp = mean(w10),
                                              .groups="drop") %>%
    ggplot(aes(x=Month, y=averageTmp, color=as.factor(Year))) +
    facet_wrap(~Hour) +
    geom_line() +
    ggtitle("Average monthly temperature of w10 for every specific hour")
}

examinePrice <- function() {
  cat("Load:\n")
  track <- "Price"
  examineHour(track)
  examineMonth(track)
  examineTarget(track)
  data_all <- loadSet(track, 15)
  for (z in data_all$Zones) {
    data <- rbind(data_all[[z]]$Train, data_all[[z]]$Test)
    plotHistograms(data, track, z)
  }
}
