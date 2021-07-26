#setwd("D:/Studium/Semester6/BachelorArbeit/Code")

source("loadData.R")
source("util.R")
source("plot.R")
source("preprocess.R")


# for time series plots we need start and end dates for the different tracks
getStartDate <- function(track) {
  return(switch(track, "Solar"="2013-09-09 00:00:00 UTC",
                "Wind"="2013-09-09 00:00:00 UTC",
                "Load"="2011-07-01 00:00:00 UTC"))
}

getEndDate <- function(track) {
  return(switch(track, "Solar"="2013-09-19 00:00:00 UTC",
                "Wind"="2013-09-19 00:00:00 UTC",
                "Load"="2011-07-11 00:00:00 UTC"))
}


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
    plotAgainstTime(data[[zone]], getStartDate(track), getEndDate(track), track,
                    zone)
  }
}

examineSolarHourDeacc <- function() {
  # data is a list containing for every string in zones a data frame
  deacc <- deaccumulateSol(loadSolar(15))
  deacc_vars <- c(ACCUMULATED, "TARGET")

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
    plotAgainstTime(deacc[[zone]], getStartDate(track), getEndDate(track),
                    track="Solar", zone, suf="deacc")
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

examineTarget <- function(track, preprocess=no_pp) {
  data <- preprocess(loadSet(track,15))
  cat("Examine target variable")

  for (zone in data$Zones) {
    plotHeatMap(data[[zone]][c("TIMESTAMP", "TARGET")], track, zone)
    plotTargetCurves(data[[zone]][c("TIMESTAMP", "TARGET")], track, zone, e=TRUE)
    plotTargetCurves(data[[zone]][c("TIMESTAMP", "TARGET")], track, zone, e=FALSE)
    plotAreaCurves(data[[zone]][c("TIMESTAMP", "TARGET")], track, zone)
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
  data <- getWindAttributes(loadWind(15))

  for(zone in data$Zones) {
     plotTimeSeries(data[[zone]], "2012-09-09 00:00:00 UTC",
                    "2012-09-19 00:00:00 UTC", "Wind",
                    name=paste0("TimeSeries_Wind_", zone, ".png"))
    plotHistograms(data[[zone]], "Wind", zone)
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
  examineTarget(track)
}

examineLoad <- function() {
  cat("Load:\n")
  track <- "Load"
  examineHour(track, preprocess=rm_na)
  examineMonth(track, preprocess=rm_na)
  examineTarget(track, preprocess=rm_na)
  data <- rm_na(loadSet(track, 15))
  plotHistograms(data$ZONE1, track, "Zone1")
  plotTimeSeries(data$ZONE1, "2011-09-09 00:00:00 UTC",
                    "2011-09-19 00:00:00 UTC", track,
                    name=paste0("TimeSeries_Load_Sep_", "ZONE1", ".png"))
  plotTimeSeries(data$ZONE1, "2011-02-09 00:00:00 UTC",
                    "2011-02-19 00:00:00 UTC", track,
                    name=paste0("TimeSeries_Load_Feb_", "ZONE1", ".png"))
}

plotsForSlides <- function() {
  data <- deaccumulateSol(loadSolar(15))

  for(zone in "ZONE3") {
    plotData <- transmute(data[[zone]], Power=TARGET, Radiation=VAR169,
                          Hour=as.factor(hour(TIMESTAMP)))
    print(ggplot(plotData, aes(x=Radiation, y=Power)) +
      geom_point() +
      scale_color_manual(values = rainbow(24)) +
      theme_bw()+
      ggtitle("Scatterplot: Power ~ Solar radiation downwards") +
      ggsave("VAR169.pdf", path="../WorkshopIAI/"))

    print(ggplot(plotData, aes(x=Radiation, y=Power, color=Hour)) +
      geom_point() +
      scale_color_manual(values = rainbow(24)) +
      theme_bw() +
      ggtitle("Scatterplot: Power ~ Solar radiation downwards") +
      ggsave("VAR169Col.pdf", path="../WorkshopIAI/"))

    print(ggplot(plotData, aes(x=Radiation, y=Power, color=Hour)) +
      geom_point() +
      facet_wrap(~Hour) +
      scale_color_manual(values = rainbow(24)) +
      theme_bw() +
      ggtitle("Scatterplot: Power ~ Solar radiation downwards"))
  }

  S <- data.frame(score=c(0.012132, 0.012247, 0.012785, 0.013342, 0.014166,
                          0.014288, 0.014294, 0.0149964, 0.0154773, 0.01549,
                          0.015977, 0.0165608, 0.0166733, 0.01755, 0.0178692,
                          0.0184789, 0.0208583, 0.02128, 0.025524, 0.0317609,
                          0.0375917, 0.014839771, 0.013732111),
                  group=c(rep("par", 20), "ben", rep("idr", 2)),
                  name=c(paste0(1:20, c("st", "nd", "rd", rep("th", 17))),
                         "Benchmark", "IDR hours 1", "IDR hours 5"),
                  label=c(rep("", 20), "",
                          "1 variable", "5 variables\n+ subagging")) %>%
    arrange(desc(score)) %>%
    mutate(name = factor(name, ordered=TRUE, levels=name))

  W <- data.frame(score=c(0.03711091, 0.03827364, 0.03831, 0.03834417,
                          0.0389725, 0.03946167, 0.04149818, 0.04473917,
                          0.04532083, 0.04657583, 0.0629325, 0.06853833,
                          0.08670583, 0.04476315, 0.04505536, 0.044895936,
                          0.04307092),
                  group=c(rep("par", 12), "ben", rep("idr", 4)),
                  name=c(paste0(1:12, c("st", "nd", "rd", rep("th", 9))),
                         "Benchmark", "IDR all", "IDR hours", "IDR seasons",
                         "IDR wind angle"),
                  label=c(rep("", 12), "", "1 variable\n+ different grouping",
                          rep("", 3))) %>%
    arrange(desc(score)) %>%
    mutate(name = factor(name, ordered=TRUE, levels=name))

  plotResults <- function(data, track) {
    ggplot(data) +
      geom_col(aes(x=name, y=score, fill=group), show.legend=FALSE,
              position="dodge2") +
      xlab("") +
      ylab("") +
      ggtitle(paste("Mean pinball scores", track, "track")) +
      theme_bw() +
      theme(text = element_text(size = 15),
            axis.text.x = element_text(angle = 90)) +
      geom_text(aes(x=name, y=score + 0.5 * score * (score <= max(score)*2/3),
                    label=label, color=group), , size=5, show.legend=FALSE) +
      ggsave(paste0("Scores_", track, ".pdf"), path="../WorkshopIAI/",
             width=10, height=6)
  }
  plotResults(S, "solar")
  plotResults(W, "wind")
}
