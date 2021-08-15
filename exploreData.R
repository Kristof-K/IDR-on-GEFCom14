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

plotsForSlides <- function() {
  data_all <- deaccumulateSol(loadSolar(15))

  for(zone in "ZONE3") {
    data <- rbind(data_all[[zone]]$Train, data_all[[zone]]$Test)
    plotData <- transmute(data, Power=TARGET, Radiation=VAR169,
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

  d <- loadLoad(15)
  d$Zone1$Train %>% select(TIMESTAMP, TARGET, w1) %>% filter(!is.na(TARGET)) %>%
    mutate(Month = month(TIMESTAMP, label=TRUE),
           t = day(TIMESTAMP), m = month(TIMESTAMP),
           Positiv = m %in% 5:9 | (m == 4 & t > 20) | (m == 10 & t <= 20)) %>%
    select(Month, TARGET, w1, Positiv) %>%
    ggplot(aes(x=w1, y=TARGET, color=Positiv)) +
    geom_point(alpha=0.1) +
    facet_wrap(~Month) +
    xlab("Temperature") +
    ylab("Load") +
    theme_bw() +
    ggtitle("Scatterplot: Load ~ Temperature") +
    ggsave(paste0("LoadTemp.pdf"), path="../Overview",
             width=10, height=6)

  S <- data.frame(score=c(0.012132, 0.012247, 0.012785, 0.013342, 0.014166,
                          0.014288, 0.014294, 0.0149964, 0.0154773, 0.01549,
                          0.015977, 0.0165608, 0.0166733, 0.01755, 0.0178692,
                          0.0184789, 0.0208583, 0.02128, 0.025524, 0.0317609,
                          0.0375917, 0.01995319, 0.014839771, 0.013732111),
                  group=c(rep("par", 20), "ben", rep("idr", 3)),
                  name=c(paste0(1:20, c("st", "nd", "rd", rep("th", 17))),
                         "Benchmark", "IDR", "IDR hours 1", "IDR hours 5"),
                  label=c(rep("", 20), "", "IDR on\nradiation",
                          "IDR on\nradiation\ngrouped\nby hours",
                          "5 variables\n+ hour groups\n+ subagging")) %>%
    arrange(desc(score)) %>%
    mutate(name = factor(name, ordered=TRUE, levels=name),
           index=1:length(score))

  W <- data.frame(score=c(0.03711091, 0.03827364, 0.03831, 0.03834417,
                          0.0389725, 0.03946167, 0.04149818, 0.04473917,
                          0.04532083, 0.04657583, 0.0629325, 0.06853833,
                          0.08670583, 0.04476315, 0.04505536, 0.044895936,
                          0.04307092),
                  group=c(rep("par", 12), "ben", rep("idr", 4)),
                  name=c(paste0(1:12, c("st", "nd", "rd", rep("th", 9))),
                         "Benchmark", "IDR", "IDR by hours", "IDR by seasons",
                         "IDR by wind angle"),
                  label=c(rep("", 12), "",
                          "IDR on\nwind speed at 100m\n+ different grouping",
                          rep("", 3))) %>%
    arrange(desc(score)) %>%
    mutate(name = factor(name, ordered=TRUE, levels=name),
           index=1:length(score))
  
  P <- data.frame(score=c(2.72260416, 2.73264, 2.82480333, 3.3393825,
                          3.38856916, 3.67968, 4.15617545, 4.46310583,
                          4.48946636, 5.08586916, 5.20363454, 5.2952075,
                          7.7270825, 8.86687636, 10.58733333, 19.46707916,
                          5.219633, 4.051974, 4.027405),
                  group=c(rep("par", 15), "ben", rep("idr", 3)),
                  name=c(paste0(1:15, c("st", "nd", "rd", rep("th", 12))),
                         "Benchmark", "IDR", "seasons", "seasons+\nhours*"),
                  label=c(rep("", 17),
                          "IDR on\n1 variable\n+ different grouping",
                          rep("", 1))) %>%
    arrange(desc(score)) %>%
    mutate(name = factor(name, ordered=TRUE, levels=name),
           index=1:length(score))
  
  L <- data.frame(score=c(7.197126, 7.23131727, 7.45392166, 7.50520916,
                          7.535513, 7.95646833, 8.466394, 8.58871,
                          8.62505818, 8.93657583, 9.26274090, 9.407766,
                          9.48056416, 9.5641, 10.00587875, 10.25994833,
                          10.62373454, 11.50976555, 11.76980833, 12.17955666,
                          13.32187909, 15.5810025, 10.61661, 8.610069),
                  group=c(rep("par", 21), "ben", rep("idr", 2)),
                  name=c(paste0(1:21, c("st", "nd", "rd", rep("th", 18))),
                         "Benchmark", "IDR", "seasons+hours*"),
                  label=c(rep("", 22), "",
                          "IDR on 1 variable,\nmean temp.\nas forecast")) %>%
    arrange(desc(score)) %>% mutate(index=1:length(score))

  plotResults <- function(data, track, f=0.5) {
    ggplot(data) +
      geom_col(aes(x=factor(index), y=score, fill=group), show.legend=FALSE,
              position="dodge2") +
      xlab("") +
      ylab("") +
      ggtitle(paste("Mean pinball scores", track, "track")) +
      theme_bw() +
      theme(text = element_text(size = 15),
            axis.text.x = element_blank()) +
      geom_text(aes(x=index, y=score + f * score * (score <= max(score)*1/(f*3)),
                    label=label, color=group), size=5, show.legend=FALSE) +
      geom_text(aes(x=index, y=score/2, label=name), angle=90) +
      ggsave(paste0("Scores_", track, ".pdf"), path="../Overview",
             width=10, height=5)
  }
  plotResults(S, "solar")
  plotResults(W, "wind")
  plotResults(P, "price", f=1.5)
  plotResults(L, "load")
}
