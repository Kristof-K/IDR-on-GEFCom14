library(ggplot2)
library(dplyr)
library(tidyr)      # for pivot_longer
library(stringr)    # for string manipulations
library(lubridate)  # for working with dates

solarVars <- c("VAR78" = "liquid water",
               "VAR79" = "Ice water",
               "VAR134" = "surface pressure",
               "VAR157" = "relative humidity",
               "VAR164" = "total cloud cover",
               "VAR165" = "10-meter u-wind",
               "VAR166" = "10-meter v-wind",
               "VAR167" = "2-meter temperature",
               "VAR169" = "surface solar rad down",
               "VAR175" = "surface thermal rad down",
               "VAR178" = "top net solar rad",
               "VAR228" = "total precipitation")
windVars <- c("U10"="U10", "V10"="V10", "U100"="U100", "V100"="V100",
              "W10" = "10m wind speed", "W100" = "100m wind speed",
              "A10" = "10m wind direction", "A100" = "100m wind direction")

corr_c <- c("pearson", "spearman", "kendall")

months <- paste(1:12)
hours <- paste(0:23)
monthsLabel <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
      "Nov", "Dez")
hoursLabel <- paste(hours, "h")

powerLim <- c(0, 1.1) # power is normalized, i.e it lives in the unit interval


# Get the plot limits of a variable given the min and max data.frames
# - var : variable code as defined in variableNames
# - min / max : data.frame containg for every variable min and max value
getLimits <- function(var, min, max) {
  myMax <- if (max[[var]] >= 0) max[[var]] * 1.01 else max[[var]] * 0.99
  myMin <- if (min[[var]] >= 0) min[[var]] * 0.99 else min[[var]] * 1.01
  return(c(myMin, myMax))
}

# Get the variables belonging to a track
# - track : name of the track
getVars <- function(track) {
  return(switch(track, "Solar"=solarVars, "Wind"=windVars))
}
# Get a grid to plot all variables (dependent on number of vars in the track)
# - track : name of the track
getGrid <- function(track) {
  return(switch(track, "Solar"=c(3,4), "Wind"=c(2,4)))
}

# Plot scatter plots for every variable in variableNames
# - list : list of data.frames for every category
# - track : current track for naming the plots
# - zone : current zone (only label for plotting)
# - min / max : data.frame containg for every variable min and max value
scatterHours <- function(list, track, zone, min, max) {
  png(paste0(file="plots/", track, "/scatterByHours_", zone, ".png"), width=1600, height=900)
  scatterFull(list, categories=hours, track, zone, min, max, labels=hoursLabel,
              title="grouped by hour")
  dev.off()
}

scatterMonths <- function(list, track, zone, min, max) {
  png(file=paste0("plots/", track, "/scatterByMonths_", zone, ".png"), width=1600, height=900)
  scatterFull(list, categories=months, track, zone, min, max, labels=monthsLabel,
              title="grouped by month")
  dev.off()
}

scatterFull <- function(list, categories, track, zone, min, max, labels,
                        title="") {
  colors <- rainbow(length(categories))
  vars <- getVars(track)
  
  par(mfrow=getGrid(track), mar=c(4, 0, 0, 0), oma=c(3,2,3,2), mgp=c(1.4,0.6,0))
  j <- 0
  for (var in names(vars)) {
    y_axis <- if(j %% 4 == 0) "s" else "n"      # plot y axis or not
    
    plot(0, type="n", ylab="Power", yaxt=y_axis, xlab=vars[[var]],
         xlim=getLimits(var, min, max), ylim=powerLim)
    c <- 1
    for (element in categories) {
      lines(list[[element]][[var]], list[[element]][["POWER"]], pch=20, 
            type="p", col=colors[c])
      c <- c+1
    }
    j <- j+1
  }
  add <- if (title != "") paste(",", title) else ""
  mtext(paste0(zone, ": Power ~ variables", add), outer=TRUE, line=0.5, cex=1.5)
  mtext(labels, side=1, outer=TRUE, line=0.5, col=colors,
        at=seq(0.1, 0.9, 0.8 / (length(categories) - 1)))
  # set parameters back to default
  par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1, oma=c(0,0,0,0), mgp=c(3 ,1 ,0))
}

# Plot pearson, spearman and kendall correlation coefficients for every variable
# and every category
# - coefficients : 3-dim array containing the correlation coefficients (on 1st
#   axis variables, 2nd coefficient type (pearson, spearman, kendall) and on 3rd
#   the categories)
# - tracl : name of the current track (only for naming the plot)
# - zone : current zone (only label for plotting)
correlationPlotHours <- function(coefficients, track, zone) {
  png(file=paste0("plots/", track, "/correlationByHours_", zone, ".png"), width=900, height=600)
  correlationPlot(coefficients, categories=hours, track, zone,
                  title="grouped by hour", label=hoursLabel)
  dev.off()
}

correlationPlotMonths <- function(coefficients, track, zone) {
  png(file=paste0("plots/", track, "/correlationByMonths_", zone, ".png"), width=900, height=600)
  correlationPlot(coefficients, categories=months, track, zone,
                  title="grouped by month", labels=monthsLabel)
  dev.off()
}

correlationPlot <- function(coefficients, categories, track, zone, title="",
                            labels) {
  vars <- getVars(track)
  n <- length(vars)
  x <- 1:n
  colors <- rainbow(length(categories))
  shapes <- c(15, 16, 17)
  shift <- c(-0.33, 0, 0.33)
  
  par(mfrow=c(1,1), mar=c(3.5, 2, 0, 3), oma=c(3,0,3,0))
  
  plot(c(0.5,n+0.5), c(0,0), type="l", lty=3, xaxt="n", xlim=c(0,13), xlab="", 
       ylab="", ylim=c(-1.2,1.2))
  
  for(var in 1:n) {   # iterate through 1st axis of coeff
    for(i in 1:3) {                       # iterate through 2nd axis of coeff
      lines(rep(var + shift[i], length(categories)), coefficients[var, i, ],
            type="p", cex=1.2, col=colors, pch=shapes[i])
    }
    # draw separation line
    if (var < n) {
      lines(rep(var + 0.5, 2), c(-1, 1), type="l", lty=3)
    }
  }
  
  axis(1, at=x, labels=names(vars), cex.axis=0.8, las=2)
  add <- if (title != "") paste(",", title) else ""
  mtext(paste0(zone, ": Correlation(Power,variables)", add), outer=TRUE, 
        line=0.5, cex=1.5)
  mtext(labels, side=1, outer=TRUE, line=0, col=colors,
        at=seq(0.1, 0.9, 0.8 / (length(categories) - 1)))
  mtext(paste(corr_c, c("(square)", "(circle)", "(triangle)")), side=1,  
        outer=TRUE, line=1, at=c(0.3, 0.5, 0.7))
  par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1, oma=c(0,0,0,0))
}


# Plot scatter plots for a single variable, every category gets an own plot
# - list : list of data.frames for every category
# - track : name of the track (for naming the plot)
# - zone : current zone (only label for plotting)
# - min / max : data.frame containg for every variable min and max value
# - name : name of the varibale that should be plotted
# - suf : text that should be appended to the name
scatterSingleHours <- function(list, track, zone, min, max, name, suf="") {
  suf <- if (suf != "") paste0("_", suf) else suf
  png(file=paste0("plots/", track, "/scatterByHours_", name, "_", zone, suf, ".png"), width=1600, height=900)
  scatterSingle(list, categories=hours, track, zone, min, max, name,
                label=hoursLabel, title=paste0(name, ", grouped by hour"),
                grid=c(4,6))
  dev.off()
}

scatterSingleMonths <- function(list, track, zone, min, max, name, suf="") {
  suf <- if (suf != "") paste0("_", suf) else suf
  png(file=paste0("plots/", track, "/scatterByMonth_", name, "_", zone, suf, ".png"), width=1600, height=900)
  scatterSingle(list, categories=months, track, zone, min, max, name,
                labels=monthsLabel, title=paste0(name, ", grouped by month"),
                grid=c(3,4))
  dev.off()
}

scatterAllSingle <- function(list, track, zone, min, max, hour=TRUE) {
  vars <- getVars(track)
  for (var in names(vars)) {
    if (hour) {
      scatterSingleHours(list, track, zone, min, max, var)
    } else {
      scatterSingleMonths(list, track, zone, min, max, var)
    }
  }
}

scatterSingle <- function(list, categories, track, zone, min, max, name,
                       labels, title="", grid=c(1,1)) {
  colors <- rainbow(length(categories))
  vars <- getVars(track)
  
  par(mfrow=grid, mar=c(0, 0, 0, 0), oma=c(4,4,4,2), mgp=c(1.2,0.6,0))
  
  j <- 0
  for (element in categories) {
    y_axis <- if(j %% grid[2] == 0) "s" else "n"      # plot x/y axis or not
    x_axis <- if(floor(j / grid[2]) == grid[1] - 1) "s" else "n"
    
    limits <- getLimits(name, min, max)
    
    plot(list[[element]][[name]], list[[element]][["POWER"]], type="p", pch=20,   
         ylab="Power", yaxt=y_axis, xaxt=x_axis, xlab=vars[[name]],
         xlim=limits, ylim=powerLim, col=colors[j+1], cex=2)
    text(x=0.1*limits[1] + 0.9*limits[2], y=0.9*powerLim[2], labels[j + 1],
         cex=1.7)
    j <- j+1
  }
  add <- if (title != "") paste(",", title) else ""
  mtext(paste0(zone, ": Power ~ ", vars[[name]], add), outer=TRUE, line=0.5,
        cex=1.5)
  # set parameters back to default
  par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1, oma=c(0,0,0,0), mgp=c(3 ,1 ,0))
}


# plot variables against time in one plot to assess the development
# - data : data.frame with TIMESTAMPS as one solumn and other variables
# - start : first timestamp, from what time we should start plotting
# - end : last timestamp, up to what time we should plot
# - track : current track for naming the plot
# - zone : current zone for naming the plot
# - suf : text that should be appended to the name
plotAgainstTime <- function(data, start, end, track, zone, suf="") {
  suf <- if (suf != "") paste0("_", suf) else suf
  plotTimeSeries(data, start, end, track,
                 name=paste0("TimeSeries_", zone, suf, ".png"))
}

plotTimeSeries <- function(data, start, end, track, name) {
  # restrict data to timestamps of interest
  plotData <- filter(data, TIMESTAMP %within% interval(start, end))
  # get timestamps and replace them in the data frame with the values 1,...,n
  t <- plotData %>% select(TIMESTAMP) %>% pull()
  n <- dim(plotData)[1]
  plotData <- plotData %>% select(-TIMESTAMP) %>% mutate(X = 1:n)

  # normalize all variables, want to draw power with each variable in one plot
  for (var in names(plotData)) {
    if (var == "X" || var == "POWER") {
      next
    }
    tmp <- plotData[[var]]
    plotData[[var]] <- (tmp - min(tmp)) / (max(tmp) - min(tmp))
  }
  # get for every variable column a copy of POWER and remove the original power
  # ~ . is lambda with . as input variable => here constant function (no .)
  plotData <- plotData %>% mutate(across(c(-POWER, -X), ~ POWER,
                                         .names = "{.col}_p")) %>%
    select(-POWER)
  # get data in a long format with categories "VAR" specifying which data goes
  # with which variable and create column power to indicate whether this is
  # the power graph for the respective variable or the variable itsself
  plotData <- plotData %>% pivot_longer(cols = -X, names_to = "var") %>%
    mutate(Line = ifelse(str_detect(var, "_p"), "Power", "VAR*"),
           VAR = str_replace(var, "_p", "")) %>%
    select(-var)

  ticks <- as.integer(seq(1, 0.75 * n, (0.75 * n - 1) / 2))   # positions of the x-ticks
  # get the labels with new line between date und time and drop time zone
  labels <- str_replace(substr(t[ticks], 1, 16), " ", "\n")
  ggplot(data = plotData, mapping = aes(x = X, y = value)) +
    geom_line(mapping = aes(color = Line)) +
    scale_x_continuous(breaks = ticks, labels = labels, name = "") +
    facet_wrap(~ VAR) +
    ggsave(name, path=paste0("plots/", track, "/"), width=17.5, height=10.5)
}


# Plot a heatmap of the POWER values whereby the heatmap should be a 24xn iamge
# with each row representing an hour, each column a day
# - data : data.frame containing the two colums "TIMESTAMP" and "POWER"
# - track : current track for naming the plot
# - zone : current zone for naming the plot
# - suf : text that should be appended to the name
plotPowerHeatMap <- function(data, track, zone, suf="") {
  suf <- if (suf != "") paste0("_", suf) else suf
  # we want day times to be in the mid of the plot
  hour_order <- c(15:23, 0:14)
  # but then the first day just comprises 'first' hours
  first <- 24 - which(hour_order == hour(data$TIMESTAMP[1])) + 1
  n <- dim(data)[1]
  ticks <- as.integer(seq(1, n, (n - 1) / 6))
  labels <- paste0(month(data$TIMESTAMP[ticks], label=TRUE),
                   year(data$TIMESTAMP[ticks]))
  n <- n - first
  # X should count the days
  data %>% mutate(Hour=factor(hour(TIMESTAMP), ordered=TRUE, levels=hour_order),
                  X=c(rep(1, first), rep(2:(n%/%24 + 1), each=24),
                       rep(n%/%24 + 2, n%%24))) %>%
    select(-TIMESTAMP) %>%
    ggplot() +
      geom_tile(mapping = aes(x=X, y=Hour, fill=POWER)) +
      ggtitle(paste("Heatmap of", track, "power production in", zone)) +
      scale_x_continuous(breaks = ticks %/% 24, labels = labels, name = "") +
      ggsave(paste0("PowerHeatmap_", zone, suf, ".png"),
             path=paste0("plots/", track, "/"), width=18, height=8)
}

# help function for scatterWindPower and estimatePowerDistribution
# discretize wind direction
  makeDiscrete <- function(x, n, interval=TRUE) {
    boundaries <- 0:n / n * 360 - 180
    binned <- floor((x + 180) / 360 * n)
    dropLastBin <- (binned - 1 * (binned == n)) + 1  # merge last two bins
    if (interval) {
      b <- round(boundaries)
      order <- paste0("[", b[1:n], ",", b[drop(1:n)+1], ")")
      vals <- paste0("[", b[dropLastBin], ",", b[dropLastBin+1], ")")
      return(factor(vals, ordered=TRUE, levels=order))
    } else {
      # return midpoints
      return((boundaries[dropLastBin] + boundaries[dropLastBin+1]) / 2)
    }
  }

# Scatter wind power production against wind speed and color wind directions
# - data : data.frame containing the colums "POWER" and wind speed and direction
#   in 10m and 100m height
# - track : current track for naming the plot
# - zone : current zone for naming the plot
# - bins : number of bins to discretize wind direction (by default NA, i.e. no
#   binning and use continous cloring instead
scatterWindPower <- function(data, track, zone, bins=NA) {
  add <- "Cont_"
  plotData <- rbind(data.frame(Power=data$POWER, Height=10, Speed=data$W10,
                               Direction=data$A10),
                    data.frame(Power=data$POWER, Height=100, Speed=data$W100,
                               Direction=data$A100))
  plotData <- na.omit(plotData)   # remove NA values
  if (!is.na(bins)) {
    add <- paste0(bins, "Bins_")

    plotData <- mutate(plotData, Direction=makeDiscrete(Direction, bins))
  }
  # scatter plot power against speed
  scatter <- ggplot(data = plotData, mapping = aes(x=Speed, y=Power,
                                                   color=Direction)) +
              geom_point(na.rm = TRUE) +
              facet_wrap(~ Height) +
              ggtitle("Wind Power ~ Wind speed, colored in wind direction")
  # adapt colors
  if(is.na(bins)) {
    scatter <- scatter +
      scale_color_gradientn(colors = c("blue", "red", "blue"))
  } else {
    scatter <- scatter +
      scale_color_manual(values = rainbow(bins))
  }
  scatter +
    ggsave(paste0("ScatterWindPower_", add, zone, ".png"),
             path=paste0("plots/", track, "/"), width=24, height=8)
}

# Plot boxplots (discrete case) or estimated median and quartils (cont. case) of
# wind power production  as function of the wind direction
# - data : data.frame containing the colums POWER and direction in 10m and 100m
#   height
# - track : current track for naming the plot
# - zone : current zone for naming the plot
# - bins : number of bins to discretize wind direction (by default NA, i.e. no
#   binning and use continous cloring instead
estimatePowerDistribution <- function(data, track, zone, bins=NA) {
  getFeatures <- function(x) {
    return(data.frame(Name=c("Median", "lQuartil", "uQuartil"),
                      Power=unname(quantile(x, probs=c(0.5, 0.25, 0.75)))))
  }

  add <- "Cont_"
  plotData <- rbind(data.frame(Power=data$POWER, Height=10, Speed=data$W10,
                               Direction=data$A10),
                    data.frame(Power=data$POWER, Height=100, Speed=data$W100,
                               Direction=data$A100))
  plotData <- na.omit(plotData)   # remove NA values
  if (!is.na(bins)) {
    add <- paste0(bins, "Bins_")

    plotData <- mutate(plotData, Direction=makeDiscrete(Direction, bins))
  }
  box_dens <- ggplot(data = plotData, mapping = aes(x=Direction, y=Power))
  # adapt colors
  if(is.na(bins)) {
    features <- mutate(plotData, Direction=makeDiscrete(Direction, 100,
                                                        interval=FALSE)) %>%
      group_by(Direction) %>% summarise(getFeatures(Power), .groups="drop")

    box_dens <- box_dens +
      geom_point() +
      geom_smooth(data = features, mapping = aes(color=Name), method=loess,
                  se=FALSE, size=1.3, formula=y~x) +
      ggtitle("Point cloud and smoothed quantiles")
  } else {
    box_dens <- box_dens +
      geom_boxplot(mapping = aes(color=Direction)) +
      ggtitle("Boxplots for different dsicretized wind directions")
  }
  box_dens +
    ggsave(paste0("WindPowerDistributions_", add, zone, ".png"),
             path=paste0("plots/", track, "/"), width=24, height=8)
}



# plot hourly expected (or median) power wind production with data grouped by
# month
# - data : data.frame containing the two colums "TIMESTAMP" and "POWER"
# - track : current track for naming the plot
# - zone : current zone for naming the plot
# - e : if true plot expectation, if false plot median
plotPowerCurves <- function(data, track, zone, e) {
  fnc_label <- if(e) "Mean" else "Median"
  fnc <- if (e) mean else function(x) {return(quantile(x, probs=0.5)[["50%"]])}
  data %>% filter(!is.na(POWER)) %>%
    mutate(HOUR = hour(TIMESTAMP), MONTH = month(TIMESTAMP, label=TRUE,
                                                 abbr=TRUE)) %>%
    select(-TIMESTAMP) %>% group_by(HOUR, MONTH) %>%
    summarise(POWER = fnc(POWER), .groups="drop") %>%
    ggplot(mapping = aes(x=HOUR, y=POWER, color=MONTH)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = rainbow(12)) +
      ggtitle(paste(fnc_label, "power curves of", track, "in", zone)) +
      ggsave(paste0(fnc_label, "_PowerCurves_", zone, ".png"),
             path=paste0("plots/", track, "/"), width=18, height=8)
}

# plot hourly expected and median power wind production with centered 50%
# intervals, whereby data is grouped by season
# - data : data.frame containing the two colums "TIMESTAMP" and "POWER"
# - track : current track for naming the plot
# - zone : current zone for naming the plot
plotPowerAreaCurves <- function(data, track, zone) {
  seasonize <- function(t) {
    return(case_when(month(t) %in% c(12, 1, 2) ~ "Dez,Jan,Feb",
                     month(t) %in% c(3, 4, 5) ~ "Mar,Apr,May",
                     month(t) %in% c(6, 7, 8) ~ "Jun,Jul,Aug",
                     month(t) %in% c(9, 10, 11) ~ "Sep,Oct,Nov"))
  }
  getConfidence <- function(x) {
    intervals <- c("100%", "80%", "60%", "40%", "20%")
    lowerVals <- c(min(x), unname(quantile(x, probs=1:4 * 0.1)))
    upperVals <- c(max(x), unname(quantile(x, probs=9:6 * 0.1)))
    return(data.frame(Width=intervals, L=lowerVals, U=upperVals))
  }
  plotData <- data %>% filter(!is.na(POWER)) %>%
    mutate(HOUR = hour(TIMESTAMP), SEASON = seasonize(TIMESTAMP)) %>%
    select(-TIMESTAMP) %>% group_by(HOUR, SEASON)

  meanAndMedian <-  summarise(plotData, Mean=mean(POWER), Median=median(POWER),
                  .groups="drop")
  c_intervals <- summarise(plotData, getConfidence(POWER), .groups="drop")

  ggplot(mapping = aes(x=HOUR)) +
    facet_wrap(~SEASON) +
    geom_ribbon(data = c_intervals, mapping=aes(ymin=L, ymax=U, group=Width,
                                                fill=SEASON),
                alpha = 0.15, show.legend=FALSE) +
    geom_line(data = meanAndMedian, mapping = aes(y=Mean, color=SEASON),
              linetype=1, size=1.2, show.legend=FALSE) +
    geom_line(data = meanAndMedian, mapping = aes(y=Median, color=SEASON),
              linetype=2, size=1.2, show.legend=FALSE) +
    ylab("Power") +
    xlab("Hour") +
    ggtitle(paste("Mean (solid), median (dashed) and confidence intervals of",
                  track, "power production in", zone)) +
    ggsave(paste0("PowerAreaCurves_", zone, ".png"),
             path=paste0("plots/", track, "/"), width=18, height=8)
}