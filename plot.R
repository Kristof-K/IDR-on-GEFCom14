library(ggplot2)
library(dplyr)
library(tidyr)      # for pivot_longer
library(stringr)    # for string manipulations
library(lubridate)  # for working with dates

source("util.R")

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
              "S10" = "10m wind speed", "S100" = "100m wind speed",
              "A10" = "10m wind angle", "A100" = "100m wind angle")
loadVars <- setNames(paste("Temperature", 1:25), paste0("w", 1:25))
priceVars <- c("Forecasted.Total.Load" = "forecasted total load",
               "Forecasted.Zonal.Load" = "forecasted zonal load")

corr_c <- c("pearson", "spearman", "kendall")

months <- paste(1:12)
hours <- paste(0:23)
monthsLabel <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                 "Oct", "Nov", "Dez")
hoursLabel <- paste(hours, "h")


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
  return(switch(track, "Solar"=solarVars, "Wind"=windVars, "Load"=loadVars,
                "Price"=priceVars))
}
# Get a grid to plot all variables (dependent on number of vars in the track)
# - track : name of the track
getGrid <- function(track) {
  return(switch(track, "Solar"=c(3,4), "Wind"=c(2,4), "Load"=c(5,5),
                "Price"=c(1,2)))
}

getTarget <- function(track) {
  return(switch(track, "Solar"="Power", "Wind"="Power", "Load"="Load",
                "Price"="Price"))
}

getTargetLimits <- function(track) {
  return(switch(track, "Solar"=c(0, 1.1), "Wind"=c(0, 1.1), "Load"=c(0, 325),
                "Price"=c(0, 375)))
}

isSouthern <- function(track) {
  return(switch(track, "Solar"=TRUE, "Wind"=TRUE, "Load"=FALSE, "Price"=FALSE))
}

# Plot scatter plots for every variable in variableNames
# - df_list : list of data.frames for every category
# - track : current track for naming the plots
# - zone : current zone (only label for plotting)
# - min / max : data.frame containg for every variable min and max value
scatterHours <- function(df_list, track, zone, min, max) {
  png(paste0(file="plots/", track, "/scatterByHours_", zone, ".png"), width=1600, height=900)
  scatterFull(df_list, categories=hours, track, zone, min, max, labels=hoursLabel,
              title="grouped by hour")
  dev.off()
}

scatterMonths <- function(df_list, track, zone, min, max) {
  png(file=paste0("plots/", track, "/scatterByMonths_", zone, ".png"), width=1600, height=900)
  scatterFull(df_list, categories=months, track, zone, min, max, labels=monthsLabel,
              title="grouped by month")
  dev.off()
}

scatterFull <- function(df_list, categories, track, zone, min, max, labels,
                        title="") {
  colors <- rainbow(length(categories))
  vars <- getVars(track)
  target <- getTarget(track)
  plotGrid <- getGrid(track)
  yLimits <- getTargetLimits(track)
  
  par(mfrow=plotGrid, mar=c(4, 0, 0, 0), oma=c(3,2,3,2), mgp=c(1.4,0.6,0))
  j <- 0
  for (var in names(vars)) {
    y_axis <- if(j %% plotGrid[2] == 0) "s" else "n"      # plot y axis or not
    
    plot(0, type="n", ylab=paste(target), yaxt=y_axis, xlab=vars[[var]],
         xlim=getLimits(var, min, max), ylim=yLimits)
    c <- 1
    for (element in categories) {
      lines(df_list[[element]][[var]], df_list[[element]][["TARGET"]], pch=20,
            type="p", col=colors[c])
      c <- c+1
    }
    j <- j+1
  }
  add <- if (title != "") paste(",", title) else ""
  mtext(paste0(zone, ": ", target, " ~ variables", add), outer=TRUE, line=0.5,
        cex=1.5)
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
  target <- getTarget(track)
  
  par(mfrow=c(1,1), mar=c(3.5, 2, 0, 3), oma=c(3,0,3,0))
  
  plot(c(0.5,n+0.5), c(0,0), type="l", lty=3, xaxt="n", xlim=c(0,n+1), xlab="",
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
  mtext(paste0(zone, ": Correlation(", target, ",variables)", add), outer=TRUE,
        line=0.5, cex=1.5)
  mtext(labels, side=1, outer=TRUE, line=0, col=colors,
        at=seq(0.1, 0.9, 0.8 / (length(categories) - 1)))
  mtext(paste(corr_c, c("(square)", "(circle)", "(triangle)")), side=1,  
        outer=TRUE, line=1, at=c(0.3, 0.5, 0.7))
  par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1, oma=c(0,0,0,0))
}


# Plot scatter plots for a single variable, every category gets an own plot
# - df_list : list of data.frames for every category
# - track : name of the track (for naming the plot)
# - zone : current zone (only label for plotting)
# - min / max : data.frame containg for every variable min and max value
# - name : name of the varibale that should be plotted
# - suf : text that should be appended to the name
scatterSingleHours <- function(df_list, track, zone, min, max, name, suf="") {
  suf <- if (suf != "") paste0("_", suf) else suf
  png(file=paste0("plots/", track, "/scatterByHours_", name, "_", zone, suf, ".png"), width=1600, height=900)
  scatterSingle(df_list, categories=hours, track, zone, min, max, name,
                label=hoursLabel, title=paste0(name, ", grouped by hour"),
                grid=c(4,6))
  dev.off()
}

scatterSingleMonths <- function(df_list, track, zone, min, max, name, suf="") {
  suf <- if (suf != "") paste0("_", suf) else suf
  png(file=paste0("plots/", track, "/scatterByMonth_", name, "_", zone, suf, ".png"), width=1600, height=900)
  scatterSingle(df_list, categories=months, track, zone, min, max, name,
                labels=monthsLabel, title=paste0(name, ", grouped by month"),
                grid=c(3,4))
  dev.off()
}

scatterAllSingle <- function(df_list, track, zone, min, max, hour=TRUE) {
  vars <- getVars(track)
  for (var in names(vars)) {
    if (hour) {
      scatterSingleHours(df_list, track, zone, min, max, var)
    } else {
      scatterSingleMonths(df_list, track, zone, min, max, var)
    }
  }
}

scatterSingle <- function(df_list, categories, track, zone, min, max, name,
                          labels, title="", grid=c(1,1)) {
  colors <- rainbow(length(categories))
  vars <- getVars(track)
  target <- getTarget(track)
  yLimits <- getTargetLimits(track)

  par(mfrow=grid, mar=c(0, 0, 0, 0), oma=c(4,4,4,2), mgp=c(1.2,0.6,0))
  
  j <- 0
  for (element in categories) {
    y_axis <- if(j %% grid[2] == 0) "s" else "n"      # plot x/y axis or not
    x_axis <- if(floor(j / grid[2]) == grid[1] - 1) "s" else "n"
    
    limits <- getLimits(name, min, max)
    
    plot(df_list[[element]][[name]], df_list[[element]][["TARGET"]], type="p",
         pch=20, ylab=paste(target), yaxt=y_axis, xaxt=x_axis,
         xlab=vars[[name]], xlim=limits, ylim=yLimits, col=colors[j+1], cex=2)
    text(x=0.1*limits[1] + 0.9*limits[2], y=0.9*yLimits[2], labels[j + 1],
         cex=1.7)
    j <- j+1
  }
  add <- if (title != "") paste(",", title) else ""
  mtext(paste0(zone, ": ", target, " ~ ", vars[[name]], add), outer=TRUE,
        line=0.5, cex=1.5)
  # set parameters back to default
  par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1, oma=c(0,0,0,0), mgp=c(3 ,1 ,0))
}


# plot variables against time in one plot to assess the development
# - data : data.frame with TIMESTAMPS as one column and other variables
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
  t <- plotData[["TIMESTAMP"]]
  n <- dim(plotData)[1]
  target <- getTarget(track)
  vars <- c(names(getVars(track)), "TARGET")
  plotData <- plotData %>% select(vars) %>% mutate(X = 1:n)

  # normalize all variables
  for (var in vars) {
    tmp <- plotData[[var]]
    plotData[[var]] <- (tmp - min(tmp, na.rm=TRUE)) /
      (max(tmp, na.rm=TRUE) - min(tmp, na.rm=TRUE))
  }
  # get for every variable column a copy of TARGET and remove the original power
  # ~ . is lambda with . as input variable => here constant function (no .)
  plotData <- plotData %>% mutate(across(c(-TARGET, -X), ~ TARGET,
                                         .names = "{.col}_p")) %>%
    select(-TARGET)
  # get data in a long format with categories "VAR" specifying which data goes
  # with which variable and create column power to indicate whether this is
  # the power graph for the respective variable or the variable itsself
  plotData <- plotData %>% pivot_longer(cols = -X, names_to = "var") %>%
    mutate(Line = ifelse(str_detect(var, "_p"), target, "VAR*"),
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


# Plot a heatmap of the TARGET values whereby the heatmap should be a 24xn iamge
# with each row representing an hour, each column a day
# - data : data.frame containing the two colums "TIMESTAMP" and "TARET"
# - track : current track for naming the plot
# - zone : current zone for naming the plot
# - suf : text that should be appended to the name
plotHeatMap <- function(data, track, zone, suf="") {
  suf <- if (suf != "") paste0("_", suf) else suf
  target <- getTarget(track)
  n <- dim(data)[1]
  ticks <- as.integer(seq(1, n, (n - 1) / 6))
  labels <- paste0(month(data$TIMESTAMP[ticks], label=TRUE),
                   year(data$TIMESTAMP[ticks]))

  if(isSouthern(track)) {
    # we want day times to be in the mid of the plot
    hour_order <- c(15:23, 0:14)
    # but then the first day just comprises 'first' hours
    first <- 24 - which(hour_order == hour(data$TIMESTAMP[1])) + 1
    n <- n - first
    x_vals <- c(rep(1, first), rep(2:(n%/%24 + 1), each=24),
                rep(n%/%24 + 2, n%%24))
  } else {
    hour_order <- 0:23
    x_vals <- rep(1:(n%/%24), each=24)
  }

  # X should count the days
  data %>% mutate(Hour=factor(hour(TIMESTAMP), ordered=TRUE, levels=hour_order),
                  X=x_vals) %>%
    select(-TIMESTAMP) %>%
    ggplot() +
      geom_tile(mapping = aes(x=X, y=Hour, fill=TARGET)) +
      ggtitle(paste("Heatmap of", track, target, "production in", zone)) +
      scale_x_continuous(breaks = ticks %/% 24, labels = labels, name = "") +
      ggsave(paste0(target, "Heatmap_", zone, suf, ".png"),
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
      order <- paste0("[", b[1:n], ",", b[(1:n)+1], ")")
      vals <- paste0("[", b[dropLastBin], ",", b[dropLastBin+1], ")")
      return(factor(vals, ordered=TRUE, levels=order))
    } else {
      # return midpoints
      return((boundaries[dropLastBin] + boundaries[dropLastBin+1]) / 2)
    }
  }

# Scatter wind power production against wind speed and color wind directions
# - data : data.frame containing the colums "TARGET" and wind speed and direction
#   in 10m and 100m height
# - track : current track for naming the plot
# - zone : current zone for naming the plot
# - bins : number of bins to discretize wind direction (by default NA, i.e. no
#   binning and use continous cloring instead
scatterWindPower <- function(data, track, zone, bins=NA) {
  add <- "Cont_"
  plotData <- rbind(data.frame(Power=data$TARGET, Height=10, Speed=data$S10,
                               Direction=data$A10),
                    data.frame(Power=data$TARGET, Height=100, Speed=data$S100,
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
# - data : data.frame containing the colums TARGET and direction in 10m and 100m
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
  plotData <- rbind(data.frame(Power=data$TARGET, Height=10, Speed=data$S10,
                               Direction=data$A10),
                    data.frame(Power=data$TARGET, Height=100, Speed=data$S100,
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
      geom_point(alpha = 0.4) +
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
# - data : data.frame containing the two colums "TIMESTAMP" and "TARGET"
# - track : current track for naming the plot
# - zone : current zone for naming the plot
# - groupingfct : how should data be grouped
plotAllTargetCurves <- function(data, track, zone, groupingfct=getMonths) {
  plotTargetCurves(data, track, zone, groupingfct, e=TRUE)
  plotTargetCurves(data, track, zone, groupingfct, e=FALSE)
  plotTargetCurves(data, track, zone, groupingfct, e=TRUE, facetIt=TRUE)
  plotTargetCurves(data, track, zone, groupingfct, e=FALSE, facetIt=TRUE)
}

plotTargetCurves <- function(data, track, zone, groupingfct, e=FALSE,
                             facetIt=FALSE) {
  target <- getTarget(track)
  fnc_label <- if(e) "Mean" else "Median"
  fnc <- if (e) mean else function(x) quantile(x, probs=0.5)[["50%"]]
  group_label <- groupingfct(NA, getName=TRUE)
  group_nr <- length(groupingfct(NA, getCategories=TRUE))
  addLabel <- if(facetIt) "_facet" else ""

  plotData <- filter(data, !is.na(TARGET))
  plotData <- mutate(plotData, HOUR = hour(TIMESTAMP),
                     Group = groupingfct(plotData, label=TRUE),
                     FACET = get4Seasons(plotData, label=TRUE)) %>%
    select(-TIMESTAMP)
  if (facetIt) {
    plotData <- plotData %>% group_by(HOUR, Group, FACET)
  } else {
    plotData <- plotData %>% group_by(HOUR, Group)
  }
  plotData <- plotData %>% summarise(TARGET = fnc(TARGET), .groups="drop")
  myPlot <- ggplot(plotData, aes(x=HOUR, y=TARGET, color=Group)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = rainbow(group_nr)) +
      xlab("hour") +
      ylab(paste(target)) +
      ggtitle(paste(fnc_label, target, "curves of", track, "in", zone))
  if (facetIt) {
    myPlot <- myPlot +
      facet_wrap(~FACET, scales="free_y")
  }
  myPlot +
    ggsave(paste0(fnc_label, "_", target, "Curves_", zone, "_", group_label,
                  addLabel, ".png"),
           path=paste0("plots/", track, "/"), width=18, height=8)
}

# plot hourly expected and median target quantity with centered 50%
# intervals, whereby data is grouped by season
# - data : data.frame containing the two colums "TIMESTAMP" and "TARGET"
# - track : current track for naming the plot
# - zone : current zone for naming the plot
plotAreaCurves <- function(data, track, zone) {
  getConfidence <- function(x) {
    intervals <- c("100%", "80%", "60%", "40%", "20%")
    lowerVals <- c(min(x), unname(quantile(x, probs=1:4 * 0.1)))
    upperVals <- c(max(x), unname(quantile(x, probs=9:6 * 0.1)))
    return(data.frame(Width=intervals, L=lowerVals, U=upperVals))
  }
  plotData <- data %>% filter(!is.na(TARGET))
  plotData <- plotData %>%
    mutate(HOUR=hour(TIMESTAMP), SEASON=get4Seasons(plotData, label=TRUE)) %>%
    select(-TIMESTAMP) %>% group_by(HOUR, SEASON)

  meanAndMedian <- summarise(plotData, Mean=mean(TARGET), Median=median(TARGET),
                  .groups="drop")
  c_intervals <- summarise(plotData, getConfidence(TARGET), .groups="drop")
  target <- getTarget(track)

  ggplot(mapping = aes(x=HOUR)) +
    facet_wrap(~SEASON) +
    geom_ribbon(data = c_intervals, mapping=aes(ymin=L, ymax=U, group=Width,
                                                fill=SEASON),
                alpha = 0.15, show.legend=FALSE) +
    geom_line(data = meanAndMedian, mapping = aes(y=Mean, color=SEASON),
              linetype=1, size=1.2, show.legend=FALSE) +
    geom_line(data = meanAndMedian, mapping = aes(y=Median, color=SEASON),
              linetype=2, size=1.2, show.legend=FALSE) +
    ylab(paste(target)) +
    xlab("Hour") +
    ggtitle(paste("Mean (solid), median (dashed) and confidence intervals of",
                  track, target, "production in", zone)) +
    ggsave(paste0(target, "AreaCurves_", zone, ".png"),
             path=paste0("plots/", track, "/"), width=18, height=8)
}

# plot histogram of every variable
# - data : data.frame with all variables as columns
# - track : current track for naming the plot
# - zone : current zone for naming the plot
plotHistograms <- function(data, track, zone) {
  vars <- c(getVars(track), "TARGET" = getTarget(track))
  data %>% select(names(vars)) %>%
    pivot_longer(cols=names(vars), names_to="Variable") %>%
    mutate(Variable = unname(vars[Variable])) %>%
    ggplot(mapping = aes(x=value)) +
      geom_histogram(bins = 50, na.rm=TRUE) +
      facet_wrap(~Variable, scales="free") +
      xlab("") +
      ggtitle(paste("Histogram of all", track, "variables in", zone)) +
      ggsave(paste0("Histograms_", zone, ".png"),
             path=paste0("plots/", track, "/"), width=18, height=12)
}

plotRanges <- function(data, track, zone, groupingfct) {
  gr_name <- groupingfct(NA, getName=TRUE)
  target <- getTarget(track)

  data %>% mutate(Group=groupingfct(data)) %>%
    ggplot(aes(as.factor(Group), TARGET)) +
      geom_violin() +
      xlab("Groups") +
      ylab(paste(target)) +
      ggtitle(paste("Violin plots of", track, "data grouped by", gr_name)) +
      ggsave(paste0("Violinplots_", zone, "_", gr_name, ".png"),
             path=paste0("plots/", track, "/"), width=18, height=12)
}
