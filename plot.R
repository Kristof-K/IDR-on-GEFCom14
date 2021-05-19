variableNames <- c("VAR78" = "liquid water", 
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

corr_c <- c("pearson", "spearman", "kendall")

yLim <- c(0, 1.1)  # Power is normalized, so we can use these limits


# Plot scatter plots for every variable in variableNames
# - list : list of data.frames for every category
# - categroy : vector of names defining the categories
# - zone : current zone (only label for plotting)
# - min / max : data.frame containg for every variable min and max value
scatterHours <- function(list, categories, zone, min, max) {
  scatterFull(list, categories, zone, min, max, append="h", 
              title="grouped by hour")
}

scatterMonths <- function(list, categories, zone, min, max) {
  l = c("Jan", "Feb", "Mar", "Apr", "May", "Jun2", "Jul", "Aug", "Sep", "Oct",
        "Nov", "Dez")
  scatterFull(list, categories, zone, min, max, labels=l,
              title="grouped by month")
}

scatterFull <- function(list, categories, zone, min, max, append="", 
                        labels=NA, title="") {
  colors <- rainbow(length(categories))
  
  par(mfrow=c(3,4), mar=c(4, 0, 0, 0), oma=c(3,2,3,3), mgp=c(1.4,0.6,0))
  j <- 0
  for (var in names(variableNames)) {
    y_axis <- if(j %% 4 == 0) "s" else "n"      # plot y axis or not
    
    myMax <- if (max[[var]] >= 0) max[[var]] * 1.05 else max[[var]] * 0.95
    myMin <- if (min[[var]] >= 0) min[[var]] * 0.95 else min[[var]] * 1.05
    plot(0, type="n", ylab="Power", yaxt=y_axis, xlab=variableNames[[var]], 
         xlim=c(myMin, myMax), ylim=yLim)
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
  text <- if (any(is.na(labels))) categories else labels
  mtext(paste0(text, append), side=1, outer=TRUE, line=0.5, col=colors, 
        at=seq(0.1, 0.9, 0.8 / (length(categories) - 1)))
  # set parameters back to default
  par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1, oma=c(0,0,0,0), mgp=c(3 ,1 ,0))
}
