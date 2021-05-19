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

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
      "Nov", "Dez")

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
  scatterFull(list, categories, zone, min, max, labels=months,
              title="grouped by month")
}

scatterFull <- function(list, categories, zone, min, max, append="", 
                        labels=NA, title="") {
  colors <- rainbow(length(categories))
  yLim <- c(0, 1.1)  # Power is normalized, so we can use these limits
  
  par(mfrow=c(3,4), mar=c(4, 0, 0, 0), oma=c(3,2,3,2), mgp=c(1.4,0.6,0))
  j <- 0
  for (var in names(variableNames)) {
    y_axis <- if(j %% 4 == 0) "s" else "n"      # plot y axis or not
    
    myMax <- if (max[[var]] >= 0) max[[var]] * 1.01 else max[[var]] * 0.99
    myMin <- if (min[[var]] >= 0) min[[var]] * 0.99 else min[[var]] * 1.01
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

# Plot pearson, spearman and kendall correlation coefficients for every variable
# and every category
# - coefficients : 3-dim array containing the correlation coefficients (on 1st
#   axis variables, 2nd coefficient type (pearson, spearman, kendall) and on 3rd
#   the categories)
# - categories : vector of names defining the categories
# - zone : current zone (only label for plotting)
correlationPlotHours <- function(coefficients, categories, zone) {
  correlationPlot(coefficients, categories, zone, title="grouped by hour", 
                  append="h") 
}

correlateMonths <- function(list, categories, zone, min, max) {
  correlationPlot(coefficients, categories, zone, title="grouped by month", 
                  labels=months)
}

correlationPlot <- function(coefficients, categories, zone, title="", append="", 
                            labels=NA,) {
  n <- length(names(variableNames))
  x <- 1:n
  colors <- rainbow(length(categories))
  shapes <- c(15, 16, 17)
  shift <- c(-0.33, 0, 0.33)
  
  par(mfrow=c(1,1), mar=c(3.5, 2, 0, 3), oma=c(3,0,3,0))
  
  plot(c(0.5,n+0.5), c(0,0), type="l", lty=3, xaxt="n", xlim=c(0,13), xlab="", 
       ylab="", ylim=c(-1.2,1.2))
  
  for(var in 1:length(variableNames)) {   # iterate through 1st axis of coeff
    for(i in 1:3) {                       # iterate through 2nd axus of coeff
      lines(rep(var + shift[i], length(categories)), coefficients[var, i, ],
            type="p", cex=1.2, col=colors, pch=shapes[i])
    }
    # draw separation line
    if (var < length(variableNames)) {
      lines(rep(var + 0.5, 2), c(-1, 1), type="l", lty=3)
    }
  }
  
  axis(1, at=x, labels=names(variableNames), cex.axis=0.8, las=2)
  add <- if (title != "") paste(",", title) else ""
  mtext(paste0(zone, ": Correlation(Power,variables)", add), outer=TRUE, 
        line=0.5, cex=1.5)
  text <- if (any(is.na(labels))) categories else labels
  mtext(paste0(text, append), side=1, outer=TRUE, line=0, col=colors, 
        at=seq(0.1, 0.9, 0.8 / (length(categories) - 1)))
  mtext(paste(corr_c, c("(square)", "(circle)", "(triangle)")), side=1, outer=TRUE, 
              line=1, at=c(0.3, 0.5, 0.7))
  par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1, oma=c(0,0,0,0))
}