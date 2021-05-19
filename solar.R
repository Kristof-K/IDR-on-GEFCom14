source("loadData.R")
source("util.R")
source("plot.R")

full_plot <- FALSE
tasks <- 1:15
zones <- c("ZONE1", "ZONE2", "ZONE3")

# TODO:
# - scatter plot mit Farbkategorisierung Jahreszeiten
# - scatter plot mit Farbkategorisierung Uhrzeit
# - scatter plot mit Farbkategorisierung Tasks
# - plot mit Korrelationskoeffizienten


# implement the whole process of examination and prediction of the solar track
runSolar <- function() {
  # data is a list containing for every string in zones a data frame
  data <- loadSolar(15)
  
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
    scatterMonths(groupByHour, hours, zone, min, max)
  }
  
  
  
  for (task in tasks) {
    data <- loadSolar(task)
    # data frame storing all correlation coefficients
    coeffs <- matrix(0, nrow=length(variableNames),
                    ncol=length(corr_c)*length(zones)) 
    k <- 1 # current columns to fill
    
    for (zone in zones) {
      examine <- data[[zone]]
      
      # plot POWER ~ variable and calc correlation coefficients
      par(mfrow=c(3,4), mar=c(4, 0, 0, 0), oma=c(0,2,3,3), mgp=c(1.4,0.6,0))
      j <- 0
      for (var in names(variableNames)) {
        coeffs[j+1,k:(k+2)] <- c(cor(examine[[var]], examine[["POWER"]], 
                                    method="pearson"),
                                cor(examine[[var]], examine[["POWER"]], 
                                    method="spearman"),
                                cor(examine[[var]], examine[["POWER"]], 
                                    method="kendall"))
        if(full_plot) {  
          if (j %% 4 == 0) {
            plot(examine[["POWER"]] ~ examine[[var]], xlab=variableNames[[var]],
                ylab="", pch=20)
          } else {
            plot(examine[["POWER"]] ~ examine[[var]], xlab=variableNames[[var]],
                ylab="", yaxt="n", pch=20)
          }
        }
        j = j + 1
      }
      mtext(paste0("Task ", task, ", ", zone, ": Power ~ var"), outer=TRUE,
            line=0.5, cex=1.5)
      par(mfrow=c(1,1))
      k = k + 3
    }
    Coefficients = data.frame(coeffs)
    rownames(Coefficients) = names(variableNames)
    colnames(Coefficients) = paste(rep(corr_c, each=3), rep(zones, 3), sep="_")
    
    n <- length(names(variableNames))
    x <- 1:n
    colors = c("red", "green", "blue")
    shapes = c(18, 17, 16)
    par(mfrow=c(1,1), mar=c(4, 0, 2, 0))
    plot(c(0,13), c(0,0), type="l", lty=3, main="Correlation coefficients",
         xaxt="n", xlim=c(0,13), ylim=c(-1.2,1.2), xlab="", ylab="")
    for (col in 1:(length(corr_c)*length(zones))) {
      lines(x, Coefficients[,col], type="p", cex=1.5,
                pch=shapes[floor(col / length(shapes)) + 1],
                col=colors[(col - 1) %% length(colors) + 1])
    }
    axis(1, at=x, labels=names(variableNames), cex.axis=0.8, las=2)
    text(c(10, 11, 12), rep(1.1, 3), zones, col=colors, cex=0.8)
    text(c(1,3,5), rep(1.1, 3), corr_c, cex=0.8)
    lines(c(0.4,2.1,4.4), rep(1.1, 3), type="p", pch=shapes, cex=1.5)
  }
}
