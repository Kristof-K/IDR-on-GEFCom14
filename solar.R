source("loadData.R")

full_plot <- FALSE
tasks <- 1:15
zones <- c("ZONE1", "ZONE2", "ZONE3")

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


# implement the whole process of examination and prediction of the solar track
runSolar <- function() {
  for (i in tasks) {
    data <- loadSolar(i)
    # data frame storing all correlation coefficients
    coeffs = matrix(0, nrow=length(variableNames),
                    ncol=length(corr_c)*length(zones)) 
    k = 1 # current columns to fill
    
    for (zone in zones) {
      examine = data[[zone]]
      
      # plot POWER ~ variable and calc correlation coefficients
      par(mfrow=c(3,4), mar=c(4, 0, 0, 0), oma=c(0,2,3,3), mgp=c(1.4,0.6,0))
      i = 0
      for (var in names(variableNames)) {
        coeffs[i+1,k:k+2] = c(cor(examine[[var]], examine[["POWER"]], 
                             method="pearson"),
                         cor(examine[[var]], examine[["POWER"]], 
                             method="spearman"),
                         cor(examine[[var]], examine[["POWER"]], 
                             method="kendall"))
        if(full_plot) {  
          if (i %% 4 == 0) {
            plot(examine[["POWER"]] ~ examine[[var]], xlab=variableNames[[var]],
                ylab="", pch=20)
          } else {
            plot(examine[["POWER"]] ~ examine[[var]], xlab=variableNames[[var]],
                ylab="", yaxt="n", pch=20)
          }
        }
        i = i + 1
      }
      mtext(paste0("Task ", i, ", ", zone, ": Power ~ var"), outer=TRUE,
            line=0.5, cex=1.5)
      par(mfrow=c(1,1))
    }
    Coefficients = data.frame(coeff)
    rownames(Coefficients) = names(variableNames)
    colNames(Coefficients) = paste(rep(c_corr, each=3), rep(zones, 3))
  }
}
