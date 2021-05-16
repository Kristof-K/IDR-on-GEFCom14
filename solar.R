source("loadData.R")

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


# implement the whole process of examination and prediction of the solar track
runSolar <- function() {
  for (i in tasks) {
    data <- loadSolar(i)
    
    for (zone in zones) {
      examine = data[[zone]]
      # plot POWER ~ variable and calc correlation coefficients
      par(mfrow=c(3,4))
      for (var in names(variableNames)) {
        pearson = cor(examine[[var]], examine[["POWER"]], method="pearson")
        spearmann = cor(examine[[var]], examine[["POWER"]], method="spearmann")
        kendall = cor(examine[[var]], examine[["POWER"]], method="kendall")
      }
      par(mfrow=c(3,4))
    }
  }
}