# working directory must be set to directory of source file
# setwd(D:/Studium/Semester6/BachelorArbeit/Code")

source("loadData.R")

data <- loadSolar(15)

par(mfrow=c(3,1))
plot(1:nrow(data$ZONE1), data$ZONE1$POWER, type="l")
plot(1:nrow(data$ZONE2), data$ZONE2$POWER, type="l")
plot(1:nrow(data$ZONE3), data$ZONE3$POWER, type="l")
par(mfrow=c(1,1))

t = 5 * 24
variableNames = c("VAR78" = "liquid water", 
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
                  "VAR228" = "total precipitation",
                  "POWER" = "power")

for(var in names(data$ZONE1)) {
  if (var == "TIMESTAMP") {
    next
  }
  plot(1:t, data$ZONE3[[var]][1:t], type="l", ylab="power", xlab="hours", col=2, 
      main=variableNames[[var]])
  lines(1:t, data$ZONE2[[var]][1:t], col=3)
  lines(1:t, data$ZONE1[[var]][1:t], col=4)
  legend(t - 20, 0.8, c("Zone3", "Zone2", "Zone1"), text.col=c(2,3,4))
}
