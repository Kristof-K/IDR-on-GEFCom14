
data <- loadSolar(1)

par(mfrow=c(3,1))
plot(1:nrow(data$ZONE1), data$ZONE1$POWER, type="l")
plot(1:nrow(data$ZONE2), data$ZONE2$POWER, type="l")
plot(1:nrow(data$ZONE3), data$ZONE3$POWER, type="l")
par(mfrow=c(1,1))


