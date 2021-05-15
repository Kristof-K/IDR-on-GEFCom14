source("loadData.R")

tasks <- 1:15


# implement the whole process of examination and prediction of the solar track
runSolar <- function() {
  for (i in tasks) {
    data <- loadSolar(i)
    
    # first step analysis correlations
  }
}