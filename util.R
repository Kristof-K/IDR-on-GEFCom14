# Calculate the pearson, spearman and kendall correlation coefficient for every
# variable for every category in list
# - list : list of data.frames for every category
# - categroy : vector of names defining the categories
# return 3-dim array: 1st axis variables, 2nd axis correlation coefficient typ
# (pearson, spearman, kendall), 3rd axis categories
getCorrelationCoefficients <- function(list, categories, numOfVariables) {
  output <- array(NA, dim=c(numOfVariables, 3, length(categories)))
  c <- 1
  for (element in categories) {       # iterate over 3rd dimension
    for (var in 1:numOfVariables) {   # iterate over 1st dimension
      examine <- list[[element]]
      if (sd(examine[["POWER"]]) != 0 && sd(examine[, var]) != 0) {
        output[var,1:3,c] <- c(cor(examine[,var], examine[["POWER"]],
                                  method="pearson"),
                              cor(examine[,var], examine[["POWER"]],
                                  method="spearman"),
                              cor(examine[,var], examine[["POWER"]],
                                  method="kendall"))
      }
    }
    c <- c+1
  }
  return(output)
}


# Calculate the pinball loss for
# - x : vector containing prediction for the 1% up to 99% quantile
# - y : vector containg the 99 respective observations
# - print : output information if True otherwise calc score
pinBallLoss <- function(x, y, print=FALSE) {
  if (print) {
    outputScoringFunction("Pinball-Loss / asymmetric piecewise linear scoring 
                          fct.")
    return(NA)
  }
  quantiles <- seq(0.01, 0.99, 0.01)
  scoreVec <- ((y < x) - quantiles) * (x - y)
  return(mean(scoreVec))
}

# Method output description consistently for a specific scoring function
# - name : name of the forecasting method
outputScoringFunction <- function(name) {
  cat("\n[SCORING FUNCTION]:", name, "\n")
}