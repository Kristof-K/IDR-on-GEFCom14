QUANTILES <- seq(0.01, 0.99, 0.01)
SOLAR_ZONES <- c("ZONE1", "ZONE2", "ZONE3")
TASKS <- 1:15
FIRST_EVAL_TASK <- 4        # in GEFCom14 the first 3 tasks weren't evaluated

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
# - x : matrix containing prediction for the 1% up to 99% quantile in every row
# - y : vector containg the respective observations
# - print : output information if True otherwise calc score
pinBallLoss <- function(x, y, print=FALSE) {
  if (print) {
    outputScoringFunction("Pinball-Loss / asymmetric piecewise linear
                          scoring fct.")
    return("")
  }
  singlePinBallLoss <- function(vec) {
    # separate y and x again
    y <- vec[1]
    x <- vec[-1]
    scoreVec <- ((y < x) - QUANTILES) * (x - y)
    return(mean(scoreVec))
  }
  # in order to use apply more easily
  join <- cbind(y, x)
  # apply pinball loss on every row in the given x vector
  return(apply(join, 1, singlePinBallLoss))
}

# Method output description consistently for a specific scoring function
# - name : name of the forecasting method
outputScoringFunction <- function(name) {
  cat("\n[SCORING FUNCTION]:", name, "\n")
}