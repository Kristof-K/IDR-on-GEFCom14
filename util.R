format <- "%Y%m%d %H:%M"    # format of the timestamps

belongsToHour <- function(timestamp, hour) {
  bool = strptime(timestamp, format)$hour == hour
  return(bool)
}

belongsToMonth <- function(timestamp, month) {
  # months are numbered starting with 0 => increase by one
  bool = ((strptime(timestamp, format)$mon + 1) == month)
  return(bool)
}

# Calculate the pearson, spearman and kendall correlation coefficient for every
# variable for every category in list
# - list : list of data.frames for every category
# - categroy : vector of names defining the categories
# return 3-dim array: 1st axis variables, 2nd axis correlation coefficient typ
# (pearson, spearman, kendall), 3rd axis categories
getCorrelationCoefficients <- function(list, categories, numOfVariables) {
  output <- array(0, dim=c(numOfVariables, 3, length(categories)))
  c <- 1
  for (element in categories) {       # iterate over 3rd dimension
    for (var in 1:numOfVariables) {   # iterate over 1st dimension
      examine <- list[[element]]
      output[var,1:3,c] <- c(cor(examine[,var], examine[["POWER"]], 
                                method="pearson"),
                            cor(examine[,var], examine[["POWER"]], 
                                method="spearman"),
                            cor(examine[,var], examine[["POWER"]], 
                                method="kendall"))
    }
    c <- c+1
  }
  return(output)
}