# define preprocess methods getting X_train and X_test and applying data
# transformation on both

source("util.R")

# Method output description consistently for a preprocessing function
# - name : name of the forecasting method (it should be a vector of words in
#   order to be printed correctly)
outputPreprocessing <- function(name) {
  cat("\n[PREPROCESSING]:", name, "\n", fill = PRINT_WIDTH)
}

no_pp <- function(data, init=FALSE) {
  name <- NONE
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  return(data)
}

