library(isodistrreg)

name_all <- "IDR on all"
descr_all <-  c("Apply", "IDR", "on", "the", "whole", "training", "set")
descr_hour <- c("Group", "training", "set", "by", "hour", "and", "apply", "IDR",
                "on", "every", "group", "separately")


# IDR applied on whole training data ===========================================
# parameter variations ---------------------------------------------------------
idrOnAll_V1 <- function(X_train, y_train, X_test, print=FALSE) {
  var <- c("VAR169")
  if (print) {
    outputForecastingMethod(name_all, var, descr_all)
    return("")
  }
  groups <- c("VAR169" = 1)
  orders <- c("comp" = 1)
  return(idrOnAll(X_train[, var], y_train, X_test[, var], groups, orders))
}


idrOnAll_V2 <- function(X_train, y_train, X_test, print=FALSE) {
  var <- c("VAR178")
  if (print) {
    outputForecastingMethod(name_all, var, descr_all)
    return("")
  }
  groups <- c("VAR178" = 1)
  orders <- c("comp" = 1)
  return(idrOnAll(X_train[, var], y_train, X_test[, var], groups, orders))
}


idrOnAll_V3 <- function(X_train, y_train, X_test, print=FALSE) {
  var <- c("VAR169", "VAR178")
  if (print) {
    outputForecastingMethod(name_all, var, descr_all)
    return("")
  }
  groups <- c("VAR178" = 1, "VAR169" = 1)
  orders <- c("comp" = 1)
  return(idrOnAll(X_train[, var], y_train, X_test[, var], groups, orders))
}

# actual implementation of IDR -------------------------------------------------
idrOnAll <- function(X_train, y_train, X_test, groups, orders) {
  # apply idr, by fitting, prediction and extracting the quantiles
  fit <- idr(y = y_train, X = X_train, groups = groups, order = orders)
  cdf_predictions <- predict(fit, data = X_test)
  quantile_predictions <- qpred(cdf_predictions, quantiles = QUANTILES)
  return(quantile_predictions)
}

# IDR applied on data grouped by hour ==========================================
# parameter variation ----------------------------------------------------------
idrOnHour <- function(X_train, y_train, X_test, print=FALSE) {
  if (print) {
    outputForecastingMethod("IDR by hour", "None", descr_hour)
    return("")
  }

}