library(isodistrreg)

# core functions ===============================================================

# Version1 : Apply idr on the whole data set
idrOnAll <- function(X_train, y_train, X_test, groups, orders) {
  # apply idr, by fitting, prediction and extracting the quantiles
  fit <- idr(y = y_train, X = X_train, groups = groups, order = orders)
  cdf_predictions <- predict(fit, data = X_test)
  quantile_predictions <- qpred(cdf_predictions, quantiles = QUANTILES)
  return(quantile_predictions)
}

# Version2 : Apply idr on data grouped by hour
idrOnHour <- function(X_train, y_train, X_test, groups, orders) {
  train <- cbind(POWER = y_train, X_train)
  # breakdown data by hour
  hours <- paste0(0:23)   # categories by which is grouped
  idr_models <- list()        # for every category fit the model
  for (h in hours) {
    indices <- (hour(train$TIMESTAMP) == h)
    y <- subset(train, indices)$POWER
    # idr cannot handle if y is constant
    if (max(y) == min(y)) {
      idr_models[[h]] <- y[1]  # make constant predictions if y is constant
      next
    }
    trainByHour <- subset(train, indices, select=c(-TIMESTAMP, -POWER))
    idr_models[[h]] <- idr(y = y, X = trainByHour,  groups = groups,
                           orders = orders)
  }
  # reduce timestamps in X_test to hours
  X_test$TIMESTAMP <- hour(X_test$TIMESTAMP)
  notTime <- !(names(X_test) == "TIMESTAMP")
  # function that makes a right prediction for every row in X_test, assume that
  # timestamp is first column in X_test (find the respective idr model)
  getQuantiles <- function(row) {
    idr <- idr_models[[paste0(row[1])]]
    # if idr_model is a number, make constant predictions
    if (is.numeric(idr)) {
      return(rep(idr, length(QUANTILES)))
    }
    # convert row[notTime] into a data.frame
    cdf_prediction <- predict(idr, data = data.frame(as.list(row[notTime])))
    quantile_predictions <- qpred(cdf_prediction, quantiles = QUANTILES)
    return(quantile_predictions)
  }
  joinedForecast <- apply(X_test, 1, getQuantiles)
  # appyl writes results (quantiles) in columns, we want it in rows
  return(t(joinedForecast))
}

# wrapper and help functions ===================================================

getAttr <- function(element, version) {
  # attributes belonging to variable selection version
  varCombis <- list(c("VAR169"), c("VAR178"), c("VAR169", "VAR178"),
                   c("VAR169", "VAR178", "VAR167"),
                   c("VAR169", "VAR178", "VAR167", "VAR157"))
  sign <- list(1, 1, c(1, 1), c(1, 1, 1), c(1, 1, 1, -1))
  # attributes belonging to idr application versions
  titles <- list(c("IDR on all"), c("IDR on hour"))
  desrciptions <- list(c("Apply", "IDR", "on", "the", "whole", "training",
                                     "set"),
                      c("Group", "training", "set", "by", "hour", "and",
                        "apply", "IDR", "on", "every", "group", "separately"))
  needTimeStamps <- list(FALSE, TRUE)
  functions <- list(idrOnAll, idrOnHour)
  attributes <- list(VAR = varCombis, TIT = titles, DES = desrciptions,
                     FUN = functions, NTS = needTimeStamps, SGN = sign)
  return(attributes[[element]][[version]])
}


unleashIDR <- function(X_train, y_train, X_test, version, variableVersion,
                print=FALSE) {
  # get variable list that is used
  vars <- getAttr(VAR, variableVersion)
  # if version needs timestamps extend variable list by timestamps
  if (getAttr(NTS, version)) {
    vars <- c("TIMESTAMP", vars)
  }
  # if print, then output information and do nothing else
  if (print) {
    outputForecastingMethod(getAttr(TIT, version),
                            paste0(getAttr(VAR, variableVersion), "(",
                                   getAttr(SGN, variableVersion), ")"),
                            getAttr(DES, version))
    return("")
  }
  groups <- setNames(rep(1, length(vars)), vars)
  orders <- c("comp" = 1)

  return(getAttr(FUN, version)(X_train[var], y_train, X_test[var], groups,
                               orders))
}

# IDR applied on whole training data ===========================================
# parameter variations ---------------------------------------------------------
idrOnAll_V1 <- function(X_train, y_train, X_test, print=FALSE) {
  var <- c("VAR169")
  groups <- c("VAR169" = 1)
  orders <- c("comp" = 1)
  return(invoke_idrOnAll(X_train, y_train, X_test, var, print, groups, orders))
}


idrOnAll_V2 <- function(X_train, y_train, X_test, print=FALSE) {
  var <- c("VAR178")
  groups <- c("VAR178" = 1)
  orders <- c("comp" = 1)
  return(invoke_idrOnAll(X_train, y_train, X_test, var, print, groups, orders))
}


idrOnAll_V3 <- function(X_train, y_train, X_test, print=FALSE) {
  var <- c("VAR169", "VAR178")
  groups <- c("VAR178" = 1, "VAR169" = 1)
  orders <- c("comp" = 1)
  return(invoke_idrOnAll(X_train, y_train, X_test, var, print, groups, orders))
}

# invoke the method
invoke_idrOnAll <- function(X_train, y_train, X_test, var, print, groups,
                            orders) {
  if (print) {
    outputForecastingMethod(name_all, var, descr_all)
    return("")
  }
  return(idrOnAll(X_train[var], y_train, X_test[var], groups, orders))
}


# actual implementation of IDR -------------------------------------------------


# IDR applied on data grouped by hour ==========================================
# parameter variations ---------------------------------------------------------
idrOnHour_V1 <- function(X_train, y_train, X_test, print=FALSE) {
  var <- c("TIMESTAMP", "VAR169")
  groups <- c("VAR169" = 1)
  orders <- c("comp" = 1)
  return(invoke_idrOnHour(X_train, y_train, X_test, var, print, groups, orders))
}


idrOnHour_V2 <- function(X_train, y_train, X_test, print=FALSE) {
  var <- c("TIMESTAMP", "VAR178")
  groups <- c("VAR178" = 1)
  orders <- c("comp" = 1)
  return(invoke_idrOnHour(X_train, y_train, X_test, var, print, groups, orders))
}


idrOnHour_V3 <- function(X_train, y_train, X_test, print=FALSE) {
  var <- c("TIMESTAMP", "VAR169", "VAR178")
  groups <- c("VAR178" = 1, "VAR169" = 1)
  orders <- c("comp" = 1)
  return(invoke_idrOnHour(X_train, y_train, X_test, var, print, groups, orders))
}

# invoke the method
invoke_idrOnHour <- function(X_train, y_train, X_test, var, print, groups,
orders) {
  if (print) {
    outputForecastingMethod(name_all, var, descr_all)
    return("")
  }
  return(idrOnHour(X_train[var], y_train, X_test[var], groups, orders))
}
