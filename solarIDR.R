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
                           orders = orders, progress = FALSE)
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

# IDR variants and variable combinations =======================================

# current idr versions
# 1 IDR on all data
# 2 IDR on every hour
# * IDR on every hour +- 1 hour (~extended probabilistoc climatological)
# * IDR on every astronomical hour (same sunposition)
# * IDR on every hour all zones at once
# * IDR subbagging
# * IDR other orders

# IDR variants:
# FUN : idr core method
# TIT : title
# NTS : need time stamps columns
# PBZ : predict by zone -> don't add zone column

IDR_ON_ALL <- list(FUN = idrOnAll, TIT = "General IDR",
                   DES = c("Apply","IDR","on","the","whole","training","set"),
                   NTS = FALSE, PBZ = TRUE)
IDR_BY_HOUR <- list(FUN = idrOnHour, TIT = "Hourly IDR",
                    DES = c("Group","training","set","by","hour","and","apply",
                            "IDR","on","every","group","separately"),
                    NTS = TRUE, PBZ = TRUE)

# Variable selections
# VAR : list of variables that are used
# SGN : with which sign are the variables used
#       (1 for positive relation, -1 for negative)

SUN_1 <- list(VAR = "VAR169", SGN = 1)
SUN_2 <- list(VAR = "VAR178", SGN = 1)
SUN_BOTH <- list(VAR = c("VAR169", "VAR178"), SGN = c(1, 1))
SUN_T <- list(VAR = c("VAR169", "VAR178", "VAR167"), SGN = c(1, 1, 1))
SUN_T_H <- list(VAR = c("VAR169", "VAR178", "VAR167", "VAR157"),
                SGN = c(1, 1, 1, -1))

# ORDERs
COMP <- "comp"
SD <- "sd"
ICX <- "icx"

# ID is a tripel, first element identifying the idr variant, second the
# variable selection, third number defining group

getVariant <- function(id) {
  return(switch(id[1], IDR_ON_ALL, IDR_BY_HOUR))
}

getVariableSelection <- function(id) {
  return(switch(id[2], SUN_1, SUN_2, SUN_BOTH, SUN_T, SUN_T_H))
}

getOrder <- function(id) {
  return(switch(id[3], COMP, ICX))
}

getVariantName <- function(id) {
  return(getVariant(id)$TIT)
}

getVariablesName <- function(id) {
  variables <- getVariableSelection(id)
  return(paste0(variables$VAR, "(", variables$SGN, ") "))
}


# wrapper ======================================================================

unleashIDR <- function(X_train, y_train, X_test, id, init=FALSE) {
  idr_v <- getVariant(id)
  variables <- getVariableSelection(id)
  pOrder <- getOrder(id)
  # if init print, then output information and return important information
  if (init) {
    outputForecastingMethod(idr_v$TIT, idr_v$DES, getVariablesName(id))
    return(list(TIT=idr_v$TIT, VAR=variables$VAR, OR=pOrder, PBZ=idr_v$PBZ))
  }
  # get variable list that is used and signs of that variables
  vars <- variables$VAR
  signs <- variables$SGN

  groups <- setNames(rep(1, length(vars)), vars)
  orders <- setNames(1, pOrder)

  # if version needs timestamps or zones extend variable list by timestamps
  if (idr_v$NTS) {
    vars <- c("TIMESTAMP", vars)
    signs <- c(1, signs)
  }
  if (!idr_v$PBZ) {
    vars <- c(vars, "ZONE")
    signs <- c(signs, 1)
  }

  # take care about signs => -1 means change sign of covariates
  X_train[vars[signs == -1]] <- (-1) * X_train[vars[signs == -1]]
  X_test[vars[signs == -1]] <- (-1) * X_test[vars[signs == -1]]

  # get IDR method
  return(idr_v$FUN(X_train[vars], y_train, X_test[vars], groups, orders))
}
