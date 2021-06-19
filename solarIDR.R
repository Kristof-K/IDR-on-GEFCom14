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
  return(idrByGroup(X_train, y_train, X_test, groups, orders))
}

idrByGroup <- function(X_train, y_train, X_test, groups, orders, bag_number=NA,
                       bag_size=NA) {
  train <- cbind(POWER = y_train, X_train)
  # matrix containing in every row quantile forecast for a row in X_test
  output <- matrix(0, nrow = nrow(X_test), ncol = length(QUANTILES))
  # breakdown data by hour
  hours <- paste0(0:23)   # categories by which is grouped

  # for every category fit the model and predict
  for (h in hours) {
    train_indices <- (hour(train$TIMESTAMP) == h)
    test_indices <- (hour(X_test$TIMESTAMP) == h)
    y <- subset(train, train_indices)$POWER
    # idr cannot handle if y is constant
    if (max(y) == min(y)) {
      output[test_indices,] <- y[1]     # constant predictions if y is constant
      next
    }
    trainByHour <- subset(train, train_indices, select=c(-TIMESTAMP, -POWER))
    makePred <- subset(X_test, test_indices, select=-TIMESTAMP)
    if (any(is.na(c(bag_number, bag_size)))) {
      model <- idr(y=y, X=trainByHour,  groups=groups, orders=orders,
                   progress=FALSE)
      predictions <- predict(model, data=makePred)
    } else {
      predictions <- idrbag(y=y, X=trainByHour,  groups=groups, orders=orders,
                          newdata=makePred, b=bag_number, p=bag_size,
                          progress=FALSE)
    }
    output[test_indices,] <- qpred(predictions, quantiles=QUANTILES)
  }
  return(output)
}

# Version3 : Apply idr on data grouped by hour, but use data from all zones
# for training
idrOnHourAllZones <- function(X_train, y_train, X_test, groups, orders) {
  X_train <- X_train[!(names(X_train) == "ZONE")]
  X_test <- X_test[!(names(X_train) == "ZONE")]
  return(idrOnHour(X_train, y_train, X_test, groups, orders))
}

# Version4 : Apply idr subagging on data grouped by hour
idrsubOnHour <- function(X_train, y_train, X_test, groups, orders) {
  bag_number <- 75
  bag_size <- 0.25
  return(idrByGroup(X_train, y_train, X_test, groups, orders,
                        bag_number=bag_number, bag_size=bag_size))
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
IDR_BY_HOUR_ALL_ZONES <- list(FUN = idrOnHourAllZones,
                              TIT = "Hourly IDR all zones",
                              DES = c("Combine","all","zones","to","one",
                                      "training","set","and","group","it","by",
                                      "hour","before","applying", "IDR","on",
                                      "every","group","separately"),
                              NTS = TRUE, PBZ = FALSE)
IDR_SUB_BY_HOUR <- list(FUN = idrsubOnHour, TIT = "Hourly IDR subagging",
                        DES = c("Group","training","set","by","hour","and",
                                "apply","IDR","in","subagging","manner","on",
                                "every","group","separately"),
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
SUN_H <- list(VAR = c("VAR169", "VAR178", "VAR157"), SGN = c(1, 1, -1))
SUN_1_H <- list(VAR = c("VAR169", "VAR157"), SGN = c(1, -1))
SUN_1_W <- list(VAR = c("VAR169", "VAR157", "VAR228"), SGN = c(1, -1, -1))
S1_W_I <- list(VAR = c("VAR169", "VAR157", "VAR228", "VAR79"),
               SGN = c(1, -1, -1, -1))
S1_W_L <- list(VAR = c("VAR169", "VAR157", "VAR228", "VAR78"),
               SGN = c(1, -1, -1, -1))
S1_W_I_C <- list(VAR = c("VAR169", "VAR157", "VAR228", "VAR79", "VAR164"),
               SGN = c(1, -1, -1, -1, -1))
S1_W_I_L <- list(VAR = c("VAR169", "VAR157", "VAR228", "VAR79", "VAR78"),
               SGN = c(1, -1, -1, -1, -1))
S1_W_I_T <- list(VAR = c("VAR169", "VAR157", "VAR228", "VAR79", "VAR175"),
               SGN = c(1, -1, -1, -1, 1))
SUN_W_I <- list(VAR = c("VAR169", "VAR157", "VAR228", "VAR79", "VAR178"),
               SGN = c(1, -1, -1, -1, 1))

# ORDERs
COMP <- "comp"
ICX <- "icx"
SD <- "sd"

# ID is a tripel, first element identifying the idr variant, second the
# variable selection, third number defining group

getVariant <- function(id) {
  return(switch(id[1], IDR_ON_ALL, IDR_BY_HOUR, IDR_BY_HOUR_ALL_ZONES,
                IDR_SUB_BY_HOUR))
}

getVariableSelection <- function(id) {
  return(switch(id[2], SUN_1, SUN_2, SUN_BOTH, SUN_T, SUN_T_H, SUN_H,
                SUN_1_H, SUN_1_W, S1_W_I, S1_W_L, S1_W_I_C, S1_W_I_L, S1_W_I_T,
                SUN_W_I))
}

getOrder <- function(id) {
  return(switch(id[3], COMP, ICX, SD))
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
