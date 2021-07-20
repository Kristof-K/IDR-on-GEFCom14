library(isodistrreg)

source("util.R")


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
# Version3 : Version 2, on data of all zones combined
idrOnHour <- function(X_train, y_train, X_test, groups, orders) {
  return(idrByGroup(X_train, y_train, X_test, groups, orders, getHours))
}

idrByGroup <- function(X_train, y_train, X_test, groups, orders, groupingfct,
                       bag_number=NA, bag_size=NA) {
  train <- cbind(POWER = y_train, X_train)
  # breakdown data by hour
  categories <- groupingfct(NA, NA, getCategories=TRUE)
  removeVars <- groupingfct(NA, NA, getGroupVar=TRUE)

  # for every hour fit the model and predict
  predictByGroup <- function(g) {
    train_indices <- groupingfct(train, g)
    test_indices <- groupingfct(X_test, g)
    if(sum(test_indices) == 0) {    # test_data belongs to other group
      return(numeric(0))
    }
    if(sum(train_indices) == 0) {
      return("ERROR: training period doesn't comprise necessary groups")
    }
    y <- subset(train, train_indices)$POWER
    # matrix containing in every row quantile forecast plus original indices
    output <- matrix(0, nrow=sum(test_indices), ncol=length(QUANTILES)+1)
    output[,1] <- which(test_indices)   # original indices in 1st column
    # idr cannot handle if y is constant
    if (max(y) - min(y) <= 0.01) {
      output[,-1] <- (max(y) + min(y)) / 2     # constant predictions if y is constant
      return(output)
    }
    trainByGroup <- subset(train, train_indices)[!(names(train) %in% c("POWER", removeVars))]
    makePred <- subset(X_test, test_indices)[!(names(X_train) %in% removeVars)]
    if (any(is.na(c(bag_number, bag_size)))) {
      model <- idr(y=y, X=trainByGroup,  groups=groups, orders=orders,
                   progress=FALSE)
      predictions <- predict(model, data=makePred)
    } else {
      predictions <- idrbag(y=y, X=trainByGroup,  groups=groups, orders=orders,
                          newdata=makePred, b=bag_number, p=bag_size,
                          progress=FALSE)
    }
    output[,-1] <- qpred(predictions, quantiles=QUANTILES)
    return(output)   # eveything worked
  }
  # combine the data.frames
  quantilesPred <- do.call(rbind, lapply(categories, predictByGroup))
  # return rows ordered in original order X_test (and remove index column)
  return(quantilesPred[order(quantilesPred[,1]), -1])
}

# Version3 : Apply idr subagging on data grouped by hour
idrsubOnHour <- function(X_train, y_train, X_test, groups, orders) {
  bag_number <- 75
  bag_size <- 0.25
  return(idrByGroup(X_train, y_train, X_test, groups, orders, getHours,
                        bag_number=bag_number, bag_size=bag_size))
}

# Version4 : Apply idr on data grouped by month
idrOnMonth <- function(X_train, y_train, X_test, groups, orders) {
  return(idrByGroup(X_train, y_train, X_test, groups, orders, getMonths))
}

# Version5 : Apply idr on data grouped by season (winder, summer and inbetween)
idrOnSeason <- function(X_train, y_train, X_test, groups, orders) {
  return(idrByGroup(X_train, y_train, X_test, groups, orders, getSeasons))
}

# Version6 : Apply idr on data grouped by wind angle
idrOnWindDirection <- function(X_train, y_train, X_test, groups, orders) {
  return(idrByGroup(X_train, y_train, X_test, groups, orders,
                    getWind100Directions))
}

# Version7 : Apply idr subagging on data grouped by wind angle
idrsubOnWindDirection <- function(X_train, y_train, X_test, groups, orders) {
  bag_number <- 75
  bag_size <- 0.25
  return(idrByGroup(X_train, y_train, X_test, groups, orders,
                    getWind100Directions, bag_number=bag_number,
                    bag_size=bag_size))
}

# Version5 : Apply idr on data grouped by season (winder, summer and inbetween)
idrOnSeasonHour <- function(X_train, y_train, X_test, groups, orders) {
  return(idrByGroup(X_train, y_train, X_test, groups, orders, getSeasonHours))
}

# IDR variants and variable combinations =======================================

# idr version ideas
# * IDR on every hour +- 1 hour (~extended probabilistoc climatological)
# * IDR on every astronomical hour (same sunposition)

# IDR variants:
# FUN : idr core method
# TIT : title
# NTS : need time stamps columns

IDR_ON_ALL <- list(FUN = idrOnAll, TIT = "General IDR",
                   DES = c("Apply","IDR","on","the","whole","training","set"),
                   ADD = list())
IDR_BY_HOUR <- list(FUN = idrOnHour, TIT = "Hourly IDR",
                    DES = c("Group","training","set","by","hour","and","apply",
                            "IDR","on","every","group","separately"),
                    ADD = list(VAR="TIMESTAMP", SGN=1))
IDR_SUB_BY_HOUR <- list(FUN = idrsubOnHour, TIT = "Hourly IDR subagging",
                        DES = c("Group","training","set","by","hour","and",
                                "apply","IDR","subagging","on","every","group",
                                "separately"),
                        ADD = list(VAR="TIMESTAMP", SGN=1))
IDR_BY_MONTH <- list(FUN = idrOnMonth, TIT = "Monthly IDR",
                    DES = c("Group","training","set","by","month","and","apply",
                            "IDR","on","every","group","separately"),
                    ADD = list(VAR="TIMESTAMP", SGN=1))
IDR_BY_SEASON <- list(FUN = idrOnSeason, TIT = "Seasonly IDR",
                    DES = c("Group","training","set","by","season","and","apply",
                            "IDR","on","every","group","separately"),
                    ADD = list(VAR="TIMESTAMP", SGN=1))
IDR_BY_WINDDIR100 <- list(FUN = idrOnWindDirection, TIT = "100m wind dir IDR",
                          DES = c("Group","training","set","by","binned","wind",
                                  "direction","(12)","at","100m","height","and",
                                  "apply","IDR", "on","every","group",
                                  "separately"),
                          ADD = list(VAR="A100", SGN=1))
IDR_SUB_BY_WINDDIR100 <- list(FUN = idrsubOnWindDirection,
                              TIT = "100m wind dir IDR subagging",
                              DES = c("Group","training","set","by","binned",
                                      "wind","direction","at","100m","height",
                                      "and","apply","IDR","subagging","on",
                                      "every","group","separately"),
                              ADD = list(VAR="A100", SGN=1))
IDR_BY_SEASON_HOUR <- list(FUN = idrOnSeasonHour, TIT = "Season+hour IDR",
                    DES = c("Group","training","set","by","season","and","apply",
                            "IDR","on","every","group","separately"),
                    ADD = list(VAR="TIMESTAMP", SGN=1))

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

W10 <- list(VAR = "S10", SGN = 1)
W100 <- list(VAR = "S100", SGN = 1)
W110 <- list(VAR = c("S10", "S100"), SGN = c(1, 1))

# ORDERs
COMP <- "comp"
ICX <- "icx"
SD <- "sd"


# ID is a 4-tupel, first element identifying the idr variant, second the
# variable selection, third number defining partial order and 4th a value
# between

getVariant <- function(id) {
  return(switch(id[1], IDR_ON_ALL, IDR_BY_HOUR, IDR_SUB_BY_HOUR, IDR_BY_MONTH,
                IDR_BY_SEASON, IDR_BY_WINDDIR100, IDR_SUB_BY_WINDDIR100,
                IDR_BY_SEASON_HOUR))
}

getVariableSelection <- function(id, track) {
  if (track == "Solar") {
    return(switch(id[2], SUN_1, SUN_2, SUN_BOTH, SUN_T, SUN_T_H, SUN_H,
                  SUN_1_H, SUN_1_W, S1_W_I, S1_W_L, S1_W_I_C, S1_W_I_L, S1_W_I_T,
                  SUN_W_I))
  } else if (track == "Wind") {
    return(switch(id[2], W10, W100, W110))
  }
}

getOrder <- function(id) {
  return(switch(id[3], COMP, ICX, SD))
}

getVariantName <- function(id) {
  return(getVariant(id)$TIT)
}

getVariablesName <- function(id, track) {
  variables <- getVariableSelection(id, track)
  return(paste0(variables$VAR, "(", variables$SGN, ") "))
}

getIDStr <- function(id) {
  return(paste0(id, collapse="_"))
}


# wrapper ======================================================================

# combine data with highly correlated (target time series) (correlation higher
# than threshold) and then train idr model with combined data
idr_by_zone <- function(idr_fcn, X_train, y_train, X_test, groups, orders,
                        thresh) {
  train <- cbind(POWER=y_train, X_train)

  applyIDRonZone <- function(zone) {
    test_indices <- (X_test$ZONE == zone)
    X_test_zone <- subset(X_test, test_indices, select=-ZONE)
    X_train_col <- subset(X_train, ZONE==zone, select=-ZONE)
    y_col <- subset(train, ZONE==zone)$POWER
    y_zone <- y_col
    output <- matrix(0, nrow=sum(test_indices), ncol=length(QUANTILES)+1)
    output[,1] <- which(test_indices)   # original indices in 1st column

    for(z in unique(X_train$ZONE)) {
      if (z == zone) next
      y_z <- subset(train, ZONE==z)$POWER
      # removing NA rows can lead to different long blocks for each zone
      minIndex <- min(length(y_z), length(y_zone))
      # add all zones with Power correlation greater than threshold
      if (cor(y_z[1:minIndex], y_zone[1:minIndex]) >= thresh) {
        X_train_add <- subset(train, ZONE==z, select=c(-ZONE, -POWER))
        y_col <- c(y_col, y_z)    # add new y values
        X_train_col <- rbind(X_train_col, X_train_add)
      }
    }
    output[,-1] <- idr_fcn(X_train_col, y_col, X_test_zone, groups, orders)
    return(output)
  }
  pred <- do.call(rbind, lapply(unique(X_test$ZONE), applyIDRonZone))
  return(pred[order(pred[,1]), -1])   # restore original order
}

unleashIDR <- function(track, X_train, y_train, X_test, id, init=FALSE) {
  idr_v <- getVariant(id)
  variables <- getVariableSelection(id, track)
  pOrder <- getOrder(id)
  thresh <- if(is.na(id[4])) 1 else id[4]
  pbz <- if(thresh >= 1) TRUE else FALSE
  # if init print, then output information and return important information
  if (init) {
    tit <- paste0(idr_v$TIT, " (", getIDStr(id),")")
    outputForecastingMethod(tit, idr_v$DES, getVariablesName(id, track))
    return(list(TRACK=track, TIT=idr_v$TIT, VAR=variables$VAR, OR=pOrder,
                PBZ=pbz, ID=getIDStr(id)))
  }
  # get variable list that is used and signs of that variables
  vars <- variables$VAR
  signs <- variables$SGN

  groups <- setNames(rep(1, length(vars)), vars)
  orders <- setNames(1, pOrder)

  if (!is.null(idr_v$ADD)) {
    vars <- c(idr_v$ADD$VAR, vars)
    signs <- c(idr_v$ADD$SGN, signs)
  }
  # take care about signs => -1 means change sign of covariates
  X_train[vars[signs == -1]] <- (-1) * X_train[vars[signs == -1]]
  X_test[vars[signs == -1]] <- (-1) * X_test[vars[signs == -1]]

  if (!pbz) {        # idr by zone gets all data
    vars <- c(vars, "ZONE")
    return(idr_by_zone(idr_v$FUN, X_train[vars], y_train, X_test[vars], groups,
                       orders, thresh))
  } else {
    return(idr_v$FUN(X_train[vars], y_train, X_test[vars], groups, orders))
  }
}

unleashSolIDR <- function(X_train, y_train, X_test, id, init=FALSE) {
  return(unleashIDR("Solar", X_train, y_train, X_test, id, init))
}

unleashWinIDR <- function(X_train, y_train, X_test, id, init=FALSE) {
  # some POWER values in the wind track are NA
  indices <- !is.na(y_train)
  return(unleashIDR("Wind", X_train[indices,], y_train[indices], X_test,
                    id, init))
}
