library(isodistrreg)

source("util.R")


# core functions ===============================================================

# Version1 : Apply idr on the whole data set
idrOnAll <- function(X_train, y_train, X_test, groups, orders, thresh=1,
                     groupingfct=getHours, bag_number=NA, bag_size=NA) {
  # apply idr, by fitting, prediction and extracting the quantiles
  fit <- idr(y = y_train, X = X_train, groups = groups, order = orders)
  cdf_predictions <- predict(fit, data = X_test)
  quantile_predictions <- qpred(cdf_predictions, quantiles = QUANTILES)
  return(quantile_predictions)
}

# Version2: Apply idr an data grouped by a groupingfct
idrByGroup <- function(X_train, y_train, X_test, groups, orders, thresh=1,
                       groupingfct=getHours, bag_number=NA, bag_size=NA) {
  train <- cbind(TARGET = y_train, X_train)
  # breakdown data by hour
  categories <- groupingfct(NA, NA, getCategories=TRUE)
  removeVars <- groupingfct(NA, NA, getGroupVar=TRUE)

  # for every hour fit the model and predict
  predictByGroup <- function(g) {
    train_indices <- groupingfct(train, g)
    test_indices <- groupingfct(X_test, g, test=TRUE)
    if(sum(test_indices) == 0) {    # test_data belongs to other group
      return(numeric(0))
    }
    if(sum(train_indices) <= 10) {
      # increase train data by using neighboring groups
      for(i in 1:ceiling(length(categories) / 2)) {
        prev <- if (g - i < 1) length(categories) + (g - i) else g - i
        succ <- if (g + i > length(categories)) (g + i) %% length(categories)
                    else g + i
        train_indices <- train_indices | groupingfct(train, prev) |
                           groupingfct(train, succ)
        if (sum(train_indices) > 10) break
      }
      if(sum(train_indices) <= 10) {
        return("ERROR: training period doesn't comprise necessary groups")
      }
    }
    y <- subset(train, train_indices)$TARGET
    # matrix containing in every row quantile forecast plus original indices
    output <- matrix(0, nrow=sum(test_indices), ncol=length(QUANTILES)+1)
    output[,1] <- which(test_indices)   # original indices in 1st column

    trainByGroup <- subset(train, train_indices)[!(names(train) %in% c("TARGET", removeVars))]
    makePred <- subset(X_test, test_indices)[!(names(X_test) %in% removeVars)]
    output[,-1] <- myIDR(trainByGroup, y, makePred, groups, orders, bag_number,
                         bag_size)
    return(output)   # eveything worked
  }
  # combine the data.frames
  quantilesPred <- do.call(rbind, lapply(categories, predictByGroup))
  # return rows ordered in original order X_test (and remove index column)
  return(quantilesPred[order(quantilesPred[,1]), -1])
}

myIDR <- function(X_train, y_train, X_test, groups, orders, bag_number=NA,
                  bag_size=NA) {
  # idr cannot handle if y is constant
  if (max(y_train) == min(y_train)) {
    return(y_train[1])     # constant predictions if y is constant
  }
  # idr cannot predict if there is only one combination of covariates
  if (all(sapply(X_train, function(x) length(unique(x))==1))) {
    model <- idr(y=y_train, X=X_train,  groups=groups, orders=orders,
                 progress=FALSE)
    cdf_vals <- as.vector(model$cdf[1,])
    jumps <- (cdf_vals != lag(cdf_vals, default=0))
    vals <- cdf_vals[jumps]
    preds <- list()
    for(i in 1:nrow(X_test)) {
      preds[[i]] <- data.frame(points=model$thresholds[jumps], cdf=vals)
    }
    # pack it as idr (in order to apply idr function qpred)
    predictions <- structure(preds, class = "idr", incomparables= integer(0))
  } else if (any(is.na(c(bag_number, bag_size)))) {
    model <- idr(y=y_train, X=X_train,  groups=groups, orders=orders,
                 progress=FALSE)
    predictions <- predict(model, data=X_test)
  } else {
    predictions <- idrbag(y=y_train, X=X_train,  groups=groups, orders=orders,
                          newdata=X_test, b=bag_number, p=bag_size,
                          progress=FALSE)
  }
  return(qpred(predictions, quantiles=QUANTILES))
}

# Version3: combine data with highly correlated (target time series) data
idrByZones <- function(X_train, y_train, X_test, groups, orders, thresh=1,
                        groupingfct=gethours, bag_number=NA, bag_size=NA) {
  train <- cbind(TARGET=y_train, X_train)

  applyIDRonZone <- function(zone) {
    test_indices <- (X_test$ZONEID == zone)
    X_test_zone <- subset(X_test, test_indices, select=-ZONEID)
    X_train_col <- subset(X_train, ZONEID==zone, select=-ZONEID)
    y_col <- subset(train, ZONEID==zone)$TARGET
    y_zone <- y_col
    output <- matrix(0, nrow=sum(test_indices), ncol=length(QUANTILES)+1)
    output[,1] <- which(test_indices)   # original indices in 1st column

    for(z in unique(X_train$ZONEID)) {
      if (z == zone) next
      y_z <- subset(train, ZONEID==z)$TARGET
      # removing NA rows can lead to different long blocks for each zone
      minIndex <- min(length(y_z), length(y_zone))
      # add all zones with TARGET correlation greater than threshold
      if (cor(y_z[1:minIndex], y_zone[1:minIndex]) >= thresh) {
        X_train_add <- subset(train, ZONEID==z, select=c(-ZONEID, -TARGET))
        y_col <- c(y_col, y_z)    # add new y values
        X_train_col <- rbind(X_train_col, X_train_add)
      }
    }
    output[,-1] <- idrByGroup(X_train_col, y_col, X_test_zone, groups, orders,
                              groupingfct=groupingfct, bag_number=bag_number,
                              bag_size=bag_size)
    return(output)
  }
  pred <- do.call(rbind, lapply(unique(X_test$ZONEID), applyIDRonZone))
  return(pred[order(pred[,1]), -1])   # restore original order
}


# IDR variants and variable combinations =======================================

# idr version ideas
# * IDR on every astronomical hour (same sunposition)

# IDR variants:
# FUN : idr core method
# TIT : title
# ADD : variables that should be added to the variable list

IDR_ON_ALL <- list(FUN = idrOnAll, TIT = "General IDR",
                   DES = c("Apply","IDR","on","the","whole","training","set"),
                   ADD = NULL)
IDR_BY_GROUP <- list(FUN = idrByGroup, TIT = "IDR grouped data",
                    DES = c("Group","training","set","and","apply", "IDR","on",
                            "every","group","separately"), ADD = NULL)
IDR_BY_ZONE <- list(FUN = idrByZones, TIT = "IDR zone combined data",
                    DES = c("Combine", "highly","correlated","data","(regarding",
                            "target","variable)","and","apply","idr_by_group",
                            "on","this","data"), ADD = "ZONEID")

# Variable selections
# VAR : list of variables that are used
# ADA : sign correction (1 for positive relation, -1 for negative)

ONE <- function(data, col) return(data[[col]])
INV <- function(data, col) return((-1) * data[[col]])
INV_WIN <- function(data, col) return((2 * getSumWin(data) - 3) * data[[col]])

solarV <- c("VAR169", "VAR178", "VAR167", "VAR157", "VAR228", "VAR79", "VAR78",
            "VAR164", "VAR175", "VAR134", "VAR165", "VAR166")
solarA <- list(ONE, ONE, ONE, INV, INV, INV, INV, INV, ONE, ONE, INV, INV)

windV <- c("S10", "S100", "U10", "V10", "U100", "V100", "A10", "A100", "SX")
windA <- list(ONE, ONE, ONE, ONE, ONE, ONE, ONE, ONE, ONE)

#loadV <- c("w1", "w2", "w10", "w25", "w11", "w13", "w15", "w22", "w23", "w24")
#loadV <- c("w3", "w4", "w5", "w6", "w7", "w8", "w9", "w12", "w14", "w15")
loadV <- c("w16", "w17", "w18", "w19", "w20", "w21")
loadA <- list(INV_WIN, INV_WIN, INV_WIN, INV_WIN, INV_WIN, INV_WIN, INV_WIN,
              INV_WIN, INV_WIN, INV_WIN)

priceV <- c("Forecasted.Total.Load", "Forecasted.Zonal.Load", "WDAY", "HOUR6",
            "WDAYHOUR6", "WDAY4", "WDAY4_CAT", "WDAY2", "WDAY2HOUR6")
priceA <- list(ONE, ONE, ONE, ONE, ONE, ONE, ONE, ONE, ONE)

# ORDERs
COMP <- "comp"
ICX <- "icx"
SD <- "sd"


# ID is a 6-tupel:
# (variable selection, order, data grouping, zone threshold,
# number of subaggs, size of subaggs)

getVariant <- function(id) {
  if (id[3] == 1) return(IDR_ON_ALL)    # no grouping
  if (id[4] < 1) return(IDR_BY_ZONE)    # combine zones for training
  return(IDR_BY_GROUP)
}

getVariableSelection <- function(id, track) {
  # array of digits
  indices <- as.numeric(strsplit(as.character(format(id[1], scientific=FALSE)),
                                 split ="")[[1]])
  # which indices are non_zero
  nonzero_ind <- which(rev(indices != 0))
  if (track == "Solar") {
    vars <- solarV[nonzero_ind]
    ada_fcns <- solarA[nonzero_ind]
  } else if (track == "Wind") {
    vars <- windV[nonzero_ind]
    ada_fcns <- windA[nonzero_ind]
  } else if (track == "Load") {
    vars <- loadV[nonzero_ind]
    ada_fcns <- loadA[nonzero_ind]
  } else if (track == "Price") {
    vars <- priceV[nonzero_ind]
    ada_fcns <- priceA[nonzero_ind]
  }
  return(list(VAR = vars, ADA = ada_fcns))
}

getOrder <- function(id) {
  return(switch(id[2], COMP, ICX, SD))
}


# wrapper ======================================================================

unleashIDR <- function(track, X_train, y_train, X_test, id, init=FALSE) {
  variables <- getVariableSelection(id, track)
  pOrder <- getOrder(id)
  # addional arguments
  if(is.na(id[3])) id[3] <- 1           # grouping : default no grouping
  id[4] <- min(c(id[4], 1), na.rm=TRUE) # zone merging : defualt no zone merging
  if(!is.na(id[5]) && id[5] < 2) id[5] <- 75                        # bag number
  if(!is.na(id[6]) && (id[6] <= 0 || id[6] >= 1)) id[6] <- 0.25     # bag size
  groupingfct <- getGroupingfct(id[3])
  idr_v <- getVariant(id)
  id_str <- paste0(id, collapse="_")
  # if init print, then output information and return important information
  if (init) {
    pbz <- (id[4] >= 1)
    tit <- paste0(idr_v$TIT, " (", id_str, ")")
    outputForecastingMethod(tit, idr_v$DES, variables$VAR)
    return(list(TRACK=track, TIT=idr_v$TIT, VAR=variables$VAR, OR=pOrder,
                PBZ=pbz, ID=id_str, GR=groupingfct(NA, NA, getName=TRUE)))
  }
  # get variable list and adapt them
  vars <- variables$VAR
  for (i in 1:length(vars)) {
    X_train[vars[i]] <- variables$ADA[[i]](X_train, vars[i])
    X_test[vars[i]] <- variables$ADA[[i]](X_test, vars[i])
  }

  groups <- setNames(rep(1, length(vars)), vars)
  orders <- setNames(1, pOrder)

  if (!is.null(idr_v$ADD)) vars <- c(idr_v$ADD, vars)
  groupvar <- groupingfct(NA, NA, getGroupVar=TRUE)
  if (!is.null(groupvar)) vars <- c(vars, groupvar)

  return(idr_v$FUN(X_train[vars], y_train, X_test[vars], groups, orders,
                   groupingfct=groupingfct, thresh=id[4], bag_number=id[5],
                   bag_size=id[6]))
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

unleashLoaIDR <- function(X_train, y_train, X_test, id, init=FALSE) {
  # inital TARGET values in the load track are NA
  indices <- !is.na(y_train)
  return(unleashIDR("Load", X_train[indices,], y_train[indices], X_test,
                    id, init))
}

unleashPriIDR <- function(X_train, y_train, X_test, id, init=FALSE) {
  return(unleashIDR("Price", X_train, y_train, X_test, id, init))
}