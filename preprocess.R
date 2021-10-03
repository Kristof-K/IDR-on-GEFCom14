# define preprocess methods getting X_train and X_test and applying data
# transformation on both

library(dplyr)    # for lag in deaccumulateSun and transmute in

source("util.R")

# For Solar track
ACCUMULATED <- c("VAR169", "VAR178", "VAR175", "VAR228")
INVERT <- c("VAR157", "VAR228", "VAR79", "VAR78", "VAR164", "VAR165", "VAR166")

# Method output description consistently for a preprocessing function
# - name : name of the forecasting method (it should be a vector of words in
#   order to be printed correctly)
outputPreprocessing <- function(name) {
  cat("\n[PREPROCESSING]:", name, "\n", fill = PRINT_WIDTH)
}

# Preprocessing functions
# all functions expect data to be of format definied in load.R

no_pp <- function(data, init=FALSE) {
  name <- NONE
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  return(data)
}

rm_na <- function(data, init=FALSE) {
  name <- "RemoveNAs"
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  for(zone in data$Zones) {
    for (t in c("Train", "Test")) {
      data[[zone]][[t]] <- filter(data[[zone]][[t]], !is.na(TARGET))
    }
  }
  return(data)
}

# SOLAR ========================================================================
deaccuInvertSol <- function(data, init=FALSE) {
  name <- "deacc_and_invert_vars"
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  subShifted <- function(col) {
    return(col - lag(col, default=0))
  }
  # assume that data is ordered in time and conitnuous and first entry starts
  # at 1:00, since there starts the daily sun accumulation
  for (zone in data$Zones) {
    for(t in c("Train", "Test")) {
      for(var in ACCUMULATED) {
        current <- data[[zone]][[t]][[var]]
        asMatrix <- matrix(current, nrow=24)    # every column contains one day
        # deaccumulate every day individually
        deacc <- apply(asMatrix, 2, subShifted)

        data[[zone]][[t]][[var]] <- as.vector(deacc)
      }
      for(var in INVERT) {
        data[[zone]][[t]][[var]] <- (-1) * data[[zone]][[t]][[var]]
      }
    }
  }
  return(data)
}

# WIND =========================================================================
getWindAttributes <- function(data, init=FALSE) {
  name <- "CalcWindAttributes"
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  # determined as max of Spearman correlation between wind power prod. and
  # linearly approximated wind speed at height h (use subagging to stabilize)
  indivHeights <- setNames(c(81, 113, 107, 77, 106, 117, 83, 92, 83, 134),
                           paste0("Zone", 1:10))
  for(zone in data$Zones) {
    alpha <- (indivHeights[[zone]] - 10) / (100 - 10)
    for(t in c("Train", "Test")) {
      data[[zone]][[t]] <- mutate(data[[zone]][[t]], S10 = sqrt(U10^2 + V10^2),
                                  S100 = sqrt(U100^2 + V100^2),
                                  A10 = atan2(V10, U10) * 360 / (2*pi),
                                  A100 = atan2(V100, U100) * 360 / (2*pi),
                                  SX = (1-alpha) * S10 + alpha * S100) %>%
        relocate(TARGET, .after = last_col())
    }
  }
  return(data)
}

# PRICE ========================================================================
addPriceRegressors <- function(data, init=FALSE) {
  name <- "addPriceRegressors"
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  # electricity price means for the respective groupings
  hourVals <- c(34.41, 38.06, 52.11, 56.81, 61.65, 48.15)
  wdayVals <- c(43.11, 49.83, 49.57, 49.26, 51.77, 50.67, 45.52)
  wdayHourVals <- c(35.81, 32.58, 42.8, 46.15, 54.17, 47.17, 33.83,	39.76,
                    54.57, 58.11, 63.92, 48.77, 33.25, 39.49, 53.95, 58.71,
                    63.7, 48.31, 32.66,	38.97, 53.58, 59.47, 63.33, 47.54, 34.2,
                    40.4, 55.63, 63.14,	67.52, 49.73, 34.78, 40.51,	55.94,
                    61.71, 62.96, 48.12, 36.36,	34.73, 48.29, 50.38, 55.93,
                    47.41)
  wday2Vals <- c(50.22, 44.32)
  wday2HourVals <- c(33.74, 39.82, 54.73, 60.23, 64.28, 48.49,
                     36.09, 33.66, 45.55, 48.26, 55.05, 47.29)
  for(zone in data$Zones) {
    for(t in c("Train", "Test")) {
      d <- data[[zone]][[t]]
      wdays <- getWday(d)
      data[[zone]][[t]] <- mutate(d,
                                  WDAY = wdayVals[getWday(d)],
                                  HOUR6 = hourVals[get6DayTime(d)],
                                  WDAYHOUR6 = wdayHourVals[getWday6Hour(d)],
                                  WDAY4 = case_when(wdays %in% 2:4 ~  49.56,
                                                    wdays %in% 5:6 ~ 51.22,
                                                    wdays == 7 ~ 45.52,
                                                    wdays == 1 ~ 43.11),
                                  WDAY4_CAT = case_when(wdays %in% 2:4 ~  3,
                                                    wdays %in% 5:6 ~ 4,
                                                    wdays == 7 ~ 2,
                                                    wdays == 1 ~ 1),
                                  WDAY2 = wday2Vals[get2Wday(d)],
                                  WDAY2HOUR6 = wday2HourVals[get2Wday6Hour(d)])
    }
  }
  return(data)
}

# LOAD =========================================================================

# Temperature forecasts --------------------------------------------------------
# Construct a temperatue forecast generator just by defining a method with
# summarizes a vector to a number
constructTempGenerator <- function(fct, name) {
  tempGenerator <- (function(data, init=FALSE, getName=FALSE) {
    if (getName) return(name)
    if (init) {
      outputPreprocessing(name)
      return(name)
    }
    tCols <- paste0("w", 1:25)
    for(zone in data$Zones) {
      newW <- mutate(data[[zone]]$Train, h=hour(TIMESTAMP), d=day(TIMESTAMP),
                    m=month(TIMESTAMP)) %>%
        group_by(m, d, h) %>%
        summarise(across(all_of(tCols), ~fct(.)), .groups="drop")
      findRow <- function(t) {
        r <- which(newW[["m"]]==month(t) & newW[["d"]]==day(t) &
                    newW[["h"]]==hour(t))
        return(as.numeric(newW[r, tCols]))
      }
      # sapply packs individual results in columns => transpose it
      filtered <- t(sapply(data[[zone]]$Test$TIMESTAMP, findRow))
      colnames(filtered) <- tCols

      data[[zone]]$Test <- cbind(data[[zone]]$Test, data.frame(filtered))
      data[[zone]]$Train <- filter(data[[zone]]$Train, !is.na(TARGET))
    }
    return(data)
  })
  return(tempGenerator)
}

meanTmp <- constructTempGenerator(mean, "meanTemp")
lastTmp <- constructTempGenerator(function(x) last(x), "lastTemp")
sampleTmp <- constructTempGenerator(function(x) sample(x, 1),
                                     "sampleTemp")

weightFctGen <- function(fct) {
  return(
    function(x) {
      n <- length(x)
      return(sum(x * fct(1:n)) / sum(fct(1:n)))
    }
  )
}

# last weight should always have double the weight of first weight
lin2 <- function(x) 1 / (length(x) - 2) * x + 1
linWeightMean <- constructTempGenerator(weightFctGen(lin2), "lwMeanTemp")
quad2 <- function(x) 1 / (length(x)^2 - 2) * x^2 - 1 / (length(x) - 2) * x
quadWeightMean <- constructTempGenerator(weightFctGen(quad2),
                                         "qwMeanTemp")
root2 <- function(x) 1 / (length(x)^0.5 - 2) * x^0.5 -
  1 / (length(x)^(1/3) - 2) * x^(1/3)
rootWeightMean <- constructTempGenerator(weightFctGen(function(x) x^0.5),
                                         "rwMeanTemp")

# Temperature preprocessing ----------------------------------------------------
# Invert Winter temperatures for a given Temperature generator
invertWinter <- function(tmpGen=meanTmp) {
  inverted <- (function(data, init=FALSE) {
    name <- paste0("invWin_", tmpGen(NA, getName=TRUE))
    if (init) {
      outputPreprocessing(name)
      return(name)
    }
    tCols <- paste0("w", 1:25)

    for(zone in data$Zones) {
      curr <- data[[zone]]$Train
      # Summer is 2 => multiply with 1; Winter is 1 => multiply with -1
      data[[zone]]$Train[,tCols] <- (2 * getSumWin(curr) - 3) * curr[,tCols]
    }
    return(tmpGen(data))
  })
  return(inverted)
}

invWin_meanTmp <- invertWinter(meanTmp)
invWin_lwMean <- invertWinter(linWeightMean)

# Transform temperature values to temperature differences
transformToDiffs <- function(f_diff, f_mid, tmpGen=meanTmp) {
  diffed <- (function(data, init=FALSE) {
    name <- paste(f_diff(NA, NA, init=TRUE), f_mid(NA, NA, init=TRUE),
                  tmpGen(NA, getName=TRUE), sep="_")
    if (init) {
      outputPreprocessing(name)
      return(name)
    }
    tCols <- paste0("w", 1:25)

    for(zone in data$Zones) {
      f_trans <- function(col, target) return(f_diff(col, f_mid(col, target)))
      data[[zone]]$Train <- mutate(data[[zone]]$Train,
                                   across(all_of(tCols), .fns=f_trans, TARGET))
    }
    return(tmpGen(data))
  })
  return(diffed)
}

# Define functions calculating a position measure
getDiffMed <- function(col, target, init=FALSE) {
  if (init) return("Med")
  return(median(col))
}
getDiffMedS <- function(col, target, init=FALSE) {
  if (init) return("CondMed")
  non_na <- !is.na(target)
  lower10Tail <- (target[non_na] <= quantile(target[non_na], probs=0.1))
  return(median((col[non_na])[lower10Tail]))
}
getDiffMean <- function(col, target, init=FALSE) {
  if (init) return("Mean")
  return(mean(col))
}
getDiffMeanS <- function(col, target, init=FALSE) {
  if (init) return("WMean")
  non_na <- !is.na(target)
  target <- target[non_na]
  weights <- max(target) - target
  normalize <- length(target) * max(target) - sum(target)
  return(sum(col[non_na] * weights) / normalize)
}
getDiffMax <- function(col, target, init=FALSE) {
  if (init) return("MinMax")
  non_na <- !is.na(target)
  target <- target[non_na]
  col <- col[non_na]
  op <- data.frame(cbind(TARGET=target, bins = cut(col, 500), Exa=col)) %>%
    group_by(bins) %>% summarise(q95 = quantile(TARGET, probs=0.95),
                                 mid = median(Exa), .groups="drop") %>%
    filter(q95 == min(q95)) %>% pull(mid)
  return(median(op))  # could be that we have several vals in op
}
# Define functions calculating a difference
squ_diff <- function(x, y, init=FALSE) {
  if (init) return("squ")
  return((x - y)^2)
}
abs_diff <- function(x, y, init=FALSE) {
  if (init) return("abs")
  return(abs(x - y))
}

abs_meanTmp <- transformToDiffs(abs_diff, getDiffMed, meanTmp)
squ_meanTmp <- transformToDiffs(squ_diff, getDiffMed, meanTmp)

squ_lastTmp <- transformToDiffs(squ_diff, getDiffMed, lastTmp)
squ_sampleTmp <- transformToDiffs(squ_diff, getDiffMed, sampleTmp)
squ_lwMean <- transformToDiffs(squ_diff, getDiffMed, linWeightMean)
squ_qwMean <- transformToDiffs(squ_diff, getDiffMed, quadWeightMean)
squ_rwMean <- transformToDiffs(squ_diff, getDiffMed, rootWeightMean)

abs_meanTmp_CMed <- transformToDiffs(abs_diff, getDiffMedS, meanTmp)
squ_meanTmp_CMed <- transformToDiffs(squ_diff, getDiffMedS, meanTmp)
abs_meanTmp_Mea <- transformToDiffs(abs_diff, getDiffMean, meanTmp)
squ_meanTmp_Mea <- transformToDiffs(squ_diff, getDiffMean, meanTmp)
abs_meanTmp_WMea <- transformToDiffs(abs_diff, getDiffMeanS, meanTmp)
squ_meanTmp_WMea <- transformToDiffs(squ_diff, getDiffMeanS, meanTmp)
abs_meanTmp_MM <- transformToDiffs(abs_diff, getDiffMax, meanTmp)
squ_meanTmp_MM <- transformToDiffs(squ_diff, getDiffMax, meanTmp)

# linear weighting with last element having 3, 4, n times the weight of first
lin3 <- function(x) 1 / (length(x) - 3) * x + 1
linWeight3 <- constructTempGenerator(weightFctGen(lin3), "lw3MeanTemp")
squared_lw3Mean <- transformToDiffs(squ_diff, getDiffMed, linWeight3)

lin4 <- function(x) 1 / (length(x) - 4) * x + 1
linWeight4 <- constructTempGenerator(weightFctGen(lin4), "lw4MeanTemp")
squared_lw4Mean <- transformToDiffs(squ_diff, getDiffMed, linWeight4)

linWeightN <- constructTempGenerator(weightFctGen(function(x) x), "lwNMeanTemp")
squared_lwNMean <- transformToDiffs(squ_diff, getDiffMed, linWeightN)

addLoadSummaries <- function(data, init=FALSE) {
  name <- "AddLoadMeans"
  if (init) {
    outputPreprocessing(name)
    return(name)
  }
  data <- squ_meanTmp(data)
  for(zone in data$Zones) {
    for(t in c("Train", "Test")) {
      data[[zone]][[t]] <- mutate(data[[zone]][[t]],
                                  W9=w9,
                                  M2= rowMeans(cbind(w14, w11)),
                                  M3 = rowMeans(cbind(w14, w11, w21)),
                                  Med2 = apply(cbind(w14, w11), 1,
                                               median),
                                  Med3 = apply(cbind(w14, w11, w21),
                                               1, median))
    }
  }
  return(data)
}