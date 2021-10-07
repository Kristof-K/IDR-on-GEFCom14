library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

source("preprocess.R")
source("loadData.R")
source("util.R")

plotsForThesisLoad <- function() {
  cols <- paste0("w", 1:25)
  d <- loadLoad(4)$Zone1$Train %>% filter(!is.na(TARGET))

  # TRANSFORM TEMPERATURES IN DISTANCES ========================================
  mid_fnc <- list("median" = getDiffMed,
                  "cond-median" = getDiffMedS,
                  "mean"=getDiffMean,
                  "weigh-mean" = getDiffMeanS,
                  "min-max-bin" = getDiffMax)

  mid_points <- summarise(d,
                          across(cols, .fns = mid_fnc, .names = "{.col}_{.fn}",
                                 TARGET)) %>%
    pivot_longer(cols = everything(), names_to=c("WeatherStation", "MidPoint"), names_sep = "_")
  point_clouds <- select(d, all_of(cols), TARGET) %>%
    pivot_longer(cols = -TARGET, names_to="WeatherStation")

  ggplot() +
    geom_point(data=point_clouds, aes(x=value, y=TARGET), alpha=0.1) +
    geom_vline(data=mid_points, aes(xintercept=value, color=MidPoint)) +
    facet_wrap(~WeatherStation, ncol=3) +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13),
          legend.position="bottom")
  ggsave("LoadMidPoints.pdf", path="plots/ForThesis/", width=11.69,
         height=30)

  station <- "w10"
  low10 <- quantile(filter(point_clouds, WeatherStation == station)$TARGET,
                    probs=0.1)
  point_cloud <- ggplot() +
    geom_point(data=filter(point_clouds, WeatherStation == station),
                           aes(x=value, y=TARGET), alpha=0.1) +
    geom_vline(data=filter(mid_points, WeatherStation == station),
               aes(xintercept=value, color=MidPoint), size=1.2) +
    geom_hline(yintercept = low10, color="gray", linetype=2, size=1.2) +
    theme_bw() +
    ylab("Load") +
    xlab("Temperature") +
    ggtitle("Temperature Deviations (w10)") +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13),
          legend.justification=c(1,0), legend.position=c(1,0)) +
    guides(color = guide_legend(title=NULL))

  plot_data <- data.frame()
  for (fnc_m in c("cond-median", "median")) {
    mid <- mid_fnc[[fnc_m]](d$w9, d$TARGET)
    add_df <- data.frame(Load=d$TARGET, T=abs(d$w9 - mid),
                         Lab=fnc_m,
                         Col=ifelse(d$w9 > mid, "Right arm", "Left arm"))
    plot_data <- rbind(plot_data, add_df)
  }
  preprocessed <- ggplot(plot_data, aes(x=T, y=Load, color=Col)) +
    geom_point(alpha=0.1) +
    facet_wrap(~Lab, nrow=1, scales="free_x") +
    ylab("") +
    xlab("Absolute temperature deviation") +
    ggtitle("") +
    scale_color_brewer(palette="Dark2") +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13),
          legend.justification=c(1,0), legend.position=c(1,0)) +
    guides(color = guide_legend(title=NULL))
  grid.arrange(point_cloud, preprocessed, nrow=1)
  ggsave("LoadDist2.pdf", path="plots/ForThesis/", width=11.69,
         height=4.2)

  # LOOK AT MONTHS SEPARATELY OF ONE TEMPERATURE SERIES ========================
  d %>% select(TIMESTAMP, TARGET, w1) %>%
    mutate(Month = getMonths(d, label=TRUE),
           t = day(TIMESTAMP), m = month(TIMESTAMP),
           p = m %in% 5:9 | (m == 4 & t > 16) | (m == 10 & t <= 14),
           Period = ifelse(p, "17.04 - 14.10", "15.10 - 16.04")) %>%
    select(Month, TARGET, w1, Period) %>%
    ggplot(aes(x=w1, y=TARGET, color=Period)) +
    geom_point(alpha=0.2) +
    facet_wrap(~Month) +
    xlab("Temperature") +
    ylab("Load") +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 16),
            axis.text = element_text(size = 13)) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    ggtitle("Load vs. Temperature (w1)")
  ggsave("LoadScatterw1.pdf", path="plots/ForThesis/", width=11.69,
         height=6.2)

  corr <- d %>% mutate(Month = getMonths(d, label=TRUE)) %>% group_by(Month) %>%
    summarise(across(.cols = starts_with("w"),
                     .fns = function(x) cor(TARGET, x,
                                                method="spearman")),
    .groups = "drop") %>%
    pivot_longer(cols = -Month, names_to = "Temperature",
                 values_to = "Correlation")
  top10 <- corr %>% group_by(Month) %>%
    summarise(Temperature, Correlation = 26 - rank(abs(Correlation)),
              .groups = "drop") %>%
    filter(Correlation <= 10)

  ggplot(mapping = aes(y=Month, x = factor(Temperature, ordered=TRUE,
                                           levels=paste0("w", 1:25)))) +
    geom_tile(data = corr, aes(fill = Correlation)) +
    geom_text(data = top10, aes(label = Correlation)) +
    scale_fill_gradient2() +
    xlab("Weather station") +
    ylab("Month") +
    ggtitle("Spearman Correlation: Load And Temperature") +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13))
  ggsave("LoadCorrelationsSub3.png", path="plots/ForThesis/", width=11.69,
         height=6)

  # use subagging to get more reliable estimates of most correlated weather stat
  n <- 77
  frac <- 0.66

  corr <- NA
  for (i in 1:n) {
    sampled <- slice_sample(d, prop = frac, replace = TRUE)
    corr_add <- sampled %>%
    mutate(Month = getMonths(sampled, label=TRUE)) %>% group_by(Month) %>%
    summarise(across(.cols = starts_with("w"),
                     .fns = function(x) cor(TARGET, x,
                                                method="spearman")),
    .groups = "drop")
    if (all(is.na(corr))) corr <- corr_add
    else corr[cols] <- corr[cols] + corr_add[cols]
  }
  corr[cols] <- corr[cols] / n
  top10 <- corr %>% pivot_longer(cols = -Month, names_to = "Temperature",
                            values_to = "Correlation") %>%
    group_by(Month) %>%
    summarise(Temperature, Correlation = 26 - rank(abs(Correlation)),
              .groups = "drop") %>%
    filter(Correlation <= 10)

  fns_list <- list("median"=function(col, target) {
    return(cor(target, squ_diff(col, getDiffMed(col ,target)), method="spearman"))
  },
                   "cond-med"=function(col, target) {
    return(cor(target, squ_diff(col, getDiffMedS(col ,target)), method="spearman"))
  },
                   "mean"=function(col, target) {
    return(cor(target, squ_diff(col, getDiffMean(col ,target)), method="spearman"))
  },
                   "weigh-mean"=function(col, target) {
    return(cor(target, squ_diff(col, getDiffMeanS(col ,target)), method="spearman"))
  },
                   "min-max-bin"=function(col, target) {
    return(cor(target, squ_diff(col, getDiffMax(col ,target)), method="spearman"))
  })

  n <- 100
  frac <- 0.7

  out <- data.frame()
  for (i in 1:n) {
    corrs <- slice_sample(d, prop = frac, replace = TRUE) %>%
      summarise(across(all_of(cols), fns_list, .names = "{.col}_{.fn}", TARGET))
    if (i == 1) {
      out <- corrs
    } else {
      out <- out + corrs
    }
  }
  corr_vals <- (out / n) %>%
    pivot_longer(cols=everything(), names_to=c("WS", "Mid"), names_sep="_") %>%
    mutate(WS = factor(substring(WS, 2), ordered=TRUE, levels=paste(1:25)),
           Mid = factor(Mid, ordered=TRUE, levels=names(fns_list)))
  top25 <- mutate(corr_vals, rk = 25 * 5 + 1 - rank(value)) %>%
    filter(rk <= 50) %>% select(-value)

  ggplot(mapping=aes(x=WS, y=Mid)) +
    geom_tile(data=corr_vals, aes(fill=value)) +
    geom_text(data=top25, aes(label = rk, alpha = 53 - rk), show.legend=FALSE,
              size=5) +
    xlab("Weather station") +
    ylab("") +
    ggtitle("Correlation: Load and Squared Temperatuere Deviations") +
    scale_fill_gradient(low="darkorchid4", high="coral", name = "Correlation") +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13))
  ggsave("LoadCorrTempDiff.pdf", path="plots/ForThesis/", width=11.69,
         height=4)

  # examine separation of winter / summer --------------------------------------
  ex_m <- 10
  plot_data <- data.frame()
  for(i in 10:21) {
    add <- d %>% select(TIMESTAMP, TARGET, w1) %>%
      mutate(t = day(TIMESTAMP), m = month(TIMESTAMP),
             p = m %in% 5:9 | (m == 4 & t > i) | (m == 10 & t <= i),
             Period = ifelse(p, "Summer", "Winter")) %>%
      filter(m == ex_m) %>%
      select(TARGET, w1, Period) %>% mutate(Sep = i)
    plot_data <- rbind(plot_data, add)
  }
  ggplot(data=plot_data, aes(x=w1, y=TARGET, color=Period)) +
    geom_point(alpha=0.25) +
    facet_wrap(~Sep) +
    ggtitle("Different separation days for October")
  ggsave("LoadSepOctober.png", path="plots/ForThesis/", width=18,
         height=8)

  d %>% mutate(Season = getSumWin(d)) %>%
    ggplot(aes(x=w10, y=TARGET, color=Season)) +
    facet_wrap(~Season) +
    geom_point(alpha=0.5)

  # EXAMINE LOAD DIFFERENCES BETWEEN MONTHS AND WEEKDAYS =======================
  byMonth <- d %>% mutate(Month = getMonths(d, label=TRUE),
                          Hour = hour(TIMESTAMP)) %>%
    group_by(Month, Hour) %>% summarise(Mean=mean(TARGET), .groups="drop") %>%
    ggplot(aes(x=Hour, y=Mean, color=Month)) +
    geom_line(size=1.0) +
    xlab("Hour") +
    ylab("Load") +
    ggtitle("Mean Load by Hour, Month and Weekday") +
    scale_color_discrete() +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13),
          legend.position="bottom") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
  byWday <- d %>% mutate(Weekday = getWday(d, label=TRUE),
                         Hour = hour(TIMESTAMP)) %>%
    group_by(Weekday, Hour) %>% summarise(Mean=mean(TARGET), .groups="drop") %>%
    ggplot(aes(x=Hour, y=Mean, color=Weekday)) +
    geom_line(size=1.0) +
    xlab("Hour") +
    ylab("") +
    ggtitle("") +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13),
          legend.position="bottom") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))

  grid.arrange(byMonth, byWday, nrow=1)
  ggsave("LoadDev.pdf", path="plots/ForThesis/", width=11.69, height=5)

  # EXAMINE TEMPERATURE DEVELOPMENT OVER THE YEARS =============================
  getConfidence <- function(x) {
    intervals <- c("100%", "80%", "60%", "40%", "20%")
    lowerVals <- c(min(x), unname(quantile(x, probs=1:4 * 0.1)))
    upperVals <- c(max(x), unname(quantile(x, probs=9:6 * 0.1)))
    return(data.frame(Width=intervals, L=lowerVals, U=upperVals))
  }

  plot_data <- loadLoad(4)$Zone1$Train %>%
    mutate(Time = paste(month(TIMESTAMP), year(TIMESTAMP), sep="."),
           x = month(TIMESTAMP) + 12 * (year(TIMESTAMP) - 2001))

  n <- max(plot_data$x)
  ticks <- seq(1, n, (n - 1) / 6)
  labels <- unique(plot_data$Time)[ticks]

  plot_data <- plot_data %>% group_by(x)
  meanAndMedian <-
    summarise(plot_data, Mean=mean(w25), Median=median(w25), .groups="drop") %>%
    pivot_longer(cols = -x, names_to="Curve")
  c_intervals <- summarise(plot_data, getConfidence(w25), .groups="drop")

  ggplot(mapping = aes(x=x)) +
    geom_ribbon(data = c_intervals, mapping=aes(ymin=L, ymax=U, group=Width),
                alpha = 0.15) +
    geom_line(data = meanAndMedian, mapping = aes(y=value, color=Curve),
              size=1.1) +
    ylab("Temperature") +
    scale_x_continuous(breaks = ticks, labels = labels, name = "") +
    ggtitle("Temperature Summary Statistics (w25)") +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13),
          legend.justification=c(1,0), legend.position=c(1,0)) +
     guides(color = guide_legend(nrow = 1, byrow = TRUE, title="",
                                 title.position="left"))
  ggsave("LoadTempDev.pdf", path="plots/ForThesis/", width=11.69,
         height=4)

  # ANALYZE SCORES RETURNED BY simulateCompetition (stored in tmp) =============
  tmp <- evaluation(unleashLoaIDR, pinBallLoss, c(100, 1, 22),
                    preprocessfct=meanTemp, tune=TRUE, ret=TRUE)
  plot_data <- data.frame()
  for(i in 1:length(tmp)) {
    plot_data <- rbind(plot_data, cbind(tmp[[i]], Task=i, x=1:nrow(tmp[[i]])))
  }

  group_fct <- day
  name <- "Day"

  plot_data %>% mutate(Group = group_fct(TIMESTAMP)) %>%
    group_by(Task, Group) %>%
    summarise(Mean_score = mean(SCORE), .groups="drop") %>%
    ggplot(aes(x=Group, y=Mean_score, color=factor(Task))) +
    geom_line() +
    ylab("Mean score") +
    xlab(name)+
    ggtitle("Mean Pinball Score In Initial Tuning Phase") +
    scale_color_discrete(name="Month", labels=c("Oct", "Nov", "Dec")) +
    theme_bw()
  ggsave(paste0("LoadScoreBy", name, "2.png"), path="plots/ForThesis/", width=18,
         height=8)
  n <- nrow(plot_data)
  ticks <- as.integer(seq(1, n, (n - 1) / 4))
  labels <- paste0(month(plot_data$TIMESTAMP[ticks], label=TRUE),
                   year(plot_data$TIMESTAMP[ticks]))
  plot_data %>%
    mutate(x = (0:(n-1)) %/% 24,
           hour=factor(hour(TIMESTAMP), ordered=TRUE,levels=c(1:23, 0))) %>%
    ggplot() +
    geom_tile(mapping = aes(x=x, y=hour, fill=SCORE)) +
    ggtitle("Electricity Price During Initial Tuning Phase") +
    ylab("Hour") +
    scale_x_continuous(breaks = (ticks-1) %/% 24, labels = labels, name = "") +
    scale_fill_gradient(low="darkorchid4", high="coral", name = "Score") +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13))

  # ANAlYZE TEMPERATURE FORECAST DEVIATIONS ====================================
  task <- 1
  curr_test <- loadLoad(task)
  curr_test$Zone1$Train <- filter(curr_test$Zone1$Train, !is.na(TARGET))
  curr_test <- meanTemp(curr_test)$Zone1$Test
  #curr_test <- meanTemp(loadLoad(task))$Zone1$Test
  last <- curr_test$TIMESTAMP[1]
  pred <- curr_test[cols]
  true <- loadLoad(task + 1)$Zone1$Train %>% filter(TIMESTAMP >= last) %>%
    select(all_of(cols))

  #colMeans(abs(true - pred))
  squ_error <- colMeans((true - pred)^2)
  mean(squ_error)

  # vals with all data 77.67 77.39 168.89

  # INITAIL TRIES ===============================================================
  scores <- read.csv2("../LoadBestSimpleModels.csv") %>%
    mutate(WS = substring(WS, 2))
  col_names <-c("simple"="w*", "nn"="w*,-w*", "seas2"="Summer,Winter",
                "Na1"="1", "squ"="squared diff", "abs"="absolute diff",
                "Na2"="2", "Med"="Median", "MaxMinBin"="Min-max-bin",
                "squMean"="Mean", "Na3"="3", "Mean"="Mean temp",
                "LinMean"="Lin-weigh-mean temp", "Last"="Last temp")

  sorted <- arrange(scores, Mean)
  sorted %>% mutate(WS=factor(WS, ordered=TRUE, levels=sorted[["WS"]])) %>%
    pivot_longer(cols=-WS, names_to="y", values_to="Score") %>%
    mutate(y=factor(col_names[y], ordered=TRUE, levels=unname(col_names))) %>%
    ggplot(aes(x=WS, y=y, fill=Score)) +
    geom_tile() +
    xlab("Weather station") +
    ylab("") +
    scale_y_discrete(breaks=unname(col_names)[c(1:3, 5:6, 8:10, 12:14)]) +
    ggtitle("Mean Pinball Score (Initial Tuning Phase)") +
    theme_bw()  +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13))
  ggsave("LoadFirstScores.pdf",
             path="plots/ForThesis/", width=11.69, height=4.5)
}