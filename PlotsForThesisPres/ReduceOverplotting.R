library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

source("preprocess.R")
source("plot.R")
source("loadData.R")
source("util.R")

# since geom_point leads despite low alpha to overplotting, use
# geom_bin2d or geom_hex for plotting of scatter plots


solarPlots <- function() {
  data <- deaccuInvertSol(loadSolar(4))
  zone <- "Zone1"
  data[[zone]]$Train %>%
      mutate(across(.cols = c(-TARGET, -TIMESTAMP),
                    .fns = function(x) (x-min(x))/(max(x)-min(x))),
      Hour = factor(getHours(data[[zone]]$Train) - 1)) %>%
      select(-ZONEID, -TIMESTAMP) %>%
      pivot_longer(cols = c(-TARGET, -Hour), names_to = "ECMWF") %>%
      mutate(ECMWF = solarVars[ECMWF]) %>%
      ggplot(aes(x=value, y=TARGET)) +
      facet_wrap(~ECMWF) +
      geom_bin2d(aes(fill = Hour), alpha = 0.5, bins=60) +
      ylab("Solar power production") +
      xlab("ECMWF weather forecast") +
      ggtitle("Binned Scatter Plot of Solar Track Colored by Hour") +
      theme_bw()  +
      theme(legend.position = "bottom", text = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      scale_x_continuous(breaks = 0:4 * 0.25,
                         labels = c("0", "0.25", "0.5", "0.75", "1")) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))
    ggsave("SolarScatterAll_Zone1_binned.pdf", path="plots/ForThesis/",
           width=11.69, height=8)
}

windPlots <- function() {
  data <-getWindAttributes(loadWind(4))
  var_order <- windVars[c("A10", "S10", "U10", "V10", "A100", "S100", "U100",
                          "V100")]

  bin8 <- function(angle) {
    levels <- c("[-180,-135]", "(-135,-90]", "(-90,-45]", "(-45,0]", "(0,45]",
                "(45,90]", "(90,135]", "(135,180]")
    out <- case_when(
      angle >= -180 & angle <= -135 ~ 1,
      angle > -135 & angle <= -90 ~ 2,
      angle > -90 & angle <= -45 ~ 3,
      angle > -45 & angle <= 0 ~ 4,
      angle > 0 & angle <= 45 ~ 5,
      angle > 45 & angle <= 90 ~ 6,
      angle > 90 & angle <= 135 ~ 7,
      angle > 135 & angle <= 180 ~ 8,
    )
    return(factor(levels[out], ordered = TRUE, levels=levels))
  }

  # 1st plot -----------------------------------------------------------
  data[["Zone1"]]$Train %>% select(-ZONEID, -TIMESTAMP, -SX) %>%
    mutate(A100_bin = bin8(A100)) %>%
    mutate(across(.cols = c(-TARGET, -A100_bin),
                  .fns = function(x) (x-min(x))/(max(x)-min(x)))) %>%
    pivot_longer(cols = c(-TARGET, -A100_bin), names_to = "ECMWF") %>%
    mutate(ECMWF=factor(windVars[ECMWF], ordered=TRUE, levels=var_order)) %>%
    ggplot(aes(x=value, y=TARGET, fill=A100_bin)) +
    facet_wrap(~ECMWF, nrow=2) +
    geom_bin2d(alpha = 0.33, bins = 80) +
    ylab("Wind power production") +
    xlab("ECMWF weather forecast") +
    ggtitle("Binned Scatter Plot of Wind Track Colored by Wind Angle") +
    theme_bw()  +
    theme(legend.position = "bottom", text = element_text(size = 16),
          axis.text = element_text(size = 13)) +
    scale_x_continuous(breaks = 0:4 * 0.25,
                       labels = c("0", "0.25", "0.5", "0.75", "1")) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_fill_discrete(name = "Wind angle")
  ggsave(paste0("WindScatterAll_Zone1_binned.pdf"), path="plots/ForThesis/",
         width=11.69, height=6)

  # 2nd plot -------------------------------------------------------
  compare10and100 <- data.frame()
  for(zone in c("Zone2", "Zone4", "Zone6", "Zone8")) {
    add10 <- data[[zone]]$Train %>%
      mutate(S10 = (S10 - min(S10)) / (max(S10) - min(S10)),
             Angle = bin8(A100)) %>%
      select(ZONEID, TARGET, S10, Angle)
    colnames(add10)[colnames(add10) == "S10"] <- "Speed"
    add10 <- cbind(add10, Height=10)
    add100 <- data[[zone]]$Train %>%
      mutate(S100 = (S100 - min(S100)) / (max(S100) - min(S100)),
             Angle = bin8(A100)) %>%
      select(ZONEID, TARGET, S100, Angle)
    colnames(add100)[colnames(add100) == "S100"] <- "Speed"
    add100 <- cbind(add100, Height=100)
    compare10and100 <- rbind(compare10and100, add10, add100)
  }
  compare10and100 %>%
    mutate(ZONEID = paste("Zone", ZONEID),
           Height = paste0("Height ", Height, "m")) %>%
    ggplot(aes(x=Speed, y=TARGET, fill=Angle)) +
    geom_bin2d(alpha = 0.33, bins=80) +
    facet_grid(rows = vars(Height), cols = vars(ZONEID)) +
    ylab("Wind power production") +
    xlab("Wind speed") +
    ggtitle("Wind Power vs. Wind Speed") +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 16),
          axis.text = element_text(size = 13)) +
    scale_x_continuous(breaks = 0:4 * 0.25,
                         labels = c("0", "0.25", "0.5", "0.75", "1")) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_fill_discrete(name = "Wind angle")
  ggsave("WindScatterSpeed2_binned.pdf",
           path="plots/ForThesis/", width=11.69, height=6)
  
  # 3rd plot --------------------------------------------------
  zone <- "Zone3"
  hour_data <- data[[zone]]$Train %>%
    mutate(Group = hour(TIMESTAMP)) %>% select(Group, TARGET, S100) %>%
    filter(Group %in% c(0,6,12,18)) %>% mutate(Group = paste("Hour", Group))
  month_data <- data[[zone]]$Train %>%
    mutate(Group = getMonths(data[[zone]]$Train, label=TRUE)) %>%
    select(Group, TARGET, S100) %>%
    filter(Group %in% c("Jan", "Apr", "Jul", "Oct"))
  season_data <- data[[zone]]$Train %>%
    mutate(Group = get4Seasons(data[[zone]]$Train, label=TRUE)) %>%
    select(Group, TARGET, S100)
  angle_labels <- c("[-180,-135]", "(-90,-45]", "(0,45]", "(90,135]")
  angle_data <- data[[zone]]$Train %>%
    mutate(Group = bin8(A100)) %>% select(Group, TARGET, S100) %>%
    filter(Group %in% angle_labels) %>% mutate(Group = paste("Angle", Group))
  order <- c(paste("Hour", c(0, 6, 12, 18)), "Jan", "Apr", "Jul",
             "Oct", "Dec,Jan,Feb", "Mar,Apr,May", "Jun,Jul,Aug",
             "Sep,Oct,Nov", paste("Angle", angle_labels))
  rbind(hour_data, month_data, season_data, angle_data) %>%
    mutate(Group = factor(Group, ordered=TRUE, levels=order)) %>%
    ggplot(aes(x=S100, y=TARGET)) +
    geom_bin2d(binwidth=c(10 / 30, 1 / 30)) +
    scale_fill_gradient(low="black", high="#fcbf38", name="Count") +
    facet_wrap(~Group, nrow=4) +
    ylab("Wind power production") +
    xlab("Wind speed at 100m") +
    ggtitle("Wind Power vs. Wind Speed Facetted") +
    theme_bw()  +
    theme(text = element_text(size = 16),
          axis.text = element_text(size = 13))
  ggsave("WindScatterS100_Zone3.pdf", path="plots/ForThesis/", width=11.69,
         height=7.5)
}

pricePlots <- function() {
  data <- loadPrice(4)$Zone1$Train %>% rename(Price = TARGET)

  # 1st plot --------------------------------------------------
  # scatter plot with histogram
  scatter <- data %>% mutate(Season = get4Seasons(data, label=TRUE)) %>%
    select(-ZONEID, -TIMESTAMP) %>%
    pivot_longer(cols = c(-Price, -Season), names_to = "Var") %>%
    mutate(Var = priceVars[Var]) %>%
    ggplot(aes(x=value, y=Price, fill=Season)) +
    geom_bin2d(alpha = 0.6, show.legend = FALSE, bins=125) +
    facet_wrap(~Var, nrow=1, scales="free_x") +
    ylab("Electricity price") +
    xlab("Forecasted load") +
    ggtitle("Overview Price Track") +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13))
  histogram <- data %>%
    mutate(Season = get4Seasons(data, label=TRUE), Type="price histogram") %>%
    select(Price, Season, Type) %>%
    ggplot(aes(y=Price, fill=Season)) +
    geom_histogram(bins=45) +
    ylab("") +
    xlab("Frequency") +
    scale_y_continuous(labels = NULL) +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13),
          legend.justification=c(1,1), legend.position=c(0.99,0.99)) +
    facet_wrap(~Type) +
    ggtitle("")
  grid.arrange(scatter, histogram, nrow=1, widths = c(5,2))
  #ggsave("PriceOverview.png", path="plots/ForThesis/", width=11.69, height=5)

  # 2nd plot --------------------------------------------------------
  d <- mutate(data, Month = getMonths(data, label=TRUE)) %>%
    mutate(across(.cols = c(names(priceVars), Price),
                  .fns = function(x) (x - min(x)) / (max(x)-min(x))))
  ftl_cor <- d %>% group_by(Month) %>%
    summarise(Spearman_cor =  round(cor(Price, Forecasted.Total.Load,
                                        method = "spearman"), digits=3),
              .groups = "drop") %>%
    mutate(Price = 0.94, Forecasted.Total.Load = 0.21)
  fzl_cor <- d %>% group_by(Month) %>%
    summarise(Spearman_cor = round(cor(Price, Forecasted.Zonal.Load,
                                       method = "spearman"), digits=3),
              .groups = "drop") %>%
    mutate(Price = 0.94, Forecasted.Zonal.Load = 0.21)

  # scatter plott by month
  ftl_plot <- ggplot(mapping = aes(x=Forecasted.Total.Load, y=Price)) +
    geom_bin2d(data = d, show.legend=FALSE, bins=70) +
    geom_label(data = ftl_cor, aes(label = Spearman_cor), show.legend=FALSE,
               size=5.8) +
    facet_wrap(~Month, nrow=4) +
    xlab("Forecasted total load") +
    ylab("Electricity price") +
    scale_x_continuous(breaks = 0:4 * 0.25,
                      labels = c("0", "0.25", "0.5", "0.75", "1")) +
    scale_fill_gradient(low="black", high="#fcbf38") +
    ggtitle("Electricity Price vs. Forecasted Load") +
    theme_bw() +
    theme(text = element_text(size = 18), axis.text = element_text(size = 13))
  fzl_plot <- ggplot(mapping = aes(x=Forecasted.Zonal.Load, y=Price)) +
    geom_bin2d(data = d, bins=70) +
    geom_label(data = fzl_cor, aes(label = Spearman_cor), show.legend=FALSE,
               size=5.8) +
    facet_wrap(~Month, nrow=4) +
    xlab("Forecasted zonal load") +
    ylab("") +
    scale_x_continuous(breaks = 0:4 * 0.25,
                      labels = c("0", "0.25", "0.5", "0.75", "1")) +
    scale_fill_gradient(low="black", high="#fcbf38", name="Count") +
    ggtitle("") +
    scale_y_continuous(labels = NULL) +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13))
  grid.arrange(ftl_plot, fzl_plot, nrow=1, widths=c(0.49, 0.51))
  #ggsave("PriceOverview.png", path="plots/ForThesis/", width=11.69, height=8.95)
}

loadPlots <- function() {
  cols <- paste0("w", 1:25)
  d <- loadLoad(4)$Zone1$Train %>% filter(!is.na(TARGET))

  # 1st plot ----------------------------------------------------
  mid_fnc <- list("median" = getDiffMed,
                  "cond-median" = getDiffMedS,
                  "mean"=getDiffMean,
                  "weigh-mean" = getDiffMeanS,
                  "min-max-bin" = getDiffMax)

  mid_points <- summarise(d,
                          across(all_of(cols), .fns = mid_fnc,
                                 .names = "{.col}_{.fn}",
                                 TARGET)) %>%
    pivot_longer(cols = everything(), names_to=c("WeatherStation", "MidPoint"), names_sep = "_")
  point_clouds <- select(d, all_of(cols), TARGET) %>%
    pivot_longer(cols = -TARGET, names_to="WeatherStation")

  station <- "w10"
  low10 <- quantile(filter(point_clouds, WeatherStation == station)$TARGET,
                    probs=0.1)
  point_cloud <- ggplot() +
    geom_bin2d(data=filter(point_clouds, WeatherStation == station),
               aes(x=value, y=TARGET), bins=70) +
    scale_fill_gradient(low="black", high="#fcbf38", name="Count") +
    geom_vline(data=filter(mid_points, WeatherStation == station),
               aes(xintercept=value, color=MidPoint), size=1.1) +
    geom_hline(yintercept = low10, color="gray", linetype=2, size=1.2) +
    theme_bw() +
    ylab("Load") +
    xlab("Temperature") +
    ggtitle("Temperature Deviations (w10)") +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13)) +
    guides(color = guide_legend(title=NULL))

  plot_data <- data.frame()
  for (fnc_m in c("cond-median", "median")) {
    mid <- mid_fnc[[fnc_m]](d$w10, d$TARGET)
    add_df <- data.frame(Load=d$TARGET, T=abs(d$w10 - mid),
                         Lab=fnc_m,
                         Col=ifelse(d$w10 > mid, "Right arm", "Left arm"))
    plot_data <- rbind(plot_data, add_df)
  }
  preprocessed <- ggplot(plot_data, aes(x=T, y=Load, fill=Col)) +
    geom_bin2d(alpha=0.4, bins=70) +
    facet_wrap(~Lab, nrow=1, scales="free_x") +
    ylab("") +
    xlab("Absolute temperature deviation") +
    ggtitle("") +
    scale_fill_brewer(palette="Dark2") +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13),
          legend.justification=c(1,0), legend.position=c(0.99,0.01)) +
    guides(fill = guide_legend(title=NULL))
  grid.arrange(point_cloud, preprocessed, nrow=1, widths=c(0.55, 0.45))
  ggsave("LoadDist2.pdf", path="plots/ForThesis/", width=11.69,
         height=4.2)

  # 2nd plot ------------------------------------------------------
  d %>% select(TIMESTAMP, TARGET, w1) %>%
    mutate(Month = getMonths(d, label=TRUE),
           t = day(TIMESTAMP), m = month(TIMESTAMP),
           p = m %in% 5:9 | (m == 4 & t > 16) | (m == 10 & t <= 14),
           Period = ifelse(p, "17.04 - 14.10", "15.10 - 16.04")) %>%
    select(Month, TARGET, w1, Period) %>%
    ggplot(aes(x=w1, y=TARGET, fill=Period)) +
    geom_bin2d(alpha=0.5, bins=90) +
    facet_wrap(~Month) +
    xlab("Temperature") +
    ylab("Load") +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 16),
            axis.text = element_text(size = 13)) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    ggtitle("Load vs. Temperature (w1)")
  ggsave("LoadScatterw1_binned.pdf", path="plots/ForThesis/", width=11.69,
         height=6.2)
}