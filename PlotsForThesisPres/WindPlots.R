library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

source("preprocess.R")
source("plot.R")
source("loadData.R")
source("util.R")

# create plots of the wind track for thesis or presentation

plotsForThesisWind <- function() {
  data <-getWindAttributes(loadWind(4))
  groupName <- "Wind angle"
  target <- "Wind power production"

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

  for (zone in data$Zones) {
    # scatter all variables
    data[[zone]]$Train %>% select(-ZONEID, -TIMESTAMP) %>%
      mutate(A100_bin = bin8(A100)) %>%
      mutate(across(.cols = c(-TARGET, -A100_bin),
                    .fns = function(x) (x-min(x))/(max(x)-min(x)))) %>%
      pivot_longer(cols = c(-TARGET, -A100_bin), names_to = "ECMWF") %>%
      mutate(ECMWF=factor(windVars[ECMWF], ordered=TRUE, levels=var_order)) %>%
      ggplot(aes(x=value, y=TARGET, color=A100_bin)) +
      facet_wrap(~ECMWF, nrow=2) +
      geom_point(alpha = 0.5) +
      ylab(target) +
      xlab("ECMWF weather forecast") +
      ggtitle("Scatter Plot of Wind Track Colored by Wind Angle") +
      theme_bw()  +
      theme(legend.position = "bottom", text = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      scale_x_continuous(breaks = 0:4 * 0.25,
                         labels = c("0", "0.25", "0.5", "0.75", "1")) +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_color_discrete(name = groupName) +
      ggsave(paste0("WindScatterAll_", zone, ".pdf"),
             path="plots/ForThesis/", width=11.69, height=6)
    # assemble facetted plot for four main categories
    hour_data <- data[[zone]]$Train %>%
      mutate(Group = hour(TIMESTAMP), Angle = bin8(A100)) %>%
      select(Group, TARGET, S100, Angle) %>% filter(Group %in% c(0,6,12,18)) %>%
      mutate(Group = paste("Hour", Group))
    month_data <- data[[zone]]$Train %>%
      mutate(Group = getMonths(data[[zone]]$Train, label=TRUE),
             Angle = bin8(A100)) %>%
      select(Group, TARGET, S100, Angle) %>%
      filter(Group %in% c("Jan", "Apr", "Jul", "Oct"))
    season_data <- data[[zone]]$Train %>%
      mutate(Group = get4Seasons(data[[zone]]$Train, label=TRUE),
             Angle = bin8(A100)) %>%
      select(Group, TARGET, S100, Angle)
    angle_labels <- c("[-180,-135]", "(-90,-45]", "(0,45]", "(90,135]")
    angle_data <- data[[zone]]$Train %>%
      mutate(Group = bin8(A100), Angle = bin8(A100)) %>%
      select(Group, TARGET, S100, Angle) %>% filter(Group %in% angle_labels) %>%
      mutate(Group = paste("Angle", Group))
    order <- c(paste("Hour", c(0, 6, 12, 18)), "Jan", "Apr", "Jul",
               "Oct", "Dez,Jan,Feb", "Mar,Apr,May", "Jun,Jul,Aug",
               "Sep,Oct,Nov", paste("Angle", angle_labels))
    rbind(hour_data, month_data, season_data, angle_data) %>%
      mutate(Group = factor(Group, ordered=TRUE, levels=order)) %>%
      ggplot(aes(x=S100, y=TARGET)) +
      geom_point(alpha=0.5) +
      facet_wrap(~Group, nrow=4) +
      ylab("Wind power production") +
      xlab("Wind speed at 100m") +
      ggtitle("Wind Power vs. Wind Speed Facetted") +
      theme_bw()  +
      theme(text = element_text(size = 16),
            axis.text = element_text(size = 13)) +
       ggsave(paste0("WindScatterS100_", zone, ".pdf"),
             path="plots/ForThesis/", width=11.69, height=7.5)
    data[[zone]]$Train %>% mutate(Angle = bin8(A100)) %>%
      ggplot(aes(x=S100, y=TARGET, color=Angle)) +
      facet_wrap(~Angle, nrow=2) +
      geom_point(alpha=0.5, show.legend=FALSE) +
      ylab("Wind power production") +
      xlab("Wind speed at 100m height") +
      ggtitle(paste("Wind Power vs. Wind Speed for", zone)) +
      ggsave(paste0("WindScatter_ByAngle_", zone, ".png"),
             path="plots/ForThesis/", width=11.69, height=7.5)

  }
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
    ggplot(aes(x=Speed, y=TARGET, color=Angle)) +
    geom_point(alpha = 0.5) +
    facet_grid(rows = vars(Height), cols = vars(ZONEID)) +
    ylab("Wind power production") +
    xlab("Wind speed") +
    ggtitle("Wind Power vs. Wind Speed") +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 16),
          axis.text = element_text(size = 13)) +
    scale_x_continuous(breaks = 0:4 * 0.25,
                         labels = c("0", "0.25", "0.5", "0.75", "1")) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_color_discrete(name = groupName)
  ggsave("WindScatterSpeed2.pdf",
           path="plots/ForThesis/", width=11.69, height=6)
  # look at linear combinations of height of wind speed
  corr <- data.frame()
  for (zone in data$Zones) {
    d <- data[[zone]]$Train
    for (alpha in seq(0, 1.5, 1/18)) {
      height <- 10 * (1 - alpha) + 100 * alpha
      c <- cor(d$TARGET, d$S10 * (1 - alpha) + d$S100 * alpha,
               method = "spearman")
      corr <- rbind(corr, data.frame("Zone"=zone, "Correlation"=c,
                                     "Height"=height))
    }
  }
  top3 <- corr %>% group_by(Zone) %>%
    summarise(Height, Correlation = 29 - rank(Correlation),
              .groups = "drop") %>%
    filter(Correlation <= 3)
  ggplot(mapping = aes(x = factor(Zone, ordered=TRUE,
                                  levels=paste0("Zone", 1:10)),
                       y = Height)) +
    geom_tile(data = corr, aes(fill = Correlation)) +
    geom_text(data = top3, aes(label = Correlation)) +
    scale_fill_gradient(low="darkorchid4", high="coral", name = "Correlation") +
    xlab("Zone") +
    ylab("Height [m]") +
    ggtitle(paste("Correlation: Wind Power and Wind Speed",
                  "at Different Heights")) +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13))
  ggsave("WindCorrelations.pdf",
             path="plots/ForThesis/", width=11.69, height=5)
  # use subagging to get more reliable estimates for the height of the optimal
  # wind speed
  n <- 70
  frac <- 0.7

  getMaxByZone <- function() {
    max_height <- matrix(0, nrow=2, ncol=10)
    colnames(max_height) <- paste0("Zone", 1:10)
    rownames(max_height) <- c("MaxCorr", "Height")

    for (zone in data$Zones) {
      d <- data[[zone]]$Train %>% slice_sample(prop = frac, replace = TRUE)
      for (alpha in seq(0, 1.5, 1/90)) {
        height <- 10 * (1 - alpha) + 100 * alpha
        c <- cor(d$TARGET, d$S10 * (1 - alpha) + d$S100 * alpha,
                 method = "spearman")
        if (c > max_height["MaxCorr", zone]) {
          max_height[, zone] <- c(c, height)
        }
      }
    }
    return(max_height)
  }

  max_vals <- matrix(0, nrow=2, ncol=10)
  for (i in 1:n) {
    max_vals <- max_vals + getMaxByZone()
  }
  max_vals / n
}