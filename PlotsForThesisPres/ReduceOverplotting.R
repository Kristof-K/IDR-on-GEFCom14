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

  data[[zone]]$Train %>%
      mutate(across(.cols = c(-TARGET, -TIMESTAMP),
                    .fns = function(x) (x-min(x))/(max(x)-min(x))),
      Hour = factor(getHours(data[[zone]]$Train) - 1)) %>%
      select(-ZONEID, -TIMESTAMP) %>%
      pivot_longer(cols = c(-TARGET, -Hour), names_to = "ECMWF") %>%
      mutate(ECMWF = solarVars[ECMWF]) %>%
      ggplot(aes(x=value, y=TARGET)) +
      facet_wrap(~ECMWF) +
      geom_bin2d(aes(fill = Hour), alpha = 0.25) +
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

  data[["Zone1"]]$Train %>% select(-ZONEID, -TIMESTAMP, -SX) %>%
    mutate(A100_bin = bin8(A100)) %>%
    mutate(across(.cols = c(-TARGET, -A100_bin),
                  .fns = function(x) (x-min(x))/(max(x)-min(x)))) %>%
    pivot_longer(cols = c(-TARGET, -A100_bin), names_to = "ECMWF") %>%
    mutate(ECMWF=factor(windVars[ECMWF], ordered=TRUE, levels=var_order)) %>%
    ggplot(aes(x=value, y=TARGET, fill=A100_bin)) +
    facet_wrap(~ECMWF, nrow=2) +
    geom_bin2d(alpha = 0.25, bins = 50) +
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
    geom_bin2d(alpha = 0.25, bins=50) +
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
    scale_fill_discrete(name = groupName)
  ggsave("WindScatterSpeed2_binned.pdf",
           path="plots/ForThesis/", width=11.69, height=6)
}

pricePlots <- function() {

}

loadPlots <- function() {

}