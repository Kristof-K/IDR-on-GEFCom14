source("loadData.R")
source("util.R")
source("plot.R")
source("preprocess.R")

# ==============================================================================
# Plots for slide and thesis
# ==============================================================================

plotsForSlides <- function() {
  data_all <- deaccumulateSol(loadSolar(15))

  for(zone in "Zone3") {
    data <- rbind(data_all[[zone]]$Train, data_all[[zone]]$Test)
    plotData <- transmute(data, Power=TARGET, Radiation=VAR169,
                          Hour=as.factor(hour(TIMESTAMP)))
    print(ggplot(plotData, aes(x=Radiation, y=Power)) +
      geom_point() +
      scale_color_manual(values = rainbow(24)) +
      theme_bw()+
      ggtitle("Scatterplot: Power ~ Solar radiation downwards") +
      ggsave("VAR169.pdf", path="../WorkshopIAI/"))

    print(ggplot(plotData, aes(x=Radiation, y=Power, color=Hour)) +
      geom_point() +
      scale_color_manual(values = rainbow(24)) +
      theme_bw() +
      ggtitle("Scatterplot: Power ~ Solar radiation downwards") +
      ggsave("VAR169Col.pdf", path="../WorkshopIAI/"))

    print(ggplot(plotData, aes(x=Radiation, y=Power, color=Hour)) +
      geom_point() +
      facet_wrap(~Hour) +
      scale_color_manual(values = rainbow(24)) +
      theme_bw() +
      ggtitle("Scatterplot: Power ~ Solar radiation downwards"))
  }

  d <- loadLoad(15)
  d$Zone1$Train %>% select(TIMESTAMP, TARGET, w1) %>% filter(!is.na(TARGET)) %>%
    mutate(Month = month(TIMESTAMP, label=TRUE),
           t = day(TIMESTAMP), m = month(TIMESTAMP),
           p = m %in% 5:9 | (m == 4 & t > 18) | (m == 10 & t <= 18),
           Period = ifelse(p, "19.04 - 18.10", "19.10 - 18.04")) %>%
    select(Month, TARGET, w1, Period) %>%
    ggplot(aes(x=w1, y=TARGET, color=Period)) +
    geom_point(alpha=0.25) +
    facet_wrap(~Month) +
    xlab("Temperature") +
    ylab("Load") +
    theme_bw() +
    ggtitle("Scatterplot: Load ~ Temperature") +
    ggsave("LoadTemp.pdf", path="../Overview", width=10, height=6)

  S <- data.frame(score=c(0.012132, 0.012247, 0.012785, 0.013342, 0.014166,
                          0.014288, 0.014294, 0.0149964, 0.0154773, 0.01549,
                          0.015977, 0.0165608, 0.0166733, 0.01755, 0.0178692,
                          0.0184789, 0.0208583, 0.02128, 0.025524, 0.0317609,
                          0.0375917, 0.01995319, 0.014839771, 0.013732111),
                  group=c(rep("par", 20), "ben", rep("idr", 3)),
                  name=c(paste0(1:20, c("st", "nd", "rd", rep("th", 17))),
                         "Benchmark", "IDR", "IDR hours 1", "IDR hours 5"),
                  label=c(rep("", 20), "", "IDR on\nradiation",
                          "IDR on\nradiation\ngrouped\nby hours",
                          "5 variables\n+ hour groups\n+ subagging")) %>%
    arrange(desc(score)) %>%
    mutate(name = factor(name, ordered=TRUE, levels=name),
           index=1:length(score))

  W <- data.frame(score=c(0.03711091, 0.03827364, 0.03831, 0.03834417,
                          0.0389725, 0.03946167, 0.04149818, 0.04473917,
                          0.04532083, 0.04657583, 0.0629325, 0.06853833,
                          0.08670583, 0.04476315, 0.04505536, 0.044895936,
                          0.04307092),
                  group=c(rep("par", 12), "ben", rep("idr", 4)),
                  name=c(paste0(1:12, c("st", "nd", "rd", rep("th", 9))),
                         "Benchmark", "IDR", "IDR by hours", "IDR by seasons",
                         "IDR by wind angle"),
                  label=c(rep("", 12), "",
                          "IDR on\nwind speed at 100m\n+ different grouping",
                          rep("", 3))) %>%
    arrange(desc(score)) %>%
    mutate(name = factor(name, ordered=TRUE, levels=name),
           index=1:length(score))

  P <- data.frame(score=c(2.72260416, 2.73264, 2.82480333, 3.3393825,
                          3.38856916, 3.67968, 4.15617545, 4.46310583,
                          4.48946636, 5.08586916, 5.20363454, 5.2952075,
                          7.7270825, 8.86687636, 10.58733333, 19.46707916,
                          5.219633, 4.051974, 4.027405),
                  group=c(rep("par", 15), "ben", rep("idr", 3)),
                  name=c(paste0(1:15, c("st", "nd", "rd", rep("th", 12))),
                         "Benchmark", "IDR", "seasons", "seasons+\nhours*"),
                  label=c(rep("", 17),
                          "IDR on\n1 variable\n+ different grouping",
                          rep("", 1))) %>%
    arrange(desc(score)) %>%
    mutate(name = factor(name, ordered=TRUE, levels=name),
           index=1:length(score))

  L <- data.frame(score=c(7.197126, 7.23131727, 7.45392166, 7.50520916,
                          7.535513, 7.95646833, 8.466394, 8.58871,
                          8.62505818, 8.93657583, 9.26274090, 9.407766,
                          9.48056416, 9.5641, 10.00587875, 10.25994833,
                          10.62373454, 11.50976555, 11.76980833, 12.17955666,
                          13.32187909, 15.5810025, 10.61661, 8.610069),
                  group=c(rep("par", 21), "ben", rep("idr", 2)),
                  name=c(paste0(1:21, c("st", "nd", "rd", rep("th", 18))),
                         "Benchmark", "IDR", "seasons+hours*"),
                  label=c(rep("", 22), "",
                          "IDR on 1 variable,\nmean temp.\nas forecast")) %>%
    arrange(desc(score)) %>% mutate(index=1:length(score))

  plotResults <- function(data, track, f=0.5) {
    ggplot(data) +
      geom_col(aes(x=factor(index), y=score, fill=group), show.legend=FALSE,
              position="dodge2") +
      xlab("") +
      ylab("") +
      ggtitle(paste("Mean pinball scores", track, "track")) +
      theme_bw() +
      theme(text = element_text(size = 15),
            axis.text.x = element_blank()) +
      geom_text(aes(x=index, y=score + f * score * (score <= max(score)*1/(f*3)),
                    label=label, color=group), size=5, show.legend=FALSE) +
      geom_text(aes(x=index, y=score/2, label=name), angle=90) +
      ggsave(paste0("Scores_", track, ".pdf"), path="../Overview",
             width=10, height=5)
  }
  plotResults(S, "solar")
  plotResults(W, "wind")
  plotResults(P, "price", f=1.5)
  plotResults(L, "load")
}

plotsForThesisSolar <- function() {
  # prevent labels from overlapping (just leave labels out)
  # scale_x_continuous(guide = guide_axis(check.overlap = TRUE))+
  # use scientific labels
  # scale_x_continuous(labels = function(x) format(x, scientific=TRUE)
  data <- deaccumulateSol(loadSolar(3))
  var <- "VAR169"
  groupName <- "Hour"
  groupfct <- function(d) getHours(d) - 1
  nrow <- 4
  target <- "Solar power production"

  for (zone in data$Zones) {
    # scatter all variables
    data[[zone]]$Train %>% select(-ZONEID) %>%
      mutate(across(.cols = c(-TARGET, -TIMESTAMP),
                    .fns = function(x) (x-min(x))/(max(x)-min(x)))) %>%
      pivot_longer(cols = c(-TARGET, -TIMESTAMP), names_to = "ECMWF") %>%
      mutate(Group = factor(groupfct(data[[zone]]$Train)),
               ECMWF = solarVars[ECMWF]) %>%
      ggplot(aes(x=value, y=TARGET, color=Group)) +
      facet_wrap(~ECMWF) +
      geom_point(alpha = 0.5) +
      ylab(target) +
      xlab("ECMF weather forecast") +
      ggtitle(paste("Solar power production vs. ECMWF weather forecasts",
                    "colored by hour")) +
      theme_bw()  +
      theme(legend.position = "bottom", text = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      scale_x_continuous(breaks = 0:4 * 0.25,
                         labels = c("0", "0.25", "0.5", "0.75", "1")) +
      guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
      scale_color_discrete(name = groupName) +
      ggsave(paste0("SolarScatterAll_", zone, ".png"),
             path="plots/ForThesis/", width=18, height=8)
      # plot single variable
      data[[zone]]$Train %>% select(all_of(var), TARGET, TIMESTAMP) %>%
        rename(X=var) %>%
        mutate(across(.cols = c(-TARGET, -TIMESTAMP),
                      .fns = function(x) (x-min(x))/(max(x)-min(x)))) %>%
        mutate(Group = factor(groupfct(data[[zone]]$Train))) %>%
        ggplot(aes(x=X, y=TARGET, color=Group)) +
        facet_wrap(~Group, nrow=nrow) +
        geom_point(alpha = 0.5, show.legend = FALSE) +
        ylab(target) +
        xlab("Surface solar radiation downwards") +
        ggtitle(paste("Solar power production vs. surface solar rad down",
                      "facetted by hour")) +
        theme_bw()  +
        theme(text = element_text(size = 16),
              axis.text = element_text(size = 13)) +
        scale_x_continuous(breaks = 0:4 * 0.25, 
                           labels = c("0", "0.25", "0.5", "0.75", "1")) +
        ggsave(paste0("SolarScatterVAR169_", zone, ".png"),
               path="plots/ForThesis/", width=18, height=8)
    }
    colors <- c("#FC717F", "#ED8141", "#CF9400", "#A3A500", "#72B000",
                "#00BC59", "#00C19C", "#00B4EF", "#7997FF", "#DC71FA",
                "#F763E0", "#FF65AE")
    data[[zone]]$Train %>% 
      transmute(TARGET = TARGET, 
                Hour = factor(hour(TIMESTAMP), ordered = TRUE,
                              levels = c(15:23, 0:14)),
                Month = getMonths(data[[zone]]$Train, label=TRUE)) %>% 
      group_by(Hour, Month)  %>% 
      summarise(TARGET = mean(TARGET), .groups="drop") %>%
      ggplot(aes(x=Hour, y=TARGET, color=Month, group=Month)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = colors) +
      xlab("Hour") +
      ylab(target) +
      ggtitle("Mean solar power production curves by hour and month") +
      theme_bw()  +
      theme(legend.position = "bottom", text = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      ggsave(paste0("SolarPowerMean_", zone, ".png"),
             path="plots/ForThesis/", width=18, height=8)
}

plotsForThesisWind <- function() {
  data <-getWindAttributes(loadWind(3))
  groupName <- "Wind angle"
  target <- "Wind power production"

  for (zone in data$Zones) {
    # scatter all variables
    data[[zone]]$Train %>% select(-ZONEID, -TIMESTAMP) %>%
      mutate(A100_bin = cut_interval(A100, n = 8)) %>%
      mutate(across(.cols = c(-TARGET, -A100_bin),
                    .fns = function(x) (x-min(x))/(max(x)-min(x)))) %>%
      pivot_longer(cols = c(-TARGET, -A100_bin), names_to = "ECMWF") %>%
      mutate(ECMWF = windVars[ECMWF]) %>%
      ggplot(aes(x=value, y=TARGET, color=A100_bin)) +
      facet_wrap(~ECMWF) +
      geom_point(alpha = 0.5) +
      ylab(target) +
      xlab("ECMF weather forecast") +
      ggtitle(paste("Wind power production vs. ECMWF weather forecasts",
                    "colored by discretized wind angle")) +
      theme_bw()  +
      theme(legend.position = "bottom", text = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      scale_x_continuous(breaks = 0:4 * 0.25,
                         labels = c("0", "0.25", "0.5", "0.75", "1")) +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_color_discrete(name = groupName) +
      ggsave(paste0("WindScatterAll_", zone, ".png"),
             path="plots/ForThesis/", width=18, height=8)
  }

  # look at linear combinations of height of wind speed
  corr <- data.frame()
  for (zone in data$Zones) {
    d <- data[[zone]]$Train
    for (alpha in seq(0, 1.5, 0.05)) {
      height <- 10 * (1 - alpha) + 100 * alpha
      c <- cor(d$TARGET, d$S10 * (1 - alpha) + d$S100 * alpha,
               method = "spearman")
      corr <- rbind(corr, data.frame("Zone"=zone, "Value"=c, "Height"=height))
    }
  }
  top3 <- corr %>% group_by(Zone) %>%
    summarise(Height, Value = 32 - rank(Value), .groups = "drop") %>%
    filter(Value <= 3)
  ggplot(mapping = aes(x = Zone, y = Height)) +
    geom_tile(data = corr, aes(fill = Value)) +
    geom_text(data = top3, aes(label = Value))
}

plotsForThesisPrice <- function() {
  data <- loadPrice(3)$Zone1$Train %>% rename(Price = TARGET)

  outlier <- mutate(data, Outlier = (Price > quantile(Price, probs=0.99))) %>%
    select(-ZONEID, -TIMESTAMP)
  # plot histograms
  outlier %>% pivot_longer(cols = -Outlier, names_to = "Var") %>%
    mutate(Var = ifelse(Var == "Price", "Price", priceVars[Var])) %>%
    ggplot(aes(x=value, fill=Outlier)) +
    facet_wrap(~Var, scales="free") +
    geom_histogram(bins=40) +
    theme_bw()
  # plot scatter plots
  outlier %>% pivot_longer(cols = c(-Outlier, -Price), names_to = "Var") %>%
    mutate(Var = ifelse(Var == "Price", "Price", priceVars[Var])) %>%
    ggplot(aes(x=value, y=Price, color=Outlier)) +
    facet_wrap(~Var, scales="free") +
    geom_point() +
    theme_bw()
}

plotsForThesisLoad <- function() {
  d <- loadLoad(3)$Zone1$Train %>% filter(!is.na(TARGET))
  d %>% select(TIMESTAMP, TARGET, w1) %>%
    mutate(Month = getMonths(d, label=TRUE),
           t = day(TIMESTAMP), m = month(TIMESTAMP),
           p = m %in% 5:9 | (m == 4 & t > 18) | (m == 10 & t <= 18),
           Period = ifelse(p, "19.04 - 18.10", "19.10 - 18.04")) %>%
    select(Month, TARGET, w1, Period) %>%
    ggplot(aes(x=w1, y=TARGET, color=Period)) +
    geom_point(alpha=0.5) +
    facet_wrap(~Month) +
    xlab("Temperature") +
    ylab("Load") +
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(size = 16),
            axis.text = element_text(size = 13)) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    ggtitle("Load vs. temperature (w1)") +
    ggsave("LoadScatterw1.png", path="plots/ForThesis/", width=18,
           height=8)

  corr <- d %>% mutate(Month = getMonths(d, label=TRUE)) %>% group_by(Month) %>%
    summarise(across(.cols = starts_with("w"),
                     .fns = function(x) abs(cor(TARGET, x,
                                                method="pearson"))),
    .groups = "drop") %>%
    pivot_longer(cols = -Month, names_to = "Temperature")
  top10 <- corr %>% group_by(Month) %>%
    summarise(Temperature, value = 26 - rank(value), .groups = "drop") %>%
    filter(value <= 10)

  ggplot(mapping = aes(y=Month, x = Temperature)) +
    geom_tile(data = corr, aes(fill = value)) +
    geom_text(data = top10, aes(label = value))
}