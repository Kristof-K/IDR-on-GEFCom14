library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

source("preprocess.R")
source("plot.R")
source("loadData.R")
source("util.R")

plotsForThesisPrice <- function() {
  data <- loadPrice(4)$Zone1$Train %>% rename(Price = TARGET)

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

  # scatter plot with histogram
  scatter <- data %>% mutate(Season = get4Seasons(data, label=TRUE)) %>%
    select(-ZONEID, -TIMESTAMP) %>%
    pivot_longer(cols = c(-Price, -Season), names_to = "Var") %>%
    mutate(Var = priceVars[Var]) %>%
    ggplot(aes(x=value, y=Price, color=Season)) +
    geom_point(alpha = 0.5, show.legend = FALSE) +
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

  d <- mutate(data, Month = getMonths(data, label=TRUE)) %>%
    mutate(across(.cols = c(names(priceVars), Price),
                  .fns = function(x) (x - min(x)) / (max(x)-min(x))))
  ftl_cor <- d %>% group_by(Month) %>%
    summarise(Spearman_cor =  round(cor(Price, Forecasted.Total.Load,
                                        method = "spearman"), digits=3),
              .groups = "drop") %>%
    mutate(Price = 0.89, Forecasted.Total.Load = 0.18)
  fzl_cor <- d %>% group_by(Month) %>%
    summarise(Spearman_cor = round(cor(Price, Forecasted.Zonal.Load,
                                       method = "spearman"), digits=3),
              .groups = "drop") %>%
    mutate(Price = 0.89, Forecasted.Zonal.Load = 0.18)

  # scatter plott by month
  ftl_plot <- ggplot(mapping = aes(x=Forecasted.Total.Load, y=Price)) +
    geom_point(data = d, aes(color=Month), alpha = 0.5, show.legend=FALSE) +
    geom_label(data = ftl_cor, aes(label = Spearman_cor), show.legend=FALSE,
               size=5.8) +
    facet_wrap(~Month, nrow=4) +
    xlab("Forecasted total load") +
    ylab("Electricity price") +
    scale_x_continuous(breaks = 0:4 * 0.25,
                      labels = c("0", "0.25", "0.5", "0.75", "1")) +
    ggtitle("Electricity Price vs. Forecasted Load") +
    theme_bw() +
    theme(text = element_text(size = 18), axis.text = element_text(size = 13))
  fzl_plot <- ggplot(mapping = aes(x=Forecasted.Zonal.Load, y=Price)) +
    geom_point(data = d, aes(color=Month), alpha = 0.5, show.legend=FALSE) +
    geom_label(data = fzl_cor, aes(label = Spearman_cor), show.legend=FALSE,
               size=5.8) +
    facet_wrap(~Month, nrow=4) +
    xlab("Forecasted zonal load") +
    ylab("") +
    scale_x_continuous(breaks = 0:4 * 0.25,
                      labels = c("0", "0.25", "0.5", "0.75", "1")) +
    ggtitle("") +
    scale_y_continuous(labels = NULL) +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13))
  grid.arrange(ftl_plot, fzl_plot, nrow=1)
  #ggsave("PriceOverview.png", path="plots/ForThesis/", width=11.69, height=8.95)

  # plot log headtmap of price values
  n <- nrow(data)
  ticks <- as.integer(seq(1, n, (n - 1) / 9))
  labels <- paste0(month(data$TIMESTAMP[ticks], label=TRUE),
                   year(data$TIMESTAMP[ticks]))
  d <- data %>% mutate(logPrice = log(Price), x = (0:(n-1)) %/% 24)

  heatmap <- d %>%
    mutate(hour=factor(hour(TIMESTAMP), ordered=TRUE,levels=c(1:23, 0))) %>%
    ggplot() +
    geom_tile(mapping = aes(x=x, y=hour, fill=logPrice)) +
    ggtitle("Electricity Price During Initial Tuning Phase") +
    ylab("Hour") +
    scale_x_continuous(breaks = (ticks-1) %/% 24, labels = labels, name = "") +
    scale_fill_gradient(low="darkorchid4", high="coral", name = "ln(Price)") +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13))
  #ggsave("PriceHeatmap,png", path="plots/ForThesis/", width=4.2, height=8.95)

  byTime <- d %>% mutate(Hour = get6DayTime(d, label=TRUE)) %>%
    group_by(Hour, x) %>%
    summarise(Mean_price = mean(logPrice), .groups="drop") %>%
    ggplot(aes(x=x, y=Mean_price, color=Hour)) +
    geom_point(alpha=0.5) +
    ylab("ln(Price)") +
    ggtitle("Mean ln(Price) by Hour Group and Day") +
    scale_color_discrete() +
    scale_x_continuous(breaks = (ticks-1) %/% 24, labels = labels, name = "") +
    theme_bw() +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13))

  byWeekday <- d %>%
    transmute(logPrice = logPrice, Hour = hour(TIMESTAMP),
              Weekday = getWday(d, label=TRUE)) %>%
    group_by(Hour, Weekday)  %>%
    summarise(logPrice = mean(logPrice), .groups="drop") %>%
    ggplot(aes(x=Hour, y=logPrice, color=Weekday, group=Weekday)) +
    geom_line(size = 1.2) +
    geom_vline(xintercept=c(3.5, 7.5, 11.5, 15.5, 19.5), linetype=2,
               color="gray")+
    scale_color_discrete() +
    xlab("Hour") +
    ylab("ln(Price)") +
    ggtitle("Mean ln(Price) by Hour and Weekday") +
    theme_bw()  +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13))

  grid.arrange(byTime, byWeekday, nrow=2)
  #ggsave("PriceHeatmap,png", path="plots/ForThesis/", width=7.8, height=8.95)


  # Get mean values by HourGroupd and Weekday
  tmp <- data %>% mutate(HourGroup = get6DayTime2(data, label=TRUE),
                         Weekday = getWday(data, label=TRUE)) %>%
    group_by(HourGroup, Weekday) %>%
    summarise(MeanPrice = mean(Price), .groups="drop") %>%
    pivot_wider(id_cols=c(HourGroup, Weekday), names_from = Weekday,
                values_from=MeanPrice)

  data %>% mutate(Month = month(TIMESTAMP)) %>% filter(Month %in% c(10,11,12)) %>%
    ggplot(aes(x=Forecasted.Total.Load, y=TARGET, color=factor(Month))) +
    geom_point(alpha=0.5) +
    ylab("Electricity price") +
    xlab("Forecasted zonal load") +
    theme_bw()

  t1 <- loadPrice(1)$Zone1$Test$TIMESTAMP
  t2 <- loadPrice(2)$Zone1$Test$TIMESTAMP
  t3 <- loadPrice(3)$Zone1$Test$TIMESTAMP

  data %>%
    mutate(Type = 1 * (TIMESTAMP %within% interval(t1[1], t1[length(t1)]))
      + 2 * (TIMESTAMP %within% interval(t2[1], t2[length(t2)]))
      + 3 * (TIMESTAMP %within% interval(t3[1], t3[length(t3)]))) %>%
    mutate(Type = ifelse(Type == 0, "Train", paste("Task", Type)),
           Month = getMonths(data, label=TRUE)) %>%
    filter(Month %in% c("Jun", "Jul")) %>%
    ggplot(aes(x=Forecasted.Zonal.Load, y=Price, color=Type,
               shape=Month)) +
    geom_point(alpha=0.5, size=1.8) +
    facet_wrap(~Month) +
    theme_bw()
}
