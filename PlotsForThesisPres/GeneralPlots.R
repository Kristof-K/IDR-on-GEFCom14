library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

source("preprocess.R")
source("loadData.R")

plotPIT <- function() {
  task_color <- c("#F8766D", "#00BA48", "#619CFF")
  w10 <- data.frame()
  w9 <- data.frame()
  for(i in 1:3) {
    add10 <- paste0("predictions/Load-10000000100_1_1-squ_CondMed_meanTemp_",
                    "tune/Task", i, "_1.csv")
    add9 <- paste0("predictions/Load-1_1_1-squ_Med_meanTemp_tune/Task", i,
                   "_1.csv")
    w10 <- rbind(w10, cbind(read.csv(add10), Task=i))
    w9 <- rbind(w9, cbind(read.csv(add9), Task=i))
  }
  combine <- rbind(cbind(w10, WS="w10"), cbind(w9, WS="w9"))

  Hist <- transmute(combine, Task = factor(Task, ordered=TRUE, levels=3:1),
                    WS  = WS,
            PIT = rowSums(y >= combine[,paste0("X", 1:99 * 0.01)]) * 0.01) %>%
    ggplot() +
    geom_histogram(aes(x=PIT, fill=Task), binwidth=0.05, boundary=0) +
    facet_wrap(~WS) +
    ylab("Count") +
    ggtitle("Forecast Performance") +
    scale_fill_manual(values = rev(task_color)) +
    theme_bw()  +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13),
          legend.justification=c(1,1), legend.position=c(1,1))

  n <- nrow(combine) / 2
  breaks <- as.integer(seq(1, n, (n - 1) / 4))
  labels <- paste0(day(combine$time[breaks]), ".", month(combine$time[breaks]))

  task12 <- min(which(combine$Task == 2))
  task23 <- min(which(combine$Task == 3))

  true_obs <- filter(combine, WS=="w9") %>%
    transmute(x = 1:n, y = y)
  t_text <- data.frame(x=c(task12 / 2, task12 + (task23 - task12) / 2,
                           task23 + (n - task23) / 2),
                     y = rep(312.5, 3), lab=paste("Task", 1:3))

  con_bans <- select(combine, WS = WS, all_of(paste0("X", c(1:4, 6:9) * 0.1))) %>%
    rename(l_80 = X0.1, l_60 = X0.2, l_40 = X0.3, l_20 = X0.4,
           u_80 = X0.9, u_60 = X0.8, u_40 = X0.7, u_20 = X0.6) %>%
    mutate(x = rep(1:n, 2)) %>%
    pivot_longer(cols = c(-x, -WS), names_to = c("lu", "width"),
                 names_sep="_") %>%
    pivot_wider(id_cols = c(WS, x, width), names_from = lu,
                values_from = value)
  scatter <- ggplot(mapping=aes(x=x)) +
    geom_ribbon(data=con_bans, aes(ymin=l, ymax=u, group=width), alpha=0.5) +
    geom_point(data=true_obs, aes(y=y), color="red", alpha=0.2, size=0.75) +
    geom_vline(xintercept = c(task12, task23), color="blue", linetype=2) +
    geom_text(data=t_text, aes(x=x, y=y, label=lab),
              color=rep(task_color, 2), size=5) +
    facet_wrap(~WS) +
    ggtitle("") +
    ylab("Load") +
    scale_x_continuous(breaks = breaks, labels = labels, name = "Date") +
    theme_bw()  +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13))

  grid.arrange(Hist, scatter, nrow=1)
  ggsave("LoadPIT.pdf", path="plots/ForThesis/", width=11.69, height=4.2)
}

plotResults <- function() {
  library("gridExtra")
  tracks <- c("Load", "Price", "Wind", "Solar")
  paths_curves <- c("../BestLoad.csv", "../BestPrice.csv", "../BestWind.csv",
                    "../BestSolar.csv")
  paths_bars <- c("../BestLoadFinal.csv", "../BestPriceFinal.csv",
                  "../BestWindFinal.csv", "../BestSolarFinal.csv")
  # number of best models by track (same order as tracks)
  nr_best <- c(5, 5, 4, 4)
  additional <- c(2, 2, 0, 1)

  label_pos <- c(0.09, 0.13, 0.1, 0.13)
  benchmark_pos <- c(0.3, 0.42, 0.33, 0.4)

  for (i in 1:4) {
    raw_data <- read.csv2(paths_curves[i])
    # minus task, minus benchmark, minus idr nodels = nr of participants
    p <- ncol(raw_data) - 2 - nr_best[i]
    participants <- raw_data %>% select(Task, starts_with("X")) %>%
      pivot_longer(cols = -Task, names_to = "Rank") %>%
      mutate(Rank = factor(substring(Rank, 2), ordered=TRUE,
                           levels=paste(1:p)))
    benchmark <- select(raw_data, Task, Benchmark) %>%
      rename(value = Benchmark)
    idr_models <- select(raw_data, Task, starts_with("IDR")) %>%
      pivot_longer(cols = -Task, names_to = "Model") %>%
      mutate(Model = factor(Model, ordered=TRUE, levels=paste0("IDR",
                                                               1:nr_best[i])))

    g <- nr_best[i] - additional[i]
    o <- additional[i]
    part_colors <- scales::seq_gradient_pal("#132B43", "#56B1F7",
                                            "Lab")(seq(0,1,length.out=p))
    idr1_colors <- scales::seq_gradient_pal("#028618", "#1fdb3f",
                                           "Lab")(seq(0,1, length.out=g))
    idr2_colors <- scales::seq_gradient_pal("#FC6A03", "#FCAE1E",
                                           "Lab")(seq(0,1, length.out=o))
    idr_colors <- c(idr1_colors, idr2_colors)

    curves <- ggplot(mapping = aes(x=Task, y=value)) +
      geom_line(data=participants, aes(color = Rank)) +
      scale_color_manual(values=part_colors) +
      ggnewscale::new_scale_color() +
      geom_line(data=benchmark, color="red") +
      geom_point(data=benchmark, color = "red", shape = 2) +
      geom_line(data=idr_models, aes(color = Model), size=1.2) +
      geom_point(data=idr_models, aes(color = Model), size=1.4, shape=1) +
      scale_x_continuous(breaks = 1:12) +
      scale_color_manual(values=idr_colors) +
      xlab("Task") +
      ylab("Mean pinball score") +
      ggtitle(paste("Final Scores of", tracks[i], "Track")) +
      theme_bw() +
      theme(text = element_text(size = 16), axis.text = element_text(size = 13),
            legend.position = "none")

    raw_data <- read.csv2(paths_bars[i]) %>%
      mutate(Text = paste0(format(round(Rating * 100, 1), nsmall=1), "%")) %>%
      arrange(desc(Rank)) %>%
      mutate(Rank = factor(paste(Rank), ordered=TRUE, levels=paste(Rank)))

    idr_tags <- paste("IDR", 1:nr_best[i])
    bar_colors <- c(setNames(part_colors, paste(1:p)), "Benchmark"="red",
                    setNames(idr_colors, idr_tags))
    draw_c <- unname(bar_colors[raw_data$Label])

    bars <- ggplot(raw_data, aes(x=Rank, y=Rating, fill=Rank)) +
      geom_col(show.legend=FALSE) +
      geom_text(aes(y=label_pos[i], label=Text),
                color=c("red", rep("black", p - 5 + nr_best[i]),
                        rep("white", 5)),
                angle=90, size=6) +
      geom_text(data=filter(raw_data, Label %in% idr_tags), aes(y=Rating * 0.75,
                                                                label=Label),
                angle=90,size=6) +
      annotate("text", x=paste(p+1), y=benchmark_pos[i], label="Benchmark",
               color="red", angle=90, size=6) +
      scale_fill_manual(values = draw_c) +
      scale_color_manual(values = draw_c) +
      scale_x_discrete(breaks = paste(1:p)) +
      ggtitle("Linear Weighted Skill Score") +
      theme_bw() +
      theme(text = element_text(size = 16), axis.text = element_text(size = 13),
            legend.position = "none")
    # for load: ifelse(Rating <=0.3, Rating*1.33, Rating * 0.75)

    grid.arrange(curves, bars, nrow=2)
    # save with height 7.5
  }
}

plotFinalPIT <- function() {
  zones <- list("Load"=1, "Price"=1, "Wind"=1:10, "Solar"=1:3)
  models <- list("Load"=c("Load-1_1_1-meanTemp",
                          "Load-1_1_1-squ_Med_meanTemp",
                          "Load-1001_1_1-squ_Med_meanTemp",
                          "Load-1001_1_29_1_75_0.25-squ_Med_meanTemp",
                          "Load-1e+07_1_8_80_0.33-squ_CondMed_meanTemp"),
                 "Price"=c("Price-1_1_1-addPriceRegressors",
                           "Price-10_1_16-addPriceRegressors",
                           "Price-1000010_1_16_1-addPriceRegressors",
                           "Price-10_1_5-addPriceRegressors",
                           "Price-10_1_8_1_75_0.25-addPriceRegressors"),
                 "Wind"=c("Wind-10_1_1-CalcWindAttributes",
                          "Wind-10_1_6-CalcWindAttributes",
                          "Wind-1e+08_1_6-CalcWindAttributes",
                          "Wind-100000010_1_6-CalcWindAttributes"),
                 "Solar"=c("Solar-1_1_1-deacc_and_invert_vars",
                           "Solar-1_1_9-deacc_and_invert_vars",
                           "Solar-100001_1_9_1_70_0.7",
                           "Solar-111011_1_2_1_75_0.25-deacc_and_invert_vars"))
  current_track <- "Wind"
  # load data
  raw_data <- data.frame()
  m <- 1
  for(model in models[[current_track]]) {
    for (task in 1:12) {
      for (zone in zones[[current_track]]) {
        file <- paste0("predictions/", model,
                     "/Task", task, "_", zone, ".csv")
        raw_data <- rbind(raw_data, cbind(read.csv(file), Task=task,
                                          Model=paste("IDR", m)))
      }
    }
    m <- m + 1
  }
  raw_data %>%
    transmute(Model = Model, Task = factor(Task, ordered=TRUE, levels=12:1),
              PIT = rowSums(y >=raw_data[,paste0("X", 1:99 * 0.01)]) * 0.01) %>%
    ggplot() +
    facet_wrap(~Model, nrow=1) +
    geom_histogram(aes(x=PIT, fill=Task), binwidth=0.05, boundary=0) +
    ylab("Count") +
    ggtitle(paste("PIT Histograms", current_track, "Track")) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_fill_discrete() +
    scale_x_continuous(breaks = 0:4 * 0.25,
                       labels=paste(c(0, 0.25, 0.5, 0.75, 1))) +
    theme_bw()  +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13),
          legend.position="bottom")
  ggsave(paste0(current_track, "_FinalPIT.pdf"), path="plots/ForThesis/",
         width=11.69,height=4.2)
}

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
