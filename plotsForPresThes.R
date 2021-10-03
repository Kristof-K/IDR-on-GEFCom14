source("loadData.R")
source("util.R")
source("plot.R")
source("preprocess.R")

# ==============================================================================
# Plots for slide and thesis
# ==============================================================================

plotPIT <- function() {
  task_color <- c("#F8766D", "#00BA48", "#619CFF")
  w10 <- data.frame()
  w9 <- data.frame()
  for(i in 1:3) {
    add10 <- paste0("predictions/Load-10000000100_1_1-squ_CondMed_meanTemp/Task",
                    i, "_1.csv")
    add9 <- paste0("predictions/Load-1_1_1-squ_Med_meanTemp/Task", i, "_1.csv")
    w10 <- rbind(w10, cbind(read.csv(add10), Task=i))
    w9 <- rbind(w9, cbind(read.csv(add9), Task=i))
  }
  combine <- rbind(cbind(w10, WS="w10"), cbind(w9, WS="w9"))

  Hist <- transmute(combine, Task = factor(Task, ordered=TRUE, levels=3:1),
                    WS  = WS,
            PIT = rowSums(y >= combine[,paste0("X", 1:99 * 0.01)]) * 0.01) %>%
    ggplot() +
    geom_histogram(aes(x=PIT, fill=Task), bins=20) +
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
  scalibrary(gridExtra)
  grid.arrange(Hist, scatter, nrow=1)
  ggsave("LoadPIT.pdf", path="plots/ForThesis/", width=11.69, height=4.2)
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

# ==============================================================================
# SOLAR
# ==============================================================================
plotsForThesisSolar <- function() {
  # prevent labels from overlapping (just leave labels out)
  # scale_x_continuous(guide = guide_axis(check.overlap = TRUE))+
  # use scientific labels
  # scale_x_continuous(labels = function(x) format(x, scientific=TRUE)
  data <- deaccumulateSol(loadSolar(4))
  var <- "VAR169"
  groupName <- "Hour"
  groupfct <- function(d) getHours(d) - 1
  nrow <- 4
  target <- "Solar power production"

  for (zone in data$Zones) {
    # scatter all variables
    data[[zone]]$Train %>%
      mutate(across(.cols = c(-TARGET, -TIMESTAMP),
                    .fns = function(x) (x-min(x))/(max(x)-min(x))),
      Group = factor(groupfct(data[[zone]]$Train))) %>%
      select(-ZONEID, -TIMESTAMP) %>%
      pivot_longer(cols = c(-TARGET, -Group), names_to = "ECMWF") %>%
      mutate(ECMWF = solarVars[ECMWF]) %>%
      ggplot(aes(x=value, y=TARGET, color=Group)) +
      facet_wrap(~ECMWF) +
      geom_point(alpha = 0.5) +
      ylab(target) +
      xlab("ECMWF weather forecast") +
      ggtitle("Scatter Plot of Solar Track Colored by Hour") +
      theme_bw()  +
      theme(legend.position = "bottom", text = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      scale_x_continuous(breaks = 0:4 * 0.25,
                         labels = c("0", "0.25", "0.5", "0.75", "1")) +
      guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
      scale_color_discrete(name = groupName) +
      ggsave(paste0("SolarScatterAll_", zone, ".pdf"),
             path="plots/ForThesis/", width=11.69, height=8)
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
        ggtitle("Power vs. Radiation Facetted by Hour") +
        theme_bw()  +
        theme(text = element_text(size = 16),
              axis.text = element_text(size = 13)) +
        scale_x_continuous(breaks = 0:4 * 0.25, 
                           labels = c("0", "0.25", "0.5", "0.75", "1")) +
        ggsave(paste0("SolarScatterVAR169_", zone, ".pdf"),
               path="plots/ForThesis/", width=11.69, height=8)
    }
    # plot power curves
    colors <- c("#FC717F", "#ED8141", "#CF9400", "#A3A500", "#72B000",
                "#00BC59", "#00C19C", "#00B4EF", "#7997FF", "#DC71FA",
                "#F763E0", "#FF65AE")
    zone <- "Zone2"
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
      ggtitle("Mean Power Curves by Hour and Month") +
      theme_bw()  +
      theme(legend.position = "bottom", text = element_text(size = 16),
            axis.text = element_text(size = 13)) +
      guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
      ggsave(paste0("SolarPowerMean_", zone, ".pdf"),
             path="plots/ForThesis/", width=11.69, height=5)

  tune_results <- c(0.01267822,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA,	NA, 0.012639036, 0.013172959, 0.012828283,
                    0.012673956, 0.012886168, 0.012400424, 0.012990511, 0.012939531, 0.013277446, 0.014445207,
                    0.013131918, 0.014423559, 0.012834898, 0.01349761, 0.020216589, 0.013014286, 0.013294046,
                    0.012779336, 0.013608201, 0.013383948, 0.013208328, 0.015187683, 0.013221076, 0.014666742,
                    0.012620445, 0.01297488, 0.015981613, 0.016384129, 0.012836368,	0.012677464, 0.012918514,
                    0.012973811, 0.013481524, 0.013960972, 0.013200076, 0.014612858, 0.012845526, 0.01319305,
                    0.018304571, 0.016736697, 0.020986741, 0.012423161, 0.013055709, 0.012801626, 0.013901812,
                    0.014024197, 0.01340063, 0.014666226, 0.012345311, 0.012633753, 0.016704145, 0.015907363,
                    0.019674947, 0.019788498, 0.012700784, 0.012869064, 0.013134951, 0.013603031, 0.012825567,
                    0.01437698, 0.012927954, 0.013126939, 0.01753826, 0.016411399, 0.020192732,	0.019281243,
                    0.020138433, 0.013112811, 0.014594125, 0.014473083, 0.014546887, 0.014625995, 0.012727937,
                    0.01299236, 0.016415968, 0.01597104, 0.018988152, 0.018694834,	0.019241758, 0.018931945,
                    0.01429727, 0.013888452, 0.013729802, 0.014173882, 0.013205003, 0.013866685, 0.020453965,
                    0.016712511, 0.021053046, 0.019978119, 0.020251267, 0.01865759, 0.023329123, 0.015509256,
                    0.013765513, 0.014847769, 0.014120757, 0.014832655, 0.022038882, 0.017440518,0.021679103,
                    0.020762826, 0.021179321, 0.020079669, 0.024713647, 0.023399179, 0.014693115, 0.017555492,
                    0.012960927, 0.013607595, 0.020363166, 0.017025461,	0.021338174, 0.020497223, 0.021346178,
                    0.019440668, 0.023940227, 0.024174564, 0.023450667,	0.015675356, 0.014128538, 0.014822914,
                    0.02054081, 0.017199968, 0.021175021,	0.019658463, 0.020490523, 0.01899032,	0.023017632,
                    0.025793037, 0.023274764, 0.022998436)
  solarV <- c("SSRD", "TNSR", "T2", "RH", "TP", "TCIW", "TCLW", "TCC", "STRD", "SP", "U10", "V10")
  name_results <- matrix(tune_results, nrow=12, ncol=12, byrow=TRUE)
  colnames(name_results) <- solarV
  cbind(Variable = solarV, data.frame(name_results)) %>%
    pivot_longer(cols = -Variable, names_to = "Var2", values_to = "Score") %>%
    ggplot(aes(x=factor(Var2, ordered=TRUE, levels=solarV),
               y=factor(Variable, ordered=TRUE, levels=solarV), fill=Score)) +
    geom_tile() +
    xlab("Explanatory variables") +
    ylab("") +
    ggtitle("Mean Pinball Score (Initial Tuning Phase)") +
    theme_bw()  +
    theme(text = element_text(size = 16), axis.text = element_text(size = 13)) +
    ggsave("SolarPinballscores.pdf",
             path="plots/ForThesis/", width=11.69, height=5)
}

# ==============================================================================
# WIND
# ==============================================================================
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
    scale_color_discrete(name = groupName) +
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

# ==============================================================================
# PRICE
# ==============================================================================
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
  library(gridExtra)    # for combining plots
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
          legend.justification=c(1,1), legend.position=c(1,1)) +
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

# ==============================================================================
# LOAD
# ==============================================================================
plotsForThesisLoad <- function() {
  cols <- paste0("w", 1:25)
  d <- loadLoad(4)$Zone1$Train %>% filter(!is.na(TARGET))

  # TRANSFORM TEMPERATURES IN DISTANCES ========================================
  source("preprocess.R")
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
  library(gridExtra)
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

  source("preprocess.R")
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

  library("gridExtra")
  grid.arrange(byMonth, byWday, nrow=1)
  ggsave("LoadDev.pdf", path="plots/ForThesis/", width=11.69,
         height=5)

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