library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

source("preprocess.R")
source("plot.R")
source("loadData.R")
source("util.R")

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
