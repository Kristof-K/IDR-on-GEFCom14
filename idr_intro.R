library(dplyr)
library(ggplot2)
library(tidyr)        # for pivot_longer
library(gridExtra)    # for multiple plots

#
# IDR is a non-parametric distributional regression technique
#


# DISTRIBUTIONAL REGRESSION
# -> we want to estimate the conditional distribution of the response variable
#    Y given explanatory variables X_1,...,X_k
#
#    Hence ideally we would like to have:
#             f(x_1,..., x_k) = P(Y | X_1=x_1,...,X_k=x_k)
#   => estimation fct f maps tupels on CDFs
#
#   We need to deduce from (x_1,...,x_k,y) to P(Y | X_1=x_1,...,X_k=x_k)



# NON-PARAMETRIC
# -> we don't expect the conditional distribution of Y to be of a specific type
#    (e.g. Gaussian, Gamma, ...)



# ISOTONIC
# -> we assume that there is a isotonic relationship between the explanatory
#    variables and the conditional distribution of Y, namely:
#
#  (x_1,...,x_k) <= (z_1,...,z_k) => P(Y|X_1=x_1,...X_k=x_k) <= P(Y|X_1=z_1,...X_k=z_k)
#
#   What are these inequalities? <- partial orders
#   - left: e.g. component-wise order:
#      (x_1,...,x_k) <= (z_1,...,z_k) :<=> x_1 <= z_1, ..., x_k <= z_k
#   - right: stochastic dominance:
#       F <= G :<=> F(x) >= G(x) for all x in IR



X <- seq(-4, 4, 0.1)
F <- pnorm(X, mean = -1, sd = 1)
G <- pnorm(X, mean = 1, sd = 1)


plot_stochDom <- function(X, F, G, F_lab="F", G_lab="G") {
  data.frame(X=X, F=F, G=G) %>% pivot_longer(cols=-X, names_to="CDF") %>%
    ggplot() +
      geom_line(mapping = aes(x=X, y=value, color=CDF), show.legend = FALSE) +
      scale_color_manual(values = c("orange", "skyblue")) +
      annotate("text", x = -1.5, y = 0.75, label = F_lab,
              color = "orange", size = 10) +
      annotate("text", x = 1.5, y = 0.25, label = G_lab,
              color = "skyblue", size = 10) +
      labs(title = "Stochastic dominance")
}

plot_stochDom(X, F, G, "F ~ N(-1,1)", "G ~ N(1,1)")

# load IDR package
library(isodistrreg)
# show available methods
ls("package:isodistrreg")


# a simple example
y <- c(1, 2)
x <- c(1, 2)
fit1 <- idr(y=y, data.frame(X=x))
fit1
names(fit1)
fit1$thresholds
fit1$cdf

# predict on training data = get estimated CDFs
plot(predict(fit1, data=data.frame(X=1)), main="P(Y | X = 1)")
plot(predict(fit1, data=data.frame(X=2)), main="P(Y | X = 2)")
# predict out of training sample = real prediction
predict(fit1, data=data.frame(X=0.5))

# Create long data frame containing columns X, value, cdfs, color, in which
# jumps of a cdf belonging to one value in val are listed
# groups assigns every cdf a unique name and color contains for every group
# the prediction value, which can be used to color every cdf
get_pred_df <- function(fit, val) {
  cdf_names <- paste0("cdf", 1:length(val))
  m <- length(fit$thresholds)
  x_left <- fit$thresholds[1] - 0.15 * (fit$thresholds[m] - fit$thresholds[1])
  x_right <- fit$thresholds[m] + 0.15 * (fit$thresholds[m] - fit$thresholds[1])

  predictions <- predict(fit, data=data.frame(X=val))
  pred_cdfs <- data.frame()
   # fill data.frame by iterating through the predictions
   for (j in 1:length(val)) {
     new_cdf <- data.frame(X=c(x_left, predictions[[j]][["points"]], x_right),
                           value=c(0, predictions[[j]][["cdf"]], 1),
                           cdfs=cdf_names[j], color=val[j])
     pred_cdfs <- rbind(pred_cdfs, new_cdf)
   }
  return(pred_cdfs)
}

# Method visualizing point cloud and estimated conditional distribution fct.
# Additionally via pred predicted conditdional distribution fct.s can also be
# added.
# if print_all equals False only predicted cdfs are drawn
visualizeIDR <- function(x, y, pred=numeric(0), print_all=TRUE, ret=FALSE) {
  # apply IDR
  fit <- idr(y=y, X=data.frame(X=x))

  pointcloud <- data.frame(x=x, y=y, p=paste0("p", 1:length(y))) %>%
    ggplot(mapping = aes(x=x, y=y, group=p, color=x)) +
      geom_point(show.legend = FALSE) +
      labs(title = "Point cloud") +
      xlab("X") +
      ylab("Y") +
      theme_bw()

  if (print_all) {
    # plot all estimated conditional cdfs (predict on training data x!)
    cdfs_plot <- ggplot(mapping = aes(x=X, y=value, group=cdfs, color=color)) +
                  geom_step(data = get_pred_df(fit, x), show.legend = FALSE)
  } else {
    cdfs_plot <- ggplot(mapping = aes(x=X, y=value, group=cdfs, color=color))
  }
  # add title and remove labels
  cdfs_plot <- cdfs_plot + labs(title = "CDF estimates of IDR") +
    ylab("") + xlab("") +
    theme_bw()

  # if there are predictions, draw them also in the graphs
  s <- length(pred)
  if (s > 0) {
    # extend points clouds by dashed vertical lines
    vert_lines <- data.frame(x=rep(pred, each=2), y=rep(c(min(y), max(y)), s),
                          p=rep(paste0("l", 1:s), each=2))
    pointcloud <- pointcloud +
      geom_line(data=vert_lines, show.legend=FALSE, linetype = "dashed")
    # add predicted cdfs to the cdf graph
    cdfs_plot <- cdfs_plot +
      geom_step(data=get_pred_df(fit, pred), linetype = "dashed",
                show.legend = FALSE)
  }
  if (ret) {
    return(list(pointCloud=pointcloud, cdfs=cdfs_plot))
  }
  # now draw plots in one grid
  grid.arrange(pointcloud, cdfs_plot, ncol=2)
}

visualizeIDR(x=c(1,2), y=c(1,2))

# And what is with prediction
visualizeIDR(x=c(1,2), y=c(1,2), pred=1.5)
# ~ interpolation
visualizeIDR(x=c(1,2), y=c(1,2), pred=c(1.2, 1.8))
# ~ extrapolation
visualizeIDR(x=c(1,2), y=c(1,2), pred=c(0.8, 2.2))

# Role of isotonicity
visualizeIDR(x=c(1,2), y=c(2,1))



# And now a more complex example (from the simulation study in the IDR paper) --

n <- 500
X <- runif(n, 0, 10)     # X ~ U(0, 10)
Y <- rgamma(n, sqrt(X), scale=pmin(pmax(1, X), 6))

# check stochastic dominance
grid <- seq(0, 40, 0.1)
plot_stochDom(X=grid , F=pgamma(grid, sqrt(2), scale=2),
              G=pgamma(grid, sqrt(4), scale = 4))

# look at data
ggplot(data = data.frame(X=X, Y=Y)) +
  geom_point(mapping = aes(x=X, y=Y)) +
  ggtitle("Bivariate point cloud of (X,Y)") +
  theme_bw()

# apply IDR
visualizeIDR(x=X, y=Y)
plotList <- visualizeIDR(x=X, y=Y, ret=TRUE)
P1 <- plotList$pointCloud
P2 <- plotList$cdfs
# make some predictions
visualizeIDR(x=X, y=Y, pred=c(0.5, 3, 5, 7, 9.5), print_all=FALSE)



# compare IDR CDFs with true CDFs ----------------------------------------------
groups <- c(X = 1)
orders <- c("comp" = 1)
fit <- idr(y=Y, X=data.frame(X=X), groups, orders)
predict_val <- c(0.5, 3, 5, 7, 9.5)
idr_cdfs <- get_pred_df(fit, val=predict_val)

col_vec <- c("#808000", "#228B22", "#32CD32", "#ADFF2F", "#00FF7F")

# determine true CDFs
true_cdfs <- data.frame()
grid <- seq(min(Y)-0.15*(max(Y)-min(Y)), max(Y)+0.15*(max(Y)-min(Y)), 0.1)
for(t in predict_val) {
  gamma_dist <- data.frame(X=grid, value=pgamma(grid, sqrt(t),
                                                  scale=min(max(1,t),6)),
                           cdfs=paste0("cdf",t), color=t)
  true_cdfs <- rbind(true_cdfs, gamma_dist)
}


P3 <- ggplot(mapping = aes(x=X, y=value, group=cdfs, color=factor(color))) +
  geom_step(data=idr_cdfs, show.legend = FALSE) +
  geom_line(data=true_cdfs, linetype = "dashed", show.legend=FALSE) +
  ggtitle(label = paste0("true cdf (solid,continuous) vs. ",
                         "predicted cdf (dashed,stepfunction)")) +
  ylab("Probability") +
  xlab("Threshold") +
  scale_color_manual(values = col_vec) +
  theme_bw()

P3


# Extract mean and some quantiles ----------------------------------------------

predictions <- predict(fit, data=data.frame(X=X))
q <- c(0.25, 0.5, 0.75)

col_vec2 <- c("#FF0000", "#4B0082", "#FF00FF", "#DB7093")
#col_vec2 <- c("#FF0000", "#4B0082", "#9932CC", "#FF00FF", "#DB7093")

quantiles <- qpred(predictions, quantiles = q)

calc_expectation <- function(step_cdf) {
  # substract current height from previous height = jump height
  jumps <- step_cdf[["cdf"]] - lag(step_cdf[["cdf"]], default=0)
  return(sum(step_cdf[["points"]] * jumps))
}

expectations <- sapply(predictions, calc_expectation)

features <- data.frame(cbind(quantiles, expectations, X))
colnames(features) <- c("Lower Quartil", "Median", "Upper Quartil",
                        "Expectation", "X")

P4 <- pivot_longer(features, cols = -X, names_to = "Functional") %>%
  ggplot(mapping = aes(x=X, y=value, color=Functional)) +
    geom_step(size=1.3) +
    geom_point(data = data.frame(X=X, value=Y), color="black") +
    ggtitle(label = "Point cloud and conditional functionals with IDR") +
    xlab("X") +
    ylab("Y") +
    scale_color_manual(values = col_vec2) +
    theme_bw() +
    theme(legend.position = "bottom")
P4


# Extract all quantiles --------------------------------------------------------

d <- 0.1
q <- seq(d, 1-d, d)
allX <- FALSE
x_vals <- X

if (allX) {
  x_vals <- seq(0, 10, 0.001)
  predictions <- predict(fit, data=data.frame(X=x_vals))
}

quantiles <- qpred(predictions, quantiles = q)
nr <- floor(0.49/d)
colnames(quantiles) <- c(paste0("l", nr:1), "m", paste0("u", 1:nr))

lower <- data.frame(quantiles) %>% select(starts_with("l")) %>%
  mutate(X=x_vals) %>% pivot_longer(cols = -X, names_to = "Q") %>% arrange(Q, X)
P5 <- data.frame(quantiles) %>% select(starts_with("u")) %>%
  mutate(X=x_vals) %>%
  pivot_longer(cols=-X, names_to="Q", names_prefix="u", values_to="upper") %>%
  arrange(Q, X) %>% mutate(lower=lower$value) %>%
  ggplot(mapping = aes(x=X)) +
    geom_ribbon(mapping = aes(ymin=lower, ymax=upper, group=Q),
                alpha=d*3, fill="red", show.legend=FALSE) +
    geom_point(data=data.frame(X=X, Y=Y), mapping = aes(y=Y), color = "black") +
    ggtitle(label = "point cloud and estimated quantile areas") +
    theme_bw()
P5

cat("Fertig")


# plot for bachelor thesis =====================================================
P1 <- P1 +
  ggtitle("(a) Point Cloud of Training Data") +
  geom_vline(xintercept=predict_val, color=col_vec, size=1.3, alpha=0.5) +
  theme(text = element_text(size = 16), axis.text = element_text(size = 13))
P2 <- P2 + ylab("Probability") + xlab("Threshold") +
  ggtitle("(b) CDF Estimates of IDR") +
  theme(text = element_text(size = 16), axis.text = element_text(size = 13))
P3 <- P3 + ggtitle("(c) Predicted CDFs vs. True CDFs") +
  theme(text = element_text(size = 16), axis.text = element_text(size = 13))
#g eom_hline(yintercept=q, color=col_vec2[2:4], alpha=0.5) +
P4 <- P4 + ggtitle("(d) Functional Estimates of IDR ") +
  theme(text = element_text(size = 16), axis.text = element_text(size = 13),
        legend.position=c(0.01,0.99), legend.justification=c(0.01,0.99))
grid.arrange(P1, P2, P3, P4, nrow=4)
# svae with height 15.3


# Non isotonic example =========================================================
n <- 500
X <- runif(n, min = -5, max = 5)
Y <- rnorm(n, mean = abs(X), sd = (5.5 - abs(X)) / 5.5)

visualizeIDR(x=X, y=Y)
visualizeIDR(x=X, y=Y, pred=c(-4, -2, 0, 2, 4), print_all = FALSE)

groups <- c(X = 1)
orders <- c("comp" = 1)
fit <- idr(y=Y, X=data.frame(X=X), groups, orders)
predict_val <- c(-4, -2, 0, 2, 4)

# to compare predictions with true CDFs
idr_cdfs <- get_pred_df(fit, val=predict_val)

# determine true CDFs
true_cdfs <- data.frame()
grid <- seq(min(Y)-0.15*(max(Y)-min(Y)), max(Y)+0.15*(max(Y)-min(Y)), 0.1)
for(t in predict_val) {
  norm_dist <- data.frame(X=grid, value=pnorm(grid, mean=abs(t),
                                              sd = (5.5 - abs(t)) / 5.5),
                          cdfs=paste0("cdf",t), color=t)
  true_cdfs <- rbind(true_cdfs, norm_dist)
}
P3 <- ggplot(mapping = aes(x=X, y=value, group=cdfs, color=factor(color))) +
  geom_step(data=idr_cdfs, show.legend = FALSE) +
  geom_line(data=true_cdfs, linetype = "dashed", show.legend=FALSE) +
  ggtitle(label = paste0("true cdf (solid,continuous) vs. ",
                         "predicted cdf (dashed,stepfunction)")) +
  ylab("Probability") +
  xlab("Threshold") +
  theme_bw()

#scale_color_manual(values = col_vec) +

P3


P5_orig <- P5
predictions <- predict(fit, data=data.frame(X=X))
x_vals <- X
# now go to all quantiles and start plotting (after if) short before P5
P5_orig <- P5_orig +
  ggtitle("Isotonic Setting") +
  theme(text = element_text(size = 16), axis.text = element_text(size = 13))
P5 <- P5 +
  ggtitle("Non-Isotonic Setting") +
  theme(text = element_text(size = 16), axis.text = element_text(size = 13)) +
  ylab("")
grid.arrange(P5_orig, P5, nrow=1)
# save with height = 5

# old quantile plot ============================================================
library(stringr)      # to manipulate strings

data.frame(quantiles) %>% mutate(X=X) %>%
  pivot_longer(cols = -X, names_to = "Quantile") %>%
  mutate(strength = as.double(str_replace(Quantile, "q", ""))) %>%
  mutate(strength = 1 * (1 - strength) * strength) %>%
  ggplot(mapping = aes(x=X, y=value)) +
    geom_step(mapping = aes(group=Quantile, alpha=strength), color="red",
         show.legend=FALSE) +
    geom_point(data=data.frame(X=X, value=Y), color="black")

# test get_pred_df =============================================================
plot(predict(fit, data=data.frame(X=0)))
df <- get_pred_df(fit, 0)
n <- nrow(df)
y_vals <- c(rep(df$value[c(-n, -(n-1))], each=2), df$value[c(n-1, n-1)])
x_vals <- c(df$X[1], rep(df$X[c(-1, -n)], each=2), df$X[n])
lines(x_vals, y_vals, type="l", col="red")