library(dplyr)
library(tidyr)        # for pivot_longer
library(ggplot2)
library(gridExtra)    # for multiple plots
library(stringr)      # to manipulate strings

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
#  (x_1,...,x_k) <= (z_1,...,z_k) => P(Y | X_1=x_1,...X_k=x_k) <= P(Y | X_1=z_1,...X_k=z_k)
#
#   What are these inequalities?
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

predict(fit1)

plot(predict(fit1, data=data.frame(X=1)), main="P(Y | X = 1)")
plot(predict(fit1, data=data.frame(X=2)), main="P(Y | X = 2)")



# Create long data frame containing with columns X, value, cdfs, color
# listing jumping points of a cdf belonging to a prediction in pred
get_pred_df <- function(fit, pred, pred_points, x_min, x_max) {
  predictions <- predict(fit, data=data.frame(X=pred))
  pred_cdfs <- data.frame()
   # fill data.frame by iterating through the predictions
   for (j in 1:length(pred)) {
     new_cdf <- data.frame(X=c(x_min, predictions[[j]][["points"]], x_max),
                           value=c(0, predictions[[j]][["cdf"]], 1),
                           cdfs=pred_points[j], color=pred[j])
     pred_cdfs <- rbind(pred_cdfs, new_cdf)
   }
  return(pred_cdfs)
}

# Method visualizing point cloud and estimated conditional distribution fct.
# Additionally via pred predicted conditdional distribution fct.s can also be
# added.
# if print_all equals False only predicted cdfs are drawn
visualizeIDR <- function(x, y, pred=numeric(0), print_all=TRUE) {
  points <- paste0("p", 1:length(y))

  fit <- idr(y=y, X=data.frame(X=x))
  cdfs <- cbind(0, fit$cdf, 1)
  rownames(cdfs) <- points

  pointcloud <- data.frame(x=x, y=y, p=points) %>%
    ggplot(mapping = aes(x=x, y=y, group=p, color=x)) +
      geom_point(show.legend = FALSE) +
      labs(title = "point cloud")

  m <- length(fit$thresholds)
  x_min <- fit$thresholds[1] - 0.15 * (fit$thresholds[m] - fit$thresholds[1])
  x_max <- fit$thresholds[m] + 0.15 * (fit$thresholds[m] - fit$thresholds[1])

  if (print_all) {
    cdfs_plot <- data.frame(t(cdfs)) %>%
      mutate(X = c(x_min, fit$thresholds, x_max)) %>%
      pivot_longer(cols = -X, names_to = "cdfs") %>%
      mutate(color = x[strtoi(str_replace(cdfs, "p", ""))]) %>%
      ggplot(mapping = aes(x=X, y=value, group=cdfs, color=color)) +
        geom_step(show.legend = FALSE) +
        labs(title = "CDFs") +
        ylab("") +
        xlab("")
  } else {
    cdfs_plot <- ggplot(mapping = aes(x=X, y=value, group=cdfs, color=color))
  }

  # if there are predictions, draw them also in the graphs
  s <- length(pred)
  if (s > 0) {
    y_max <- max(y)
    y_min <- min(y)

    pred_points <- paste0("p", length(y)+1:s)
    # extend points clouds by dashed vertical lines
    vert_lines <- data.frame(x=rep(pred, each=2), y=rep(c(y_min, y_max), s),
                          p=rep(pred_points, each=2))
    pointcloud <- pointcloud +
      geom_line(data=vert_lines, show.legend=FALSE, linetype = "dashed")
    # draw predicted cdfs to the cdf graph
    pred_cdfs <- get_pred_df(fit, pred, pred_points, x_min, x_max)
    cdfs_plot <- cdfs_plot +
      geom_step(data=pred_cdfs, linetype = "dashed", show.legend = FALSE)
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


# And now a more complex example (from the simulation study in the IDR paper)

n <- 600
X <- runif(n, 0, 10)     # X ~ U(0, 10)
Y <- rgamma(n, sqrt(X), scale=pmin(pmax(1, X), 6))

# check stochastic dominance
grid <- seq(0, 40, 0.1)
plot_stochDom(X=grid , F=pgamma(grid, sqrt(2), scale=2),
              G=pgamma(grid, sqrt(4), scale = 4))

visualizeIDR(x=X, y=Y)

visualizeIDR(x=X, y=Y, pred=c(0.5, 3, 5, 7, 9.5), print_all=FALSE)