library(dplyr)
library(tidyr)        # for pivot_longer
library(ggplot2)
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


data.frame(X=X, F=F, G=G) %>% pivot_longer(cols=-X, names_to="CDF") %>%
  ggplot() +
    geom_line(mapping = aes(x=X, y=value, color=CDF), show.legend = FALSE) +
    scale_color_manual(values = c("orange", "skyblue")) +
    annotate("text", x = -1.5, y = 0.75, label = "F ~ N(-1, 1)",
             color = "orange", size = 10) +
    annotate("text", x = 1.5, y = 0.25, label = "G ~ N(1, 1)",
             color = "skyblue", size = 10)

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
plot(predict(fit1, data.frame(X=1)))

visualizeIDR(c(1,2), c(1,2))
visualizeIDR(c(1,2), c(2,1))


visualizeIDR <- function(X, y) {
  n <- length(y)
  points <- paste0("p", 1:n)

  fit <- idr(y=y, X=data.frame(X=X))
  cdfs <- cbind(0, fit$cdf, 1)
  rownames(cdfs) <- points

  pointcloud <- data.frame(x=X, y=y, p=points) %>%
    ggplot() +
      geom_point(mapping = aes(x=x, y=y, color=p), show.legend = FALSE) +
      labs(title = "point cloud")

  m <- length(fit$thresholds)
  x_min <- fit$thresholds[1] - 0.15 * (fit$thresholds[m] - fit$thresholds[1])
  x_max <- fit$thresholds[m] + 0.15 * (fit$thresholds[m] - fit$thresholds[1])

  cdfs_plot <- data.frame(t(cdfs)) %>%
    mutate(X = c(x_min, fit$thresholds, x_max)) %>%
    pivot_longer(cols = -X, names_to = "cdfs") %>%
    ggplot() +
      geom_step(mapping = aes(x=X, y=value, color=cdfs), show.legend = FALSE) +
      labs(title = "CDFs") +
      ylab("") +
      xlab("")

  grid.arrange(pointcloud, cdfs_plot, ncol=2)
}