library(invgamma)
library(MASS)
library(xtable)
set.seed(1)

#############################################################################
###########   0 - Simulate Data
#############################################################################
# beta_0 hyper parameters (known)
m0 <- 5
t0 <- 1

m1 <- 5
t1 <- 1

a <- .5 # shape
g <- .7 # scale

n <- 100
x <- rnorm(n, 0, 1)

tphi <- rinvgamma(1, shape = a, rate = g)
tb0 <- rnorm(1, m0, sqrt(t0))
tb1 <- rnorm(1, m1, sqrt(t1))
tphi
tb0
tb1
y <- rnorm(n, tb0 + tb1 * x, sqrt(tphi))

#############################################################################
###########   1 - Functions for grid evaluation of posterior densities
#############################################################################
rb0cond <- function(y, x, b1, phi, t0, m0) {
  grid <- seq(-10, 10, .001)

  p <- numeric(length = length(grid))
  for (i in 1:length(p)) {
    p[i] <- (-(1 / (2 * phi)) * sum((y - (grid[i] + b1 * x))^2)) + (-(1 / (2 * t0)) * (grid[i] - m0)^2)
  }

  draw <- sample(grid, size = 1, prob = exp(1 - p / max(p)))
  return(draw)
}

rb1cond <- function(y, x, phi, t1, m1, b0) {
  grid <- seq(-10, 10, .001)

  p <- numeric(length = length(grid))
  for (i in 1:length(p)) {
    p[i] <- (-(1 / (2 * phi)) * sum((y - (b0 + grid[i] * x))^2)) + (-(1 / (2 * t1)) * (grid[i] - m1)^2)
  }

  draw <- sample(grid, size = 1, prob = exp(1 - p / max(p)))
  return(draw)
}

#############################################################################
###########   2 -Implement Gibbs Sampling
#############################################################################

iter <- 1000
burnin <- 101
phi <- b0 <- b1 <- numeric(iter)
phi[1] <- b0[1] <- b1[1] <- 6

for (i in 2:iter) {
  phi[i] <- rinvgamma(1, shape = (n / 2 + a), rate = .5 * sum((y - (b0[i - 1] + b1[i - 1] * x))^2) + g)
  b0[i] <- rb0cond(y, x, b1[i - 1], phi[i], t0, m0)
  b1[i] <- rb1cond(y, x, phi[i], t1, m1, b0[i])
}

#############################################################################
###########   3 - Visualize Results
#############################################################################

par(mfrow = c(2, 2))
plot(phi[burnin:iter], type = "l")
abline(h = tphi, col = "red")
plot(b0[burnin:iter], type = "l")
abline(h = tb0, col = "red")
plot(b1[burnin:iter], type = "l")
abline(h = tb1, col = "red")

z <- kde2d(b0, b1, n = 50)
plot(b0, b1, pch = 19, cex = .4)
contour(z, drawlabels = FALSE, nlevels = 10, col = "red", add = TRUE)

gibbs_sampler <- function(mdl, alpha = 0.001, beta = 0.001, niter = 5000) {
  x <- mdl$y[1:length(mdl$y) - 1]
  y <- mdl$y[2:length(mdl$y)]
  tau.b <- mdl$tau.b

  n <- length(y)
  a <- mean(y)
  b <- 0
  tau <- 1
  result <- matrix(nrow = niter, ncol = 3)
  for (i in 1:niter) {
    mu0 <- runif(1, min = mdl$mu0[1], max = mdl$mu0[2])
    b <- rnorm(1, mean = (tau * sum((y - mu0) * x)) / (tau * sum(x^2) + tau.b), sd = 1 / sqrt(tau * sum(x^2 + tau.b)))
    tau <- rgamma(1, shape = alpha + n / 2, rate = beta + 0.5 * sum((y - mu0 - b * x)^2))
    result[i, ] <- c(mu0, b, tau)
  }
  return(result)
}

data2 <- data.frame(growth = c(12, 10, 8, 11, 6, 7), tainin = 0:5)

mdl <- list(
  "mu0" = c(0, 1),
  "tau.b" = 0.001,
  "y" = data2[, 2]
)
growth.lm <- gibbs_sampler(mdl = mdl, niter = 10000)
growth.lm <- growth.lm[-(1:2000), ]
colSums(growth.lm) / 8000
y <- data2[, 2]
x <- data2[, 1]
y - x
sum(y - x)
data2[, 1]
x <- y[1:length(y) - 1]
y <- y[2:length(y)]