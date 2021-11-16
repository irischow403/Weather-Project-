library(rstan)
library("dplyr")
library("tidyr")
library(tidyverse)
library("fastDummies")
weather <- read_csv("weather2.csv")

y <- weather[,"codedPrecipitation"]
x <- weather[,"Temperature"]
M <- length(weather)
e_vec <- rep(NA, times = M)
  
for (m in 1:M) 
{
  e_vec[m] <- weather$Temperature[m]
}
  
n <- length(y)

x_mean <- mean(e_vec)
x_sd <- sd(e_vec)
std_x <- (x - x_mean)/x_sd # standardize our predictor

x_grid <- seq(15, 75, by = 1) # create an equally space grid of potential distances for FG attempts
n_grid <- length(x_grid)

prior_pred_model <- stan_model(file = "weather_single.stan")

mu_std_alpha <- 1.5
sigma_std_alpha <- 1
mu_std_beta <- -3
sigma_std_beta <- 0.25

sim1 <- sampling(object = prior_pred_model, algorithm = "Fixed_param",
                 data = list(n_grid = n_grid, x_grid = x_grid, 
                             x_mean = x_mean,x_sd = x_sd,
                             mu_std_alpha = mu_std_alpha, sigma_std_alpha = sigma_std_alpha, 
                             mu_std_beta = mu_std_beta, sigma_std_beta = sigma_std_beta))
prob_grid1 <- rstan::extract(sim1, par = c("prob_grid"))[["prob_grid"]]

# prob_grid1 is a matrix with rows indexing the sample and columns indexing the points in x_grid
# the k-th column of prob_grid1 records the samples of probability of success at the k-th grid point in x_grid


png("prior_pred_weather.png", width = 9, height= 4, units = "in", res = 400)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", xlim = c(0, 100), ylim = c(0,1),
     xlab = "x", ylab = "Prob", main = "Prior predictive Precipitation success prob.")
lines(x_grid, prob_grid1[1,], col = 'gray')
dev.off()

png("prior_pred_10draw_weather.png", width = 9, height= 4, units = "in", res = 400)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", xlim = c(0, 100), ylim = c(0,1),
     xlab = "x", ylab = "Prob", main = "Prior predictive precipitation success prob.")
for(i in 1:10){
  lines(x_grid, prob_grid1[i,], col = 'gray')
}
dev.off()

png("prior_pred_100draw_weather.png", width = 9, height= 4, units = "in", res = 400)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", xlim = c(0, 100), ylim = c(0,1),
     xlab = "x", ylab = "Prob", main = "Prior predictive precipitation success prob.")
for(i in 1:100){
  lines(x_grid, prob_grid1[i,], col = 'gray')
}
dev.off()

png("prior_pred_4000draw_weather.png", width = 9, height= 4, units = "in", res = 400)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", xlim = c(0, 100), ylim = c(0,1),
     xlab = "x", ylab = "Prob", main = "Prior predictive precipitation success prob.")
for(i in 1:4000){
  lines(x_grid, prob_grid1[i,], col = 'gray', lwd = 0.1)
}
dev.off()

# Plotting all of the draws can get messy
# What if we instead plot the prior predictive mean & quantiles at every point
# remember columns of prob_grid1 correspond to each point in the grid
# we want to compute the mean, 2.5% quantile and 97.5% quantile of each column

prior_pred_mean <- apply(prob_grid1, MARGIN = 2, FUN = mean) # computes mean of each column of prob_grid1
prior_pred_l95 <- apply(prob_grid1, MARGIN = 2, FUN = quantile, probs = c(0.025))
prior_pred_u95 <- apply(prob_grid1, MARGIN = 2, FUN = quantile, probs = c(0.975))

png("prior_pred_mean_quantile_weather.png", width = 9, height = 4, units = "in", res = 300)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type= "n", xlim = c(0, 100), ylim = c(0,1),
     main = "Prior predictive precipitation success prob", xlab = "Temperature", ylab = "Prob.")
polygon(x = c(x_grid, rev(x_grid)), 
        y = c(prior_pred_l95, rev(prior_pred_u95)),
        col = rgb(0.9,0.9,0.9,1),
        border = NA)
lines(x_grid, prior_pred_mean, lwd = 1.5)
legend("bottomleft", legend = c("Prior pred. mean", "95% uncertainty region"),
       pch = c(NA, 15), lty = c(1, NA), col = c("black", rgb(0.9, 0.9, 0.9,1)))
dev.off()

