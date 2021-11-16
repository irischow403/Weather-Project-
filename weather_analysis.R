library(rstan)
library("dplyr")
library("tidyr")
library(tidyverse)
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
std_x <- (e_vec - x_mean)/x_sd # standardize our predictor

x_grid <- seq(15, 75, by = 1) # create an equally space grid of potential distances for FG attempts
n_grid <- length(x_grid)

mu_std_alpha <- 1.5
sigma_std_alpha <- 1
mu_std_beta <- -3
sigma_std_beta <- 0.25

fg_data <- list(n = n, y = y, std_x = std_x, x_mean = x_mean, x_sd = x_sd,
                n_grid = n_grid, x_grid = x_grid,
                mu_std_alpha = mu_std_alpha, sigma_std_alpha = sigma_std_alpha,
                mu_std_beta = mu_std_beta, sigma_std_beta = sigma_std_beta)

logistic_model <- stan_model(file = "logistic_single_predictor.stan")

fg_fit <- sampling(object = logistic_model, 
                   data = fg_data)

# All of our R-hats look OK!
summary(fg_fit)[[1]][,"Rhat"]

std_alpha_samples <- rstan::extract(fg_fit, pars = "std_alpha")[["std_alpha"]]
alpha_samples <- rstan::extract(fg_fit, pars = "alpha")[["alpha"]]

std_beta_samples <- rstan::extract(fg_fit, pars = "std_beta")[["std_beta"]]
beta_samples <- rstan::extract(fg_fit, pars = "beta")[["beta"]]


png("posterior_parameters_weather.png", width = 6, height = 6, units = "in", res = 300)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0), mfrow = c(2,2))

hist(std_alpha_samples, main = "std_alpha", breaks = 100, xlab = "std_alpha")
hist(alpha_samples, main = "alpha", breaks = 100, xlab = "alpha")
hist(std_beta_samples, main = "std_beta", breaks = 100, xlab = "std_beta")
hist(beta_samples, main = "beta", breaks = 100, xlab = "beta")
dev.off()


###
# Remember std_alpha is the log-odds of success of an *average* kick
# We can convert that to a probability
avg_kick_prob <- 1/(1 + exp(-1 * std_alpha_samples))
hist(avg_kick_prob, breaks = 100,
     main = "Posterior draws for prob of avg. kick", xlab = "Prob.")

#############
# How has our uncertainty about how FG success prob. changes as *function* of 
# distance changed after we observed data?
# We can look at the posterior predictive probabilities at each point in x_grid
post_pred_grid <- extract(fg_fit, pars = "prob_grid")[["prob_grid"]]

png("post_pred_400draw_weather.png", width = 9, height= 4, units = "in", res = 400)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", xlim = c(15, 75), ylim = c(0,1),
     xlab = "x", ylab = "Prob", main = "Posterior predictive precipitation success prob.")
for(i in 1:4000){
  lines(x_grid, post_pred_grid[i,], col = 'gray', lwd = 0.2)
}
dev.off()


post_pred_mean <- apply(post_pred_grid, MARGIN = 2, FUN = mean)
post_pred_l95 <- apply(post_pred_grid, MARGIN = 2, FUN = quantile, probs = 0.025)
post_pred_u95 <- apply(post_pred_grid, MARGIN = 2, FUN = quantile, probs = 0.975)


png("post_pred_mean_quantile.png", width = 9, height = 4, units = "in", res = 300)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type= "n", xlim = c(15, 70), ylim = c(0,1),
     main = "Posterior predictive FG success prob", xlab = "Distance", ylab = "Prob.")
polygon(x = c(x_grid, rev(x_grid)), 
        y = c(post_pred_l95, rev(post_pred_u95)),
        col = rgb(0.9,0.9,0.9,1),
        border = NA)
lines(x_grid, post_pred_mean, lwd = 1.5)
legend("bottomleft", legend = c("Post. pred. mean", "95% uncertainty region"),
       pch = c(NA, 15), lty = c(1, NA), col = c("black", rgb(0.9, 0.9, 0.9,1)))
dev.off()
