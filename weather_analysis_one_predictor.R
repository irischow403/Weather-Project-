library(rstan)
library("dplyr")
library("tidyr")
library(tidyverse)
weather <- read_csv("weather2.csv")

y <- weather[,"codedPrecipitation"]
x <- weather[,"Temperature"]
M <- length(weather$X1) #Number of days in data set (total rows)

temp_vec <- rep(NA, times = M) #Stores temp values as numeric
for (m in 1:M) {
  temp_vec[m] <- weather$Temperature[m]
}
precipitation_vec <- rep(NA, times = M) #Stores coded precipitation values as numeric
for (m in 1:M) {
  precipitation_vec[m] <- weather$codedPrecipitation[m]
}


x_mean <- mean(temp_vec)
x_sd <- sd(temp_vec)
std_x <- (temp_vec - x_mean)/x_sd # standardize our predictor

## bounds were found with min(temp_vec) and max(temp_vec) , then by choosing a number slightly more extreme.
x_grid <- seq(-20, 90, by = 1) # create an equally spaced grid of potential degrees for temperature on any given day.
n_grid <- length(x_grid)

mu_std_alpha <- -0.53 #log-odds of probability of precipitation on any day (.37)
sigma_std_alpha <- 1    # have no prior knowledge or estimate of standard deviation to change from 1
mu_std_beta <- 0.41        # predicted the probability of rain as 68 degrees (one standard deviation  over the mean temperature,) to be .1 larger
# than the mean probablity of precipitation (p=.47) and took the log odds of that probability (log odds = -.12) and subtracted mu_std_alpha 
sigma_std_beta <- 1       # again, did not have prior knowledge to influence the sigma value

precip_data <- list(n = M, y = precipitation_vec, 
                std_x = std_x, x_mean = x_mean, x_sd = x_sd, n_grid = n_grid, x_grid = x_grid,
                mu_std_alpha = mu_std_alpha, sigma_std_alpha = sigma_std_alpha,
                mu_std_beta = mu_std_beta, sigma_std_beta = sigma_std_beta) #modify

logistic_model <- stan_model(file = "logistic_single_predictor.stan")

precip_fit <- sampling(object = logistic_model, 
                   data = precip_data)

# All of our R-hats look OK!
summary(precip_fit)[[1]][,"Rhat"]

std_alpha_samples <- rstan::extract(precip_fit, pars = "std_alpha")[["std_alpha"]]
alpha_samples <- rstan::extract(precip_fit, pars = "alpha")[["alpha"]]

std_beta_samples <- rstan::extract(precip_fit, pars = "std_beta")[["std_beta"]]
beta_samples <- rstan::extract(precip_fit, pars = "beta")[["beta"]]


png("posterior_parameters.png", width = 6, height = 6, units = "in", res = 300)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0), mfrow = c(2,2))

hist(std_alpha_samples, main = "std_alpha", breaks = 100, xlab = "std_alpha")
hist(alpha_samples, main = "alpha", breaks = 100, xlab = "alpha")
hist(std_beta_samples, main = "std_beta", breaks = 100, xlab = "std_beta")
hist(beta_samples, main = "beta", breaks = 100, xlab = "beta")
dev.off()

###
# Remember std_alpha is the log-odds of precipitation of an *average* day in temp.
# We can convert that to a probability
avg_precip_prob <- 1/(1 + exp(-1 * std_alpha_samples))
hist(avg_precip_prob, breaks = 100,
     main = "Posterior draws for prob of avg. temp", xlab = "Prob.")

#############
# How has our uncertainty about precipitation occurrence prob. changes as *function* of 
# temperature changed after we observed data?
# We can look at the posterior predictive probabilities at each point in x_grid
post_pred_grid <- rstan::extract(precip_fit, pars = "prob_grid")[["prob_grid"]]

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
plot(1, type= "n", xlim = c(-20, 90), ylim = c(0,1),
     main = "Posterior predictive precipitation success prob", xlab = "Temperature", ylab = "Prob.")
polygon(x = c(x_grid, rev(x_grid)), 
        y = c(post_pred_l95, rev(post_pred_u95)),
        col = rgb(0.9,0.9,0.9,1),
        border = NA)
lines(x_grid, post_pred_mean, lwd = 1.5)
legend("bottomleft", legend = c("Post. pred. mean", "95% uncertainty region"),
       pch = c(NA, 15), lty = c(1, NA), col = c("black", rgb(0.9, 0.9, 0.9,1)))
dev.off()
