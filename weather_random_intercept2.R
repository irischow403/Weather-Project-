library(rstan)
library("dplyr")
library("tidyr")
library(tidyverse)
weather <- read_csv("weather2.csv")

y <- weather[,"codedPrecipitation"]
x <- weather[,"Temperature"]
M <- length(weather$X1) #Number of days in data set (total rows)

temp_vec <-  weather$Temperature
precipitation_vec <- weather$codedPrecipitation

x_mean <- mean(temp_vec)
x_sd <- sd(temp_vec)
std_x <- (temp_vec - x_mean)/x_sd # standardize our predictor

## bounds were found with min(temp_vec) and max(temp_vec) , then by choosing a number slightly more extreme.
x_grid <- seq(-20, 90, by = 1) # create an equally spaced grid of potential degrees for temperature on any given day.
n_grid <- length(x_grid)

## What about a hierarchical intercept in our weather models?
weather[,"month2"] <- factor(weather$month)
month_names <- levels(weather$month2)
weather[,"months_num"] <- as.numeric(unlist(weather[,"month2"])) #number the months sequentially
month <- pull(weather[,"month2"])


J <- length(month_names) # ensure that there are only 12 months in our dataset


mu_std_alpha <- -0.53   #log-odds probability of precipitation on any day (.37)
sigma_std_alpha <- 1    # have no prior knowledge or estimate of standard deviation to change from 1
mu_std_beta <- 0.41     # predicted the prob. of rain as 68 degrees (one standard deviation more than mean temp,) to be .1 larger
                        # than the mean prob. of precipitation (p=.47) and took the log odds of that probability (log odds = -.12) and subtracted mu_std_alpha 
sigma_std_beta <- 1     # again, did not have prior knowledge to influence the sigma value.
nu <- 7                 #?
A <- 1                  #?


hier_model <- stan_model(file = "logistic_single_predictor_random_intercept.stan")

precip_data <- list(n = M, J = J, y = precipitation_vec,
                    std_x = std_x, group_id = as.numeric(month), x_mean = x_mean,
                    x_sd = x_sd, n_grid = n_grid, x_grid = x_grid, 
                    mu_std_alpha = mu_std_alpha, sigma_std_alpha = sigma_std_alpha,
                    nu = nu, A = A, mu_std_beta = mu_std_beta, sigma_std_beta = sigma_std_beta)

# We don't want to save our samples of the auxiliary parameters
# or the std_beta parameters
# To tell Stan which samples to save, we use the pars argument
hier_fit <- sampling(object = hier_model, 
                     data = precip_data,
                     pars = c("std_alpha", "alpha", "beta", "prob_grid"),
                     iter = 500, chains = 2
                     ) 
# we have tons and tons of parameters so instead of printing out all of our Rhats
# we will look at the range of Rhats

range(summary(hier_fit)[[1]][,"Rhat"])


std_alpha_samples <- rstan::extract(hier_fit, pars = "std_alpha")[["std_alpha"]]
#std_alpha_samples is a 4000 x J matrix
colnames(std_alpha_samples) <- month_names

png("month_baseline_probs.png", width = 9, height = 4, units = "in", res = 400)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
boxplot(1/(1 + exp(-1 * std_alpha_samples)), pch = 16, cex = 0.5, medlwd = 0.5,
        main = "Posteriors of month's baseline precip. probs. ")
dev.off()

post_pred_samples <- rstan::extract(hier_fit, pars = "prob_grid")[["prob_grid"]]

# post_pred_samples is a 3 dimension array!
# 1st dimension indexes posterior sample
# 2nd dimension indexes the month (i.e. the groups)
# 3rd dimension indexes the grid point
dim(post_pred_samples)

# For readability (and ability to subset quickly), let's add some names
# to the 3rd dimension of post_pred_samples
dimnames(post_pred_samples)[[2]] <- c(month_names, "new_month") 

### Ended here ###
# Let's look at 3 kickers: Dan Bailey, Nick Folk, and the new kicker
bailey_grid_samples <- post_pred_samples[,"Bailey",]
folk_grid_samples <- post_pred_samples[,"Folk",]
new_kicker_grid_samples <- post_pred_samples[,"new_kicker",]

bailey_mean <- apply(bailey_grid_samples, MAR = 2, FUN = mean)
bailey_l95 <- apply(bailey_grid_samples, MAR = 2, FUN = quantile, probs = 0.025)
bailey_u95 <- apply(bailey_grid_samples, MAR = 2, FUN = quantile, probs = 0.975)

folk_mean <- apply(folk_grid_samples, MAR = 2, FUN = mean)
folk_l95 <- apply(folk_grid_samples, MAR = 2, FUN = quantile, probs = 0.025)
folk_u95 <- apply(folk_grid_samples, MAR = 2, FUN = quantile, probs = 0.975)

new_kicker_mean <- apply(new_kicker_grid_samples, MAR = 2, FUN = mean)
new_kicker_l95 <- apply(new_kicker_grid_samples, MAR = 2, FUN = quantile, probs = 0.025)
new_kicker_u95 <- apply(new_kicker_grid_samples, MAR = 2, FUN = quantile, probs = 0.975)

png("post_pred_random_intercept.png", width = 9, height = 4, units = "in", res = 400)
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0), mfrow = c(1,2))
plot(1, type= "n", xlim = c(15, 70), ylim = c(0,1),
     main = "Posterior predictive FG success prob", xlab = "Distance", ylab = "Prob.")
polygon(x = c(x_grid, rev(x_grid)), 
        y = c(bailey_l95, rev(bailey_u95)),
        col = rgb(1, 0, 0, 1/4),
        border = NA)
polygon(x = c(x_grid, rev(x_grid)), 
        y = c(folk_l95, rev(folk_u95)),
        col = rgb(0, 1, 0, 1/4),
        border = NA)
lines(x = x_grid, y = bailey_mean, col = 'red', lwd = 2)
lines(x = x_grid, y = folk_mean, col = 'green', lwd = 2)
legend("bottomleft", legend = c("Dan Bailey", "Nick Folk"),
       col = c("red", "green"), lty = 1)

plot(1, type= "n", xlim = c(15, 70), ylim = c(0,1),
     main = "Posterior predictive FG success prob", xlab = "Distance", ylab = "Prob.")
polygon(x = c(x_grid, rev(x_grid)), 
        y = c(new_kicker_l95, rev(new_kicker_u95)),
        col = rgb(0, 0, 1, 1/4),
        border = NA)
lines(x = x_grid, y = new_kicker_mean, col = 'blue', lwd = 2)
legend("bottomleft", legend = c("New kicker"),
       col = c("blue"), lty = 1)
dev.off()
