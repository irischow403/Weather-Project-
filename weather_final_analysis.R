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

mu_std_alpha <- -0.53   # Log-odds probability of precipitation on any day (.37)
sigma_std_alpha <- 1    # Have no prior knowledge or estimate of standard 
                        #  deviation to change from 1
mu_std_beta <- 0.41     # Predicted the prob. of rain as 68 degrees (one 
                        #  standard deviation more than mean temp,) to be .1 larger
                        #  than the mean prob. of precipitation (p=.47) and took 
                        #  the log odds of that probability (log odds = -.12)  
                        #  and subtracted mu_std_alpha 
sigma_std_beta <- 1     # Again, did not have prior knowledge to influence the 
                        #  sigma value.

################################################################################
## Logistic Single Predictor ##
################################################################################
precip_data <- list(n = M, 
                    y = precipitation_vec,
                    std_x = std_x,
                    x_mean = x_mean,
                    x_sd = x_sd,
                    n_grid = n_grid,
                    x_grid = x_grid, 
                    mu_std_alpha = mu_std_alpha, 
                    sigma_std_alpha = sigma_std_alpha,
                    mu_std_beta = mu_std_beta, 
                    sigma_std_beta = sigma_std_beta)

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

################################################################################
## Random Intercept ##
################################################################################
nu <- 7
A <- 1

## What about a hierarchical intercept in our weather models?
weather[,"month2"] <- factor(weather$month)         #integer values for month
month_names <- levels(weather$month2)               #character entries for 12 lvls
weather[,"months_num"] <- as.numeric(unlist(weather[,"month2"])) 
month <- pull(weather[,"month2"])

J <- length(month_names)# Ensure that there are only 12 months in our data set

hier_model <- stan_model(file = "logistic_single_predictor_random_intercept.stan")

precip_data <- list(n = M, 
                    J = J, 
                    y = precipitation_vec,
                    std_x = std_x,
                    group_id = as.numeric(month),
                    x_mean = x_mean,
                    x_sd = x_sd, 
                    n_grid = n_grid, 
                    x_grid = x_grid, 
                    mu_std_alpha = mu_std_alpha,
                    sigma_std_alpha = sigma_std_alpha,
                    mu_std_beta = mu_std_beta,
                    sigma_std_beta = sigma_std_beta,
                    nu = nu,
                    A = A)

# We don't want to save our samples of the auxiliary parameters
# or the std_beta parameters
# To tell Stan which samples to save, we use the pars argument
hier_fit <- sampling(object = hier_model, 
                     data = precip_data,
                     pars = c("std_alpha", "alpha", "beta", "prob_grid"),
                     iter = 2000, chains = 4
)
# we have tons and tons of parameters so instead of printing out all of our Rhats
# we will look at the range of Rhats

range(summary(hier_fit)[[1]][,"Rhat"])


std_alpha_samples <- rstan::extract(hier_fit, pars = "std_alpha")[["std_alpha"]]
#std_alpha_samples is a 4000 x J matrix
colnames(std_alpha_samples) <- c("Jan","Feb","Mar","Apr","May","June",
                                 "July","Aug","Sep","Oct","Nov","Dec")

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

