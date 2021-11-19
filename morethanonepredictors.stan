// Stan code for a basic logistic regression model with a single predictor
// Note that it assumes that the predictors have been standardized to have mean 0 and 
// standard deviation 1

data{
  int<lower = 0> n; // number of data points
  int<lower = 0, upper = 1> y[n]; // array of observed 0-1 outcomes
  vector[n] std_x1; // vector of standardized predictors
  vector[n] std_x2;
  
  real x_mean; // mean of our single predictor (for re-scaling at the end) //diff mean and var
  real<lower = 0> x_sd; // standard deviations of our single predictor (for re-scaling at the end)
  
  int n_grid; // number of points in the grid of x values at which we want to evaluate P(y = 1|x)
  vector[n_grid] x_grid1;//Jan
  vector[n_grid] x_grid2;//Feb

  real mu_std_alpha; // prior mean for intercept on standardized scale
  real<lower = 0> sigma_std_alpha; // prior sd for intercept on standardized scale
  real mu_std_beta1; // prior mean for slopes
  real mu_std_beta2;
  real<lower = 0> sigma_std_beta1;// prior sd for slope 
  real<lower = 0> sigma_std_beta2;
 
}
parameters{
  real std_alpha; // intercept (on standardized scale)
  real std_beta1; // slopes (on standardized scale)
  real std_beta2;
  real std_beta3;
  real std_beta4;
}

model{
  std_alpha ~ normal(mu_std_alpha, sigma_std_alpha);
  std_beta1 ~ normal(mu_std_beta1, sigma_std_beta1);
  std_beta2 ~ normal(mu_std_beta2, sigma_std_beta2);
  for(i in 1:n){
    y[i] ~ bernoulli_logit(std_alpha + std_beta1 * std_x1[i] + std_beta2 * std_x2[i]);
  }
}
generated quantities{
  real offset; // we have to adjust the intercept back to the original scale
  real alpha;
  real beta1;
  real beta2;
  vector<lower = 0, upper = 1>[n_grid] prob_grid; // prob. that y = 1 at each grid point
  
  offset = (std_beta1+std_beta2)/x_sd * x_mean;
  alpha = std_alpha - offset;
  beta1 = std_beta1/x_sd;
  beta2 = std_beta1/x_sd;
  
  
  for(i in 1:n_grid){
    prob_grid[i] = inv_logit(alpha + x_grid1[i] * beta1 + x_grid2[i] *beta2);
  }
  
}


