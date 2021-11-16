// draw from prior predictive distribution of P(Y = 1 | X = x)
// for all x in a grid of values (x_grid)
// for logistic regression
// NOTE: priors are specified on a standardized scale, so parameters must be
// rescaled back to the original scale of the problem
data{
  int<lower = 0> n_grid; // number of grid points
  vector[n_grid] x_grid;
  
  real x_mean; // mean of our single predictor
  real<lower = 0> x_sd; // sd of our single predictor
  
  real mu_std_alpha; // prior mean for intercept on standardized scale
  real<lower = 0> sigma_std_alpha; // prior sd for intercept on standardized scale
  real mu_std_beta; // prior mean for slopes
  real<lower = 0> sigma_std_beta;// prior sd for slope 
}
generated quantities{
  real std_alpha;
  real alpha;
  real offset;
  real std_beta;
  real beta;
  
  vector[n_grid] prob_grid;
  
  std_alpha = normal_rng(mu_std_alpha, sigma_std_alpha); // draw std_alpha from its prior
  std_beta = normal_rng(mu_std_beta, sigma_std_beta); // draw std_beta from its prior
  
  offset = std_beta * x_mean/x_sd;
  
  alpha = std_alpha - offset;
  beta = std_beta/x_sd;
  
  for(i in 1:n_grid){
    prob_grid[i] = inv_logit(alpha + x_grid[i] * beta);
  }
  
}
