functions {
  real gaussian_copula_density(real u_x, real u_y, real rho) {
    // Numerical stabilization parameters
    real epsilon = 0.01;
    
    // Safely clamp u values to (epsilon, 1-epsilon) with gradient preservation
    real clamped_u_x = u_x * (1 - 2*epsilon) + epsilon;
    real clamped_u_y = u_y * (1 - 2*epsilon) + epsilon;
    
    // Stable inverse normal approximation with bounded outputs
    real z_x = inv_Phi(clamped_u_x);
    real z_y = inv_Phi(clamped_u_y);

    // Quadratic form computation with stabilized denominator
    real rho_sq = square(rho);
    real inv_one_m_rho_sq = 1 - rho_sq;
    
    return -0.5 * log(2 * pi())
           - 0.5 * log1m(rho_sq)
           - 0.5 * inv_one_m_rho_sq * (z_x*z_x + z_y*z_y - 2*rho*z_x*z_y);
  }
}

data {
  int<lower=1> K; // number of mixture components
  int<lower=1> N; // number of data points
  int<lower=1> I; // number of individuals
  array[I, N] real<lower=0> y; // observations
  array[I, N] real<lower=0> x; // observations
  array[K] real<lower=0> alphax;
  array[K] real<lower=0> betax;
  array[K] real<lower=0> alphay;
  array[K] real<lower=0> betay;
  row_vector<lower=0>[K] theta;; // Dirichlet prior parameters for each participant
  int<lower=-1, upper=1> rho; // Correlation parameters for each participant and component
}

transformed data {
  real p_min = 1e-10;
  real p_mult = 1 - K * p_min;
}

parameters {
  row_stochastic_matrix[I, K] v; // Base simplex for mixture weights
}

transformed parameters {
  row_stochastic_matrix[I, K] w = p_min + p_mult * v; // Adjusted mixture weights
}

model {
  // Priors for v (implied for w)
  for (i in 1:I) {
    v[i] ~ dirichlet(theta);
  }

  // Likelihood
  for (i in 1:I) {
    for (n in 1:N) {
      row_vector[K] log_w = log(w[i]);
      row_vector[K] lps = log_w;

      for (k in 1:K) {
        real beta_x = beta_lpdf(x[i, n] | alphax[k], betax[k]);
        real beta_y = beta_lpdf(y[i, n] | alphay[k], betay[k]);
        real u_x = beta_cdf(x[i, n] | alphax[k], betax[k]);
        real u_y = beta_cdf(y[i, n] | alphay[k], betay[k]);
        real log_copula = gaussian_copula_density(u_x, u_y , rho);
        
        lps[k] += beta_x + beta_y + log_copula;
      }
      target += log_sum_exp(lps);
  }
}
}

generated quantities {
  array[I, N] real x_rep;
  array[I, N] real y_rep;
  array[I, N] real log_lik; // Log-likelihood for each observation
  array[I, N, K] real w_prob; // Mixing probabilities for each participant, trial, and cluster

  for (i in 1:I) {
    for (n in 1:N) {
      vector[K] ps;
      vector[K] lps;

      for (k in 1:K) {
        real beta_x = beta_lpdf(x[i, n] | alphax[k], betax[k]);
        real beta_y = beta_lpdf(y[i, n] | alphay[k], betay[k]);
        real u_x = beta_cdf(x[i, n] | alphax[k], betax[k]);
        real u_y = beta_cdf(y[i, n] | alphay[k], betay[k]);

        real log_copula = gaussian_copula_density(u_x, u_y , rho);
        
        lps[k] = log(w[i, k]) + beta_x + beta_y + log_copula;
      }
      
      // Normalize probabilities
      ps = softmax(lps);
      log_lik[i, n] = log_sum_exp(lps);

      // Save the mixing probabilities for each cluster k in each trial n for participant i
      for (k in 1:K) {
        w_prob[i, n, k] = ps[k]; // Storing the normalized mixing probability for the k-th cluster
      }

      // Sample a component
      int comp = categorical_rng(ps);
      x_rep[i, n] = beta_rng(alphax[comp], betax[comp]); // Generate new x data
      y_rep[i, n] = beta_rng(alphay[comp], betay[comp]); // Generate new y data
    }
  }
}



