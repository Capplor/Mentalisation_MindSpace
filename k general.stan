functions {

  vector bounded_softmax(vector logits, real ps_min, real ps_max) {
    int K = num_elements(logits);
    vector[K] ps = softmax(logits);
    
    // Linearly rescale to [ps_min, ps_max] while preserving relative probabilities
    real min_ps = min(ps);
    real max_ps = max(ps);
    if (max_ps - min_ps > 0) {
      ps = ps_min + (ps_max - ps_min) * (ps - min_ps) / (max_ps - min_ps);
    }
    ps = ps / sum(ps);  // Renormalize to sum to 1
    return ps;
  }
  
  
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
  array[K] real<lower=-1, upper=1> rho; // Correlation parameters for each participant and component
}

transformed data {
  real ps_min = 0.01;
  real ps_max = 0.96;
}

parameters {
  row_stochastic_matrix[I, K] w; // Base simplex for mixture weights
}


model {
  // Priors for v (implied for w)
  for (i in 1:I) {
    w[i] ~ dirichlet(theta);
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
        real log_copula = gaussian_copula_density(u_x, u_y , rho[k]);
        
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

        real log_copula = gaussian_copula_density(u_x, u_y , rho[k]);
        
        lps[k] = log(w[i, k]) + beta_x + beta_y + log_copula;
      }
      
      // Normalize probabilities
      ps = softmax(lps);
      log_lik[i, n] = log_sum_exp(lps);
      w_prob[i, n] = to_array_1d(bounded_softmax(lps,ps_min, ps_max));
      
      // Generate posterior predictions
      int comp = categorical_rng(ps);
      x_rep[i, n] = beta_rng(alphax[comp], betax[comp]);
      y_rep[i, n] = beta_rng(alphay[comp], betay[comp]);
    }
  }
}










