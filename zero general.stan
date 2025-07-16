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
  row_vector<lower=0>[K] theta; // Dirichlet prior parameters for each participant
}

parameters {
  row_stochastic_matrix[I, K] w;
}

model {
  // Prior for mixture weights
  for (i in 1:I) {
    w[i] ~ dirichlet(theta);
  }

  matrix[I,K] log_w = log(w);
  
  // Likelihood calculation
  for (i in 1:I) {
    for (n in 1:N) {
      row_vector[K] lps = log(w[i]);

      for (k in 1:K) {
        real beta_x = beta_lpdf(x[i, n] | alphax[k], betax[k]);
        real beta_y = beta_lpdf(y[i, n] | alphay[k], betay[k]);
        real u_x = beta_cdf(x[i, n] | alphax[k], betax[k]);
        real u_y = beta_cdf(y[i, n] | alphay[k], betay[k]);
        
        lps[k] += beta_x + beta_y;
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
        
        lps[k] = log(w[i, k]) + beta_x + beta_y;
      }
      
      // Normalize probabilities
      ps = softmax(lps);
      log_lik[i, n] = log_sum_exp(lps);
      w_prob[i, n] = to_array_1d(ps);

      // Sample a component
      int comp = categorical_rng(ps);
      x_rep[i, n] = beta_rng(alphax[comp], betax[comp]); // Generate new x data
      y_rep[i, n] = beta_rng(alphay[comp], betay[comp]); // Generate new y data
    }
  }
}


