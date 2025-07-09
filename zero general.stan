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


