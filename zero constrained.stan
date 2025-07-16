
data {
  int<lower=1> K;
  int<lower=1> N;
  int<lower=1> I;
  
  array[I, N] real<lower=0, upper=1> x;
  array[I, N] real<lower=0, upper=1> y;
  
  array[I, K] real<lower=0> alphax;
  array[I, K] real<lower=0> betax;
  array[I, K] real<lower=0> alphay;
  array[I, K] real<lower=0> betay;
  
  array[I] row_vector<lower=0.04>[K] theta;
}

parameters {
  row_stochastic_matrix[I, K] w;
}

model {
  // Prior for mixture weights
  for (i in 1:I) {
    w[i] ~ dirichlet(theta[i]);
  }

  matrix[I,K] log_w = log(w);
  
  // Likelihood calculation
  for (i in 1:I) {
    for (n in 1:N) {
      row_vector[K] lps = log(w[i]);
      
      for (k in 1:K) {
        // Beta component log-probs
        real beta_x = beta_lpdf(x[i, n] | alphax[i, k], betax[i, k]);
        real beta_y = beta_lpdf(y[i, n] | alphay[i, k], betay[i, k]);
        
        lps[k] += beta_x + beta_y;
      }
      target += log_sum_exp(lps);
    }
  }
}

generated quantities {
  array[I, N] real x_rep;
  array[I, N] real y_rep;
  array[I, N] real log_lik;
  array[I, N, K] real w_prob;

  for (i in 1:I) {
    for (n in 1:N) {
      vector[K] lps;
      vector[K] ps;

      for (k in 1:K) {
        // Component probabilities
        real beta_x = beta_lpdf(x[i, n] | alphax[i, k], betax[i, k]);
        real beta_y = beta_lpdf(y[i, n] | alphay[i, k], betay[i, k]);
        
        lps[k] = log(w[i, k]) + beta_x + beta_y;
      }
      
      // Normalize probabilities
      ps = softmax(lps);
      log_lik[i, n] = log_sum_exp(lps);
      w_prob[i, n] = to_array_1d(ps);
      
      // Generate posterior predictions
      int comp = categorical_rng(ps);
      x_rep[i, n] = beta_rng(alphax[i, comp], betax[i, comp]);
      y_rep[i, n] = beta_rng(alphay[i, comp], betay[i, comp]);
    }
  }
}
  
  
  