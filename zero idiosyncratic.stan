
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
  array[I, K] real<lower=-0.99, upper=0.99> rho;
}

generated quantities {
  array[I, N] real x_rep;
  array[I, N] real y_rep;
  array[I, N] real log_lik;
  array[I, N, K] real w_prob;
  // Store the drawn weights for inspection if needed
  array[I, N, K] real w_drawn;

  for (i in 1:I) {
    for (n in 1:N) {
      // Draw fresh weights for THIS TRIAL from elicited theta
      vector[K] w = dirichlet_rng(to_vector(theta[i]));;
      
      vector[K] lps;

      for (k in 1:K) {
        // Prior weight (trial-specific) + likelihood
        real beta_x = beta_lpdf(x[i, n] | alphax[i, k], betax[i, k]);
        real beta_y = beta_lpdf(y[i, n] | alphay[i, k], betay[i, k]);
        
        lps[k] = log(w[k]) + beta_x + beta_y;
      }
      
      // Soft posterior probabilities
      vector[K] ps = softmax(lps);
      
      log_lik[i, n] = log_sum_exp(lps);
      w_prob[i, n] = to_array_1d(ps);
      w_drawn[i, n] = to_array_1d(w);  // Store for diagnostics
      
      // Posterior predictive
      int comp = categorical_rng(ps);
      x_rep[i, n] = beta_rng(alphax[i, comp], betax[i, comp]);
      y_rep[i, n] = beta_rng(alphay[i, comp], betay[i, comp]);
    }
  }
}
