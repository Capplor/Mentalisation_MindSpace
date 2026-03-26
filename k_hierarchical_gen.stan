functions {
  real inv_Phi_approx(real u) {
    // Approximate inverse CDF of standard normal using scaled logit
    return (1.0 / 1.702) * logit(u);
  }

  real gaussian_copula_density(real u_x, real u_y, real rho) {
    // Numerical stabilization parameters
    real epsilon = 1e-6;

    // Clamp u values to (epsilon, 1-epsilon)
    real clamped_u_x = u_x * (1 - 2 * epsilon) + epsilon;
    real clamped_u_y = u_y * (1 - 2 * epsilon) + epsilon;

    // Approximate inverse normal CDF
    real z_x = inv_Phi_approx(clamped_u_x);
    real z_y = inv_Phi_approx(clamped_u_y);

    // Quadratic form for Gaussian copula
    real rho_sq = square(rho);
    real one_m_rho_sq = 1 - rho_sq;

    return -0.5 * log1m(rho_sq)
           - 0.5 * (square(z_x) + square(z_y) - 2 * rho * z_x * z_y) / one_m_rho_sq
           + 0.5 * (square(z_x) + square(z_y));
  }
}

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
        real u_x = beta_cdf(x[i, n] | alphax[i, k], betax[i, k]);
        real u_y = beta_cdf(y[i, n] | alphay[i, k], betay[i, k]);
        real log_copula = gaussian_copula_density(u_x, u_y , rho[i, k]);
        
        lps[k] = log(w[k]) + beta_x + beta_y + log_copula;
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




