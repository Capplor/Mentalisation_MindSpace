
# Full Dataset ####
set.seed(123)
total_wide_mn <- read_excel("total_wide.xlsx")

total_wide_mn$x <- scale(total_wide_mn$x)
total_wide_mn$y <- scale(total_wide_mn$y)

total_wide_mn$x <- as.numeric(total_wide_mn$x)
total_wide_mn$y <- as.numeric(total_wide_mn$y)

log1<- nnet::multinom(fc~1, data= total_wide_mn)
log2<- nnet::multinom(fc~x, data= total_wide_mn)
log3<- nnet::multinom(fc~x+y, data= total_wide_mn)
log4<- nnet::multinom(fc~x*y, data= total_wide_mn)

est <- summary(log4)$coefficients
se <- summary(log4)$standard.errors
z <- est/se
z

2*(1-pnorm(abs(z)))

car::Anova(log3, type = 3)
car::Anova(log4, type = 3)

# Compare AIC and BIC
model_comparison <- data.frame(
  Model = c("Additive (log3)", "Interaction (log4)"),
  AIC = c(AIC(log3), AIC(log4)),
  BIC = c(BIC(log3), BIC(log4))
)

print(model_comparison)

# Graphs ####

x_vals <- seq(min(total_wide_mn$x), max(total_wide_mn$x), length.out = 45)
y_vals <- seq(min(total_wide_mn$y), max(total_wide_mn$y), length.out = 45)

grid <- expand.grid(x = x_vals, y = y_vals)

# Predict probabilities using the interaction model

grid_predictions3 <- predict(log3, newdata = grid, type = "probs")
grid_predictions4 <- predict(log4, newdata = grid, type = "probs")

# Reshape predictions for visualization
predictions_long3 <- cbind(grid, grid_predictions3)

predictions_long4 <- cbind(grid, grid_predictions4)

predictions_melted3 <- melt(predictions_long3, id.vars = c("x", "y"),
                            variable.name = "Category", 
                            value.name = "Probability")

predictions_melted4 <- melt(predictions_long4, id.vars = c("x", "y"),
                            variable.name = "Category", 
                            value.name = "Probability")

# Visualize interaction effects

ggplot(predictions_melted3, aes(x = x, y = y, fill = Probability)) +
  geom_tile() +
  facet_wrap(~ Category, scales = "free", ncol = 2) +
  scale_fill_viridis_c() +
  labs(title = "Additive Effects by Category",
       x = "x", y = "y", fill = "Probability") +
  theme_minimal()

ggplot(predictions_melted4, aes(x = x, y = y, fill = Probability)) +
  geom_tile() +
  facet_wrap(~ Category, scales = "free", ncol = 2) +
  scale_fill_viridis_c() +
  labs(title = "Interaction Effects by Category",
       x = "x", y = "y", fill = "Probability") +
  theme_minimal()

# Multilevel ####


fit_hierarchical_int <- brm(
  fc ~ 1,    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)

fit_hierarchical_ranint <- brm(
  fc ~ 1 + (1 | participant),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_rantp<- brm(
  fc ~ 1 + (1 | participant)+(1|N),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_x <- brm(
  fc ~ x + (1 | participant),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_xt <- brm(
  fc ~ x + (1 | participant)+(1|N),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_y <- brm(
  fc ~ y + (1 | participant),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_yt <- brm(
  fc ~ y + (1 | participant)+(1|N),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_xy <- brm(
  fc ~ x + y + (1 | participant),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_xyt <- brm(
  fc ~ x + y + (1 | participant)+(1|N),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)
fit_hierarchical_xy_intt <- brm(
  fc ~ x * y + (1 | participant)+(1|N),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_xyslope <- brm(
  fc ~ x + y + (x + y | participant),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_xy_int <- brm(
  fc ~ x * y + (1 | participant),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)
fit_hierarchical_xy_nested <- brm(
  fc ~ x * y + (1 | participant/N),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_xyslopey <- brm(
  fc ~ x * y + (1+ y | participant),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_xyslopex <- brm(
  fc ~ x * y + (1+ x | participant),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_xyslopeM <- brm(
  fc ~ x * y + (x + y | participant),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)


fit_hierarchical_xyslope <- brm(
  fc ~ x * y + (x * y | participant),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_xyslopeT <- brm(
  fc ~ x * y + (x * y | participant)+(1|N),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_xyslopeMT <- brm(
  fc ~ x * y + (x + y | participant)+(1|N),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)
fit_hierarchical_xyslopeyT <- brm(
  fc ~ x * y + (1+ y | participant)+(1|N),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)

fit_hierarchical_xyslopexT <- brm(
  fc ~ x * y + (1+ x | participant)+(1|N),    
  data = total_wide_mn,
  family = categorical(),             # Multinomial logistic regression
  chains = 4, cores = 8, iter = 10000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)  # Sampling settings
)


# Define models in a named list for easier comparison
models <- list(
  int          = fit_hierarchical_int,          # ~ 1
  ran_int      = fit_hierarchical_ranint,       # ~ 1 + (1|participant)
  ran_int_N    = fit_hierarchical_rantp,        # ~ 1 + (1|participant)+(1|N)
  x            = fit_hierarchical_x,            # ~ x + (1|participant)
  y            = fit_hierarchical_y,            # ~ y + (1|participant)
  xy           = fit_hierarchical_xy,           # ~ x+y + (1|participant)
  x_N          = fit_hierarchical_xt,           # ~ x + (1|participant)+(1|N)
  y_N          = fit_hierarchical_yt,           # ~ y + (1|participant)+(1|N)
  xy_N         = fit_hierarchical_xyt,          # ~ x+y + (1|participant)+(1|N)
  xy_int       = fit_hierarchical_xy_int,       # ~ x*y + (1|participant)
  xy_int_N     = fit_hierarchical_xy_intt,      # ~ x*y + (1|participant)+(1|N)
  xy_nest      = fit_hierarchical_xy_nested,    # ~ x*y + (1|participant/N)
  slope_x      = fit_hierarchical_xyslopex,     # ~ x*y + (1+x | participant)
  slope_y      = fit_hierarchical_xyslopey,     # ~ x*y + (1+y | participant)
  slope_M      = fit_hierarchical_xyslopeM,     # ~ x*y + (x+y | participant)
  slope_full   = fit_hierarchical_xyslope,      # ~ x*y + (x*y | participant)
  slope_x_T    = fit_hierarchical_xyslopexT,    # ~ x*y + (1+x|participant)+(1|N)
  slope_y_T    = fit_hierarchical_xyslopeyT,   # ~ x*y + (1+y|participant)+(1|N)
  slope_M_T    = fit_hierarchical_xyslopeMT,    # ~ x*y + (x+y|participant)+(1|N)
  slope_full_T = fit_hierarchical_xyslopeT      # ~ x*y + (x*y|participant)+(1|N)
)

# Compute marginal likelihoods for each model
bridge_results <- lapply(models, bridge_sampler)

bf_results <- sapply(names(bridge_results), function(model) {
  bayes_factor(bridge_results[[model]], bridge_results$xy_int)
})

# Compute LOO for each model
loo_results <- lapply(models, loo)

# Compare models using LOO
loo_comparison <- loo_compare(loo_results)

# For model comparison, use loo_compare
comparison <- loo_compare(loo_results)

# Print the comparison
print(comparison, simplify = FALSE)

# Optional: Plot the comparison
plot(comparison)

# Plots ####

plot_predictions(fit_hierarchical_xy_intt, condition = c("x", "y"), type = "response") +
  labs(x = "Predictor X", y = "P(Choice)")  

fit_hierarchical_xy_intt |> 
  tidybayes::tidy_draws() |> 
  dplyr::select(ends_with("__")) |> 
  filter(divergent__ == TRUE)

tidybayes::tidy_draws(fit_hierarchical_xy_intt) |> 
  dplyr::select(starts_with("b")) |> 
  pivot_longer(cols = everything()) |> 
  group_by(name) |> 
  reframe(aida::summarize_sample_vector(value)[-1])

posterior_Intercept <- 
  tidybayes::tidy_draws(fit_hierarchical_xy_intt) 

ranef(fit_hierarchical_xy_intt)$participant[,,"mucompetitor_Intercept"] |> 
  as.data.frame() |> 
  rownames_to_column("participant") |> 
  ggplot(aes(y = participant, x = Estimate)) +
  geom_errorbar(aes(xmin = `Q2.5`, xmax = `Q97.5`), 
                color = "darkgrey", alpha = 0.7)+
  geom_vline(aes(xintercept = 0), color = "#C71000FF", 
             size = 2, alpha = 0.8) +
  geom_point(color = "#008EA0FF", size = 2)

ranef(fit_hierarchical_xy_intt)$participant[,,"muexplorer_Intercept"] |> 
  as.data.frame() |> 
  rownames_to_column("participant") |> 
  ggplot(aes(y = participant, x = Estimate)) +
  geom_errorbar(aes(xmin = `Q2.5`, xmax = `Q97.5`), 
                color = "darkgrey", alpha = 0.7)+
  geom_vline(aes(xintercept = 0), color = "#C71000FF", 
             size = 2, alpha = 0.8) +
  geom_point(color = "#008EA0FF", size = 2)

ranef(fit_hierarchical_xy_intt)$participant[,,"mupacifist_Intercept"] |> 
  as.data.frame() |> 
  rownames_to_column("participant") |> 
  ggplot(aes(y = participant, x = Estimate)) +
  geom_errorbar(aes(xmin = `Q2.5`, xmax = `Q97.5`), 
                color = "darkgrey", alpha = 0.7)+
  geom_vline(aes(xintercept = 0), color = "#C71000FF", 
             size = 2, alpha = 0.8) +
  geom_point(color = "#008EA0FF", size = 2)


fit_hierarchical_xy_intt |> 
  tidy_draws() |> 
  dplyr::select(starts_with("b")) |> 
  pivot_longer(cols = everything()) |> 
  ggplot(aes(x = value, y = name)) +
  tidybayes::stat_halfeye(fill = "#FF6F00FF") +
  ylab("") +
  geom_vline(aes(xintercept = 0), color = "#C71000FF", size = 2)+
  theme_classic()

tidybayes::tidy_draws(fit_hierarchical_xy_intt) |> 
  dplyr::select(starts_with("b_")) |> 
  pivot_longer(cols = everything()) |> 
  group_by(name) |> 
  reframe(aida::summarize_sample_vector(value)[-1])

fit_hierarchical_xy_nested |> 
  tidybayes::tidy_draws() |> 
  dplyr::select(ends_with("__")) |> 
  filter(divergent__ == TRUE)




















