# Define the simulate_copula function
simulate_copula <- function(p, shapex, shapey, t, var_name) {
  myCop <- normalCopula(param = as.numeric(p), dim = 2, dispstr = "un")
  myMvd <- mvdc(
    copula = myCop, margins = c("beta", "beta"),
    paramMargins = list(
      list(shape1 = shapex[1], shape2 = shapex[2]),
      list(shape1 = shapey[1], shape2 = shapey[2])
    )
  )
  sim_cop <- as.data.frame(rMvdc(1000 * t, myMvd))
  sim_cop$var <- var_name
  return(sim_cop)
}

simulate_beta <- function(shapex, shapey, t, var_name){
  # Create proper data frame structure
  data.frame(
    value = c(
      rbeta(1000*t, shape1 = shapex[1], shape2 = shapex[2]),
      rbeta(1000*t, shape1 = shapey[1], shape2 = shapey[2])
    ),
    dim = rep(c("Aesthetic Appreciation", "Organisation"), each = 1000*t),
    var = var_name,
    stringsAsFactors = FALSE
  )
}

# Set seed for reproducibility
set.seed(123)

# Define the variable names
variables <- c("Builder", "Explorer", "Competitor", "Pacifist")

# K Correlation ####

for (i in 1:nrow(r_k)) {
  copula_results <- list()  # Reset results for each i
  
  # Inner loop for j
  for (j in 2:5) {
    # Simulate copula data for each variable
    copula_results[[variables[j - 1]]] <- simulate_copula(
      p = r_k[i, j],
      shapex = shape_aes[i, c((j * 2) - 1, j * 2)],
      shapey = shape_org[i, c((j * 2) - 1, j * 2)],
      t = theta[i, j - 1],
      var_name = variables[j - 1]
    )
  }
  
  # Combine results for this value of i
  copula_tot <- do.call(rbind, copula_results)
  
  # Plot for current i
  p <- ggplot(copula_tot, aes(x = V1, y = V2, colour = var)) +
    ggsci::scale_color_futurama() +
    geom_density2d() +
    geom_point() +
    geom_hline(yintercept = y[i, 4], size = 1.3)+
    geom_vline(xintercept = x[i, 4], size = 1.3)+
    theme_bw() +
    xlim(0, 1) + 
    ylim(0, 1) +
    coord_fixed() +
    labs(
      x = "Aestetic Appreciation", 
      y = "Organisation",
      title = paste("Cluster ρ Simulations for i =", i)
    ) +
    theme(legend.position = "bottom")
  
  # Add marginal density plots
  p <- ggExtra::ggMarginal(
    p, data = copula_tot, margMapping = aes(color = var),
    groupFill = TRUE, groupColour = TRUE
  )
  
  # Print or save the plot for this value of i
  print(p)  # Use ggsave() if saving to a file, e.g., ggsave(filename = paste0("plot_i_", i, ".png"))
}

# Single Correlation ####

for (i in 1:nrow(r_one)) {
  copula_results <- list()  # Reset results for each i
  
  # Inner loop for j
  for (j in 2:5) {
    # Simulate copula data for each variable
    copula_results[[variables[j - 1]]] <- simulate_copula(
      p = r_one[i,2],
      shapex = shape_aes[i, c((j * 2) - 1, j * 2)],
      shapey = shape_org[i, c((j * 2) - 1, j * 2)],
      t = theta[i, j - 1],
      var_name = variables[j - 1]
    )
  }
  
  # Combine results for this value of i
  copula_tot <- do.call(rbind, copula_results)
  
  # Plot for current i
  p <- ggplot(copula_tot, aes(x = V1, y = V2, colour = var)) +
    ggsci::scale_color_futurama() +
    geom_density2d() +
    geom_point() +
    geom_hline(yintercept = y[i, 4], size = 1.3)+
    geom_vline(xintercept = x[i, 4], size = 1.3)+
    theme_bw() +
    xlim(0, 1) + 
    ylim(0, 1) +
    coord_fixed() +
    labs(
      x = "Aestetic Appreciation", 
      y = "Organisation",
      title = paste("General ρ Simulations for i =", i)
    ) +
    theme(legend.position = "bottom")
  
  # Add marginal density plots
  p <- ggExtra::ggMarginal(
    p, data = copula_tot, margMapping = aes(color = var),
    groupFill = TRUE, groupColour = TRUE
  )
  
  # Print or save the plot for this value of i
  print(p)  # Use ggsave() if saving to a file, e.g., ggsave(filename = paste0("plot_i_", i, ".png"))
}

# No Correlation ####


for (i in 1:nrow(shape_aes)) {
  beta_results <- list()
  
  for (j in 2:5) {
    beta_results[[variables[j - 1]]] <- simulate_beta(
      shapex = shape_aes[i, c((j * 2) - 1, j * 2)],
      shapey = shape_org[i, c((j * 2) - 1, j * 2)],
      t = theta[i, j - 1],
      var_name = variables[j - 1]
    )
  }
  
  beta_tot <- do.call(rbind, beta_results)
  
  # Create separate data frames for reference lines
  vline_data <- data.frame(
    dim = c("Aesthetic Appreciation", "Organisation"),
    xval = c(x[i, 4], NA),  # Only for Aesthetic
    yval = c(NA, y[i, 4])    # Only for Organisation
  )
  
  p <- ggplot(beta_tot, aes(x = value, y = dim, fill = var)) +
    geom_density_ridges(
      alpha = 0.7, 
      scale = 0.9,
      quantile_lines = TRUE, 
      quantiles = 2
    ) +
    facet_grid(var ~ .) +
    ggsci::scale_fill_futurama() +
    theme_minimal() +
    labs(
      x = "Value", 
      y = "Dimension",
      title = paste("No ρ for i =", i),
      fill = "Variable"
    ) +
    theme(legend.position = "bottom",
          strip.text.y = element_blank(),
          axis.title.y = element_blank())
  
  print(p)
}
