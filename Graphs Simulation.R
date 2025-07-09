#Import datasets if not run Main Data####
theta <- read_excel("theta.xlsx")
shape_org <- read_excel("shape_org.xlsx")
shape_aes <- read_excel("shape_aes.xlsx")
data<- readRDS("all_data_lists.rds")
x<- data$data_k_0$x
y<- data$data_k_0$y
r_k<- data$data_k_0$rho
r_one<- data$data_1$rho
rm(data)
# Define the simulate_copula function
simulate_copula <- function(p, shapex, shapey, t, var_name) {
  # Convert parameters to numeric if they aren't already
  p <- as.numeric(p)
  t <- as.numeric(t)
  shapex <- as.numeric(shapex)
  shapey <- as.numeric(shapey)
  
  myCop <- normalCopula(param = p, dim = 2, dispstr = "un")
  myMvd <- mvdc(
    copula = myCop, 
    margins = c("beta", "beta"),
    paramMargins = list(
      list(shape1 = shapex[1], shape2 = shapex[2]),
      list(shape1 = shapey[1], shape2 = shapey[2])
    )
  )
  sim_cop <- as.data.frame(rMvdc(1000 * t, myMvd))
  colnames(sim_cop) <- c("V1", "V2")
  sim_cop$var <- var_name
  return(sim_cop)
}

simulate_beta <- function(shapex, shapey, t, var_name){
  t <- as.numeric(t)
  shapex <- as.numeric(shapex)
  shapey <- as.numeric(shapey)
  
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
    geom_hline(yintercept = y[i, 1], size = 1.15, color = "black")+
    geom_vline(xintercept = x[i, 1], size = 1.15, color = "black")+
    #geom_hline(yintercept = y[i, 2], size = 1.15, color = "#3288BD")+
    #geom_vline(xintercept = x[i, 2], size = 1.15, color = "#3288BD")+
    #geom_hline(yintercept = y[i, 3], size = 1.15, color = "#5E4FA2")+
    #geom_vline(xintercept = x[i, 3], size = 1.15, color = "#5E4FA2")+
    #geom_hline(yintercept = y[i, 4], size = 1.15, color = "#FF6348FF")+
    #geom_vline(xintercept = x[i, 4], size = 1.15, color = "#FF6348FF")+
    #geom_hline(yintercept = y[i, 5], size = 1.15, color = "#E6AB02")+
    #geom_vline(xintercept = x[i, 5], size = 1.15, color = "#E6AB02")+
    #geom_hline(yintercept = y[i, 6], size = 1.15, color = "#ADE2D0FF")+
    #geom_vline(xintercept = x[i, 6], size = 1.15, color = "#ADE2D0FF")+
    #geom_hline(yintercept = y[i, 7], size = 1.15, color = "#A6761D")+
    #geom_vline(xintercept = x[i, 7], size = 1.15, color = "#A6761D")+
    #geom_hline(yintercept = y[i, 8], size = 1.15, color = "#01665E")+
    #geom_vline(xintercept = x[i, 8], size = 1.15, color = "#01665E")+
    #geom_hline(yintercept = y[i, 9], size = 1.15, color = "#D53E4F")+
    #geom_vline(xintercept = x[i, 9], size = 1.15, color = "#D53E4F")+
    #geom_point(data = data.frame(x = x[i, ], y = y[i, ]),
    #  aes(x = x, y = y), shape = 21,  # Circle with border
    #  size = 4, fill = "white",
    #  color = c("black", "#3288BD", "#5E4FA2", "#FF6348", "#E6AB02", 
    #            "#ADE2D0", "#A6761D", "#01665E", "#D53E4F")[1:9])+
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

for (i in 1:length(r_one)) {
  copula_results <- list()  # Reset results for each i
  
  # Inner loop for j
  for (j in 2:5) {
    # Simulate copula data for each variable
    copula_results[[variables[j - 1]]] <- simulate_copula(
      p = r_one[i],
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
    geom_hline(yintercept = y[i, 1], size = 1.15, color="black")+
    geom_vline(xintercept = x[i, 1], size = 1.15, color="black")+
    #geom_hline(yintercept = y[i, 2], size = 1.15, color = "#3288BD")+
    #geom_vline(xintercept = x[i, 2], size = 1.15, color = "#3288BD")+
    #geom_hline(yintercept = y[i, 3], size = 1.15, color = "#5E4FA2")+
    #geom_vline(xintercept = x[i, 3], size = 1.15, color = "#5E4FA2")+
    #geom_hline(yintercept = y[i, 4], size = 1.15, color = "#FF6348FF")+
    #geom_vline(xintercept = x[i, 4], size = 1.15, color = "#FF6348FF")+
    #geom_hline(yintercept = y[i, 5], size = 1.15, color = "#E6AB02")+
    #geom_vline(xintercept = x[i, 5], size = 1.15, color = "#E6AB02")+
    #geom_hline(yintercept = y[i, 6], size = 1.15, color = "#ADE2D0FF")+
    #geom_vline(xintercept = x[i, 6], size = 1.15, color = "#ADE2D0FF")+
    #geom_hline(yintercept = y[i, 7], size = 1.15, color = "#A6761D")+
    #geom_vline(xintercept = x[i, 7], size = 1.15, color = "#A6761D")+
    #geom_hline(yintercept = y[i, 8], size = 1.15, color = "#01665E")+
    #geom_vline(xintercept = x[i, 8], size = 1.15, color = "#01665E")+
    #geom_hline(yintercept = y[i, 9], size = 1.15, color = "#D53E4F")+
    #geom_vline(xintercept = x[i, 9], size = 1.15, color = "#D53E4F")+
    #geom_point(data = data.frame(x = x[i, ], y = y[i, ]),
    #  aes(x = x, y = y), shape = 21,  # Circle with border
    #  size = 4, fill = "white",
    #  color = c("black", "#3288BD", "#5E4FA2", "#FF6348", "#E6AB02", 
    #            "#ADE2D0", "#A6761D", "#01665E", "#D53E4F")[1:9])+
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
