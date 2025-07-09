#Import datasets if not run Main Data####
theta <- read_excel("theta.xlsx")
shape_org <- read_excel("shape_org.xlsx")
shape_aes <- read_excel("shape_aes.xlsx")
finalcluster <- read_excel("final_Cluster.xlsx")
data<- readRDS("all_data_lists.rds")
x<- data$data_k_0$x
y<- data$data_k_0$y
r_k<- data$data_k_0$rho
r_one<- data$data_1$rho
rm(data)
# Define the simulate_copula function
simulate_copula <- function(n, p, shapex, shapey, t, var_name) {
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
  sim_cop <- as.data.frame(rMvdc(n * t, myMvd))
  colnames(sim_cop) <- c("V1", "V2")
  sim_cop$var <- var_name
  return(sim_cop)
}

simulate_beta <- function(n, shapex, shapey, t, var_name){
  t <- as.numeric(t)
  shapex <- as.numeric(shapex)
  shapey <- as.numeric(shapey)
  
  # Create proper data frame structure
  data.frame(
    value = c(
      rbeta(n*t, shape1 = shapex[1], shape2 = shapex[2]),
      rbeta(n*t, shape1 = shapey[1], shape2 = shapey[2])
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
  copula_results <- list()
  
  for (j in 2:5) {
    copula_results[[variables[j - 1]]] <- simulate_copula(
      n=1000,
      p = r_k[i, j-1],
      shapex = shape_aes[i, c((j * 2) - 1, j * 2)],
      shapey = shape_org[i, c((j * 2) - 1, j * 2)],
      t = theta[i, j - 1],
      var_name = variables[j - 1]
    )
  }
  
  copula_tot <- do.call(rbind, copula_results)
  
  # Create the base plot first
  base_plot <- ggplot(copula_tot, aes(x = V1, y = V2, colour = var)) +
    ggsci::scale_color_futurama() +
    geom_density2d() +
    geom_point() +
    geom_hline(yintercept = y[i, 1], linewidth = 1.15, color = "black") +
    geom_vline(xintercept = x[i, 1], linewidth = 1.15, color = "black") +
    geom_hline(yintercept = y[i, 2], linewidth = 1.15, color = "#3288BD") +
    geom_vline(xintercept = x[i, 2], linewidth = 1.15, color = "#3288BD") +
    geom_hline(yintercept = y[i, 3], linewidth = 1.15, color = "#5E4FA2") +
    geom_vline(xintercept = x[i, 3], linewidth = 1.15, color = "#5E4FA2") +
    geom_hline(yintercept = y[i, 4], linewidth = 1.15, color = "#FF6348") +
    geom_vline(xintercept = x[i, 4], linewidth = 1.15, color = "#FF6348") +
    geom_hline(yintercept = y[i, 5], linewidth = 1.15, color = "#E6AB02") +
    geom_vline(xintercept = x[i, 5], linewidth = 1.15, color = "#E6AB02") +
    geom_hline(yintercept = y[i, 6], linewidth = 1.15, color = "#ADE2D0") +
    geom_vline(xintercept = x[i, 6], linewidth = 1.15, color = "#ADE2D0") +
    geom_hline(yintercept = y[i, 7], linewidth = 1.15, color = "#A6761D") +
    geom_vline(xintercept = x[i, 7], linewidth = 1.15, color = "#A6761D") +
    geom_hline(yintercept = y[i, 8], linewidth = 1.15, color = "#01665E") +
    geom_vline(xintercept = x[i, 8], linewidth = 1.15, color = "#01665E") +
    geom_hline(yintercept = y[i, 9], linewidth = 1.15, color = "#D53E4F") +
    geom_vline(xintercept = x[i, 9], linewidth = 1.15, color = "#D53E4F") +
    geom_label(
      data = data.frame(
        x = x[i, ],
        y = y[i, ],
        label = t(finalcluster[i, ])[,1]
      ),
      aes(x = x, y = y, label = label, color = label),
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    scale_color_manual(values = label_colors) +
    theme_bw() +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(
      x = "Aesthetic Appreciation",
      y = "Organization",
      title = paste("Cluster ρ Simulations for i =", i)
    ) +
    theme(legend.position = "bottom")
  
  # Add marginal plots to the base plot
  p <- ggExtra::ggMarginal(
    base_plot,
    data = copula_tot,
    margMapping = aes(color = var),
    groupFill = TRUE,
    groupColour = TRUE
  )
  
  # Create annotation text with properly formatted theta values
  annotation_text <- paste(
    "Player Type Percentages:\n",
    "Builder = ", round(theta[i, 1], 2), "\n",
    "Explorer = ", round(theta[i, 2], 2), "\n",
    "Competitor = ", round(theta[i, 3], 2), "\n",
    "Pacifist = ", round(theta[i, 4], 2),
    sep = ""
  )
  
  # Create annotation plot with proper syntax
  annotation <- ggplot() + 
    annotate("text", 
             x = 0, y = 0,  # Centered in the annotation area
             label = annotation_text,
             hjust = 0.48, vjust = 0.6,
             size = 4) +
    theme_void()
  
  # Arrange plots with proper relative heights
  plot_list <- list(p , annotation) 
  final_plot <- wrap_plots(plot_list, ncol = 2, widths = c(7, 2))
  
  print(final_plot)# Use ggsave() if saving to a file, e.g., ggsave(filename = paste0("plot_i_", i, ".png"))
}
# Single Correlation ####

for (i in 1:length(r_one)) {
  copula_results <- list()  # Reset results for each i
  
  # Inner loop for j
  for (j in 2:5) {
    # Simulate copula data for each variable
    copula_results[[variables[j - 1]]] <- simulate_copula(
      n=1000,
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
    geom_hline(yintercept = y[i, 1], linewidth = 1.15, color = "black") +
    #geom_vline(xintercept = x[i, 1], linewidth = 1.15, color = "black") +
    #geom_hline(yintercept = y[i, 2], linewidth = 1.15, color = "#3288BD") +
    #geom_vline(xintercept = x[i, 2], linewidth = 1.15, color = "#3288BD") +
    #geom_hline(yintercept = y[i, 3], linewidth = 1.15, color = "#5E4FA2") +
    #geom_vline(xintercept = x[i, 3], linewidth = 1.15, color = "#5E4FA2") +
    #geom_hline(yintercept = y[i, 4], linewidth = 1.15, color = "#FF6348") +
    #geom_vline(xintercept = x[i, 4], linewidth = 1.15, color = "#FF6348") +
    #geom_hline(yintercept = y[i, 5], linewidth = 1.15, color = "#E6AB02") +
    #geom_vline(xintercept = x[i, 5], linewidth = 1.15, color = "#E6AB02") +
    #geom_hline(yintercept = y[i, 6], linewidth = 1.15, color = "#ADE2D0") +
    #geom_vline(xintercept = x[i, 6], linewidth = 1.15, color = "#ADE2D0") +
    #geom_hline(yintercept = y[i, 7], linewidth = 1.15, color = "#A6761D") +
    #geom_vline(xintercept = x[i, 7], linewidth = 1.15, color = "#A6761D") +
    #geom_hline(yintercept = y[i, 8], linewidth = 1.15, color = "#01665E") +
    #geom_vline(xintercept = x[i, 8], linewidth = 1.15, color = "#01665E") +
    #geom_hline(yintercept = y[i, 9], linewidth = 1.15, color = "#D53E4F") +
    #geom_vline(xintercept = x[i, 9], linewidth = 1.15, color = "#D53E4F") +
    #geom_label(
    #  data = data.frame(
    #    x = x[i, ],
    #    y = y[i, ],
    #    label = t(finalcluster[i, ])[,1]
    #  ),
    #  aes(x = x, y = y, label = label, color = label),
    #  inherit.aes = FALSE,
    #  show.legend = FALSE
    #) +
    #scale_color_manual(values = label_colors) +
    theme_bw() +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(
      x = "Aesthetic Appreciation",
      y = "Organization",
      title = paste("Cluster ρ Simulations for i =", i)
    ) +
    theme(legend.position = "bottom")
  
  # Add marginal plots to the base plot
  p <- ggExtra::ggMarginal(
    base_plot,
    data = copula_tot,
    margMapping = aes(color = var),
    groupFill = TRUE,
    groupColour = TRUE
  )
  
  # Create annotation text with properly formatted theta values
  annotation_text <- paste(
    "Player Type Percentages:\n",
    "Builder = ", round(theta[i, 1], 2), "\n",
    "Explorer = ", round(theta[i, 2], 2), "\n",
    "Competitor = ", round(theta[i, 3], 2), "\n",
    "Pacifist = ", round(theta[i, 4], 2),
    sep = ""
  )
  
  # Create annotation plot with proper syntax
  annotation <- ggplot() + 
    annotate("text", 
             x = 0, y = 0,  # Centered in the annotation area
             label = annotation_text,
             hjust = 0.48, vjust = 0.6,
             size = 4) +
    theme_void()
  
  # Arrange plots with proper relative heights
  plot_list <- list(p , annotation) 
  final_plot <- wrap_plots(plot_list, ncol = 2, widths = c(7, 2))
  
  print(final_plot)# Use ggsave() if saving to a file, e.g., ggsave(filename = paste0("plot_i_", i, ".png"))
}

# No Correlation ####


for (i in 1:nrow(shape_aes)) {
  beta_results <- list()
  
  for (j in 2:5) {
    beta_results[[variables[j - 1]]] <- simulate_beta(
      n=1000,
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
