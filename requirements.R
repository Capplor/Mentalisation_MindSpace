# requirements.R
# Package Installation & Loading Script

# Load pacman for efficient package management
if (!require("pacman", character.only = TRUE)) {
  install.packages("pacman")
}
library(pacman)

# Install & load CRAN packages with descriptions
p_load(
  # Base utilities
  rlang,         # Programming tools for tidy evaluation and expression manipulation

  # Data manipulation
  tidyverse,     # Core data science tools (ggplot2, dplyr, tidyr, readr, etc.)
  dplyr,         # Grammar of data manipulation (filter, mutate, summarise, etc.)
  janitor,       # Clean messy data (especially column names)
  readxl,        # Import Excel files (.xls, .xlsx)
  rio,           # Simplified data I/O across formats (CSV, Excel, SPSS, etc.)
  reshape2,      # Data reshaping: melt/cast (older, still useful in base R contexts)

  # Visualization
  ggplot2,       # Declarative grammar of graphics
  ggExtra,       # Marginal plots (e.g., histograms beside scatterplots)
  ggdensity,     # Improved density plots (useful in Bayesian output)
  ggridges,      # Ridgeline plots for distributions
  ggsci,         # Color palettes based on scientific journals
  patchwork,     # Combine ggplots with intuitive layout syntax
  gridExtra,     # Arrange multiple plots on a grid
  cowplot,       # Enhanced plot composition and themes
  bayesplot,     # Diagnostic & posterior visualizations for Bayesian models
  sdamr,         # Raincloud plots for visualizing distributions (used in cog sci)

  # Classical Statistics & Modeling
  afex,          # ANOVA, mixed-effects models with user-friendly interface
  MASS,          # Classic datasets & models (e.g., `glm.nb`, `lda`)
  nnet,          # Multinomial logistic regression and neural nets
  psych,         # Psychometric tools (e.g., correlation, describe)
  multcomp,      # Multiple comparisons and post-hoc testing
  mgcv,          # Generalized additive models (splines, smoothing)
  moments,       # Kurtosis and Skewness
  mclust,        # ARI

  # Bayesian Modeling
  brms,          # Bayesian regression modeling using Stan backend
  loo,           # Leave-one-out cross-validation and WAIC
  posterior,     # Tools for summarizing and analyzing posteriors
  rstan,         # Direct interface to Stan for advanced modeling
  rstanarm,      # Stan-based regression models with familiar `lm`/`glm` syntax
  marginaleffects, # Compute and visualize marginal effects from models
  tidybayes,     # Summary and visualisation for brms models.

  # Expert Elicitation / Bayesian Support
  SHELF,         # Elicitation of expert priors (Sheffield Elicitation Framework)
  bridgesampling,# Model comparison via marginal likelihood estimation
  shinystan,      # Interactive diagnostics and visualizations for Stan models
  copula          # model copula for graphs
)

# Install & load GitHub packages with descriptions
github_pkgs <- list(
  aida    = "michael-franke/aida-package",  # Pragmatic inference modeling tools
  faintr  = "michael-franke/faintr",        # Fitting and analyzing inference models
  cspplot = "CogSciPrag/cspplot"            # Specialized plotting for cognitive science pragmatics
)

for (pkg in names(github_pkgs)) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    remotes::install_github(github_pkgs[[pkg]])
  }
  library(pkg, character.only = TRUE)
}

# CmdStanR installation and setup
if (!require("cmdstanr", character.only = TRUE)) {
  install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
  library(cmdstanr)
  check_cmdstan_toolchain()    # Check system requirements (make, compilers)
  install_cmdstan(cores = 2)   # Install CmdStan backend
} else {
  library(cmdstanr)
}

# Verify CmdStan installation
if (cmdstanr::cmdstan_version() == "") {
  warning("CmdStan not found! Run `install_cmdstan()` manually.")
}

# Optional Font Setup for Windows users (Uncomment if needed)
# p_load(extrafont)           # Import system fonts into R for plotting
# font_import(prompt = FALSE) # Import all fonts (may take a while)
# loadfonts(device = "win")   # Load fonts for Windows graphics device

# Final confirmation message
message("✅ All required packages are installed and loaded. Check CmdStan with `cmdstanr::cmdstan_version()`.")
rm(github_pkgs)
rm(pkg)


# custom functions
#Correlation perfect


#Log Loss Formula
log_loss <- function(p_true, p_pred) {
  -sum(p_true * log(p_pred))
}

#RMSE function
rmse <- function(p_true, p_pred) {
  sqrt(mean((p_true - p_pred)^2))
}

#Cosine Similarity 

cosine_similarity <- function(p_true, p_pred) {
  sum(p_true * p_pred) / (sqrt(sum(p_true^2)) * sqrt(sum(p_pred^2)))
}

#KL Divergence

kl_divergence <- function(p, q, epsilon = 1e-12) {
  sum(p * log(p / q))
}

#Jensen-Shannon Divergence (JSD)

js_divergence <- function(p, q) {
  m <- 0.5*(p+q)
  (0.5*sum(p * log(p / m)))+(0.5*sum(q * log(q / m)))
}
#Brier-Score Divergence (JSD)

brier_score <- function(p, q) {
  mean((p-q)^2)
}


#Summarise

library(rlang)
#Simpler summetrics
#summetrics <- function(data, metric, ...) {
#  data %>%
#    group_by(...) %>% 
#    summarise(
#      mean   = mean({{ metric }}, na.rm = TRUE),
#      median = median({{ metric }}, na.rm = TRUE),
#      MAD    = mad({{ metric }}, na.rm = TRUE),
#      iqr    = IQR({{ metric }}, na.rm = TRUE),
#      sd     = sd({{ metric }}, na.rm = TRUE),
#      n      = sum(!is.na({{ metric }})),
#      se     = sd / sqrt(n),
#      ci_lower = mean - qt(0.975, df = n - 1) * se,
#      ci_upper = mean + qt(0.975, df = n - 1) * se,
#      max    = max({{ metric }}, na.rm = TRUE),
#      min    = min({{ metric }}, na.rm = TRUE),
#      .groups = "drop"
#    )
#}

summetrics <- function(data, metric, group_by = NULL, conf_level = 0.95) {
  # Convert metric to symbol
  metric_sym <- if (is.character(metric)) rlang::sym(metric) else rlang::ensym(metric)
  
  # Handle grouping
  if (!is.null(group_by)) {
    group_syms <- rlang::syms(group_by)
    data <- data %>% dplyr::group_by(!!!group_syms)
  }
  
  alpha <- 1 - conf_level
  
  # First calculate basic stats
  summary_df <- data %>%
    dplyr::summarise(
      across(
        .cols = !!metric_sym,
        .fns = list(
          mean = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          MAD = ~mad(.x, na.rm = TRUE),
          IQR = ~IQR(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          n = ~sum(!is.na(.x)),
          max = ~max(.x, na.rm = TRUE),
          min = ~min(.x, na.rm = TRUE),
          coeff_var = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE),
          skewness = ~moments::skewness(.x, na.rm = TRUE),
          kurtosis = ~moments::kurtosis(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
  # Then calculate SE and CI using the already computed stats
  metric_name <- as.character(metric_sym)
  
  summary_df <- summary_df %>%
    dplyr::mutate(
      "{metric_name}_se" := .data[[paste0(metric_name, "_sd")]] / sqrt(.data[[paste0(metric_name, "_n")]]),
      "{metric_name}_ci_lower" := .data[[paste0(metric_name, "_mean")]] - 
        qt(1 - alpha/2, df = .data[[paste0(metric_name, "_n")]]-1) * .data[[paste0(metric_name, "_se")]],
      "{metric_name}_ci_upper" := .data[[paste0(metric_name, "_mean")]] + 
        qt(1 - alpha/2, df = .data[[paste0(metric_name, "_n")]]-1) * .data[[paste0(metric_name, "_se")]]
    )
  
  return(summary_df)
}

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
# Define the simulate_beta function
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


