# requirements.R
# Package Installation & Loading Script

# Install pacman if not already installed (for efficient package management)
if (!require("pacman", character.only = TRUE)) {
  install.packages("pacman")
}
library(pacman)

# Core Packages (with descriptions) ------------------------------------------------------
p_load(
  # Data Manipulation
  dplyr,          # Data wrangling (filter, mutate, summarize)
  tidyverse,      # Meta-package (dplyr, ggplot2, tidyr, etc.)
  janitor,        # Clean column names and data frames
  readxl,         # Read Excel files
  rio,            # Import/export multiple file formats
  reshape2,       # Data reshaping (melt/cast)
  
  # Visualization
  ggplot2,        # Primary plotting system
  bayesplot,      # Bayesian model visualization
  ggdensity,      # Enhanced density plots
  ggExtra,        # Marginal plots for ggplot2
  ggridges,       # Ridgeline plots
  ggsci,          # Scientific journal color palettes
  patchwork,      # Combine ggplot2 plots
  gridExtra,      # Grid-based plot arrangements
  
  # Statistical Analysis
  afex,           # ANOVA/Mixed-effects models
  MASS,           # Modern Applied Statistics (e.g., glm.nb)
  nnet,           # Multinomial regression
  psych,          # Psychological statistics (e.g., describe())
  multcomp,       # Multiple comparisons for linear models
  
  # Bayesian Tools
  brms,           # Bayesian regression models (Stan wrapper)
  loo,            # Model comparison/WAIC/LOO-CV
  posterior,      # Posterior distribution tools
  SHELF,          # Expert elicitation tools
  
  # Specialized Packages
  compositions,   # Compositional data analysis
  copula,         # Copula modeling
  fitdistrplus,   # Fitting distributions to data
  mclust,         # Model-based clustering
  MGMM,           # Mixture of Gaussian Mixture Models
  revdbayes,      # Extreme value analysis
  shinystan       # Interactive Stan diagnostics
)

# CmdStanR Setup (Manual Installation) ------------------------------------------------
if (!require("cmdstanr", character.only = TRUE)) {
  install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
  library(cmdstanr)
  check_cmdstan_toolchain()  # Verify system dependencies
  install_cmdstan(cores = 2) # Install Stan toolchain
} else {
  library(cmdstanr)
}

# Verify Stan installation
if (cmdstanr::cmdstan_version() == "") {
  warning("CmdStan not found! Run `install_cmdstan()` manually.")
}

# Font Setup (Uncomment if needed) ----------------------------------------------------
# p_load(extrafont)
# if (!"extrafont" %in% .packages()) {
#   install.packages("extrafont")
#   library(extrafont)
#   font_import(prompt = FALSE)  # May take several minutes
#   loadfonts(device = "win")
# }

# Print confirmation
message("Required packages installed and loaded. Check Stan with `cmdstanr::cmdstan_version()`.")
