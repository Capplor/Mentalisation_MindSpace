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
  shinystan      # Interactive diagnostics and visualizations for Stan models
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
