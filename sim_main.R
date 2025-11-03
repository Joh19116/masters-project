# -------------------------------------------------------------------
# Title   : Simulation Study for Cluster Randomized Trials
# Purpose : Evaluate model performance (log vs blended log–logit links)
#            in estimating treatment effects under varying ICC, 
#            cluster sizes, and baseline prevalences.
# Framework: SimEngine
# -------------------------------------------------------------------

# ---- Load required packages ---------------------------------------
library(SimEngine)
library(lme4)
library(magrittr)
library(truncnorm)

# ---- Source custom functions --------------------------------------
# Each file contains a function used in the simulation workflow.
source("Functions/getData.R")     # Data generation (get_data)
source("Functions/getModel.R")    # Model fitting functions
source("Functions/getMu.R")       # μ range generation (get_mu_range)

# ---- Initialize simulation engine ---------------------------------
sim <- new_sim()

# -------------------------------------------------------------------
# Helper: Convert ICC -> τ (random intercept SD)
# -------------------------------------------------------------------
# For a given ICC, we can approximate τ using:
#   ICC = τ² / (τ² + σ²)
# Assuming σ² ≈ 1 on the link scale, 
#   τ = sqrt(ICC / (1 - ICC))
# If ICC <= 0, set τ = 0.
icc_to_tau <- function(icc) {
  if (icc <= 0) return(0)
  sqrt(icc / (1 - icc))
}

# -------------------------------------------------------------------
# Define simulation levels (factorial design)
# -------------------------------------------------------------------
# Each combination of these levels defines a simulation scenario.
sim %<>% set_levels(
  n_clusters   = c(10,25, 75, 100),         # number of clusters
  cluster_size = c(10, 20, 50, 100, 500),         # individuals per cluster
  icc          = c(0.01, 0.05, 0.1, 0.15, 0.2),       # intraclass correlation values
  mu           = get_mu_range(prevalence = 0.05),  # baseline μ values
  model_type   = c("log", "blended") # link function type
)

# Inspect the full design grid
sim

# -------------------------------------------------------------------
# Define the simulation script
# -------------------------------------------------------------------
# For each combination of levels above:
#   1. Compute τ from ICC
#   2. Generate data using get_data()
#   3. Fit either the log-link or blended-link model
#   4. Return estimates and convergence info
sim %<>% set_script(function() {
  
  # ---- Convert ICC to τ --------------------------------------------
  tau_val <- icc_to_tau(L$icc)
  
  # ---- Generate simulated CRT data ---------------------------------
  dat <- get_data(
    n_clusters = L$n_clusters,
    cluster_size = L$cluster_size,
    tau = tau_val,
    mu = L$mu,
    delta = 0.5,      # true treatment effect on link scale
    link = "log"      # data always generated under log link
  )
  
  # ---- Fit the appropriate model -----------------------------------
  if (L$model_type == "log") {
    res <- analyze_glmm(dat)
  } else if (L$model_type == "blended") {
    res <- analyze_glmm_blended(dat)
  }
  
  # ---- Return simulation outputs -----------------------------------
  return(list(
    delta_hat = res$delta_hat,  # estimated treatment effect
    se        = res$se,         # standard error
    conv      = res$conv        # convergence indicator
  ))
})

# -------------------------------------------------------------------
# Simulation configuration
# -------------------------------------------------------------------
# num_sim : number of replications per scenario
# packages: ensure lme4 is available to worker processes
sim %<>% set_config(
  num_sim  = 100,
  packages = c("lme4"),
  n_cores = 4
)

# Inspect simulation structure before running
sim

# -------------------------------------------------------------------
# Run simulations
# -------------------------------------------------------------------
# Executes all level combinations (grid × repetitions)
sim %<>% run()

# -------------------------------------------------------------------
# Save results
# -------------------------------------------------------------------
saveRDS(sim, file = "results/sim10_results(covs).rds")









