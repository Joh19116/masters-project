###############################################################################
# Title   : Simulation Study for Cluster Randomized Trials
# Purpose : Evaluate model performance (log vs blended log–logit links)
# Framework: SimEngine Cluster Parallelization
# Author  : Andrew Johnson
###############################################################################

# ---- Load required packages ---------------------------------------
library(SimEngine)
library(lme4)
library(magrittr)
library(truncnorm)
library(dplyr)


# ---- Source custom functions --------------------------------------
source("Functions/getData.R")
source("Functions/getModel.R")
source("Functions/getMu.R")
source("Functions/blended_link_function.R") # blended link function from Clark and Barr


# ---- Helper: Convert ICC -> τ (random intercept SD) ----------------
icc_to_tau <- function(icc) {
  if (icc <= 0) return(0)
  sqrt(icc / (1 - icc))
}

# ---- Cluster Parallelization Framework -----------------------------
# Run the full simulation on the Slurm cluster
run_on_cluster(
  
  # ================================================================
  # FIRST BLOCK: Simulation setup and design grid
  # ================================================================
  first = {
    sim <- new_sim()
    
    sim %<>% set_levels(
      n_clusters   = c(10, 26, 76, 100),
      cluster_size = c(10, 20, 50, 100, 500),
      icc          = c(0.01, 0.05, 0.10, 0.15, 0.20),
      mu           = get_mu_range(prevalence = 0.05, max_missing = 0.7),
      model_type   = c("log", "blended") 
    )
    
    sim %<>% set_script(function() {
      tau_val <- icc_to_tau(L$icc)
      
      dat <- get_data(
        n_clusters   = L$n_clusters,
        cluster_size = L$cluster_size,
        tau          = tau_val,
        mu           = L$mu,
        delta        = 0.5,
        link         = "log"
      )
      
      if (L$model_type == "log") {
        res <- analyze_glmm(dat)
      } else if (L$model_type == "blended") {
        res <- analyze_glmm_blended(dat)
      }
      
      # Normalize outputs so both model types return the same fields
      list(
        delta_hat   = res$delta_hat,
        se          = res$se,
        conv        = res$conv,
        singular    = if (!is.null(res$singular)) res$singular else NA,
        dropped_trt = if (!is.null(res$dropped_trt)) res$dropped_trt else NA,
        err         = if (!is.null(res$err)) res$err else NA_character_,
        warn        = if (!is.null(res$warn)) res$warn else NA_character_
      )
    })
    
    sim %<>% set_config(
      num_sim  = 100,
      parallel = TRUE,
      n_cores  = 20,        # Number of array jobs
      packages = c("lme4", "magrittr", "truncnorm")
    )
  },
  
  # ================================================================
  # MAIN BLOCK: Run each replicate
  # ================================================================
  main = {
    sim %<>% run()
  },
  
  
  # ================================================================
  # LAST BLOCK: Save the full simulation object only
  # ================================================================
  last = {
    saveRDS(sim, file = "results/sim16_maxmiss30_results.rds")
  },
  
  cluster_config = list(js = "slurm")
)
