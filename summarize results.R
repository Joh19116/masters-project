###############################################################################
# Summarize Simulation Results Script
# ---------------------------------------------------------------------------
# Author: Andrew Johnson
# Purpose:
#   Summarize simulation results produced by SimEngine for both log and blended
#   link models (with covariates). Computes key performance metrics such as
#   bias, variance, MSE, CI coverage, and convergence rate by model type.
###############################################################################

# ---------------------------------------------------------------------------
# Load Required Packages
# ---------------------------------------------------------------------------
library(SimEngine)
library(dplyr)
library(magrittr)

# ---------------------------------------------------------------------------
# Load Simulation Object
# ---------------------------------------------------------------------------
sim <- sim_cluster_results   # Simulation object produced by SimEngine

# Define the true parameter value used in data generation
true_delta <- 0.5

# ---------------------------------------------------------------------------
# Summarize Simulation Estimates
# ---------------------------------------------------------------------------
# Compute bias, variance, mean squared error (MSE), and confidence interval (CI)
# coverage for delta hat by model type.

summary_tbl <- sim %>%
  SimEngine::summarize(
    list(stat = "bias",     estimate = "delta_hat", truth = true_delta,
         name = "bias_delta", by = "model_type"),
    list(stat = "var",      x = "delta_hat",
         name = "var_delta", by = "model_type"),
    list(stat = "mse",      estimate = "delta_hat", truth = true_delta,
         name = "mse_delta", by = "model_type"),
    list(stat = "coverage", estimate = "delta_hat", se = "se", truth = true_delta,
         name = "ci_coverage", by = "model_type")
  )

# ---------------------------------------------------------------------------
# Convergence Rate by Scenario
# ---------------------------------------------------------------------------
# Calculate the proportion of successful model convergences within each level.
conv_tbl <- sim$results %>%
  dplyr::group_by(level_id) %>%
  dplyr::summarise(conv_rate = mean(conv, na.rm = TRUE))

# Merge convergence results into the main summary table
summary_tbl <- dplyr::left_join(summary_tbl, conv_tbl, by = "level_id")

# ---------------------------------------------------------------------------
# Optional: Compute Derived Quantities
# ---------------------------------------------------------------------------
# Compute τ (tau) from ICC and mean outcome prevalence for descriptive purposes.
#   τ = sqrt(ICC / (1 - ICC))
#   E[p] = exp(μ + ½τ²)
summary_tbl <- summary_tbl %>%
  mutate(
    tau = sqrt(icc / (1 - icc)),
    mean_outcome = exp(mu + 0.5 * tau^2)
  )

# ---------------------------------------------------------------------------
# Output Summary
# ---------------------------------------------------------------------------
# View summarized results in console
print(summary_tbl)

# Optionally, save results to CSV for downstream analysis or plotting
# write.csv(summary_tbl, "results/summary_tbl.csv", row.names = FALSE)
###############################################################################
