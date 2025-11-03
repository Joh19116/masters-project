# Summarize REsults SCripot

# 
# # -------------------------------------------------------------------
# # Summarize simulation results
# # -------------------------------------------------------------------
# true_delta : known effect size used for generating data
# Compute bias, variance, MSE, and CI coverage for δ̂ by model type.
# (Uncomment and define `true_delta` if needed)
# 
# 
sim <- `sim9_results(test with covs)`
true_delta <- 0.5

summary_tbl <- sim %>% SimEngine::summarize(
  list(stat = "bias",     estimate = "delta_hat", truth = true_delta,
       name = "bias_delta", by = "model_type"),
  list(stat = "var",      x = "delta_hat",
       name = "var_delta", by = "model_type"),
  list(stat = "mse",      estimate = "delta_hat", truth = true_delta,
       name = "mse_delta", by = "model_type"),
  list(stat = "coverage", estimate = "delta_hat", se = "se", truth = true_delta,
       name = "ci_coverage", by = "model_type")
)

# -------------------------------------------------------------------
# Calculate convergence rate per scenario
# -------------------------------------------------------------------
conv_tbl <- sim$results %>%
  dplyr::group_by(level_id) %>%
  dplyr::summarise(conv_rate = mean(conv, na.rm = TRUE))

# Merge convergence results into summary table
summary_tbl <- dplyr::left_join(summary_tbl, conv_tbl, by = "level_id")

# Inspect combined results
summary_tbl

# -------------------------------------------------------------------
# Add tao back to the table if desired
# -------------------------------------------------------------------
# Compute τ from ICC for plotting
# Compute mean outcome prevalence from μ (E[p] = exp(μ + ½τ²))
summary_tbl <- summary_tbl %>%
  mutate(tau = sqrt(icc / (1 - icc)),
         mean_outcome = exp(mu + 0.5 * tau^2))