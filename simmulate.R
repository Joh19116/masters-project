


library(SimEngine)
library(magrittr)

source("getData.R")
source("getModel.R")
source("getMu.R")

sim <- new_sim()


# Helper: convert ICC -> tau (for logit link)

icc_to_tau <- function(icc) {
  if (icc <= 0) return(0)
  sqrt(icc / (1 - icc))
}

# Define simulation levels
sim %<>% set_levels(
  n_clusters   = c(10, 100),
  cluster_size = c(10, 500),
  icc          = c(0.01, 0.2),
  mu           = get_mu_range(prevalence = 0.05),
  model_type   = c("log", "blended")
)

sim


sim %<>% set_script(function() {
  # Convert ICC to tau
  tau_val <- icc_to_tau(L$icc)
  
  
  # Generate data
  dat <- get_data(
    n_clusters = L$n_clusters,
    cluster_size = L$cluster_size,
    tau = tau_val,
    mu = L$mu,
    delta = 0.5,
    link = "log"
  )
  
  # ---- Model fitting ----
  if (L$model_type == "log") {
    res <- analyze_glmm(dat)
  } else if (L$model_type == "blended") {
    res <- analyze_glmm_blended(dat)
  }
  
  # Return results
  return(list(
    delta_hat = res$delta_hat,
    se        = res$se,
    conv      = res$conv
  ))
})

sim %<>% set_config(
  num_sim  = 10,                     
  packages = c("lme4")
)

sim





sim %<>% run()


# Save the sim object
saveRDS(sim, file = "sim5_results(small run log and blended).rds")




# true_delta <- 0.5

summary_tbl <- sim %>% summarize(
  list(stat = "bias",     estimate = "delta_hat", truth = true_delta, name = "bias_delta", by = "model_type"),
  list(stat = "var",      x        = "delta_hat",                   name = "var_delta", by = "model_type"),
  list(stat = "mse",      estimate = "delta_hat", truth = true_delta, name = "mse_delta", by = "model_type"),
  list(stat = "coverage", estimate = "delta_hat", se = "se", truth = true_delta, name = "ci_coverage", by = "model_type")
)

# 
# # --- helper functions for Monte Carlo SEs ---
# mcse_bias <- function(est, true) {
#   sqrt(var(est - true) / length(est))
# }
# 
# mcse_var <- function(est) {
#   # empirical variance of the variance estimator
#   # note: var(est) is the variance of the estimates across sims
#   # multiply by 2/(nsim-1) following Morris et al. (2019)
#   sqrt(2 * (var(est)^2) / (length(est) - 1))
# }
# 
# mcse_mse <- function(est, true) {
#   # Monte Carlo SE for MSE = sqrt(Var((est - true)^2)/nsim)
#   sqrt(var((est - true)^2) / length(est))
# }
# 
# mcse_coverage <- function(in_ci) {
#   # in_ci = logical vector, TRUE if CI covers truth
#   p_hat <- mean(in_ci)
#   sqrt(p_hat * (1 - p_hat) / length(in_ci))
# }
# 
# # --- main summary table with Monte Carlo SEs ---
# summary_tbl <- sim %>%
#   summarize(
#     list(
#       stat = "bias",
#       estimate = "delta_hat",
#       truth = true_delta,
#       name = "bias_delta",
#       mcse = mcse_bias(.data$delta_hat, true_delta)
#     ),
#     list(
#       stat = "var",
#       x = "delta_hat",
#       name = "var_delta",
#       mcse = mcse_var(.data$delta_hat)
#     ),
#     list(
#       stat = "mse",
#       estimate = "delta_hat",
#       truth = true_delta,
#       name = "mse_delta",
#       mcse = mcse_mse(.data$delta_hat, true_delta)
#     ),
#     list(
#       stat = "coverage",
#       estimate = "delta_hat",
#       se = "se",
#       truth = true_delta,
#       name = "ci_coverage",
#       mcse = mcse_coverage(
#         (.data$delta_hat - 1.96 * .data$se <= true_delta) &
#           (.data$delta_hat + 1.96 * .data$se >= true_delta)
#       )
#     )
#   )







# Add convergence rate manually:
conv_tbl <- sim$results %>%
  dplyr::group_by(level_id) %>%
  dplyr::summarise(conv_rate = mean(conv, na.rm = TRUE))

# Merge back into summary
summary_tbl <- dplyr::left_join(summary_tbl, conv_tbl, by = "level_id")

summary_tbl

summary_tbl <- summary_tbl %>%
  mutate(tau = sqrt(icc / (1 - icc)),
         mean_outcome = exp(mu + 0.5 * tau^2))







