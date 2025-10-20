# -------------------------------------------------------------------
# Function: get_mu_range
# Purpose : Generate a sequence of valid μ (intercept) values
#           for CRT data generation under a log-link model.
#
#           The range is determined by two constraints:
#           (1) desired baseline prevalence (how rare the event is)
#           (2) maximum acceptable "missingness" (probabilities > 1)
#
# Returns  : Numeric vector of μ values evenly spaced between μ_min and μ_max
# -------------------------------------------------------------------

get_mu_range <- function(
  prevalence,        # target average outcome probability (e.g., 0.03)
  max_missing = 0.05,# acceptable proportion of invalid probs (e.g., 5%)
  tau = 1,           # SD of random cluster effects
  delta = 0.5,       # treatment effect size (log scale)
  cluster_level_mean = 0, # mean of random effects
  n_mu = 10          # how many μ values to return (e.g., 10, 20)
) {
  # ---- 1. Validate inputs -----------------------------------------
  if (!is.numeric(prevalence) || prevalence <= 0 || prevalence >= 1)
    stop("`prevalence` must be between 0 and 1 (exclusive).")
  
  if (!is.numeric(max_missing) || max_missing <= 0 || max_missing >= 1)
    stop("`max_missing` must be between 0 and 1 (exclusive).")
  
  if (!is.numeric(tau) || tau <= 0)
    stop("`tau` must be a positive number.")
  
  if (!is.numeric(delta))
    stop("`delta` must be numeric.")
  
  if (!is.numeric(n_mu) || n_mu < 2)
    stop("`n_mu` must be an integer >= 2.")
  
  # ---- 2. Critical value for Normal tail --------------------------
  # For example, if max_missing = is 5% ->  0.05 -> z = 1.645
  #
  z <- qnorm(1 - max_missing)
  
  # ---- 3. Compute lower bound (μ_min) -----------------------------
  # Ensures the baseline (control) outcome has the desired average prevalence.
  #
  mu_min <- log(prevalence) - 0.5 * tau^2
  
  # ---- 4. Compute upper bound (μ_max) -----------------------------
  # Ensures the proportion of invalid probabilities (η > 0) ≤ max_missing.
  #
  # "Missingness" occurs when η = μ + δX + α > 0.
  # To keep that ≤ ε for the worst arm (usually treatment if δ > 0):
  # μ ≤ -δ - cluster_level_mean - z * τ
  #
  mu_max <- -max(0, delta) - cluster_level_mean - z * tau
  
  # ---- 5. Check ----------------------------------------------------
  if (mu_min > mu_max) {
    warning("For the chosen inputs, μ_min > μ_max. 
             Try reducing tau or increasing acceptable missingness.")
  }
  
  # ---- 6. Create evenly spaced μ values ---------------------------
  mu_vec <- seq(from = mu_min, to = mu_max, length.out = n_mu)
  
#   # ---- 7. Return results with attributes for transparency ---------
#   attr(mu_vec, "params") <- list(
#     prevalence = prevalence,
#     max_missing = max_missing,
#     tau = tau,
#     delta = delta,
#     cluster_level_mean = cluster_level_mean,
#     n_mu = n_mu,
#     z_crit = z,
#     mu_min = mu_min,
#     mu_max = mu_max
#   )
#   
   return(mu_vec)
}

