# -------------------------------------------------------------------
# Function: get_data
# Purpose : Generate simulated cluster-level and individual-level data 
#           for a cluster randomized trial (CRT) under a specified link 
#           function and random-intercept model.
#
#           Each cluster receives a binary treatment assignment and a 
#           normally distributed random effect. Individual-level outcomes 
#           are generated according to:
#              η_ij = μ + δ * X_j + α_j
#              p_ij = g^{-1}(η_ij)
#              Y_ij ~ Bernoulli(p_ij)
#
# Arguments:
#   n_clusters          : Number of clusters (must be even for balanced allocation)
#   cluster_level_mean  : Mean of the cluster-level random effects (α_j)
#   cluster_size        : Number of individuals per cluster
#   tau                 : Standard deviation of the cluster-level random effects
#   mu                  : Intercept controlling baseline prevalence
#   delta               : Treatment effect on the link scale
#   link                : Link function ("log", "probit", "cloglog", "logit")
#
# Returns:
#   Data frame with one row per individual, including:
#     - cluster   : cluster ID
#     - treatment : cluster-level treatment indicator (0 = control, 1 = treated)
#     - outcome   : binary outcome simulated from Bernoulli(p_ij)
#     - eta       : linear predictor value (μ + δX + α)
#
#   The object also contains attributes for simulation parameters
# -------------------------------------------------------------------

get_data <- function(
  n_clusters = 20,
  cluster_level_mean = 0,
  cluster_size = 10,
  tau = 1,
  mu = -3.5,
  delta = 0.5,
  link = c("log", "probit", "cloglog", "logit")) {
  
  ## ---- Input validation --------------------------------------------------
  # Validate the specified link function
  link <- match.arg(link)
  
  # Helper validation functions
  is_scalar <- function(x) length(x) == 1L
  is_whole  <- function(x) is.finite(x) && abs(x - round(x)) < .Machine$double.eps^0.5
  
  # Validate all numeric inputs
  if (!is_scalar(n_clusters) || !is_whole(n_clusters) || n_clusters < 2)
    stop("`n_clusters` must be an integer >= 2.")
  if (n_clusters %% 2L != 0L)
    stop("`n_clusters` must be even for equal allocation.")
  if (!is_scalar(cluster_size) || !is_whole(cluster_size) || cluster_size < 1)
    stop("`cluster_size` must be an integer >= 1.")
  if (!is_scalar(tau) || tau < 0) stop("`tau` must be numeric >= 0.")
  for (nm in c("mu", "delta", "cluster_level_mean")) {
    val <- get(nm, inherits = FALSE)
    if (!is_scalar(val) || !is.finite(val))
      stop(sprintf("`%s` must be a finite scalar numeric.", nm))
  }
  
  ## ---- RNG seed ----------------------------------------------------------
  # (Seed setting handled externally by simulation framework)
  # set.seed(seed)
  
  ## ---- Treatment assignment ----------------------------------------------
  # Randomly assign half the clusters to treatment and half to control
  X <- rep(c(0L, 1L), each = n_clusters / 2L)
  X <- sample(X, size = length(X), replace = FALSE)
  
  ## ---- Random intercepts -------------------------------------------------
  # Draw cluster-level random effects
  alpha <- stats::rnorm(n_clusters, mean = cluster_level_mean, sd = tau)
  
  ## ---- Expand to individual level ---------------------------------------
  # Replicate cluster-level quantities to the individual level
  cluster_id <- rep(seq_len(n_clusters), each = cluster_size)
  X_i       <- X[cluster_id]
  alpha_i   <- alpha[cluster_id]
  
  ## ---- Linear predictor --------------------------------------------------
  # Compute η_ij = μ + δ * X_j + α_j
  eta <- mu + delta * X_i + alpha_i
  
  ## ---- Apply inverse link ------------------------------------------------
  # Compute p_ij = g^{-1}(η_ij) for the chosen link function
  p_ij <- switch(link,
                 logit   = stats::plogis(eta),
                 probit  = stats::pnorm(eta),
                 cloglog = 1 - exp(-exp(eta)),
                 log     = exp(eta)
  )
  
  ## ---- Simulate outcomes -------------------------------------------------
  # Generate binary outcomes Y_ij ~ Bernoulli(p_ij)
  n_obs   <- length(cluster_id)
  outcome <- stats::rbinom(n_obs, size = 1L, prob = p_ij)
  
  ## ---- Assemble data -----------------------------------------------------
  # Return data frame with all simulated quantities
  dat <- data.frame(
    cluster   = cluster_id,
    treatment = X_i,
    outcome   = outcome,
    eta       = eta
  )
  
  ## ---- Attributes --------------------------------------------------------
  # Store simulation parameters as attributes for transparency
  attr(dat, "params") <- list(
    # seed = seed,
    n_clusters = n_clusters,
    cluster_level_mean = cluster_level_mean,
    cluster_size = cluster_size,
    tau = tau,
    mu = mu,
    delta = delta,
    link = link
  )
  
  # Compute latent ICC if using a logit link (based on logistic variance π²/3)
  attr(dat, "latent_icc") <- if (link == "logit" && tau > 0)
    (tau^2) / (tau^2 + (pi^2)/3) else NA_real_
  
  return(dat)
}

