
get_data <- function(#seed = 707490,
                             n_clusters = 20,
                             cluster_level_mean = 0,
                             cluster_size = 10,
                             tau = 1,
                             mu = -3.5,
                             delta = 0.5,
                             link = c("log", "probit", "cloglog", "logit")) {
  
  ## ---- Input validation --------------------------------------------------
  
  # Validate that the inputed link function is one of the prespecificed
  # If no link function is chosen, the default is the fist in the list
  link <- match.arg(link)
  
  is_scalar <- function(x) length(x) == 1L
  is_whole  <- function(x) is.finite(x) && abs(x - round(x)) < .Machine$double.eps^0.5
  
  # if (!is_scalar(seed) || !is.finite(seed)) stop("`seed` must be a finite scalar.")
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
  # set.seed(seed)
  
  ## ---- Treatment assignment ----------------------------------------------
  X <- rep(c(0L, 1L), each = n_clusters / 2L)
  X <- sample(X, size = length(X), replace = FALSE)
  
  ## ---- Random intercepts -------------------------------------------------
  alpha <- stats::rnorm(n_clusters, mean = cluster_level_mean, sd = tau)
  
  ## ---- Expand to individual level ---------------------------------------
  cluster_id <- rep(seq_len(n_clusters), each = cluster_size)
  X_i       <- X[cluster_id]
  alpha_i   <- alpha[cluster_id]
  
  ## ---- Linear predictor --------------------------------------------------
  eta <- mu + delta * X_i + alpha_i
  
  ## ---- Apply inverse link ------------------------------------------------
  p_ij <- switch(link,
                 logit   = stats::plogis(eta),
                 probit  = stats::pnorm(eta),
                 cloglog = 1 - exp(-exp(eta)),
                 log     = exp(eta)
  )
  
  ## ---- Simulate outcomes -------------------------------------------------
  n_obs   <- length(cluster_id)
  outcome <- stats::rbinom(n_obs, size = 1L, prob = p_ij)
  
  ## ---- Assemble data -----------------------------------------------------
  dat <- data.frame(
    cluster   = cluster_id,
    treatment = X_i,
    outcome   = outcome,
    eta = eta
  )
  
  ## ---- Attributes --------------------------------------------------------
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
  attr(dat, "latent_icc") <- if (link == "logit" && tau > 0)
    (tau^2) / (tau^2 + (pi^2)/3) else NA_real_
  
  return(dat)
}
