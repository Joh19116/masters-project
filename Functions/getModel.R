# -------------------------------------------------------------------
# Function: analyze_glmm
# Purpose : Fit a generalized linear mixed model (GLMM) with a 
#           log link to simulated CRT data using lme4::glmer().
#
#           The model includes:
#             outcome ~ treatment + sex + cont_cov +(1 | cluster)
#           where cluster is a random intercept.
#
#           This function extracts the treatment effect estimate (δ̂),
#           its standard error, and convergence status.
#
# Arguments:
#   dat : Data frame returned by get_data(), containing outcome,
#         treatment, and cluster variables.
#
# Returns:
#   A list with:
#     - delta_hat : estimated treatment effect
#     - se        : standard error of treatment effect
#     - conv      : logical indicator of convergence success
#
# Notes:
#   - Warnings are suppressed but errors safely return NULL.
#   - Returns NA values if model fails to converge or fit.
# -------------------------------------------------------------------


analyze_glmm <- function(dat, tol_sing = 1e-4) {
  
  warn_msgs <- character(0)
  
  fit <- tryCatch(
    withCallingHandlers(
      lme4::glmer(
        outcome ~ treatment + sex + cont_cov + (1 | cluster),
        data = dat,
        family  = stats::binomial(link = "log"),
        control = lme4::glmerControl(optimizer = "bobyqa")
      ),
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  
  # Hard failure
  if (inherits(fit, "error") || is.null(fit) || !inherits(fit, "glmerMod")) {
    return(list(
      delta_hat   = NA_real_,
      se          = NA_real_,
      conv        = NA_integer_,
      singular    = NA,
      dropped_trt = NA,
      err         = if (inherits(fit, "error")) conditionMessage(fit) else "fit was NULL/not glmerMod",
      warn        = if (length(warn_msgs)) paste(unique(warn_msgs), collapse = " | ") else NA_character_
    ))
  }
  
  # Diagnostics
  singular <- lme4::isSingular(fit, tol = tol_sing)
  conv <- fit@optinfo$conv$opt
  if (is.null(conv) || length(conv) == 0) conv <- NA_integer_
  conv <- as.integer(conv)
  
  
  beta <- lme4::fixef(fit)
  
  if (!("treatment" %in% names(beta))) {
    return(list(
      delta_hat   = NA_real_,
      se          = NA_real_,
      conv        = conv,
      singular    = singular,
      dropped_trt = TRUE,
      err         = paste0("treatment coefficient missing. fixef terms: ",
                           paste(names(beta), collapse = ", ")),
      warn        = if (length(warn_msgs)) paste(unique(warn_msgs), collapse = " | ") else NA_character_
    ))
  }
  
  V <- as.matrix(stats::vcov(fit))
  if (!("treatment" %in% rownames(V))) {
    return(list(
      delta_hat   = unname(beta["treatment"]),
      se          = NA_real_,
      conv        = conv,
      singular    = singular,
      dropped_trt = FALSE,
      err         = "vcov missing treatment row/col",
      warn        = if (length(warn_msgs)) paste(unique(warn_msgs), collapse = " | ") else NA_character_
    ))
  }
  
  est <- unname(beta["treatment"])
  se  <- sqrt(V["treatment", "treatment"])
  
  list(
    delta_hat   = est,
    se          = se,
    conv        = conv,
    singular    = singular,
    dropped_trt = FALSE,
    err         = NA_character_,
    warn        = if (length(warn_msgs)) paste(unique(warn_msgs), collapse = " | ") else NA_character_
  )
}



# -------------------------------------------------------------------
# Function: analyze_glmm_blended
# Purpose : Fit the same GLMM structure as analyze_glmm, but using
#           the custom blended log–logit link instead of a standard
#           log link.
#
#           This evaluates the proposed blended link method’s
#           performance under simulated CRT data.
#
# Arguments:
#   dat : Data frame returned by get_data(), containing outcome,
#         treatment, and cluster variables.
#
# Returns:
#   A list with:
#     - delta_hat : estimated treatment effect under blended link
#     - se        : standard error of treatment effect
#     - conv      : logical indicator of convergence success
#
# Notes:
#   - Uses the blended_link(K = log(0.8)) transition parameter.
#   - The structure and output mirror analyze_glmm for comparison.
#   - Errors return NULL; warnings are muted to allow batch runs.
# -------------------------------------------------------------------

analyze_glmm_blended <- function(dat, K = log(0.9), tol_sing = 1e-4) {
  
  # Fit model; capture warnings but do not spam logs
  warn_msgs <- character(0)
  
  fit <- tryCatch(
    withCallingHandlers(
      lme4::glmer(
        outcome ~ treatment + sex + cont_cov + (1 | cluster),
        data = dat,
        family  = stats::binomial(link = blended_link(K = K)),
        control = lme4::glmerControl(optimizer = "bobyqa")
      ),
      warning = function(w) {
        warn_msgs <<- c(warn_msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  
  # Hard failure (glmer threw an error)
  if (inherits(fit, "error") || is.null(fit) || !inherits(fit, "glmerMod")) {
    return(list(
      delta_hat = NA_real_,
      se        = NA_real_,
      conv      = NA_integer_,
      singular  = NA,
      dropped_trt = NA,
      err       = if (inherits(fit, "error")) conditionMessage(fit) else "fit was NULL/not glmerMod",
      warn      = if (length(warn_msgs)) paste(unique(warn_msgs), collapse = " | ") else NA_character_
    ))
  }
  
  # Diagnostics
  singular <- lme4::isSingular(fit, tol = tol_sing)
  conv <- fit@optinfo$conv$opt
  if (is.null(conv) || length(conv) == 0) conv <- NA_integer_
  conv <- as.integer(conv)
  
  
  # Extract treatment effect robustly (avoid summary() rowname assumptions)
  beta <- lme4::fixef(fit)
  
  # If treatment got dropped due to rank deficiency, it won't be in fixef()
  if (!("treatment" %in% names(beta))) {
    return(list(
      delta_hat = NA_real_,
      se        = NA_real_,
      conv      = conv,
      singular  = singular,
      dropped_trt = TRUE,
      err       = paste0("treatment coefficient missing. fixef terms: ",
                         paste(names(beta), collapse = ", ")),
      warn      = if (length(warn_msgs)) paste(unique(warn_msgs), collapse = " | ") else NA_character_
    ))
  }
  
  V <- as.matrix(stats::vcov(fit))
  # Guard against vcov weirdness
  if (!("treatment" %in% rownames(V))) {
    return(list(
      delta_hat = NA_real_,
      se        = NA_real_,
      conv      = conv,
      singular  = singular,
      dropped_trt = FALSE,
      err       = "vcov missing treatment row/col",
      warn      = if (length(warn_msgs)) paste(unique(warn_msgs), collapse = " | ") else NA_character_
    ))
  }
  
  est <- unname(beta["treatment"])
  se  <- sqrt(V["treatment", "treatment"])
  
  list(
    delta_hat = est,
    se        = se,
    conv      = conv,
    singular  = singular,
    dropped_trt = FALSE,
    err       = NA_character_,
    warn      = if (length(warn_msgs)) paste(unique(warn_msgs), collapse = " | ") else NA_character_
  )
}


                    
