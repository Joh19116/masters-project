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


analyze_glmm <- function(dat) {
  fit <- tryCatch(
    glmer(outcome ~ treatment + sex + cont_cov + (1 | cluster),
          data = dat,
          family = binomial(link = "log"),
          control = glmerControl(optimizer = "bobyqa")),
    error = function(e) return(NULL),
    warning = function(w) invokeRestart("muffleWarning") # continue on warnings
  )
  
  if (is.null(fit) || !is(fit, "glmerMod")) {
    return(list(delta_hat = NA, se = NA, conv = FALSE))
  }
  
  est  <- summary(fit)$coef["treatment", "Estimate"]
  se   <- summary(fit)$coef["treatment", "Std. Error"]
  conv <- is.null(fit@optinfo$conv$lme4$messages) # TRUE if no convergence msgs
  
  return(list(delta_hat = est, se = se, conv = conv))
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

analyze_glmm_blended <- function(dat) {
  fit <- tryCatch(
    glmer(outcome ~ treatment + sex + cont_cov + (1 | cluster),
          data = dat,
          family = binomial(link = blended_link(K = log(0.8))),
          control = glmerControl(optimizer = "bobyqa")),
    error = function(e) return(NULL),
    warning = function(w) invokeRestart("muffleWarning") # continue on warnings
  )
  
  if (is.null(fit) || !is(fit, "glmerMod")) {
    return(list(delta_hat = NA, se = NA, conv = FALSE))
  }
  
  est  <- summary(fit)$coef["treatment", "Estimate"]
  se   <- summary(fit)$coef["treatment", "Std. Error"]
  conv <- is.null(fit@optinfo$conv$lme4$messages) # TRUE if no convergence msgs
  
  return(list(delta_hat = est, se = se, conv = conv))
}

                    
