library(lme4)
source("blended_link_function.R")

analyze_glmm <- function(dat) {
  fit <- tryCatch(
    glmer(outcome ~ treatment + (1 | cluster),
          data = dat,
          family = binomial(link = "log"),
          control = glmerControl(optimizer = "bobyqa")),
    error = function(e) return(NULL),
    warning = function(w) invokeRestart("muffleWarning") # keep going on warnings
  )
  
  if (is.null(fit) || !is(fit, "glmerMod")) {
    return(list(delta_hat = NA, se = NA, conv = FALSE))
  }
  
  est <- summary(fit)$coef["treatment", "Estimate"]
  se  <- summary(fit)$coef["treatment", "Std. Error"]
  conv <- is.null(fit@optinfo$conv$lme4$messages) # TRUE if no convergence msgs
  
  return(list(delta_hat = est, se = se, conv = conv))
} 


analyze_glmm_blended <- function(dat) {
  fit <- tryCatch(
    glmer(outcome ~ treatment + (1 | cluster),
          data = dat,
          family = binomial(link = blended_link(K=log(0.8))),
          control = glmerControl(optimizer = "bobyqa")),
    error = function(e) return(NULL),
    warning = function(w) invokeRestart("muffleWarning") # keep going on warnings
  )
  
  if (is.null(fit) || !is(fit, "glmerMod")) {
    return(list(delta_hat = NA, se = NA, conv = FALSE))
  }
  
  est <- summary(fit)$coef["treatment", "Estimate"]
  se  <- summary(fit)$coef["treatment", "Std. Error"]
  conv <- is.null(fit@optinfo$conv$lme4$messages) # TRUE if no convergence msgs
  
  return(list(delta_hat = est, se = se, conv = conv))
}    
                    
