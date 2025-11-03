# -------------------------------------------------------------------
# Function: blended_link
# Purpose : Define a custom blended log–logit link function that 
#           transitions smoothly between a log link and a logit link.
#
#           Below the cutoff point K, the function behaves like a 
#           traditional log link:      μ = exp(η)
#           Above K, it switches to a scaled logit link: 
#                                        μ = exp(a + bη) / (1 + exp(a + bη))
#
#           Constants a and b are chosen to ensure continuity and
#           smoothness (matching function value and slope) at η = K.
#
# Arguments:
#   K : Cutoff value (on η scale) where blending occurs. 
#       Controls the transition point between log and logit regions.
#
# Returns:
#   A "link-glm" object containing:
#     - linkfun : function mapping μ → η
#     - linkinv : function mapping η → μ
#     - mu.eta  : derivative of μ with respect to η
#     - valideta: function validating η domain
#     - name    : descriptive label with parameters (K, a, b)
#
# Use:
#   Passed as the link argument in a GLM or custom modeling function,
#   e.g., glm(y ~ x, family = binomial(link = blended_link()))
# -------------------------------------------------------------------

blended_link <- function(K = log(0.8)) {
  #-------------------------------------------------------
  # Calculate a and b based on chosen cutoff K
  # f1(η) = exp(η)                   (log link inverse)
  # f2(η) = exp(η)/(1+exp(η))        (logit link inverse)
  #-------------------------------------------------------
  
  # f1(K)
  f1K <- exp(K)
  
  # f2 inverse: η = log(μ / (1 - μ))
  f2_inv <- function(mu) log(mu / (1 - mu))
  
  # f1'(η) = exp(η)
  f1_prime <- function(eta) exp(eta)
  
  # f2'(η) = exp(η) / (1 + exp(η))^2
  f2_prime <- function(eta) exp(eta) / (1 + exp(eta))^2
  
  # Calculate corresponding η value for f2 that gives f1(K)
  eta2 <- f2_inv(f1K)
  
  # b = ratio of derivatives at the transition point
  b <- f1_prime(K) / f2_prime(eta2)
  
  # a = intercept ensuring functional continuity at η = K
  a <- eta2 - b * K
  
  #-------------------------------------------------------
  # Define inverse link (η → μ)
  #-------------------------------------------------------
  linkinv <- function(eta) {
    ifelse(eta <= K,
           exp(eta),
           exp(a + b * eta) / (1 + exp(a + b * eta)))
  }
  
  #-------------------------------------------------------
  # Define link function (μ → η)
  #-------------------------------------------------------
  linkfun <- function(mu) {
    eta1 <- log(mu)  # inverse of exp(η)
    eta2 <- (log(mu / (1 - mu)) - a) / b  # inverse of scaled logit
    ifelse(mu <= exp(K), eta1, eta2)
  }
  
  #-------------------------------------------------------
  # Derivative of μ with respect to η
  #-------------------------------------------------------
  mu.eta <- function(eta) {
    ifelse(eta <= K,
           exp(eta),  # derivative under log link
           {
             num <- b * exp(a + b * eta)
             den <- (1 + exp(a + b * eta))^2
             num / den
           })
  }
  
  #-------------------------------------------------------
  # Valid η values (all real numbers)
  #-------------------------------------------------------
  valideta <- function(eta) TRUE
  
  #-------------------------------------------------------
  # Return the GLM link object
  #-------------------------------------------------------
  structure(list(
    linkfun = linkfun,
    linkinv = linkinv,
    mu.eta = mu.eta,
    valideta = valideta,
    name = paste0("blended log–logit (K=", round(K,3), 
                  ", a=", round(a,3), ", b=", round(b,3), ")")
  ), class = "link-glm")
}


