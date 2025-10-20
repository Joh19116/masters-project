blended_link <- function(K = log(0.8)) {
  #-------------------------------------------------------
  # Calculate a and b based on chosen cutoff K
  # f1(eta) = exp(eta)    (log link inverse)
  # f2(eta) = exp(eta)/(1+exp(eta))  (logit inverse)
  #-------------------------------------------------------
  
  # f1(K)
  f1K <- exp(K)
  
  # f2 inverse: log(mu / (1 - mu))
  f2_inv <- function(mu) log(mu / (1 - mu))
  
  # f1'(eta) = exp(eta)
  f1_prime <- function(eta) exp(eta)
  
  # f2'(eta) = exp(eta) / (1 + exp(eta))^2
  f2_prime <- function(eta) exp(eta) / (1 + exp(eta))^2
  
  # Calculate intermediate value
  eta2 <- f2_inv(f1K)
  
  # b = f1'(K) / f2'(eta2)
  b <- f1_prime(K) / f2_prime(eta2)
  
  # a = eta2 - b*K
  a <- eta2 - b * K
  
  #-------------------------------------------------------
  # Define linkinv (eta -> mu)
  #-------------------------------------------------------
  linkinv <- function(eta) {
    ifelse(eta <= K,
           exp(eta),
           exp(a + b * eta) / (1 + exp(a + b * eta)))
  }
  
  #-------------------------------------------------------
  # Define linkfun (mu -> eta)
  #-------------------------------------------------------
  linkfun <- function(mu) {
    eta1 <- log(mu)  # inverse of exp(eta)
    eta2 <- (log(mu / (1 - mu)) - a) / b  # inverse of scaled logit
    ifelse(mu <= exp(K), eta1, eta2)
  }
  
  #-------------------------------------------------------
  # Derivative d(mu)/d(eta)
  #-------------------------------------------------------
  mu.eta <- function(eta) {
    ifelse(eta <= K,
           exp(eta),  # derivative of exp(eta)
           {
             num <- b * exp(a + b * eta)
             den <- (1 + exp(a + b * eta))^2
             num / den
           })
  }
  
  #-------------------------------------------------------
  # Valid eta values (all real numbers are fine)
  #-------------------------------------------------------
  valideta <- function(eta) TRUE
  
  # Return as a GLM link object
  structure(list(
    linkfun = linkfun,
    linkinv = linkinv,
    mu.eta = mu.eta,
    valideta = valideta,
    name = paste0("blended log–logit (K=", round(K,3), 
                  ", a=", round(a,3), ", b=", round(b,3), ")")
  ), class = "link-glm")
}

