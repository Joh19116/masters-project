
tau_to_icc <- function(tau) {
  if (tau <= 0) return(0)
  tau^2 / (1 + tau^2)
}

rho <- tau_to_icc(1)


data <- rcbin(prop = 0.5, prvar = 0, noc = 20, csize = 10, csvar = 0.4, rho = rho)
