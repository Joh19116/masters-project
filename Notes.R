
# Continous

seed <- 77
set.seed(seed)

# 
# # Parameters
# n_clusters <- 100
# cluster_size <- 10
# tau <- 2          # between-cluster SD
# sigma <- 1        # within-cluster SD
# 
# # Simulate cluster-level random intercepts
# alpha <- rnorm(n_clusters, mean = 0, sd = tau)
# 
# # Assign each individual to a cluster
# cluster_id <- rep(1:n_clusters, each = cluster_size)
# 
# # Simulate individual-level errors
# epsilon <- rnorm(n_clusters * cluster_size, mean = 0, sd = sigma)
# 
# # Combine to get outcome
# Y <- alpha[cluster_id] + epsilon
# 
# # Create data frame
# dat_continuous <- data.frame(
#   cluster = cluster_id,
#   outcome = Y
# )
# 
# head(dat_continuous)
# 

# Binary
set.seed(77)

# Parameters
n_clusters <- 100
cluster_size <- 10
tau <- 1            # SD of random intercept
mu <- -1            # baseline log-odds
delta <- 1          # treatment effect

# Cluster-level treatment assignment (0 = control, 1 = treatment)
# EVEN SPLIT RANDOMIZATION
#sample(c(rep(0,10),rep(1,10)))
X <- rbinom(n_clusters, size = 1, prob = 0.5)

# Simulate cluster-level random intercepts
alpha <- rnorm(n_clusters, mean = 0, sd = tau)

# Assign individuals to clusters
cluster_id <- rep(1:n_clusters, each = cluster_size)
X_i <- X[cluster_id]
alpha_i <- alpha[cluster_id]

# Compute logit(p_ij) and then p_ij


logit_p <- mu + delta * X_i + alpha_i

# logit(x) = log(1/(1-x))
# expit(x) = e^x/(1+e^x)
# -> 
#plogis gives expit function
p_ij <- plogis(logit_p)

# Simulate binary outcomes
Y_bin <- rbinom(n_clusters * cluster_size, size = 1, prob = p_ij)

# Create data frame
dat_binary <- data.frame(
  cluster = cluster_id,
  treatment = X_i,
  outcome = Y_bin
)

head(dat_binary)

# put this in a function 

# generate fake data
#fit the model you are intersest in
# recover true parameters
# run binary mixed model 
# lme4 package
# unbiased for delta
#couple deicmal points
#Bias, Variance, CI
#Variance 
#Confidence Interval Coverage
#95% estimate of CI bound contain delta
#SimEngine Package, google the package for documentation
# Website : https://avi-kenny.github.io/SimEngine/


# Building toward comparing the link functions
# p_ij <- plogis(logit_p) -> p_ij 
#<- expit(logit_p)
#<- exp(logit_p)
# <- cb(logit_p)


# we should varry 
# ICC = tau^2/ T^2 + Sigma^2
# 0 - 0.2
# num clusters and cluster size
# cluster 10, 50, 100
# cluster size = 10, 100, 500 ish
# 

# run sim with log link function 
# try and code the clark and bar link funciton
# vary the basline outcome probability
# plot with % converage and on the Y axis 
# mu is the xaxis 
# log link vs clark and bar link
# expirment with covariats
# store the convergence messages
# generate the datga using the log link



### Cut off line should be 0
## Adding covaraites should make the log binomal model fail
#3 covarites will change how we calcualte the optimal mu value
# guess and check mu values in the covarites model, keep missingness below about 1%
# Write the paper as you go


# GEE vs GLMM
# Poisson model in GEE converges, which does it sooner? 


# People just might be more comfortagble with mixed models, which are more efficient 
# How do we decide the choicde og cut off point K?
# Covaraints 1 binary and 1 continou sfromt he noraml distribution 
# Rtruncnorm at -2 and 2
# Duke Compute cluster!!!! 
# Github code as well
