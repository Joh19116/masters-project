# messing around withe the blened link function


source("blended_link_function.R")
source("getData.R")
source("getModel.R")


data <- get_data()
View(data)


glmer(outcome ~ treatment + (1 | cluster),
      data = data,
      family = binomial(link = "logit"),
      control = glmerControl(optimizer = "bobyqa"))

glmer(outcome ~ treatment + (1 | cluster),
      data = data,
      family = binomial(link = blended_link(K=log(0.8))),
      control = glmerControl(optimizer = "bobyqa"))



library(ggplot2)

X <- seq(-3,3, 0.01)
Y <- sapply(X, linkinv)

df <- data.frame(X=X, Y=Y)
ggplot(df, aes(x=X, y=Y)) + geom_line()




# --- helper function to compare eta distributions across two mu values ---
plot_eta_comparison <- function(mu1 = -2, mu2 = 0, tau = 1, delta = 0.5,
                                n_clusters = 20, cluster_size = 10) {
  
  set.seed(123456789)
  # Generate two datasets with different mu values
  dat1 <- get_data(
    n_clusters = n_clusters,
    cluster_size = cluster_size,
    tau = tau,
    mu = mu1,
    delta = delta,
    link = "log"
  )
  
  dat2 <- get_data(
    n_clusters = n_clusters,
    cluster_size = cluster_size,
    tau = tau,
    mu = mu2,
    delta = delta,
    link = "log"
  )
  
  # Add labels for which mu each dataset came from
  dat1$mu_label <- sprintf("mu = %.2f", mu1)
  dat2$mu_label <- sprintf("mu = %.2f", mu2)
  
  # Combine the two datasets
  dat_all <- dplyr::bind_rows(dat1, dat2)
  
  # Compute NA rate (proportion of invalid outcomes) for each dataset
  na_summary <- dat_all %>%
    dplyr::group_by(mu_label) %>%
    dplyr::summarise(na_rate = mean(is.na(outcome)) * 100)
  
  print(na_summary)
  
  # Define the eta threshold corresponding to exp(eta) = 1.05 (5% over 1)
  eta_cutoff <- log(1.05)
  
  # Plot the density comparison
  ggplot(dat_all, aes(x = eta, fill = mu_label)) +
    geom_density(alpha = 0.4) +
    geom_vline(xintercept = eta_cutoff, color = "red", linetype = "dotted", linewidth = 1) +
    annotate("text", x = eta_cutoff + 0.2, y = 0.25,
             label = "exp(η) = 1.05 (5% > 1)", color = "red", hjust = 0) +
    labs(
      title = sprintf("Comparison of η Distributions (tau = %.2f, delta = %.2f)", tau, delta),
      subtitle = sprintf("Red dotted line: 5%% missingness cutoff (exp(η) > 1)"),
      x = expression(eta[i*j]),
      y = "Density",
      fill = "Mean (μ)"
    ) +
    theme_minimal(base_size = 14)
}

# Example usage:
plot_eta_comparison(mu1 = mus[10], mu2 = -1, tau = 1, delta = 0.5)






