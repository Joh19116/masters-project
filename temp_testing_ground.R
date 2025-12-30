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



library(dplyr)
library(ggplot2)

# --- helper function to compare eta distributions at mu extremes ---
plot_eta_mu_extremes <- function(
    prevalence = 0.05,
    tau = 1,
    delta = 0.5,
    n_clusters = 20,
    cluster_size = 10
) {
  
  set.seed(123455589)
  
  # Get mu range and extract extremes
  mus <- get_mu_range(prevalence = prevalence)
  mu_min <- min(mus)
  mu_max <- max(mus)
  
  # Generate data at lower bound (safe / rare)
  dat_min <- get_data(
    n_clusters = n_clusters,
    cluster_size = cluster_size,
    tau = tau,
    mu = mu_min,
    delta = delta,
    link = "log"
  ) %>%
    mutate(mu_label = sprintf("mu_min = %.2f", mu_min))
  
  # Generate data at upper bound (edge of validity)
  dat_max <- get_data(
    n_clusters = n_clusters,
    cluster_size = cluster_size,
    tau = tau,
    mu = mu_max,
    delta = delta,
    link = "log"
  ) %>%
    mutate(mu_label = sprintf("mu_max = %.2f", mu_max))
  
  # Combine
  dat_all <- bind_rows(dat_min, dat_max)
  
  # Quantify invalid probability rate
  invalid_summary <- dat_all %>%
    group_by(mu_label) %>%
    summarise(
      pct_eta_gt_0 = mean(eta > 0) * 100,
      .groups = "drop"
    )
  
  print(invalid_summary)
  
  # Plot
  ggplot(dat_all, aes(x = eta, fill = mu_label)) +
    geom_density(alpha = 0.4, color = "black") +
    geom_vline(
      xintercept = 0,
      linetype = "dotted",
      color = "red",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = 0.05,
      y = 0.25,
      label = "eta = 0\n(exp(eta) = 1)",
      color = "red",
      hjust = 0
    ) +
    labs(
      title = "η Distributions at Lower and Upper Bounds of μ",
      subtitle = sprintf(
        "Generated using get_mu_range(prevalence = %.2f)",
        prevalence
      ),
      x = "Linear Predictor (eta)",
      y = "Density",
      fill = "Baseline Intercept (μ)"
    ) +
    coord_cartesian(xlim = c(-6, 2)) +
    theme_minimal(base_size = 14)
}

# --- run ---
plot_eta_mu_extremes(prevalence = 0.05, tau = 1, delta = 0.5)
