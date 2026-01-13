library(ggplot2)
library(tidyverse)

# load in the summary table

# summary_tbl



# Convergence by MU (overall) ####

# data manipulation

conv_mu_tbl <- summary_tbl %>%
  group_by(mu, model_type) %>%
  summarise(
    mean_conv_rate = mean(conv_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Plot

ggplot(conv_mu_tbl,
       aes(x = mu,
           y = mean_conv_rate,
           color = model_type,
           group = model_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = expression(mu),
    y = "Mean Convergence Rate",
    color = "Model Type",
    title = "Model Convergence Rate vs Baseline Risk (μ)"
  ) +
  theme_bw()



# Convergence by MU (by cluster size)  ####

# data manipulation

conv_mu_tbl <- summary_tbl %>%
  group_by(mu, model_type, n_clusters) %>%
  summarise(
    mean_conv_rate = mean(conv_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Plot

ggplot(conv_mu_tbl,
       aes(x = mu,
           y = mean_conv_rate,
           color = model_type,
           group = model_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ n_clusters)+
  labs(
    x = expression(mu),
    y = "Mean Convergence Rate",
    color = "Model Type",
    title = "Model Convergence Rate vs Baseline Risk (μ)",
    subtitle = "By N Clusters"
  ) +
  theme_bw()

# Convergence by MU (by n clusters)  ####

# data manipulation

conv_mu_tbl <- summary_tbl %>%
  group_by(mu, model_type, n_clusters) %>%
  summarise(
    mean_conv_rate = mean(conv_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Plot

ggplot(conv_mu_tbl,
       aes(x = mu,
           y = mean_conv_rate,
           color = model_type,
           group = model_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ n_clusters)+
  labs(
    x = expression(mu),
    y = "Mean Convergence Rate",
    color = "Model Type",
    title = "Model Convergence Rate vs Baseline Risk (μ)",
    subtitle = "By N Clusters"
  ) +
  theme_bw()

# Convergence by MU (by ICC)  ####

# data manipulation

conv_mu_tbl <- summary_tbl %>%
  group_by(mu, model_type, icc) %>%
  summarise(
    mean_conv_rate = mean(conv_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Plot

ggplot(conv_mu_tbl,
       aes(x = mu,
           y = mean_conv_rate,
           color = model_type,
           group = model_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ icc)+
  labs(
    x = expression(mu),
    y = "Mean Convergence Rate",
    color = "Model Type",
    title = "Model Convergence Rate vs Baseline Risk (μ)",
    subtitle = "By ICC"
  ) +
  theme_bw()




# Bias MSE and MAE ####

metrics_tbl <- summary_tbl |>
  group_by(icc, model_type) |>
  summarise(
    bias = mean(bias_delta, na.rm = TRUE),
    mae  = mean(mae_est,  na.rm = TRUE),
    mse  = mean(mse_est,  na.rm = TRUE),
    .groups = "drop"
  )


ggplot(metrics_tbl,
       aes(x = icc, y = bias, color = model_type, group = model_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "ICC",
    y = "Mean Bias",
    color = "Model Type",
    title = "Bias vs ICC"
  ) +
  theme_bw()

ggplot(metrics_tbl,
       aes(x = icc, y = mae, color = model_type, group = model_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "ICC",
    y = "Mean Absolute Error (MAE)",
    color = "Model Type",
    title = "MAE vs ICC"
  ) +
  theme_bw()

ggplot(metrics_tbl,
       aes(x = icc, y = mse, color = model_type, group = model_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "ICC",
    y = "Mean Squared Error (MSE)",
    color = "Model Type",
    title = "MSE vs ICC"
  ) +
  theme_bw()



# ------------------------------------------------------------
# Table of Error Messages 
# ------------------------------------------------------------
library(dplyr)
library(gt)

error_table_by_model <- sim$results %>%
  filter(!is.na(err)) %>%
  count(model_type, err, name = "Frequency") %>%
  group_by(model_type) %>%
  mutate(
    Percent = 100 * Frequency / sum(Frequency)
  ) %>%
  ungroup() %>%
  arrange(model_type, desc(Frequency))

error_table_by_model %>%
  gt(
    groupname_col = "model_type"
  ) %>%
  fmt_number(
    columns = Percent,
    decimals = 2
  ) %>%
  cols_label(
    err = "Error Message",
    Frequency = "Count",
    Percent = "Percent (%)"
  )
