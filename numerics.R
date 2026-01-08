library(dplyr)
library(knitr)
library(kableExtra)

# ------------------------------------------------------------
# Overall table (one row per model)
# ------------------------------------------------------------
numerics_overall <- summary_tbl |>
  group_by(model_type) |>
  summarise(
    bias = mean(bias_delta, na.rm = TRUE),
    mae  = mean(mae_est,  na.rm = TRUE),
    mse  = mean(mse_est,  na.rm = TRUE),
    rmse = sqrt(mean(mse_est, na.rm = TRUE)),
    .groups = "drop"
  )

numerics_overall |>
  mutate(
    bias = round(bias, 4),
    mae  = round(mae, 4),
    mse  = round(mse, 4),
    rmse = round(rmse, 4)
  ) |>
  kable(
    format = "html",
    caption = "Overall performance metrics by model type",
    col.names = c("Model", "Bias", "MAE", "MSE", "RMSE"),
    align = c("l", "r", "r", "r", "r")
  ) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "condensed")) |>
  column_spec(1, bold = TRUE)


# ------------------------------------------------------------
# By other variables table (one row per model per other)
# ------------------------------------------------------------
numerics_by_mu <- summary_tbl |>
  group_by(model_type, icc) |>
  summarise(
    bias = mean(bias_delta, na.rm = TRUE),
    mae  = mean(mae_est,  na.rm = TRUE),
    mse  = mean(mse_est,  na.rm = TRUE),
    rmse = sqrt(mean(mse_est, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(model_type, icc)

numerics_by_mu |>
  mutate(
    icc = icc,
    bias = round(bias, 4),
    mae  = round(mae, 4),
    mse  = round(mse, 4),
    rmse = round(rmse, 4)
  ) |>
  kable(
    format = "html",
    caption = "Performance metrics by N Clusters and model type",
    col.names = c("Model", "ICC", "Bias", "MAE", "MSE", "RMSE"),
    align = c("l", "r", "r", "r", "r", "r")
  ) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "condensed")) |>
  collapse_rows(columns = 1, valign = "top") |>
  column_spec(1, bold = TRUE)

