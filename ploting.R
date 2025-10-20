library(ggplot2)

# plot 
ggplot(summary_tbl,
       aes(x = mu, y = conv_rate,
           color = factor(icc),
           linetype = factor(n_clusters))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ cluster_size) +
  labs(x = "Baseline μ (log-odds)",
       y = "Convergence Rate",
       color = "ICC",
       linetype = "Number of Clusters",
       title = "Convergence rate across μ values") +
  theme_minimal()

library(dplyr)
library(ggplot2)

# Filter to your scenario of interest
plot_tbl <- summary_tbl %>%
  dplyr::filter(n_clusters == 10,
         cluster_size == 10,
         icc == 0.01)

# Make the plot
ggplot(plot_tbl, aes(x = mu, y = conv_rate, group = model_type)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Convergence vs μ (n_clusters=100, cluster_size=100, ICC=0.05)",
    x = "Baseline μ (log-odds)",
    y = "Convergence rate"
  ) +
  theme_minimal()








#### Testing Plots 

library(viridis)

# optional: set a consistent minimal theme for all figures
theme_pub <- theme_minimal(base_size = 14, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.position = "right",
    legend.box.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold")
  )


ggplot(data = summary_tbl, aes(
  x = mu, y = conv_rate,
  color = factor(icc),
  linetype = factor(cluster_size),
  group = interaction(icc, cluster_size)
)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ n_clusters, labeller = label_both, scales = "free_y") +
  scale_color_viridis_d(name = "ICC") +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash", "twodash")) +
  labs(
    x = expression(mu),
    y = "Convergence Rate",
    title = expression("Convergence Rate by " * mu * " across ICC and Cluster Size")
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pub


ggplot(summary_tbl, aes(x = mu, y = factor(icc), fill = conv_rate)) +
  geom_tile(color = "white", linewidth = 0.2) +
  facet_grid(model_type ~ cluster_size + n_clusters, labeller = label_both) +
  scale_fill_viridis_c(
    name = "Convergence Rate",
    option = "plasma",     # avoids yellow tones
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  labs(
    x = expression(mu),
    y = "ICC",
    title = expression("Heatmap of Convergence Rate across " * mu * ", ICC, and Model Type")
  ) +
  theme_pub +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    strip.text.y = element_text(face = "bold"),
    strip.text.x = element_text(face = "bold")
  )


ggplot(summary_tbl, aes(
  x = mu,
  y = conv_rate,
  color = factor(icc),
  linetype = model_type,
  group = interaction(icc, model_type)
)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.8, linewidth = 1.2) +
  facet_wrap(~ cluster_size, labeller = label_both, scales = "free_y") +
  scale_linetype_manual(
    name = "Model Type",
    values = c("solid", "longdash", "dotdash", "dotted")
  ) +
  labs(
    x = expression(mu),
    y = "Convergence Rate",
    title = expression("Smoothed Convergence Trends by Cluster Size, ICC, and Model Type")
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pub +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    legend.key.width = unit(1.2, "cm")
  )









