library(ggplot2)     # v3.5.0
library(tidymodels)  # v1.1.1
library(patchwork)   # v1.2.0

setwd('path_to_your_working_directory')

wet_weight_data <- read.csv("Wet_weight.csv", header = TRUE)
dry_weight_data <- read.csv("Dry_weight.csv", header = TRUE)

# Linear fit====================================================================
wet_weight_fit <- lm(mehg_log10 ~ thg_log10, data = wet_weight_data)

summary_wet_weight_fit <- summary(wet_weight_fit)

wet_weight_rsq <- summary_wet_weight_fit$adj.r.squared

dry_weight_fit <- lm(mehg_log10 ~ thg_log10, data = dry_weight_data)

summary_dry_weight_fit <- summary(dry_weight_fit)

dry_weight_rsq <- summary_dry_weight_fit$adj.r.squared

# Scatter plot==================================================================
wet_weight_plot <- ggplot(wet_weight_data, aes(x = thg_log10, y = mehg_log10)) +
  geom_point(shape = 21, size = 4, stroke = 0.5, color = 'black', fill = "#7fccba") +
  facet_grid(. ~ group) +
  geom_smooth(method = "lm", se = F, color = "#e76a42", linewidth = 1, linetype = "dashed") + 
  coord_obs_pred() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14, color = "black"),
        legend.position = 'right') +
  labs(x = 'Log10-scaled THg concentration', y = 'Log10-scaled MeHg concentraion') 

wet_weight_plot

dry_weight_plot <- ggplot(dry_weight_data, aes(x = thg_log10, y = mehg_log10)) +
  geom_point(shape = 21, size = 4, stroke = 0.5, color = 'black', fill = "#7fccba") +
  facet_grid(. ~ group) +
  geom_smooth(method = "lm", se = F, color = "#e76a42", linewidth = 1, linetype = "dashed") +
  coord_obs_pred() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14, color = "black"),
        legend.position = 'right') +
  labs(x = 'Log10-scaled THg concentration', y = '') #+

dry_weight_plot

wet_weight_plot + dry_weight_plot
#ggsave("MeHg_THg_linear_relationship.pdf", plot = last_plot(), width = 10, height = 6, units = "in")
