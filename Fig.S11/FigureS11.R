library(ggplot2)   # v3.5.0

setwd('path_to_your_working_directory')

weight_length_data <- read.csv("Weight_length.csv", header = TRUE)

weight_length_fit <- lm(log_weight ~ log_length, data = weight_length_data)
summary_weight_length_fit <- summary(weight_length_fit)
weight_length_rsq <- summary_weight_length_fit$adj.r.squared

weight_length_plot <- ggplot(weight_length_data, aes(x = log_length, y = log_weight)) +
  geom_point(shape = 21, size = 4, stroke = 0.5, color = 'black', fill = "#7fccba") +
  facet_grid(. ~ group) +
  geom_smooth(method = "lm", se = F, color = "#e76a42", linewidth = 1, linetype = "dashed") + 
  #coord_obs_pred() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14, color = "black"),
        legend.position = 'right') +
  labs(x = 'Log10-scaled length', y = 'Log10-scaled weight') 

weight_length_plot

#ggsave("weight_length_plot.pdf", plot = last_plot(), width = 6, height = 6, units = "in")