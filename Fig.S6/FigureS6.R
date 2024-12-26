library(ggplot2)           # v3.5.0
library(patchwork)         # v1.2.0

setwd('path_to_your_working_directory')

ssrd_data <- read.csv("National_mean.csv", header = TRUE)
ssrd_245 <- ssrd_data[1:3,]
ssrd_585 <- ssrd_data[c(1, 4, 5),]
ssrd_245$ssp <- factor(ssrd_245$ssp, levels = c("present", "near_245", "far_245"))
ssrd_585$ssp <- factor(ssrd_585$ssp, levels = c("present", "near_585", "far_585"))

p_245 <- ggplot(ssrd_245, aes(x = ssp, y = ssrd)) +
  geom_bar(stat = "identity", fill = "#7fccba", width = 0.35) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12, color = "black"),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        legend.position = 'none') +
  labs(title = "SSRD trend under SSP2-4.5 scenario",
       x = "",
       y = "ssrd (J m-2)") +
  lims(y = c(0, 20000000)) 

p_585 <- ggplot(ssrd_585, aes(x = ssp, y = ssrd)) +
  geom_bar(stat = "identity", fill = "#7fccba", width = 0.35) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        legend.position = 'none') +
  labs(title = "SSRD trend under SSP5-8.5 scenario",
       x = "",
       y = "") +
  lims(y = c(0, 20000000))

p_245 + p_585

#ggsave("ssrd_trend_bar.pdf", plot = last_plot(), width = 10, height = 6, units = "in")
