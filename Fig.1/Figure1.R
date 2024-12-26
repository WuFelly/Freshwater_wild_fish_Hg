library(ggplot2)    # v3.5.0
library(patchwork)  # v1.2.0
library(FSA)        # v0.9.5

setwd('path_to_your_working_directory')
## Fig1a========================================================================
fig1a_bar <- read.csv("Fig1a_bar_40cm.csv", header = TRUE, sep = ",")
fig1a_jitter <- read.csv("Fig1a_jitter_40cm.csv", header = TRUE, sep = ",")
fig1a_bar$sample_source <- factor(fig1a_bar$sample_source, levels = c("Wild", "Farmed"))

fig1a <- ggplot() + 
  geom_bar(data = fig1a_bar, aes(x = sample_source, y = mean_mehg),
           fill = "white",
           color = c("#7fccba", "#479E9B"),
           position = position_dodge(width = 0.2),
           stat = "identity", width = 0.25, linewidth = 1) +
  geom_jitter(data = fig1a_jitter, aes(x = sample_source, y = mehg, color = sample_source),
              size = 3, height = 0.02, width = 0.1, alpha = 0.8) +
  scale_color_manual(values = c("#479E9B", "#7fccba")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) + 
  geom_errorbar(data = fig1a_bar, aes(x = sample_source, 
                                      ymin = mean_mehg - sd_mehg, ymax = mean_mehg + sd_mehg),
                width = 0.1, color = c("#7fccba","#479E9B"), linewidth = 0.25,
                position = position_dodge(width = 0.65)) +
  theme(legend.position = "none") +
  labs(x = "", y = "MeHg content (μg kg-1, ww)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 14, color = "black"),
        panel.grid  = element_blank(),
        panel.background = element_blank())

fig1a

## Fig1b========================================================================
fig1b_bar <- read.csv("Fig1b_bar_50cm.csv", header = TRUE, sep = ",")
fig1b_jitter <- read.csv("Fig1b_jitter_50cm.csv", header = TRUE, sep = ",")
fig1b_bar$sample_source <- factor(fig1b_bar$sample_source, levels = c("Wild", "Farmed"))

fig1b <- ggplot() + 
  geom_bar(data = fig1b_bar, aes(x = sample_source, y = mean_mehg),
           fill = "white",
           color = c("#7fccba", "#479E9B"),
           position = position_dodge(width = 0.2),
           stat = "identity", width = 0.25, linewidth = 1) +
  geom_jitter(data = fig1b_jitter, aes(x = sample_source, y = mehg, color = sample_source),
              size = 3, height = 0.02, width = 0.1, alpha = 0.8) +
  scale_color_manual(values = c("#479E9B", "#7fccba")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) + 
  geom_errorbar(data = fig1b_bar, aes(x = sample_source, 
                                      ymin = mean_mehg - sd_mehg, ymax = mean_mehg + sd_mehg),
                width = 0.1, color = c("#7fccba","#479E9B"), linewidth = 0.25,
                position = position_dodge(width = 0.65)) +
  theme(legend.position = "none") +
  labs(x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_blank())

fig1b

## Fig1c========================================================================
fig1c_bar <- read.csv("Fig1c_bar_60cm.csv", header = TRUE, sep = ",")
fig1c_jitter <- read.csv("Fig1c_jitter_60cm.csv", header = TRUE, sep = ",")
fig1c_bar$sample_source <- factor(fig1c_bar$sample_source, levels = c("Wild", "Farmed"))

fig1c <- ggplot() + 
  geom_bar(data = fig1c_bar, aes(x = sample_source, y = mean_mehg),
           fill = "white",
           color = c("#7fccba", "#479E9B"),
           position = position_dodge(width = 0.2),
           stat = "identity", width = 0.25, linewidth = 1) +
  geom_jitter(data = fig1c_jitter, aes(x = sample_source, y = mehg, color = sample_source),
              size = 3, height = 0.02, width = 0.1, alpha = 0.8) +
  scale_color_manual(values = c("#479E9B", "#7fccba")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) + 
  geom_errorbar(data = fig1c_bar, aes(x = sample_source, 
                                      ymin = mean_mehg - sd_mehg, ymax = mean_mehg + sd_mehg),
                width = 0.1, color = c("#7fccba","#479E9B"), linewidth = 0.25,
                position = position_dodge(width = 0.65)) +
  theme(legend.position = "none") +
  labs(x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_blank())

fig1c  

## fig1d========================================================================
fig1d_data <- read.csv("fig1d_wild_farmed_carnivore.csv", header = TRUE)
fig1d_data$Sample_source <- factor(fig1d_data$Sample_source, levels = c("Wild", "Farmed"))

fig1d <- ggplot(fig1d_data, aes(x = Sample_source, y = log_mean_mehg,
                                fill = Sample_source)) +
  geom_boxplot(color = 'black', position = position_dodge(width = 0.2), 
               outliers = FALSE,
               width = 0.25, linewidth = 0.5) +
  stat_summary(geom = "point", fun = 'mean', shape = 23, 
               size = 3, fill = "#e76a42") +
  scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 2)) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("#7fccba", "#479E9B")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 14, color = "black"),
        panel.grid  = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

fig1d

n_wild_carni <- length(fig1d_data$Sample_source[fig1d_data$Sample_source == "Wild"])
n_farmed_carni <- length(fig1d_data$Sample_source[fig1d_data$Sample_source == "Farmed"])

## fig1e========================================================================
fig1e_data <- read.csv("fig1e_wild_farmed_omnivore.csv", header = TRUE)
fig1e_data$Sample_source <- factor(fig1e_data$Sample_source, levels = c("Wild", "Farmed"))

fig1e <- ggplot(fig1e_data, aes(x = Sample_source, y = log_mean_mehg,
                                fill = Sample_source)) +
  geom_boxplot(color = 'black', position = position_dodge(width = 0.2),
               outliers = FALSE,
               width = 0.25, linewidth = 0.5) +
  stat_summary(geom = "point", fun = 'mean', shape = 23, 
               size = 3, fill = "#e76a42") +
  scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 2)) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("#7fccba", "#479E9B")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

fig1e

n_wild_omni <- length(fig1e_data$Sample_source[fig1e_data$Sample_source == "Wild"])
n_farmed_omni <- length(fig1e_data$Sample_source[fig1e_data$Sample_source == "Farmed"])

## fig1f========================================================================
fig1f_data <- read.csv("Fig1f_wild_farmed_herbivore.csv", header = TRUE)
fig1f_data$Sample_source <- factor(fig1f_data$Sample_source, levels = c("Wild", "Farmed"))

fig1f <- ggplot(fig1f_data, aes(x = Sample_source, y = log_mean_mehg,
                                fill = Sample_source)) +
  geom_boxplot( color = 'black', position = position_dodge(width = 0.2), 
                outliers = FALSE,
                width = 0.25, linewidth = 0.5) +
  stat_summary(geom = "point", fun = 'mean', shape = 23, 
               size = 3, fill = "#e76a42") +
  scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 2)) +
  labs(x ="", y = "") +
  scale_fill_manual(values = c("#7fccba", "#479E9B")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

fig1f

n_wild_herbi <- length(fig1f_data$Sample_source[fig1f_data$Sample_source == "Wild"])
n_farmed_herbi <- length(fig1f_data$Sample_source[fig1f_data$Sample_source == "Farmed"])

## fig1g========================================================================
fig1g_data <- read.csv('fig1g_freshwater_seawater_carnivore.csv', header = TRUE)

fig1g_data$log_mean_mehg <- log10(fig1g_data$MeHg_mean_ppb)

fig1g_data <- fig1g_data[, c(2,8)]

fig1g <- ggplot(fig1g_data, aes(x = Seawater_freshwater, y = log_mean_mehg, 
                                fill = Seawater_freshwater)) +
  geom_boxplot(color = 'black', position = position_dodge(width = 0.2),
               outliers = FALSE,
               width = 0.25, linewidth = 0.5) +
  stat_summary(geom = "point", fun = 'mean', shape = 23, 
               size = 3, fill = "#e76a42") +
  labs(x = "", y = "Log10-scaled MeHg content") +
  scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 2)) +
  scale_fill_manual(values = c("#7fccba", "#479E9B")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 14, color = "black"),
        panel.grid  = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

fig1g

n_fresh_carni <- length(fig1g_data$Seawater_freshwater[fig1g_data$Seawater_freshwater == "Freshwater"])
n_seawater_carni <- length(fig1g_data$Seawater_freshwater[fig1g_data$Seawater_freshwater == "Seawater"])

## fig1h========================================================================
fig1h_data <- read.csv('fig1h_freshwater_seawater_omnivore.csv', header = TRUE)

fig1h_data$log_mean_mehg <- log10(fig1h_data$MeHg_mean_ppb)

fig1h_data <- fig1h_data[, c(2,8)]

fig1h <- ggplot(fig1h_data, aes(x = Seawater_freshwater, y = log_mean_mehg, 
                                fill = Seawater_freshwater)) +
  geom_boxplot(color = 'black', position = position_dodge(width = 0.2), 
               outliers = FALSE,
               width = 0.25, linewidth = 0.5) +
  stat_summary(geom = "point", fun = 'mean', shape = 23, 
               size = 3, fill = "#e76a42") +
  labs(x = "", y = "") +
  scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 2)) +
  scale_fill_manual(values = c("#7fccba", "#479E9B")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

fig1h

n_fresh_omni <- length(fig1h_data$Seawater_freshwater[fig1h_data$Seawater_freshwater == "Freshwater"])
n_seawater_omni <- length(fig1h_data$Seawater_freshwater[fig1h_data$Seawater_freshwater == "Seawater"])

## fig1i========================================================================
fig1i_data <- read.csv('fig1i_freshwater_seawater_herbivore.csv', header = TRUE)

fig1i_data$log_mean_mehg <- log10(fig1i_data$MeHg_mean_ppb)

fig1i_data <- fig1i_data[, c(2,8)]

fig1i <- ggplot(fig1i_data, aes(x = Seawater_freshwater, y = log_mean_mehg, 
                                fill = Seawater_freshwater)) +
  geom_boxplot(color = 'black', position = position_dodge(width = 0.2), 
               outliers = FALSE,
               width = 0.25, linewidth = 0.5) +
  stat_summary(geom = "point", fun = 'mean', shape = 23, 
               size = 3, fill = "#e76a42") +
  labs(x = "", y = "") +
  scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 2)) +
  scale_fill_manual(values = c("#7fccba", "#479E9B")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

fig1i

n_fresh_herbi <- length(fig1i_data$Seawater_freshwater[fig1i_data$Seawater_freshwater == "Freshwater"])
n_seawater_herbi <- length(fig1i_data$Seawater_freshwater[fig1i_data$Seawater_freshwater == "Seawater"])


# Statistical significance======================================================
dunn_test_1a <- dunnTest(mehg ~ sample_source, data = fig1a_jitter, 
                         method = "bonferroni")
dunn_test_1a

dunn_test_1b <- dunnTest(mehg ~ sample_source, data = fig1b_jitter, 
                         method = "bonferroni")
dunn_test_1b

dunn_test_1c <- dunnTest(mehg ~ sample_source, data = fig1c_jitter, 
                         method = "bonferroni")
dunn_test_1c

dunn_test_1d <- dunnTest(log_mean_mehg ~ Sample_source, data = fig1d_data, 
                         method = "bonferroni")
dunn_test_1d

dunn_test_1e <- dunnTest(log_mean_mehg ~ Sample_source, data = fig1e_data, 
                         method = "bonferroni")
dunn_test_1e

dunn_test_1f <- dunnTest(log_mean_mehg ~ Sample_source, data = fig1f_data, 
                         method = "bonferroni")
dunn_test_1f

dunn_test_1g <- dunnTest(log_mean_mehg ~ Seawater_freshwater, data = fig1g_data, 
                         method = "bonferroni")
dunn_test_1g

dunn_test_1h <- dunnTest(log_mean_mehg ~ Seawater_freshwater, data = fig1h_data, 
                         method = "bonferroni")
dunn_test_1h

dunn_test_1i <- dunnTest(log_mean_mehg ~ Seawater_freshwater, data = fig1i_data, 
                         method = "bonferroni")
dunn_test_1i

# Merge plots===================================================================
fig1a + fig1b + fig1c + fig1d + fig1e + fig1f + fig1g + fig1h + fig1i +
  plot_layout(design = "
              AABBCC
              DDEEFF
              GGHHII")

# ggsave("Fig1_nine_plots0621.pdf", plot = last_plot(), width = 9, height = 9, units = "in")
# 
# save.image("Fig1a_i_0621.RData")


