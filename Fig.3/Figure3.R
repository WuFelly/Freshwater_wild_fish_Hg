library(ggplot2)    # v 3.5.0
library(patchwork)  # v 1.2.0

setwd('path_to_your_working_directory')

# Bar plot======================================================================
mehg_changes <- read.csv("Bar_plot_data.csv", header = TRUE)

piscivore_changes <- ggplot(mehg_changes, aes(x = Time, y = Piscivore_rate)) +
  geom_bar(fill = '#97bade', stat = "identity", position = "dodge", width = 0.3) +
  theme(axis.title = element_text(color = "black",size = 16),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text.y = element_text(color = "black",size = 14),
        axis.text.x  = element_blank(),
        panel.grid.major  = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(suffix = ""), 
                     limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  labs(x = "", y = "Changes in MeHg content (%)")

omnivore_changes <- ggplot(mehg_changes, aes(x = Time, y = Omnivore_rate)) +
  geom_bar(fill = '#97bade', stat = "identity", position = "dodge", width = 0.3) +
  theme(axis.title.x = element_text(color = "black", size = 16),
        axis.title.y = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),
        #axis.text.x = element_text(size = 14),
        axis.text = element_blank(),
        panel.grid.major  = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(suffix = ""), 
                     limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  labs(x = "", y = "Changes in MeHg content (%)")

piscivore_changes|omnivore_changes

#ggsave("Changes in MeHg content.pdf", plot = last_plot(), width = 7.75, height = 5.83, units = 'in')

# Violin plot===================================================================
violin_piscivore <- read.csv('Violin_plot_piscivore.csv', header = TRUE)

piscivore_violin <- ggplot(data = violin_piscivore, aes(x = scenarios, y = change_rate)) +
  geom_violin(fill = "#e0b387", color = "#e0b387", width = 0.6) +
  geom_boxplot(fill = "white", color = "black", width = 0.1, outlier.shape = NA) +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::percent_format(suffix = ""), 
                     limits = c(-1, 4), breaks = seq(-1, 4, 1)) +
  theme(axis.title.x = element_text(color = "black", size = 16),
        axis.title.y = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major  = element_blank(),
        panel.background = element_blank()) +
  guides(fill = "none")

piscivore_violin

violin_omnivore <- read.csv('Violin_plot_omnivore.csv', header = TRUE)

omnivore_violin <- ggplot(data = violin_omnivore, aes(x = scenarios, y = change_rate)) +
  geom_violin(fill = "#e0b387", color = "#e0b387", width = 0.6) +
  geom_boxplot(fill = "white", color = "black",width = 0.1, outlier.shape = NA) +
  labs(x = "", y = "Changes in MeHg content (%)") +
  theme(axis.title.x = element_text(color = "black", size = 16),
        axis.title.y = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),
        #axis.text.x = element_text(size = 14),
        axis.text = element_blank(),
        panel.grid.major  = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(suffix = ""), 
                     limits = c(-1, 4), breaks = seq(-1, 4, 1)) +
  guides(fill = "none")

omnivore_violin

piscivore_changes|piscivore_violin|omnivore_changes|omnivore_violin  

#ggsave("Changes in MeHg content_bar_violin.pdf", plot = last_plot(), width = 16, height = 9, units = 'in')

