library(ggplot2)     # v3.5.0
library(patchwork)   # v1.2.0

setwd('path_to_your_working_directory')
overall_inter <- read.csv("Overall_interaction_stregth.csv", header = TRUE)
two_way_inter <- read.csv("Two_way_interaction_strength.csv", header = TRUE)

overall_inter$Variable <- factor(overall_inter$Variable, 
                                 levels = overall_inter$Variable[order(overall_inter$Interaction)])

two_way_inter$Variable <- factor(two_way_inter$Variable, 
                                 levels = two_way_inter$Variable[order(two_way_inter$Interaction)])

overall_inter_lol <- ggplot(overall_inter, aes(x = Variable, y = Interaction)) +
  geom_segment(aes(x = Variable, xend = Variable, y=0, yend = Interaction), 
               color="black", linewidth = 0.5) +
  geom_point(shape = 21, size = 5, fill = "#7fccba", color = "black", stroke = 0.5) +
  coord_flip() +
  labs(y = "Overall interaction strength", x = NULL) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none") 

overall_inter_lol

two_way_inter_lol <- ggplot(two_way_inter, aes(x = Variable, y = Interaction)) +
  geom_segment(aes(x = Variable, xend = Variable, y=0, yend = Interaction), 
               color="black", linewidth = 0.5) +
  geom_point(shape = 21, size = 5, fill = "#7fccba", color = "black", stroke = 0.5) +
  coord_flip() +
  labs(y = "Two-way interaction strength", x = NULL) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16),
        legend.position = "none") 

two_way_inter_lol

overall_inter_lol + two_way_inter_lol

#ggsave('Feature_interaction_1230.pdf', plot = last_plot(), width = 16, height = 9, unit = 'in')

