#### About this script ####
## Title: Probability of causation of lung cancer due to asbestos: mixed model
##
## Purpose: Plot of the probability of causation (PoC) of lung cancer due to
## asbestos based on SYNERGY data. The model incorporates random effects with 
## a random intercept for each study source (`study_name`) and random slopes 
## for the exposure (`asbestos_cum0`) within each study source, and adjusted
## for confounding due to age, sex, and smoking. 
##
## Note: This script is sourced into the main quarto markdown file (.qmd) 
## after function loading. Refer to README file of repository for instructions 
## on how to use. 

#### Create plot #### 
mixed_PoC_plot <- ggplot(df.mixed.cum0, aes(x = asbestos_cum0)) +
  
  # Dashed line for the PoC 0.5 threshold
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "royalblue2", size = 0.5) +
  
  # Main PoC line and ribbon
  geom_line(aes(y = PoC, color = "PoC"), size = 0.7) +
  geom_ribbon(aes(ymin = PoC, ymax = PoC_UB, fill = "Upper 95%CI"), alpha = 0.4) +
  
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 70, by = 5)) +
  labs(
    title = "Mixed-effects logistic regression model",
    x = "Fiber-years (ff/ml-years)",
    y = "Probability of Causation (PoC)",
    tag = "B",
    color = NULL,  
    fill = NULL  
  ) +
  scale_color_manual(values = "darkblue") +
  scale_fill_manual(values = "lightblue") +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    plot.tag = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  ) + 
  expand_limits(x = 0, y = 0) + 
  coord_cartesian(expand = FALSE)

#### Save plot #### 
ggsave(
  mixed_PoC_plot,
  filename = file.path(figfolder, "PoC_cum0_mixed.pdf"),
  width = 6, height = 4, dpi = 600, bg = "white"
)

#### Combine with logistic regression plot and save ####
ggsave(
  filename = file.path(figfolder, "PoC_cum0_logistic_mixed.pdf"),
  plot = arrangeGrob(logistic_PoC_plot, mixed_PoC_plot, nrow = 2),
  width = 8, height = 10, dpi = 600, bg = "white"
)

ggsave(
  filename = file.path(manuscript_images, "Figure_2.png"),
  plot = arrangeGrob(logistic_PoC_plot, mixed_PoC_plot, nrow = 2),
  width = 8, height = 10, dpi = 600, bg = "white"
)
