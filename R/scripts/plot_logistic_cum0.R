#### About this script ####
## Title: Probability of causation of lung cancer due to asbestos: logistic model
##
## Purpose: Plot of the probability of causation (PoC) of lung cancer due to
## asbestos, under three scenarios: 1. no misclassification error assumed,
## 2. misclassification error with a factor of 1.5 and 3. misclassification 
## error with a factor of 2. Calculations are based on SYNERGY data and 
## adjusted for confounding due to age, sex, smoking, and center. 
##
## Note: This script is sourced into the main quarto markdown file (.qmd) 
## after function loading. Refer to README file of repository for instructions 
## on how to use. 

#### Create plot #### 
logistic_PoC_plot <- ggplot(df.cum0, aes(x = asbestos_cum0)) +
  
  # Dashed line for the PoC 0.5 threshold
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "royalblue2", linewidth = 0.5) +
  
  # Main PoC line and ribbon
  geom_line(aes(y = PoC, color = "None"), linewidth = 0.7) +
  geom_ribbon(aes(ymin = PoC, ymax = PoC_UB, fill = "None"), alpha = 0.4) +
  
  # Add curve for scenario 1.5 with UB confidence interval
  geom_line(aes(y = PoC_scenario_b1.5, color = "1.5"), linewidth = 0.5) +
  geom_ribbon(aes(ymin = PoC_scenario_b1.5, ymax = PoC_scenario_UB1.5, fill = "1.5"), alpha = 0.4) +
  
  # Add curve for scenario 2 with UB confidence interval
  geom_line(aes(y = PoC_scenario_b2, color = "2"), linewidth = 0.5) +
  geom_ribbon(aes(ymin = PoC_scenario_b2, ymax = PoC_scenario_UB2, fill = "2"), alpha = 0.4) +
  
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 70, by = 5)) +
  labs(
    title = "Logistic regression model",
    x = "Fiber-years (ff/ml-years)",
    y = "Probability of Causation (PoC)",
    tag = "A",
    color = "Error Factor", 
    fill = "Upper 95%CI" 
  ) +
  scale_color_manual(
    values = c("None" = "black", "1.5" = "paleturquoise3", "2" = "skyblue3"),
    breaks = c("2", "1.5", "None")
  ) +
  scale_fill_manual(
    values = c("None" = "gray", "1.5" = "paleturquoise1", "2" = "skyblue1"),
    breaks = c("2", "1.5", "None") 
  ) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.line = element_line(colour = "black"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    plot.tag = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  ) + 
  expand_limits(x = 0, y = 0) + 
  coord_cartesian(expand = FALSE)

#### Save plot #### 
ggsave(
  logistic_PoC_plot,
  filename = file.path(figfolder, "PoC_cum0_logistic.pdf"),
  width = 6, height = 4, dpi = 600, bg = "white"
)
