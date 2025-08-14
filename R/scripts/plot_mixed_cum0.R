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

#### Create RR plot #### 

# Create a grid of exposure values for smooth plotting
expo_grid <- seq(0, 65, 0.1)  # Match your x-axis limits

# Calculate RR estimates across the exposure grid 
df_grid_mixed <- data.frame(
  asbestos_cum0 = expo_grid,
  # Main RR estimate
  RR = exp(b * expo_grid),
  # Upper prediction interval
  RR_PI_upper = exp(UB * expo_grid),
  # Lower prediction interval
  RR_PI_lower = exp(LB * expo_grid)
) %>% 
  # Upper limit for plotting.
  mutate(RR_PI_upper = case_when(RR_PI_upper >= 6 ~ 6, T ~ RR_PI_upper)) %>% 
  # Add PoC calculations
  mutate(
    PoC = PoCfun(b * asbestos_cum0),
    PoC_UB = PoCfun(UB * asbestos_cum0),
    PoC_LB = PoCfun(LB * asbestos_cum0)
  )

# Create the RR plot
mixed_RR_plot <- ggplot(df_grid_mixed, aes(x = asbestos_cum0)) +
  # Reference line at RR = 2
  geom_hline(yintercept = 2, linetype = "dotted", color = "red", linewidth = 0.5) +
  # 95% Prediction interval
  geom_ribbon(aes(ymin = RR, ymax = RR_PI_upper, fill = "Upper PI"), alpha = 0.4) +
  # Main RR line
  geom_line(aes(y = RR, color = "RR"), linewidth = 0.7) +
  ylim(1, 6) +
  scale_x_continuous(breaks = seq(0, 70, by = 5), limits = c(0, 65)) +
  labs(
    title = "Lung cancer risk increase due to asbestos in the SYNERGY study",
    x = "Asbestos fibre-years (ff/ml-years)",
    y = "Relative Risk (RR)",
    color = NULL,
    fill = NULL,
    tag = "B"
  ) +
  scale_color_manual(values = "midnightblue") +
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
    axis.line = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    plot.tag = element_text(face = "bold")
  ) + 
  coord_cartesian(expand = FALSE)


#### Create PoC plot #### 

mixed_PoC_plot <- ggplot(df_grid_mixed, aes(x = asbestos_cum0)) +
  # Dashed line for the PoC 0.5 threshold
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "royalblue2", linewidth = 0.5) +
  # Main PoC line
  geom_line(aes(y = PoC, color = "PoC"), linewidth = 0.7) +
  # Upper 95% prediction interval
  geom_ribbon(aes(ymin = PoC, ymax = PoC_UB, fill = "Upper PI"), alpha = 0.4) +
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 70, by = 5), limits = c(0, 65)) +
  labs(
    title = "PoC of lung cancer due to asbestos in the SYNERGY study",
    x = "Asbestos fibre-years (ff/ml-years)",
    y = "Probability of Causation (PoC)",
    tag = "B",
    color = NULL,  
    fill = NULL  
  ) +
  scale_color_manual(values = "midnightblue") +
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
    legend.title = element_text(face = "bold", size = 9)
  ) + 
  coord_cartesian(expand = FALSE)



#### Save plots #### 
saveRDS(mixed_RR_plot, file.path(tempfolder, "plot_RR_cum0_mixed.RDS"))
saveRDS(mixed_PoC_plot, file.path(tempfolder, "plot_PoC_cum0_mixed.RDS"))

ggsave(
  mixed_RR_plot,
  filename = file.path(figfolder, "RR_cum0_mixed.pdf"),
  width = 6, height = 4, dpi = 600, bg = "white"
)

ggsave(
  mixed_PoC_plot,
  filename = file.path(figfolder, "PoC_cum0_mixed.pdf"),
  width = 6, height = 4, dpi = 600, bg = "white"
)

# Save data 
df_grid_mixed %>% 
  write.table(., paste0(tempfolder, "/grid_mixed.txt"), sep = "\t")

# Save asbestos values in SYNERGY for plotting later on
df %>% select(asbestos_cum0, source_controls, status) %>% 
  write.table(., paste0(tempfolder, "/asbestos_SYNERGY.txt"), sep = "\t")