#### About this script ####
## Title: Exposure-response spline plots of cumulative asbestos - lung cancer
##
## Purpose: Exposure-response spline plots of the relationship between
## cumulative asbestos and lung cancer from nonlinear meta-regression models 
## as determined by van der Bij, et al.[@vanderbij2013] The range of values 
## of exposure are from the SYNERGY study. 
##
## Note: This script is sourced into the main quarto markdown file (.qmd) 
## after function loading. Refer to README file of repository for instructions 
## on how to use. 


#### Recreate spline plots from van der Bij, et al. (2013) ####

# Calculate spline data for the 4 models 
spline_results <- data.frame()
x <- seq(0, 200, 0.1)
for (mix in 1:4) {
  results <- getRR(mix = mix, x = x, MOD)
  results$Model <- paste("Model =", mix)
  spline_results <- bind_rows(spline_results, results)
}

# Plot splines, with a shaded are corresponding to exposure values observed in
# SYNERGY study (0 to 65 ff/ml-years) and dashed line in y = 2 (RR = 2)
spline_plot <- ggplot(spline_results, aes(x = Exposure)) +
  annotate("rect", xmin = 0, xmax = 65, ymin = -Inf, ymax = Inf,
           fill = "gray95", alpha = 0.5) +
  geom_hline(aes(yintercept = 2, linetype = "RR = 2"), color = "red", linewidth = 0.5) +
  geom_line(aes(y = RR, linetype = "RR"), linewidth = 0.7) +
  geom_line(aes(y = RR_PI_lower, linetype = "95% PI"), color ="grey20", linewidth = 0.5) +
  geom_line(aes(y = RR_PI_upper, linetype = "95% PI"), color ="grey20", linewidth = 0.5) +
  geom_rug(data = datasets[[4]], aes(x = exposure), color = "grey25",
           alpha = 0.6, sides = "b", length = unit(0.02, "npc"), 
           show.legend = TRUE) +
  xlim(NA, 200) +
  scale_linetype_manual(
    name = "Line",
    values = c("RR" = "solid", "95% PI" = "dashed", "RR = 2" = "dotted"),
    breaks = c("RR", "95% PI", "RR = 2")
  ) +
  facet_wrap(~ Model, nrow = 1, ncol = 4) + 
  labs(title = "Exposure-Response relations of lung cancer due to asbestos in the ECHA study",
       x = "Asbestos fibre-years (ff/ml-years)",
       y = "Relative Risk (RR)") +
  theme_minimal() +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  expand_limits(x = 0, y = 0) +
  coord_cartesian(expand = FALSE)


#### RR spline for the range of exposure values in SYNERGY ####

# Select subset of variables of interest and apply PoC function: 
expo_grid <- seq(0, 100, 0.01)

# Apply PoC function to the exposure grid 
df_grid <- data.frame(asbestos_cum0 = expo_grid) %>%
  cbind(
    ., 
    getPoC(x = expo_grid, mix = 4, MOD, scale_factor = 1) %>% 
      select(-Exposure)
  )

# Create RR spline plot
spline_RR <- ggplot(df_grid, aes(x = asbestos_cum0)) +
  geom_hline(yintercept = 2, linetype = "dotted", color = "red", linewidth = 0.5) +
  # Upper 95% prediction interval
  geom_ribbon(aes(ymin = RR, ymax = RR_PI_upper, fill = "Upper PI"), alpha = 0.4) +
  # Main RR line
  geom_line(aes(y = RR, color = "RR"), linewidth = 0.7) +
  ylim(1, 6) +
  scale_x_continuous(breaks = seq(0, 70, by = 5), limits = c(0, 65)) +
  labs(
    title = "Lung cancer risk increase due to asbestos in the ECHA study",
    x = "Asbestos fibre-years (ff/ml-years)",
    y = "Relative Risk (RR)",
    color = NULL,
    fill = NULL,
    tag = "A"
  ) +
  scale_color_manual(values = "seagreen") +
  scale_fill_manual(values = "seagreen3") +
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

#### PoC and upper bound for the range of exposure values in SYNERGY ####
spline_PoC <- ggplot(df_grid, aes(x = asbestos_cum0)) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "royalblue2", linewidth = 0.5) +
  geom_line(aes(y = PoC, color = "PoC"), linewidth = 0.7) +
  geom_ribbon(aes(ymin = PoC, ymax = PoC_PI_upper, fill = "Upper PI"), alpha = 0.4) +
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 70, by = 5), limits = c(0, 65)) +
  labs(title = "PoC of lung cancer due to asbestos in the ECHA study",
       x = "Asbestos fibre-years (ff/ml-years)",
       y = "Probability of Causation (PoC)",
       color = NULL,
       fill = NULL,
       tag = "A") +
  theme_minimal() + 
  scale_color_manual(values = "seagreen") +
  scale_fill_manual(values = "seagreen3") +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.line = element_line(colour = "black"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    plot.tag = element_text(face = "bold")
  ) + 
  coord_cartesian(expand = FALSE)


#### Save plots #### 
saveRDS(spline_plot, file.path(tempfolder, "spline_plot.RDS"))
saveRDS(spline_RR, file.path(tempfolder, "spline_RR.RDS"))
saveRDS(spline_PoC, file.path(tempfolder, "spline_PoC.RDS"))

ggsave(
  spline_plot,
  filename = file.path(figfolder, "Exposure_Response_Curves.pdf"),
  width = 10, height = 5, dpi = 600, bg = "white"
)

ggsave(
  spline_RR,
  filename = file.path(figfolder, "Spline_RR.pdf"),
  width = 6, height = 4, dpi = 600, bg = "white"
)

ggsave(
  spline_PoC,
  filename = file.path(figfolder, "Spline_PoC.pdf"),
  width = 6, height = 4, dpi = 600, bg = "white"
)

#### Derivatives #### 

# Function to calculate numerical derivatives from grid data
calculate_spline_slopes <- function(df_grid) {
  
  slopes <- df_grid %>%
    arrange(asbestos_cum0) %>%
    mutate(
      # Calculate numerical derivative for PoC (point estimate)
      PoC_slope = (lead(PoC) - lag(PoC)) / (lead(asbestos_cum0) - lag(asbestos_cum0)),
      
      # Calculate derivative for  PI
      PoC_upper_slope = (lead(PoC_PI_upper) - lag(PoC_PI_upper)) / (lead(asbestos_cum0) - lag(asbestos_cum0)),
      PoC_lower_slope = (lead(PoC_PI_lower) - lag(PoC_PI_lower)) / (lead(asbestos_cum0) - lag(asbestos_cum0)),
      
      # Convert to "risk increase per fibre-year" (similar to linear model units)
      # This approximates d(PoC)/d(exposure) 
      Risk_per_ffyr_instantaneous = PoC_slope * 100,  # Convert to percentage
      Risk_per_ffyr_upper_instantaneous = PoC_upper_slope * 100,
      Risk_per_ffyr_lower_instantaneous = PoC_lower_slope * 100
    ) 
  
  return(slopes)
}

# Calculate slopes for later use 
df_grid <- calculate_spline_slopes(df_grid)

# Save data 
df_grid %>% 
  write.table(., paste0(tempfolder, "/spline_grid.txt"), sep = "\t")
