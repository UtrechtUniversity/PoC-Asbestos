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
x <- seq(0, 200, 1)
for (mix in 1:4) {
  results <- getPoC(mix = mix, x = x, MOD)
  results$Model <- paste("Model =", mix)
  spline_results <- bind_rows(spline_results, results)
}

# Plot splines, with a shaded are corresponding to exposure values observed in
# SYNERGY study (0 to 65 ff/ml-years) and dashed line in y = 2 (RR = 2)
spline_plot <- ggplot(spline_results, aes(x = Exposure, y = RR)) +
  geom_rect(aes(xmin = 0, xmax = 65, ymin = -Inf, ymax = Inf), 
            fill = "gray95", alpha = 0.5) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_line(linewidth = 0.7) +
  ylim(0.9, 3.5) +
  facet_wrap(~ Model, nrow = 1, ncol = 4) + 
  labs(title = "Exposure-Response Curves (asbestos and lung cancer)",
       x = "Fiber-years (ff/ml-years)",
       y = "Relative Risk (RR)") +
  theme_minimal() + 
  theme(
    panel.spacing = unit(2, "lines"),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) + 
  expand_limits(x = 0, y = 0) + 
  coord_cartesian(expand = FALSE)


# Save plot
ggsave(
  spline_plot,
  filename = file.path(figfolder, "Exposure_Response_Curves.pdf"),
  width = 10, height = 5, dpi = 600, bg = "white"
)

ggsave(
  spline_plot,
  filename = file.path(manuscript_images, "Figure_S1.png"),
  width = 10, height = 5, dpi = 600, bg = "white"
)

#### RR spline for the range of exposure values in SYNERGY ####

# Select subset of variables of interest and apply PoC function: 
df.spline.cum0 <- df %>%
  select(subjctid, status, sex, agegroup, study_name, country, smoking, packyrs,
         time_quit, list_a, ever_asbestos0, asbestos_cum0, asbestos_dur0) %>%
  cbind(
    ., 
    getPoC(x = .$asbestos_cum0, mix = 4, MOD, scale_factor = 1) %>% 
      rename_with(~ paste0(., "_factor1"), -Exposure),
    getPoC(x = .$asbestos_cum0, mix = 4, MOD, scale_factor = 1.5) %>%
      rename_with(~ paste0(., "_factor1.5"), -Exposure),
    getPoC(x = .$asbestos_cum0, mix = 4, MOD, scale_factor = 2) %>%
      rename_with(~ paste0(., "_factor2"), -Exposure)
  )

# Reshape data for RR, PoC, and confidence intervals
df_long <- df.spline.cum0 %>%
  select(asbestos_cum0, starts_with("RR_factor"), starts_with("PoC_factor"), 
         starts_with("p2.5%"), starts_with("p97.5%")) %>%
  pivot_longer(
    cols = -asbestos_cum0,
    names_to = c(".value", "ErrorFactor"),
    names_pattern = "(RR|PoC|p2.5%|p97.5%)_factor(.+)"
  ) %>%
  mutate(
    ErrorFactor = factor(ErrorFactor, 
                         levels = c("1", "1.5", "2"), 
                         labels = c("None", "1.5", "2"))
  )

# Custom colors for error factor
color_values <- c("None" = "black", "1.5" = "paleturquoise3", "2" = "skyblue3")

# Create RR spline plot different error factors
spline_RR <- ggplot(df_long, aes(x = asbestos_cum0, y = RR, color = ErrorFactor)) +
  geom_hline(yintercept = 2, linetype = "dotted", color = "red", linewidth = 0.5) +
  geom_line(linewidth = 0.7) +
  ylim(0.5, 3.5) +
  scale_x_continuous(breaks = seq(0, 70, by = 5)) +
  labs(title = "Exposure-response curves",
       x = "Fiber-years (ff/ml-years)",
       y = "Relative Risk (RR)",
       color = "Error Factor",
       tag = "A") +
  scale_color_manual(values = color_values) +
  theme_minimal() + 
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    plot.tag = element_text(face = "bold")
  ) + 
  expand_limits(x = .5, y = 0) + 
  coord_cartesian(expand = FALSE)

#### PoC and upper bound for the range of exposure values in SYNERGY ####
spline_PoC <- ggplot(df_long, aes(x = asbestos_cum0, color = ErrorFactor)) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "royalblue2", linewidth = 0.5) +
  geom_line(aes(y = PoC, linetype = "PoC"), linewidth = 0.7) +
  geom_line(aes(y = `p97.5%`, linetype = "Upper Bound"), linewidth = 0.7) +
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 70, by = 5)) +
  labs(title = "Spline models with upper uncertainty boundary",
       x = "Fiber-years (ff/ml-years)",
       y = "Probability of Causation (PoC)",
       color = "Error Factor",
       linetype = "Line Type",
       tag = "B") +
  scale_color_manual(values = color_values) +  # Color scale for PoC and upper bound lines
  scale_linetype_manual(values = c("PoC" = "solid", "Upper Bound" = "dashed")) +  # Line types
  theme_minimal() + 
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
  expand_limits(x = 0, y = 0) + 
  coord_cartesian(expand = FALSE)


# Combine spline_RR and spline_PoC plots and save
ggsave(
  filename = file.path(figfolder, "Spline_RR_PoC.pdf"),
  plot = arrangeGrob(spline_RR, spline_PoC, nrow = 2),
  width = 8, height = 10, dpi = 600, bg = "white"
)

ggsave(
  filename = file.path(manuscript_images, "Figure_1.png"),
  plot = arrangeGrob(spline_RR, spline_PoC, nrow = 2),
  width = 8, height = 10, dpi = 600, bg = "white"
)