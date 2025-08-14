# Load packages 
if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,       # Used for basic data handling and visualization.,
  gridExtra        # Arrange multiple ggplot2 plots.
)

# Create directories for sub-folders
tempfolder <- "./data/temp"
figfolder <- "./results/output_figures"
manuscript_images <- "./docs/manuscript/images"

dir.create(tempfolder, showWarnings = FALSE)
dir.create(figfolder, showWarnings = FALSE)
dir.create(manuscript_images, showWarnings = FALSE)

#### Exposure-response relations ECHA #### 

spline_plot <- readRDS(file.path(tempfolder, "spline_plot.RDS"))

# Figure S2
ggsave(
  spline_plot,
  filename = file.path(manuscript_images, "Figure_S2.png"),
  width = 10, height = 5, dpi = 600, bg = "white"
)

#### PoC plots #### 

spline_PoC <- readRDS(file.path(tempfolder, "spline_PoC.RDS"))
mixed_PoC_plot <- readRDS(file.path(tempfolder, "plot_PoC_cum0_mixed.RDS"))

# Figure 2 
ggsave(
  plot = arrangeGrob(spline_PoC, mixed_PoC_plot, nrow = 2),
  filename = file.path(manuscript_images, "Figure_2.png"),
  width = 8, height = 10, dpi = 600, bg = "white"
)

#### Relative Risk plots #### 

# Read the saved data
df_grid_mixed <- read.table(file.path(tempfolder, "grid_mixed.txt"), sep = "\t", header = TRUE)
df_grid_spline <- read.table(file.path(tempfolder, "spline_grid.txt"), sep = "\t", header = TRUE)
df_asbestos_SYNERGY <- read.table(file.path(tempfolder, "asbestos_SYNERGY.txt"), sep = "\t", header = TRUE)
df_asbestos_ECHA <- read.table(file.path(tempfolder, "data_model_4.txt"), sep = "\t", header = TRUE)

# Prepare data for combined plot and rugs
# Add study labels and select exposure and RR-related columns
spline_rr_data <- df_grid_spline %>%
  select(asbestos_cum0, RR, RR_UB = RR_PI_upper) %>%  # Adjust column names as needed
  mutate(Study = "ECHA")

mixed_rr_data <- df_grid_mixed %>%
  select(asbestos_cum0, RR, RR_UB = RR_PI_upper) %>%
  mutate(Study = "SYNERGY")

df_asbestos_SYNERGY <- df_asbestos_SYNERGY %>%
  mutate(Study = "SYNERGY")

df_asbestos_ECHA <- df_asbestos_ECHA %>% 
  select(exposure) %>% 
  mutate(Study = "ECHA")

# Combine the RR datasets
combined_rr_data <- bind_rows(spline_rr_data, mixed_rr_data)

# Create the combined RR plot
combined_RR_plot <- ggplot(combined_rr_data, aes(x = asbestos_cum0)) +
  # Reference line at RR = 2
  geom_hline(yintercept = 2, linetype = "dotted", color = "red", linewidth = 0.5) +
  # Upper 95% prediction interval for each study
  geom_ribbon(aes(ymin = RR, ymax = RR_UB, fill = Study), alpha = 0.3) +
  # Main RR lines for each study
  geom_line(aes(y = RR, color = Study), linewidth = 0.8) +
  ylim(0.8, 6) +
  scale_x_continuous(breaks = seq(0, 70, by = 5), limits = c(0, 65)) +
  # Add rug with actual observed asbestos values in SYNERGY
  geom_rug(data = df_asbestos_SYNERGY, aes(x = asbestos_cum0, color = Study), 
           alpha = 0.6, sides = "t", length = unit(0.02, "npc"), 
           show.legend = TRUE) +
  # Add rug with grouped exposure estimates reported in ECHA
  geom_rug(data = df_asbestos_ECHA, aes(x = exposure, color = Study), 
           alpha = 0.6, sides = "b", length = unit(0.02, "npc"), 
           show.legend = TRUE) +
  labs(
    x = "Asbestos fibre-years (ff/ml-years)",
    y = "Relative Risk (RR)",
    color = "Study",
    fill = "Study"
  ) +
  # Set colors for the two studies
  scale_color_manual(values = c("ECHA" = "seagreen", "SYNERGY" = "midnightblue")) +
  scale_fill_manual(values = c("ECHA" = "seagreen2", "SYNERGY" = "lightblue")) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    legend.title = element_text(face = "bold")
  ) + 
  coord_cartesian(expand = FALSE)

# Figure 1 
ggsave(
  plot = combined_RR_plot,
  filename = file.path(manuscript_images, "Figure_1.png"),
  width = 8, height = 5, dpi = 600, bg = "white"
)

#### Sensitivity Analysis RR plot #### 
df_grid_sensitivity <- read.table(file.path(tempfolder, "sensitivity_grid.txt"), sep = "\t", header = TRUE)

# Prepare data for stratified plots
# Filter for population controls (Panel A)
population_rr_data <- df_grid_sensitivity %>%
  filter(source_controls == "P") %>%
  mutate(Control_type = "Population controls")

# Filter for hospital controls (Panel B)  
hospital_rr_data <- df_grid_sensitivity %>%
  filter(source_controls == "H") %>%
  mutate(Control_type = "Hospital controls")

# Prepare rug data (filter SYNERGY data by control type)
df_asbestos_SYNERGY_P <- df_asbestos_SYNERGY %>%
  filter(source_controls == "P") %>%
  mutate(Control_type = "Population controls")

df_asbestos_SYNERGY_H <- df_asbestos_SYNERGY %>%
  filter(source_controls == "H") %>%
  mutate(Control_type = "Hospital controls")

# Panel A: Population controls
population_RR_plot <- ggplot(population_rr_data, aes(x = asbestos_cum0)) +
  # Reference line at RR = 2
  geom_hline(yintercept = 2, linetype = "dotted", color = "red", linewidth = 0.5) +
  
  # Upper 95% prediction interval - map to aesthetic for legend
  geom_ribbon(aes(ymin = RR_LB, ymax = RR_UB, fill = "95% PI"), alpha = 0.3) +
  
  # Main RR line - map to aesthetic for legend
  geom_line(aes(y = RR, color = "RR"), linewidth = 0.8) +
  
  ylim(0.8, 6) +
  scale_x_continuous(breaks = seq(0, 70, by = 5), limits = c(0, 65)) +
  
  # Add rug with actual observed asbestos values for population controls
  geom_rug(data = df_asbestos_SYNERGY_P, aes(x = asbestos_cum0), 
           color = "slateblue4", alpha = 0.6, sides = "b", length = unit(0.02, "npc")) +
  
  labs(
    title = "Lung cancer risk increase in studies with population controls in SYNERGY",
    x = "Asbestos fibre-years (ff/ml-years)",
    y = "Relative Risk (RR)",
    tag = "A",
    color = NULL,  # Remove legend title
    fill = NULL    # Remove legend title
  ) +
  
  # Manually set colors
  scale_color_manual(values = c("RR" = "slateblue4")) +
  scale_fill_manual(values = c("95% PI" = "slateblue2")) +
  
  # Guide order (RR estimate first, then 95% PI)
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  ) +
  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    plot.tag = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) + 
  coord_cartesian(expand = FALSE)

# Panel B: Hospital controls
hospital_RR_plot <- ggplot(hospital_rr_data, aes(x = asbestos_cum0)) +
  # Reference line at RR = 2
  geom_hline(yintercept = 2, linetype = "dotted", color = "red", linewidth = 0.5) +
  
  # Upper 95% prediction interval - map to aesthetic for legend
  geom_ribbon(aes(ymin = RR_LB, ymax = RR_UB, fill = "95% PI"), alpha = 0.3) +
  
  # Main RR line - map to aesthetic for legend
  geom_line(aes(y = RR, color = "RR"), linewidth = 0.8) +
  
  ylim(0.8, 6) +
  scale_x_continuous(breaks = seq(0, 70, by = 5), limits = c(0, 65)) +
  
  # Add rug with actual observed asbestos values for hospital controls
  geom_rug(data = df_asbestos_SYNERGY_H, aes(x = asbestos_cum0), 
           color = "firebrick", alpha = 0.6, sides = "b", length = unit(0.02, "npc")) +
  
  labs(
    title = "Lung cancer risk increase in studies with hospital controls in SYNERGY",
    x = "Asbestos fibre-years (ff/ml-years)",
    y = "Relative Risk (RR)",
    tag = "B",
    color = NULL,  # Remove legend title
    fill = NULL    # Remove legend title
  ) +
  
  # Manually set colors
  scale_color_manual(values = c("RR" = "firebrick")) +
  scale_fill_manual(values = c("95% PI" = "lightcoral")) +
  
  # Guide order (RR estimate first, then 95% PI)
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  ) +
  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    plot.tag = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) + 
  coord_cartesian(expand = FALSE)

# Combine both panels
sensitivity_combined <- arrangeGrob(population_RR_plot, hospital_RR_plot, nrow = 2)

# Figure S3
ggsave(
  plot = sensitivity_combined,
  filename = file.path(manuscript_images, "Figure_S3.png"),
  width = 8, height = 10, dpi = 600, bg = "white"
)

#### Sensitivity Analysis PoC plot #### 

# Panel A: Population controls PoC
population_PoC_plot <- ggplot(population_rr_data, aes(x = asbestos_cum0)) +
  # Reference line at PoC = 0.5
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "royalblue2", linewidth = 0.5) +
  
  # Upper 95% prediction interval - map to aesthetic for legend
  geom_ribbon(aes(ymin = PoC, ymax = PoC_UB, fill = "Upper PI"), alpha = 0.3) +
  
  # Main PoC line - map to aesthetic for legend
  geom_line(aes(y = PoC, color = "PoC"), linewidth = 0.8) +
  
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 70, by = 5), limits = c(0, 65)) +
  
  # Add rug with actual observed asbestos values for lung cancer cases
  geom_rug(
    data = df_asbestos_SYNERGY_P %>% filter(status == "1"),
    aes(x = asbestos_cum0), color = "slateblue4", alpha = 0.6, 
    sides = "b", length = unit(0.02, "npc")
    ) +
  
  labs(
    title = "PoC of lung cancer in studies with population controls in SYNERGY",
    x = "Asbestos fibre-years (ff/ml-years)",
    y = "Probability of Causation (PoC)",
    tag = "A",
    color = NULL,  # Remove legend title
    fill = NULL    # Remove legend title
  ) +
  
  # Manually set colors
  scale_color_manual(values = c("PoC" = "slateblue4")) +
  scale_fill_manual(values = c("Upper PI" = "slateblue2")) +
  
  # Guide order (PoC estimate first, then 95% PI)
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
    panel.grid.major.y = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    plot.tag = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) + 
  coord_cartesian(expand = FALSE)

# Panel B: Hospital controls PoC
hospital_PoC_plot <- ggplot(hospital_rr_data, aes(x = asbestos_cum0)) +
  # Reference line at PoC = 0.5
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "royalblue2", linewidth = 0.5) +
  
  # Upper 95% prediction interval - map to aesthetic for legend
  geom_ribbon(aes(ymin = PoC, ymax = PoC_UB, fill = "Upper PI"), alpha = 0.3) +
  
  # Main PoC line - map to aesthetic for legend
  geom_line(aes(y = PoC, color = "PoC"), linewidth = 0.8) +
  
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 70, by = 5), limits = c(0, 65)) +
  
  # Add rug with actual observed asbestos values for lung cancer cases
  geom_rug(
    data = df_asbestos_SYNERGY_H %>% filter(status == "1"),
    aes(x = asbestos_cum0), color = "firebrick", alpha = 0.6, 
    sides = "b", length = unit(0.02, "npc")
  ) +
  
  labs(
    title = "PoC of lung cancer in studies with hospital controls in SYNERGY",
    x = "Asbestos fibre-years (ff/ml-years)",
    y = "Probability of Causation (PoC)",
    tag = "B",
    color = NULL,  # Remove legend title
    fill = NULL    # Remove legend title
  ) +
  
  # Manually set colors
  scale_color_manual(values = c("PoC" = "firebrick")) +
  scale_fill_manual(values = c("Upper PI" = "lightcoral")) +
  
  # Guide order (PoC estimate first, then 95% PI)
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
    panel.grid.major.y = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    plot.tag = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) + 
  coord_cartesian(expand = FALSE)

# Combine both PoC panels
sensitivity_PoC_combined <- arrangeGrob(population_PoC_plot, hospital_PoC_plot, nrow = 2)

# Figure S4
ggsave(
  plot = sensitivity_PoC_combined,
  filename = file.path(manuscript_images, "Figure_S4.png"),
  width = 8, height = 10, dpi = 600, bg = "white"
)