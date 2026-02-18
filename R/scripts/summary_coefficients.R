#### Table 4 #### 

# Load the saved data tables 
beta_coefficients_main_one_stage <- read.table(paste0(tabfolder, "/PoC_one-stage-main_count_plus50.txt"), sep = "\t", header = TRUE, stringsAsFactors = FALSE)
beta_coefficients_stratified_one_stage <- read.table(paste0(tabfolder, "/PoC_one-stage-stratified_count_plus50.txt"), sep = "\t", header = TRUE, stringsAsFactors = FALSE)
beta_coefficients_spline <- read.table(paste0(tabfolder, "/PoC_spline_model_cum0_count_plus50.txt"), sep = "\t", header = TRUE, stringsAsFactors = FALSE)
beta_coefficients_main_two_stage <- read.table(paste0(tabfolder, "/PoC_two_stage_main_count_plus50.txt"), sep = "\t", header = TRUE, stringsAsFactors = FALSE)
beta_coefficients_stratified_two_stage <- read.table(paste0(tabfolder, "/PoC_two_stage_stratified_count_plus50.txt"), sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# Combine all tables with analysis grouping
beta_coefficients_combined <- bind_rows(
  beta_coefficients_spline %>% mutate(Analysis = "Meta-regression of published occupational studies including SYNERGY", .before = 1),
  beta_coefficients_main_one_stage %>% mutate(Analysis = "Meta-analysis of SYNERGY studies only", .before = 1),
  beta_coefficients_stratified_one_stage %>% mutate(Analysis = "Sensitivity analysis of SYNERGY studies, stratified by source of controls", .before = 1)
) %>%
  mutate(
    Risk_per_ffyr_PI = ifelse(
      str_detect(Risk_per_ffyr_PI, "–"),
      Risk_per_ffyr_PI,  # Keep "–" unchanged
      {
        # Extract numbers, replace negatives with 0, due to the assumed model monotonicity for PoC
        lower <- as.numeric(str_extract(Risk_per_ffyr_PI, "^[^%]+"))
        upper <- as.numeric(str_extract(Risk_per_ffyr_PI, "(?<=; )[^%]+"))
        paste0("[",round(pmax(0, lower), 1), "%, ", round(pmax(0, upper), 1), "%]")
      }
    )
  )

# Create comprehensive combined table
beta_coefficients_combined_gt <- beta_coefficients_combined %>% 
  gt(groupname_col = "Analysis") %>%
  cols_label(
    Model = "Model",
    Risk_per_ffyr_est = "Estimate",
    Risk_per_ffyr_PI = "95% Prediction Interval", 
    Min_exp_50pct_PoC = "Point Estimate",
    Min_exp_conservative = "Presumably Plausible",
    Point_per_10k = "Point Estimate", 
    Presumably_plausible_per_10k = "Presumably Plausible"
  ) %>%
  tab_spanner(
    label = "Risk Increase per Fibre-year",
    columns = c(Risk_per_ffyr_est, Risk_per_ffyr_PI)
  ) %>%
  tab_spanner(
    label = "Min Exposure for 50% PoC (fibre-years)",
    columns = c(Min_exp_50pct_PoC, Min_exp_conservative)
  ) %>%
  tab_spanner(
    label = "Cases per 10,000 above 50% PoC in SYNERGY",
    columns = c(Point_per_10k, Presumably_plausible_per_10k)
  ) %>%
  cols_align(align = "center") %>%
  tab_footnote(
    footnote = "Probability of Causation (PoC) of 50% corresponds to a doubling in lung cancer risk (i.e., RR = 2) for the point estimate and the upper 95% prediction interval for the presumably plausible threshold.",
    locations = cells_column_spanners(spanners = "Min Exposure for 50% PoC (fibre-years)")
  ) %>%
  tab_footnote(
    footnote = "Negative values truncated to 0 due to monotonicity assumption. For spline model: instantaneous slope at 1.5 fibre-years (median exposure in cases).",
    locations = cells_column_spanners(spanners = "Risk Increase per Fibre-year")
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>% 
  opt_footnote_marks(marks = "letters")


gtsave(beta_coefficients_combined_gt, file = file.path(manuscript_tables, "Table4.html"))


#### Table S3 #### 

# Main meta-analysis section (overall results)
main_two_stage <- beta_coefficients_main_two_stage %>% 
  mutate(
    Analysis = "Main meta-analysis",
    Method = case_when(
      Method == "Univariate" ~ "Two-stage univariate",
      Method == "Bivariate" ~ "Two-stage bivariate",
      TRUE ~ Method
    ),
    .before = 1
  ) %>%
  select(-Model)  

main_one_stage <- beta_coefficients_main_one_stage %>% 
  mutate(
    Analysis = "Main meta-analysis",
    Method = "One-stage",
    .before = 1
  ) %>%
  select(-Model)  

# Population-based control studies section
population_two_stage <- beta_coefficients_stratified_two_stage %>% 
  filter(Model == "Population controls") %>%
  mutate(
    Analysis = "Population-based control studies",
    Method = case_when(
      Method == "Univariate" ~ "Two-stage univariate",
      Method == "Bivariate" ~ "Two-stage bivariate",
      TRUE ~ Method
    ),
    .before = 1
  ) %>%
  select(-Model)

population_one_stage <- beta_coefficients_stratified_one_stage %>% 
  filter(Model == "Population controls") %>%
  mutate(
    Analysis = "Population-based control studies", 
    Method = "One-stage",
    .before = 1
  ) %>%
  select(-Model)

# Hospital-based control studies section  
hospital_two_stage <- beta_coefficients_stratified_two_stage %>% 
  filter(Model == "Hospital controls") %>%
  mutate(
    Analysis = "Hospital-based control studies",
    Method = case_when(
      Method == "Univariate" ~ "Two-stage univariate",
      Method == "Bivariate" ~ "Two-stage bivariate",
      TRUE ~ Method
    ),
    .before = 1
  ) %>%
  select(-Model)

hospital_one_stage <- beta_coefficients_stratified_one_stage %>% 
  filter(Model == "Hospital controls") %>%
  mutate(
    Analysis = "Hospital-based control studies", 
    Method = "One-stage",
    .before = 1
  ) %>%
  select(-Model)

# Combine all tables in the specified order
beta_coefficients_S4_combined <- bind_rows(
  # Main meta-analysis
  main_two_stage,
  main_one_stage,
  
  # Population-based control studies  
  population_two_stage,
  population_one_stage,
  
  # Hospital-based control studies
  hospital_two_stage,
  hospital_one_stage
) %>%
  mutate(
    Risk_per_ffyr_PI = ifelse(
      str_detect(Risk_per_ffyr_PI, "–"),
      Risk_per_ffyr_PI,  # Keep "–" unchanged
      {
        # Extract numbers, replace negatives with 0, due to the assumed model monotonicity for PoC
        lower <- as.numeric(str_extract(Risk_per_ffyr_PI, "^[^%]+"))
        upper <- as.numeric(str_extract(Risk_per_ffyr_PI, "(?<=; )[^%]+"))
        paste0("[",round(pmax(0, lower), 1), "%, ", round(pmax(0, upper), 1), "%]")
      }
    )
  )
# Create the gt table 
beta_coefficients_S4_combined_gt <- beta_coefficients_S4_combined %>% 
  gt(groupname_col = "Analysis") %>%
  cols_label(
    Method = "Method",
    Risk_per_ffyr_est = "Estimate",
    Risk_per_ffyr_PI = "95% Prediction Interval", 
    Min_exp_50pct_PoC = "Point Estimate",
    Min_exp_conservative = "Presumably Plausible",
    Point_per_10k = "Point Estimate", 
    Presumably_plausible_per_10k = "Presumably Plausible"
  ) %>%
  tab_spanner(
    label = "Risk Increase per Fibre-year",
    columns = c(Risk_per_ffyr_est, Risk_per_ffyr_PI)
  ) %>%
  tab_spanner(
    label = "Min Exposure for 50% PoC (fibre-years)",
    columns = c(Min_exp_50pct_PoC, Min_exp_conservative)
  ) %>%
  tab_spanner(
    label = "Cases per 10,000 above 50% PoC in SYNERGY",
    columns = c(Point_per_10k, Presumably_plausible_per_10k)
  ) %>%
  cols_align(align = "center") %>%
  tab_footnote(
    footnote = "Probability of Causation (PoC) of 50% corresponds to a doubling in lung cancer risk (i.e., RR = 2) for the point estimate and the upper 95% prediction interval for the presumably plausible threshold.",
    locations = cells_column_spanners(spanners = "Min Exposure for 50% PoC (fibre-years)")
  ) %>%
  tab_footnote(
    footnote = "Negative values truncated to 0 due to monotonicity assumption.",
    locations = cells_column_spanners(spanners = "Risk Increase per Fibre-year")
  ) %>%
  tab_footnote(
    footnote = "One-stage: pooled analysis with random intercepts and slopes. Two-stage univariate: Traditional random-effects meta-analysis of exposure coefficients. Two-stage bivariate: Joint meta-analysis of intercept and exposure coefficients accounting for their correlation.",
    locations = cells_column_labels(columns = Method)
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>% 
  opt_footnote_marks(marks = "letters")

# Save the table
gtsave(beta_coefficients_S4_combined_gt, file = file.path(manuscript_tables, "TableS4.html"))
