# Data preparation
df_meta <- df %>%
  mutate(
    agegroup = factor(agegroup),
    sex = factor(sex),
    study_name = factor(study_name),
    status = as.numeric(as.character(status)),  # Convert factor to numeric (0/1)
    source_controls = factor(source_controls)
  )

# Split data by control source
df_hospital <- df_meta %>% filter(source_controls == "H")
df_population <- df_meta %>% filter(source_controls == "P")

# Fit model with the available contrast variables per study and extract covariance
fit_study_model <- function(study_data) {
  # Base model with continuous variables and age always included
  base_formula <- "status ~ asbestos_cum0 + packyrs + agegroup"
  
  # Add categorical variables only if they have variation
  if(n_distinct(study_data$sex, na.rm = TRUE) > 1) {
    base_formula <- paste(base_formula, "+ sex")
  }
  if(n_distinct(study_data$time_quit, na.rm = TRUE) > 1) {
    base_formula <- paste(base_formula, "+ time_quit")
  }
  
  # Fit the model
  tryCatch({
    model <- glm(as.formula(base_formula), data = study_data, family = binomial)
    return(list(model = model, formula = base_formula, converged = TRUE))
  }, error = function(e) {
    # Fit minimal model if prior attempts fail
    minimal_formula <- "status ~ asbestos_cum0"
    model <- glm(as.formula(minimal_formula), data = study_data, family = binomial)
    return(list(model = model, formula = minimal_formula, converged = TRUE))
  })
}

# Function to perform both univariate and bivariate meta-analysis
perform_meta_analysis <- function(data, control_type) {
  # Extract both intercept and exposure effects with covariance
  study_results <- data %>%
    group_by(study_name) %>%
    filter(n() > 10, sum(status) > 0, sum(1-status) > 0) %>%  # Basic filters
    nest() %>%
    mutate(
      model_result = map(data, fit_study_model),
      model = map(model_result, ~ .$model),
      formula_used = map_chr(model_result, ~ .$formula),
      
      # Extract intercept (alpha)
      intercept = map_dbl(model, ~ coef(.)[1]),  
      se_intercept = map_dbl(model, ~ summary(.)$coefficients[1, 2]),
      
      # Extract exposure coefficient (beta)
      coef = map_dbl(model, ~ {
        cf <- coef(.)
        if("asbestos_cum0" %in% names(cf)) cf["asbestos_cum0"] else NA
      }),
      se = map_dbl(model, ~ {
        se_vals <- summary(.)$coefficients[, 2]
        if("asbestos_cum0" %in% names(se_vals)) se_vals["asbestos_cum0"] else NA
      }),
      
      # Extract covariance between intercept and exposure
      cov_intercept_exposure = map_dbl(model, ~ {
        if("asbestos_cum0" %in% names(coef(.))) {
          vcov(.)[1, "asbestos_cum0"]  # Covariance between intercept and asbestos
        } else NA
      }),
      
      # Calculate correlation
      cor_intercept_exposure = cov_intercept_exposure / (se_intercept * se),
      
      n = map_int(data, nrow)
    ) %>%
    filter(!is.na(coef), !is.na(se), !is.na(intercept)) %>%
    select(study_name, formula_used, intercept, se_intercept, coef, se, 
           cov_intercept_exposure, cor_intercept_exposure, n)
  
  #### Univariate Meta-Analysis ####
  meta_univariate <- rma(yi = coef, sei = se, data = study_results, method = "REML")
  pred_univariate <- predict(meta_univariate)
  
  #### Bivariate Meta-Analysis ####
  # Prepare data in long format for bivariate analysis
  dat_biv <- data.frame(
    study = rep(1:nrow(study_results), each = 2),
    study_name = rep(study_results$study_name, each = 2),
    outcome = rep(c("intercept", "exposure"), nrow(study_results)),
    yi = c(rbind(study_results$intercept, study_results$coef)),
    vi = c(rbind(study_results$se_intercept^2, study_results$se^2))
  )
  
  # Extract variance-covariance matrices for each study
  V_list <- lapply(1:nrow(study_results), function(i) {
    vi1 <- study_results$se_intercept[i]^2
    vi2 <- study_results$se[i]^2
    cov12 <- study_results$cov_intercept_exposure[i]  # use covariance directly
    
    mat <- matrix(c(vi1, cov12, cov12, vi2), nrow = 2, byrow = TRUE)
    
    # Ensure positive definiteness for numerical stability
    if(min(eigen(mat, symmetric = TRUE, only.values = TRUE)$values) <= 0) {
      mat <- mat + diag(1e-8, 2)
    }
    
    mat
  })
  
  # Fit bivariate random-effects model
  meta_bivariate <- tryCatch({
    rma.mv(yi, V = V_list, 
           mods = ~ outcome - 1,  # Separate intercepts for each outcome
           random = ~ outcome | study, 
           struct = "UN",  # Unstructured random effects
           data = dat_biv,
           method = "REML")
  }, error = function(e) {
    warning(paste("Bivariate model failed for", control_type, "controls:", e$message))
    NULL
  })
  
  # Extract bivariate results if successful
  if (!is.null(meta_bivariate)) {
    exposure_idx <- which(grepl("exposure", rownames(meta_bivariate$beta)))
    exposure_est <- meta_bivariate$beta[exposure_idx]
    exposure_se <- sqrt(meta_bivariate$vb[exposure_idx, exposure_idx])
    pred_bivariate <- predict(meta_bivariate, transf = exp)
  } else {
    exposure_est <- exposure_se <- pred_bivariate <- NULL
  }
  
  return(list(
    study_results = study_results, 
    meta_univariate = meta_univariate,
    pred_univariate = pred_univariate,
    meta_bivariate = meta_bivariate,
    exposure_est = exposure_est,
    exposure_se = exposure_se,
    pred_bivariate = pred_bivariate
  ))
}

# Function to create forest plot
create_forest_plot <- function(analysis_results, control_type, filename) {
  study_results <- analysis_results$study_results
  meta_univariate <- analysis_results$meta_univariate
  control_label <- ifelse(control_type == "H", "Hospital-based control", "Population-based control")
  
  png(filename = file.path(figfolder, filename),
      width = 10, height = 8, units = "in", res = 600, bg = "white")
  
  forest(meta_univariate, 
         slab = paste(study_results$study_name, 
                      " (n=", study_results$n, ")", sep=""),
         transf = exp,
         xlab = "Relative Risk per asbestos fibre-year (ff/ml-years)",
         main = paste("Two-stage meta-analysis of lung cancer risk due to occupational asbestos:\n", 
                      control_label, "studies"),
         refline = 1,
         digits = 2,
         cex = 0.8,     # Text size
         addpred = TRUE, # Add prediction intervals
         predstyle = "dist",
         showweights = TRUE, # Show study weights
         header = c("Study in SYNERGY", "RR [95% CI]"),
         shade = TRUE,
         top = 3
  )
  
  # Add weight header
  text(x = 4.1, y = meta_univariate$k + 2, "Weight (%)", pos = 2, font = 2, cex = 0.8)
  
  # Add heterogeneity statistics
  het_text <- paste0("Heterogeneity: I² = ", round(meta_univariate$I2, 1), 
                     "%, τ² = ", round(meta_univariate$tau2, 3), 
                     ", Q-test p = ", ifelse(meta_univariate$QEp < 0.001, "<0.001", round(meta_univariate$QEp, 3)))
  text(x = par("usr")[1], y = -1.5, het_text, pos = 4, cex = 0.7, font = 1)
  
  dev.off()
}

# Function to display  results
display_results <- function(analysis_results, control_type) {
  study_results <- analysis_results$study_results
  meta_univariate <- analysis_results$meta_univariate
  pred_univariate <- analysis_results$pred_univariate
  meta_bivariate <- analysis_results$meta_bivariate
  exposure_est <- analysis_results$exposure_est
  pred_bivariate <- analysis_results$pred_bivariate
  
  control_label <- ifelse(control_type == "H", "HOSPITAL", "POPULATION")
  
  cat(paste(control_label, "CONTROLS META-ANALYSIS\n"))
  cat(paste(rep("=", nchar(control_label) + 25), collapse = ""), "\n\n")
  
  # Display study characteristics
  cat("STUDY CHARACTERISTICS:\n")
  cat("  Studies included:    ", nrow(study_results), "\n")
  cat("  Total participants:  ", sum(study_results$n), "\n")
  
  # Univariate results
  cat("\nUNIVARIATE APPROACH:\n")
  cat("  Pooled RR:", sprintf("%.3f", exp(coef(meta_univariate))), "\n")
  cat("  95% PI:   ", sprintf("(%.3f, %.3f)", 
                              exp(pred_univariate$pi.lb), 
                              exp(pred_univariate$pi.ub)), "\n")
  cat("  I²:       ", sprintf("%.1f%%", meta_univariate$I2), "\n")
  cat("  τ²:       ", sprintf("%.4f", meta_univariate$tau2), "\n")
  
  # Bivariate results (if available)
  if (!is.null(meta_bivariate) && !is.null(exposure_est)) {
    cat("\nBIVARIATE APPROACH:\n")
    cat("  Pooled RR:", sprintf("%.3f", exp(exposure_est)), "\n")
    if (!is.null(pred_bivariate) && length(pred_bivariate$pi.lb) >= 2) {
      cat("  95% PI:   ", sprintf("(%.3f, %.3f)", 
                                  pred_bivariate$pi.lb[2], 
                                  pred_bivariate$pi.ub[2]), "\n")
    }
  } else {
    cat("\nBIVARIATE APPROACH: Failed to converge\n")
  }
  
  # Heterogeneity test
  Q_stat <- meta_univariate$QE
  df_Q <- meta_univariate$k - meta_univariate$p
  cat("\nHETEROGENEITY TEST:\n")
  cat("  Q =", sprintf("%.2f", Q_stat), 
      ", df =", df_Q, ", p =", sprintf("%.4f", pchisq(Q_stat, df_Q, lower.tail = FALSE)), "\n")
  
  cat("\n")
}

#==============================================================================
# META-ANALYSIS FOR HOSPITAL CONTROLS
#==============================================================================

# Perform comprehensive meta-analysis for hospital controls
hospital_analysis <- perform_meta_analysis(df_hospital, "H")

# Display results
display_results(hospital_analysis, "H")

# Create forest plot for hospital controls
create_forest_plot(hospital_analysis, "H", "Figure_forest_hospital_controls.png")

#==============================================================================
# META-ANALYSIS FOR POPULATION CONTROLS
#==============================================================================

# Perform comprehensive meta-analysis for population controls
population_analysis <- perform_meta_analysis(df_population, "P")

# Display results
display_results(population_analysis, "P")

# Create forest plot for population controls
create_forest_plot(population_analysis, "P", "Figure_forest_population_controls.png")

#==============================================================================
# SUMMARY COMPARISON OF APPROACHES
#==============================================================================

cat("\n\nCOMPARISON\n")
cat("=================================\n")

# Hospital controls summary
cat("Hospital Controls:\n")
cat("  Studies included:", nrow(hospital_analysis$study_results), "\n")
cat("  Univariate RR:  ", round(exp(coef(hospital_analysis$meta_univariate)), 3), "\n")
if (!is.null(hospital_analysis$exposure_est)) {
  cat("  Bivariate RR:   ", round(exp(hospital_analysis$exposure_est), 3), "\n")
} else {
  cat("  Bivariate RR:   ", "Failed to converge\n")
}
cat("  I²:             ", round(hospital_analysis$meta_univariate$I2, 1), "%\n")

# Population controls summary
cat("\nPopulation Controls:\n")
cat("  Studies included:", nrow(population_analysis$study_results), "\n")
cat("  Univariate RR:  ", round(exp(coef(population_analysis$meta_univariate)), 3), "\n")
if (!is.null(population_analysis$exposure_est)) {
  cat("  Bivariate RR:   ", round(exp(population_analysis$exposure_est), 3), "\n")
} else {
  cat("  Bivariate RR:   ", "Failed to converge\n")
}
cat("  I²:             ", round(population_analysis$meta_univariate$I2, 1), "%\n")

# Method comparison (if both bivariate models converged)
if (!is.null(hospital_analysis$exposure_est) && !is.null(population_analysis$exposure_est)) {
  cat("\nMETHOD COMPARISON:\n")
  cat("Hospital Controls - Difference (Bivariate vs Univariate):\n")
  hosp_diff <- exp(hospital_analysis$exposure_est) - exp(coef(hospital_analysis$meta_univariate))
  cat("  Absolute difference:", sprintf("%.4f", hosp_diff), "\n")
  
  cat("Population Controls - Difference (Bivariate vs Univariate):\n")
  pop_diff <- exp(population_analysis$exposure_est) - exp(coef(population_analysis$meta_univariate))
  cat("  Absolute difference:", sprintf("%.4f", pop_diff), "\n")
}


#### Beta coefficients and PoC #### 

# Extract univariate results
b_P_uni <- coef(population_analysis$meta_univariate)
LB_P_uni <- population_analysis$pred_univariate$pi.lb
UB_P_uni <- population_analysis$pred_univariate$pi.ub

b_H_uni <- coef(hospital_analysis$meta_univariate)
LB_H_uni <- hospital_analysis$pred_univariate$pi.lb
UB_H_uni <- hospital_analysis$pred_univariate$pi.ub

# Extract bivariate results (with fallback to univariate if not available)
b_P_biv <- ifelse(!is.null(population_analysis$exposure_est), population_analysis$exposure_est, b_P_uni)
b_H_biv <- ifelse(!is.null(hospital_analysis$exposure_est), hospital_analysis$exposure_est, b_H_uni)

# For bivariate prediction intervals, use univariate if bivariate not available
if (!is.null(population_analysis$pred_bivariate) && length(population_analysis$pred_bivariate$pi.lb) >= 2) {
  LB_P_biv <- log(population_analysis$pred_bivariate$pi.lb[2])
  UB_P_biv <- log(population_analysis$pred_bivariate$pi.ub[2])
} else {
  # Fallback to confidence intervals from bivariate model or univariate prediction intervals
  se_P_biv <- ifelse(!is.null(population_analysis$exposure_se), population_analysis$exposure_se, population_analysis$meta_univariate$se)
  LB_P_biv <- b_P_biv - 1.96 * se_P_biv
  UB_P_biv <- b_P_biv + 1.96 * se_P_biv
}

if (!is.null(hospital_analysis$pred_bivariate) && length(hospital_analysis$pred_bivariate$pi.lb) >= 2) {
  LB_H_biv <- log(hospital_analysis$pred_bivariate$pi.lb[2])
  UB_H_biv <- log(hospital_analysis$pred_bivariate$pi.ub[2])
} else {
  # Fallback to confidence intervals from bivariate model or univariate prediction intervals
  se_H_biv <- ifelse(!is.null(hospital_analysis$exposure_se), hospital_analysis$exposure_se, hospital_analysis$meta_univariate$se)
  LB_H_biv <- b_H_biv - 1.96 * se_H_biv
  UB_H_biv <- b_H_biv + 1.96 * se_H_biv
}

# Create combined table with both univariate and bivariate results
beta_coefficients_two_stage <- data.frame(
  Model = c("Population controls", "Population controls", "Hospital controls", "Hospital controls"),
  Method = c("Univariate", "Bivariate", "Univariate", "Bivariate"),
  Risk_per_ffyr_est = c(
    paste0(round((exp(b_P_uni) - 1) * 100, 1), "%"),
    paste0(round((exp(b_P_biv) - 1) * 100, 1), "%"),
    paste0(round((exp(b_H_uni) - 1) * 100, 1), "%"),
    paste0(round((exp(b_H_biv) - 1) * 100, 1), "%")
  ),
  Risk_per_ffyr_PI = c(
    paste0(round((exp(LB_P_uni) - 1) * 100, 1), "% ; ", 
           round((exp(UB_P_uni) - 1) * 100, 1), "%"),
    paste0(round((exp(LB_P_biv) - 1) * 100, 1), "% ; ", 
           round((exp(UB_P_biv) - 1) * 100, 1), "%"),
    paste0(round((exp(LB_H_uni) - 1) * 100, 1), "% ; ", 
           round((exp(UB_H_uni) - 1) * 100, 1), "%"),
    paste0(round((exp(LB_H_biv) - 1) * 100, 1), "% ; ", 
           round((exp(UB_H_biv) - 1) * 100, 1), "%")
  ),
  Min_exp_50pct_PoC = c(
    round(log(2) / b_P_uni, 2),
    round(log(2) / b_P_biv, 2),
    round(log(2) / b_H_uni, 2),
    round(log(2) / b_H_biv, 2)
  ),
  Min_exp_conservative = c(
    round(log(2) / UB_P_uni, 2),
    round(log(2) / UB_P_biv, 2),
    round(log(2) / UB_H_uni, 2),
    round(log(2) / UB_H_biv, 2)
  )
)

# Add case counts using previously sourced function
# source("scripts/add_case_counts.R")
beta_coefficients_two_stage <- add_case_counts(
  beta_table = beta_coefficients_two_stage,
  data = df
)

# Create the formatted gt table (adapting your existing code)
beta_coefficients_two_stage_gt <- beta_coefficients_two_stage %>% 
  gt() %>%
  cols_label(
    Model = "Control Source",
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
  # Group rows by control source
  tab_row_group(
    label = "Population Controls",
    rows = 1:2
  ) %>%
  tab_row_group(
    label = "Hospital Controls", 
    rows = 3:4
  ) %>%
  cols_align(align = "center") %>%
  tab_header(
    title = "Two-Stage Meta-Analysis: Asbestos Exposure-Response by Control Source",
    subtitle = "Comparing Univariate and Bivariate Approaches"
  ) %>%
  tab_footnote(
    footnote = "Probability of Causation (PoC)",
    locations = cells_column_spanners(spanners = "Min Exposure for 50% PoC (fibre-years)")
  ) %>%
  tab_footnote(
    footnote = "Univariate: Traditional random-effects meta-analysis of exposure coefficients. Bivariate: Joint meta-analysis of intercept and exposure coefficients accounting for their correlation.",
    locations = cells_column_labels(columns = Method)
  )

# Save the table
beta_coefficients_two_stage_gt %>%  
  gtsave(filename = "rtf/PoC_two_stage_stratified_count_plus50.rtf", path = tabfolder)

# Also save the raw data table
beta_coefficients_two_stage %>% 
  write.table(., paste0(tabfolder, "/PoC_two_stage_stratified_count_plus50.txt"), sep = "\t", row.names = FALSE)