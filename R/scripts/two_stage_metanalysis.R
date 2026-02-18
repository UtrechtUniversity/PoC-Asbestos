df_meta <- df %>%
  mutate(
    agegroup = factor(agegroup),
    sex = factor(sex),
    study_name = factor(study_name),
    status = as.numeric(as.character(status)),
    source_controls = factor(source_controls)
  ) %>% 
  arrange(source_controls)

# Modified function to extract full covariance information
fit_study_model <- function(study_data) {
  base_formula <- "status ~ asbestos_cum0 + packyrs + agegroup"
  
  if(n_distinct(study_data$sex, na.rm = TRUE) > 1) {
    base_formula <- paste(base_formula, "+ sex")
  }
  if(n_distinct(study_data$time_quit, na.rm = TRUE) > 1) {
    base_formula <- paste(base_formula, "+ time_quit")
  }
  
  tryCatch({
    model <- glm(as.formula(base_formula), data = study_data, family = binomial)
    return(list(model = model, formula = base_formula, converged = TRUE))
  }, error = function(e) {
    minimal_formula <- "status ~ asbestos_cum0"
    model <- glm(as.formula(minimal_formula), data = study_data, family = binomial)
    return(list(model = model, formula = minimal_formula, converged = TRUE))
  })
}

# Extract both intercept and exposure effects with covariance
SYNERGY_study_results <- df_meta %>%
  group_by(study_name) %>%
  filter(n() > 10, sum(status) > 0, sum(1-status) > 0) %>%
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

meta_univariate <- rma(yi = coef, sei = se, data = SYNERGY_study_results, method = "REML")
summary(meta_univariate)

## Prediction intervals for univariate meta-analysis
pred_univariate <- predict(meta_univariate)

I2_univariate <- meta_univariate$I2
Q_stat <- meta_univariate$QE
df_Q <- meta_univariate$k - meta_univariate$p

#### Bivariate Meta-Analysis ####

# Prepare data in long format for bivariate analysis
dat_biv <- data.frame(
  study = rep(1:nrow(SYNERGY_study_results), each = 2),
  study_name = rep(SYNERGY_study_results$study_name, each = 2),
  outcome = rep(c("intercept", "exposure"), nrow(SYNERGY_study_results)),
  yi = c(rbind(SYNERGY_study_results$intercept, SYNERGY_study_results$coef)),
  vi = c(rbind(SYNERGY_study_results$se_intercept^2, SYNERGY_study_results$se^2))
)

# Create variance-covariance matrices for each study
V_list <- lapply(1:nrow(SYNERGY_study_results), function(i) {
  vi1 <- SYNERGY_study_results$se_intercept[i]^2
  vi2 <- SYNERGY_study_results$se[i]^2
  cov12 <- SYNERGY_study_results$cov_intercept_exposure[i]  
  mat <- matrix(c(vi1, cov12, cov12, vi2), nrow = 2, byrow = TRUE)
  # small ridge if necessary:
  if(min(eigen(mat, symmetric=TRUE, only.values=TRUE)$values) <= 0) {
    mat <- mat + diag(1e-8, 2)
  }
  mat
})

# Fit bivariate random-effects model
meta_bivariate <- rma.mv(yi, V = V_list, 
                         mods = ~ outcome - 1,  # Separate intercepts for each outcome
                         random = ~ outcome | study, 
                         struct = "UN",  # Unstructured random effects
                         data = dat_biv,
                         method = "REML")

summary(meta_bivariate)

# Extract results for exposure effect from bivariate model
exposure_idx <- which(grepl("exposure", rownames(meta_bivariate$beta)))
exposure_est <- meta_bivariate$beta[exposure_idx]
exposure_se <- sqrt(meta_bivariate$vb[exposure_idx, exposure_idx])
exposure_ci <- c(exposure_est - 1.96*exposure_se, exposure_est + 1.96*exposure_se)

# Calculate prediction intervals for bivariate exposure effect
pred_bivariate <- predict(meta_bivariate, transf = exp) 

pi_lower_bivariate <- pred_bivariate$pi.lb[2]
pi_upper_bivariate <- pred_bivariate$pi.ub[2]

#### Results comparison #### 
cat("\n")
cat("         TWO-STAGE META-ANALYSIS RESULTS\n")

cat("\nUNIVARIATE APPROACH:\n")
cat("  Pooled RR:", sprintf("%.3f", exp(coef(meta_univariate))), "\n")
cat("  95% PI:   ", sprintf("(%.3f, %.3f)", 
                            exp(pred_univariate$pi.lb), 
                            exp(pred_univariate$pi.ub)), "\n")
cat("  I²:       ", sprintf("%.1f%%", I2_univariate), "\n")
cat("  Tau²:     ", sprintf("%.4f", meta_univariate$tau2), "\n")

cat("\nBIVARIATE APPROACH:\n")
cat("  Pooled RR:", sprintf("%.3f", exp(exposure_est)), "\n")
cat("  95% PI:   ", sprintf("(%.3f, %.3f)", pi_lower_bivariate, pi_upper_bivariate), "\n")

cat("\nSTUDY CHARACTERISTICS:\n")
cat("  Studies included:    ", nrow(SYNERGY_study_results), "\n")
cat("  Total participants:  ", sum(SYNERGY_study_results$n), "\n")
cat("  Heterogeneity test:   Q =", sprintf("%.2f", Q_stat), 
    ", df =", df_Q, ", p =", sprintf("%.4f", pchisq(Q_stat, df_Q, lower.tail = FALSE)), "\n")

cat("\n")

## Forest Plot 

SYNERGY_study_results <- SYNERGY_study_results %>%
  left_join(df_meta %>% select(study_name, source_controls) %>% distinct(), 
            by = "study_name") %>%
  arrange(source_controls, desc(study_name))

# Create row positions for grouping
hospital_studies <- which(SYNERGY_study_results$source_controls == "H")
population_studies <- which(SYNERGY_study_results$source_controls == "P")

# Forest plot 
n_hospital <- length(hospital_studies)
n_population <- length(population_studies)

# Calculate row positions with proper spacing
gap_size <- 3

# Population studies at the top
rows_population <- (n_hospital + gap_size + 1):(n_hospital + gap_size + n_population)

# Hospital studies at the bottom  
rows_hospital <- 1:n_hospital

# Combine all rows in the order they appear in the data
all_rows <- c(rows_hospital, rows_population)

# Create the forest plot
png(filename = file.path(manuscript_images, "Figure_S2.png"),
    width = 10, height = 10, units = "in", res = 600, bg = "white")

forest(meta_univariate, 
       slab = paste(SYNERGY_study_results$study_name, 
                    " (n = ", SYNERGY_study_results$n, ")", sep=""),
       transf = exp,
       xlab = "Relative Risk per asbestos fibre-year (ff/ml-years)",
       main = "Two-stage IPD meta-analysis of lung cancer risk due to occupational asbestos",
       refline = 1,
       digits = 2,
       cex = 0.8,
       addpred = TRUE,
       predstyle = "dist", 
       showweights = TRUE,
       header = c("Study in SYNERGY", "RR [95% CI]"),
       ylim = c(-3, max(all_rows) + 4),
       rows = all_rows,
       order = 1:nrow(SYNERGY_study_results),
       top = 3
)

# Add subgroup headers
pop_header_y <- max(rows_population) + 1
text(x = par("usr")[1], y = pop_header_y, 
     "Population-based controls", pos = 4, font = 4, cex = 0.9)

hosp_header_y <- max(rows_hospital) + 1
text(x = par("usr")[1], y = hosp_header_y, 
     "Hospital-based controls", pos = 4, font = 4, cex = 0.9)

# Add weight header
text(x = 4.3, y = max(all_rows) + 3, "Weight (%)", pos = 2, font = 2, cex = 0.8)

# Add heterogeneity statistics
het_text <- paste0("Heterogeneity: I² = ", round(meta_univariate$I2, 1), 
                   "%, τ² = ", round(meta_univariate$tau2, 3), 
                   ", Q-test p = ", ifelse(meta_univariate$QEp < 0.001, "<0.001", round(meta_univariate$QEp, 3)))
text(x = par("usr")[1], y = -2.5, het_text, pos = 4, cex = 0.7, font = 1)

dev.off()


#### Beta coefficients and PoC #### 

# Extract univariate results (main analysis)
b_main_uni <- coef(meta_univariate)
LB_main_uni <- pred_univariate$pi.lb
UB_main_uni <- pred_univariate$pi.ub

# Extract bivariate results 
b_main_biv <- exposure_est
se_main_biv <- exposure_se

# For bivariate prediction intervals
LB_main_biv <- log(pi_lower_bivariate)
UB_main_biv <- log(pi_upper_bivariate)

# Create combined table with both univariate and bivariate results (non-stratified)
beta_coefficients_two_stage_main <- data.frame(
  Model = c("Main", "Main"),
  Method = c("Univariate", "Bivariate"),
  Risk_per_ffyr_est = c(
    paste0(round((exp(b_main_uni) - 1) * 100, 1), "%"),
    paste0(round((exp(b_main_biv) - 1) * 100, 1), "%")
  ),
  Risk_per_ffyr_PI = c(
    paste0(round((exp(LB_main_uni) - 1) * 100, 1), "% ; ", 
           round((exp(UB_main_uni) - 1) * 100, 1), "%"),
    paste0(round((exp(LB_main_biv) - 1) * 100, 1), "% ; ", 
           round((exp(UB_main_biv) - 1) * 100, 1), "%")
  ),
  Min_exp_50pct_PoC = c(
    round(log(2) / b_main_uni, 2),
    round(log(2) / b_main_biv, 2)
  ),
  Min_exp_conservative = c(
    round(log(2) / UB_main_uni, 2),
    round(log(2) / UB_main_biv, 2)
  )
)

# Add case counts using previously sourced function 
# source("scripts/add_case_counts.R")
beta_coefficients_two_stage_main <- add_case_counts(
  beta_table = beta_coefficients_two_stage_main,
  data = df
)

# Create the formatted gt table
beta_coefficients_two_stage_main_gt <- beta_coefficients_two_stage_main %>% 
  gt() %>%
  cols_label(
    Model = "Analysis",
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
  tab_header(
    title = "Two-Stage Meta-Analysis: Asbestos Exposure-Response Analysis",
    subtitle = "Comparing Univariate and Bivariate Approaches - Main Results"
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
beta_coefficients_two_stage_main_gt %>%  
  gtsave(filename = "rtf/PoC_two_stage_main_count_plus50.rtf", path = tabfolder)

# Also save the raw data table
beta_coefficients_two_stage_main %>% 
  write.table(., paste0(tabfolder, "/PoC_two_stage_main_count_plus50.txt"), sep = "\t", row.names = FALSE)

