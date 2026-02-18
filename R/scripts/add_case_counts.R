add_case_counts <- function(beta_table, data) {
  #' Add case counts per 10,000 to beta coefficients table
  #' 
  #' @param beta_table Data frame with Min_exp_50pct_PoC and Min_exp_conservative columns
  #' @param data The main dataset with asbestos_cum0, status columns
  
  # Get all cases and calculate total
  cases_data <- data %>% filter(status == 1)
  cases_total <- nrow(cases_data)
  
  # Calculate case counts for each threshold
  beta_table %>%
    rowwise() %>%
    mutate(
      Point_per_10k = round(sum(cases_data$asbestos_cum0 >= Min_exp_50pct_PoC, na.rm = TRUE) / cases_total * 10000),
      Presumably_plausible_per_10k = round(sum(cases_data$asbestos_cum0 >= Min_exp_conservative, na.rm = TRUE) / cases_total * 10000)
    ) %>%
    ungroup()
}