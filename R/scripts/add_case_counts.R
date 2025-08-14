add_case_counts <- function(beta_table, data, cases_total, stratify_by = NULL, strata_mapping = NULL) {
  #' Add case counts per 10,000 to beta coefficients table
  #' 
  #' @param beta_table Data frame with Min_exp_50pct_PoC and Min_exp_conservative columns
  #' @param data The main dataset with asbestos_cum0, status columns
  #' @param cases_total Total number of cases for rate calculation
  #' @param stratify_by Character string of column name to stratify by (NULL for population-average)
  #' @param strata_mapping Named vector mapping beta_table$Model values to data values
  #'                      e.g., c("Hospital controls" = "H", "Population controls" = "P")
  
  if (is.null(stratify_by)) {
    # Population-average approach
    case_counts <- data %>%
      filter(status == 1) %>%
      summarise(
        Point_per_10k = round(sum(asbestos_cum0 >= beta_table$Min_exp_50pct_PoC[1], na.rm = T) / cases_total * 10000),
        Presumably_plausible_per_10k = round(sum(asbestos_cum0 >= beta_table$Min_exp_conservative[1], na.rm = T) / cases_total * 10000)
      )
    
    # Add to table (same values for all rows)
    beta_table_with_counts <- beta_table %>%
      mutate(
        Point_per_10k = case_counts$Point_per_10k,
        Presumably_plausible_per_10k = case_counts$Presumably_plausible_per_10k
      )
    
  } else {
    # Stratified approach
    case_counts_list <- map_dfr(1:nrow(beta_table), function(i) {
      model_name <- beta_table$Model[i]
      stratum_value <- strata_mapping[model_name]
      
      data %>%
        filter(status == 1 & !!sym(stratify_by) == stratum_value) %>%
        summarise(
          Point_per_10k = round(sum(asbestos_cum0 >= beta_table$Min_exp_50pct_PoC[i], na.rm = T) / cases_total * 10000),
          Presumably_plausible_per_10k = round(sum(asbestos_cum0 >= beta_table$Min_exp_conservative[i], na.rm = T) / cases_total * 10000)
        )
    })
    
    # Add to table
    beta_table_with_counts <- beta_table %>%
      bind_cols(case_counts_list)
  }
  
  return(beta_table_with_counts)
}