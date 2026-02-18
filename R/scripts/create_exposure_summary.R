create_exposure_summary <- function(
    data,
    group_vars = "study_name",  
    status_var = "status",
    binary_var,
    continuous_var,
    binary_var_label = "Exposed",
    continuous_var_label = "Exposure",
    continuous_var_unit = "",
    title = NULL,
    filter_ever_exposed = TRUE,
    continuous_decimals = 1,
    percent_decimals = 1,
    hide_totals = TRUE,
    include_range = TRUE
) {
  
  binary_var_sym <- sym(binary_var)
  continuous_var_sym <- sym(continuous_var)
  status_var_sym <- sym(status_var)
  
  # Calculate binary exposure summary on full dataset
  binary_summary <- data %>%
    group_by(across(all_of(c(group_vars, status_var)))) %>%
    summarize(
      n = n(),
      n_exposed = sum(!!binary_var_sym == 1, na.rm = TRUE),
      percent_exposed = (n_exposed / n) * 100,
      .groups = 'drop'
    )
  
  # Determine dataset for continuous variable calculations
  continuous_data <- if(filter_ever_exposed) {
    data %>% filter(!!binary_var_sym == 1)
  } else {
    data
  }
  
  # Calculate continuous variable summary on appropriate dataset
  continuous_summary <- continuous_data %>%
    group_by(across(all_of(c(group_vars, status_var)))) %>%
    summarize(
      median_continuous = median(!!continuous_var_sym, na.rm = TRUE),
      IQR = paste(
        round(quantile(!!continuous_var_sym, 0.25, na.rm = TRUE), continuous_decimals), "-",
        round(quantile(!!continuous_var_sym, 0.75, na.rm = TRUE), continuous_decimals)
      ),
      range = if(include_range) {
        paste(
          round(min(!!continuous_var_sym, na.rm = TRUE), continuous_decimals), "-",
          round(max(!!continuous_var_sym, na.rm = TRUE), continuous_decimals)
        )
      } else {
        NA_character_
      },
      .groups = 'drop'
    )
  
  # Combine binary and continuous summaries
  summary_long <- binary_summary %>%
    left_join(continuous_summary, by = c(group_vars, status_var))
  
  # Convert to wide format
  value_cols <- c("n", "n_exposed", "percent_exposed", "median_continuous", "IQR")
  if(include_range) value_cols <- c(value_cols, "range")
  
  summary_wide <- summary_long %>%
    pivot_wider(
      id_cols = all_of(group_vars),
      names_from = !!status_var_sym,
      values_from = all_of(value_cols)
    ) %>%
    rename(
      n_controls = n_0,
      n_cases = n_1,
      n_exposed_controls = n_exposed_0,
      n_exposed_cases = n_exposed_1,
      percent_exposed_controls = percent_exposed_0,
      percent_exposed_cases = percent_exposed_1,
      median_continuous_controls = median_continuous_0,
      median_continuous_cases = median_continuous_1,
      IQR_controls = IQR_0,
      IQR_cases = IQR_1
    )
  
  if(include_range) {
    summary_wide <- summary_wide %>%
      rename(
        range_controls = range_0,
        range_cases = range_1
      )
  }
  
  # Create labels for grouping variables
  group_labels <- setNames(
    str_to_title(str_replace_all(group_vars, "_", " ")),
    group_vars
  )
  
  # Prepare all column labels
  all_labels <- c(
    group_labels,
    list(
      n_cases = "Cases",
      n_controls = "Controls",
      n_exposed_cases = "Cases",
      n_exposed_controls = "Controls",
      percent_exposed_cases = "Cases",
      percent_exposed_controls = "Controls",
      median_continuous_cases = "Cases",
      median_continuous_controls = "Controls",
      IQR_cases = "Cases",
      IQR_controls = "Controls"
    )
  )
  
  # Add range labels if needed
  if(include_range) {
    all_labels <- c(all_labels, list(
      range_cases = "Cases",
      range_controls = "Controls"
    ))
  }
  
  # Create gt table
  gt_table <- summary_wide %>%
    gt() %>%
    tab_spanner(
      label = "Total (n)",
      columns = c(n_cases, n_controls)
    ) %>%
    tab_spanner(
      label = paste0(binary_var_label, " (n)"),
      columns = c(n_exposed_cases, n_exposed_controls)
    ) %>%
    tab_spanner(
      label = paste0(binary_var_label, " (%)"),
      columns = c(percent_exposed_cases, percent_exposed_controls)
    ) %>%
    tab_spanner(
      label = paste0("Median ", continuous_var_label, 
                     if(continuous_var_unit != "") paste0(" (", continuous_var_unit, ")") else "",
                     if(filter_ever_exposed) " (among exposed)" else ""),
      columns = c(median_continuous_cases, median_continuous_controls)
    ) %>%
    tab_spanner(
      label = paste0("IQR", 
                     if(continuous_var_unit != "") paste0(" (", continuous_var_unit, ")") else "",
                     if(filter_ever_exposed) " (among exposed)" else ""),
      columns = c(IQR_cases, IQR_controls)
    ) %>%
    cols_label(!!!all_labels)
  
  # Add title only if provided
  if(!is.null(title) && title != "") {
    gt_table <- gt_table %>% tab_header(title = title)
  }
  
  # Add range spanner if included
  if(include_range) {
    gt_table <- gt_table %>%
      tab_spanner(
        label = paste0("Range", 
                       if(continuous_var_unit != "") paste0(" (", continuous_var_unit, ")") else "",
                       if(filter_ever_exposed) " (among exposed)" else ""),
        columns = c(range_cases, range_controls)
      )
  }
  
  gt_table <- gt_table %>%
    fmt_number(
      columns = starts_with("percent_"),
      decimals = percent_decimals
    ) %>%
    fmt_number(
      columns = c(starts_with("n_"), starts_with("median_continuous_")),
      decimals = continuous_decimals
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    )
  
  # Adjust column width based on number of columns
  gt_table <- gt_table %>% cols_width(everything() ~ pct(10))
  
  # Hide totals if requested
  if(hide_totals) {
    cols_to_hide <- c("n_cases", "n_controls", "n_exposed_cases", "n_exposed_controls")
    gt_table <- gt_table %>% cols_hide(columns = all_of(cols_to_hide))
  }
  
  return(gt_table)
}