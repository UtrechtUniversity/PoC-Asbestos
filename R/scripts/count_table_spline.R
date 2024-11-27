count_table <- df.spline.cum0 %>%
  select(subjctid, status, ever_asbestos0, asbestos_cum0,
         matches("PoC_factor|p97.5%_factor")) %>%
  rename_with(~ gsub("p97.5%_", "upper_bound_", .), matches("p97.5%")) %>%
  filter(status == 1) %>%
  summarize(
    Scenario = rep(c("No Error Factor", "Error Factor: 1.5", "Error Factor: 2"), each = 2),
    Bound = rep(c("PoC", "Upper Bound"), times = 3),
    
    # Count Cases with PoC >= 0.5
    Cases = c(
      sum(PoC_factor1 >= 0.5, na.rm = TRUE),
      sum(upper_bound_factor1 >= 0.5, na.rm = TRUE),
      sum(PoC_factor1.5 >= 0.5, na.rm = TRUE),
      sum(upper_bound_factor1.5 >= 0.5, na.rm = TRUE),
      sum(PoC_factor2 >= 0.5, na.rm = TRUE),
      sum(upper_bound_factor2 >= 0.5, na.rm = TRUE)
    ),
    
    # Calculate Cases per 10,000
    `Cases per 10,000` = round(Cases / cases * 10000, 0),
    
    # Minimum cumulative exposure for each condition
    `Min cum exp` = c(
      min(asbestos_cum0[PoC_factor1 >= 0.5], na.rm = TRUE),
      min(asbestos_cum0[upper_bound_factor1 >= 0.5], na.rm = TRUE),
      min(asbestos_cum0[PoC_factor1.5 >= 0.5], na.rm = TRUE),
      min(asbestos_cum0[upper_bound_factor1.5 >= 0.5], na.rm = TRUE),
      min(asbestos_cum0[PoC_factor2 >= 0.5], na.rm = TRUE),
      min(asbestos_cum0[upper_bound_factor2 >= 0.5], na.rm = TRUE)
    )
  ) %>%
  group_by(Scenario) 

# Save as text file
count_table %>% 
  write.table(., paste0(tabfolder, "/PoC_spline_model_cum0_count_plus50.txt"), sep = "\t")

#Create gt table
count_table <- count_table %>%
  gt() %>%
  fmt_number(
    columns = `Cases per 10,000`,
    decimals = 0
  ) %>%
  fmt_number(
    columns = `Min cum exp`,
    decimals = 5
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) 

# Save gt table 
count_table %>% 
  gtsave(filename = "PoC_spline_model_cum0_count_plus50.rtf", 
         path = paste0(tabfolder,"/rtf"))