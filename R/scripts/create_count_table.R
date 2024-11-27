create_count_table <- function(data, x, cases_count) {
  #' Create a count table for PoC
  #'
  #' This function generates a summary table that counts how many participants 
  #' in the dataset have values greater than or equal to 0.5 in columns related to 
  #' Probability of Causation (PoC). Additionally, it calculates the minimum
  #' values of the specified exposure value as vector `x` for subsets of participants 
  #' with status 1 and PoC values >= 0.5, if the vector exists in the dataset.
  #'
  #' @param data A data frame. It must have columns named `status`, and other columns containing "poc" or "percentage" in their names. The specified vector `x` is optional.
  #' @param x A character string specifying the name of the vector to calculate the min for.
  #' @param cases_count An integer specifying the number of cases (used for calculating Cases per 10,000).
  #' @return A data frame with the summary count of participants and the min 
  #' values of the specified vector `x` if the column exists.
  #' @import dplyr
  #' @import purrr
  #' @import tibble
  
    # Create the count table
    count_table <- data %>%
      filter(status == "1") %>% 
      select(contains(c("poc", "%"))) %>%
      summarise_all(~ sum(. >= 0.5)) %>%
      t() %>%
      as.data.frame() %>%
      rename(Cases = V1) %>% 
      rownames_to_column(var = "Scenario") %>% 
      mutate('Cases per 10,000' = round(Cases / cases_count * 10000))
    
    # Check if the specified vector exists
    if (x %in% colnames(data)) {
      selected_columns <- colnames(data)[grepl("poc|%", colnames(data), ignore.case = TRUE)]
      
      # Calculate the min values for selected columns where value >= 0.5
      min_values <- map(selected_columns, function(column) {
        filtered_data <- data %>%
          filter(status == "1" & !!sym(column) >= 0.5)
        
        if (nrow(filtered_data) > 0) {
          min(filtered_data[[x]], na.rm = TRUE)  # Use [[x]] to access the column
        } else {
          NA  # Return NA if no valid data
        }
      })
      
      # Round the min values to 2 decimal places
      min_values <- round(unlist(min_values), 2)
      
      # Add min values to the count table
      count_table <- count_table %>%
        mutate('Min cum exp' = min_values)
    }
    
    return(count_table)
  }
  