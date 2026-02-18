table1 <- table1(
  ~ sex + age + asbestos_cum0 + asbestos_dur0 + smoking + 
    packyears + time_quit |
    factor(status, levels = c("0", "1"), labels = c("Controls", "Lung cancer cases")),
  data = df %>% mutate(
    asbestos_cum0 = ifelse(df$asbestos_cum0 == 0, NA, df$asbestos_cum0) %>% 
      table1::setLabel(., "Asbestos (ff/ml-years)"),
    asbestos_dur0 = ifelse(df$asbestos_dur0 == 0, NA, df$asbestos_dur0) %>% 
      table1::setLabel(., "Exposure duration (years)")
  ),
  overall = c(left = "Total"),
  caption = "Table 1. Descriptive characteristics of participants",
  render.continuous = c(
    . = "Mean (SD)",
    . = "Median (Q1, Q3)",
    . = "Min, Max"
  ) 
)

# Convert to flextable to edit and save 
table1 <- t1flex(table1, tablefn = "qflextable") 

# Remove rows that will not be displayed in final output table
table1 <- table1 %>% 
  delete_rows(., i = c(6:7,    # Age median, min, max
                       23,25,  # Pack-years mean, min, max
                       27,28   # Time since quitting, rm non smokers and current 
  )
  )

# Replace missing values for "Never exposed" to match the ever-exposed zero's
table1$body$dataset[,1] <- str_replace(
  string = table1$body$dataset[,1],
  pattern = "Missing",
  replacement = "Never exposed"
)

# Convert to gt table
table1 <- table1$body$dataset %>% gt

# Save in rich text format (rtf)
table1 %>% 
  gtsave(filename = "rtf/table1.rtf", path = tabfolder)

# Save in html format in manuscript folder
table1 %>% 
  gtsave(filename = "Table3.html", path = manuscript_tables)

# Save as R object 
save(table1, file = file.path(tabfolder, "table1.RDS"))