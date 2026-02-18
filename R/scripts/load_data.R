# Load dataset  
df <- read.delim(file.path(inputfolder,"syn_all_subjects.txt"))

# Convert columns to factor
factor_cols <- c(
  "study_name", "country", "sex", "histotyp", "status", "agegroup", "smoking",
  "ever_smok", "time_quit", "list_a", "blue", "constr", "miner", "metalw", 
  "transp", "veh_mech", "farmer", "since60", "since70", "ever_asbestos0",
  "asbestos_time_since_exp40_0", "asbestos_dur_cat0", "asbestos_qq0",
  "ever_silica0", "silica_dur_cat0", "silica_qq0", "ever_chromium0", 
  "chromium_dur_cat0", "chromium_qq0", "ever_nickel0", "nickel_dur_cat0",
  "nickel_qq0", "ever_pah0", "pah_dur_cat0", "pah_qq0"
)

df[factor_cols] <- lapply(df[factor_cols], as.factor)

# Set reference levels and add labels for table display
df <- df %>%
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
    smoking = factor(smoking, levels = 0:2, labels = c("Never smoker", "Former smoker", "Current smoker")),
    time_quit = factor(time_quit, levels = c(6, 1:5), labels = c("Never smoker", "Current smoker", "0-7 years", "8-15 years", "16-25 years", ">25 years")),
    agegroup = relevel(agegroup, ref = "1"),
    study_name = relevel(study_name, ref = "AUT")
  )

label(df$sex) <- "Sex"
label(df$age) <- "Age"
label(df$asbestos_cum0) <- "Asbestos (ff/ml-years)"
label(df$asbestos_dur0) <- "Exposure duration (years)"
label(df$smoking) <- "Smoking"
label(df$packyears) <- "Pack-years"
label(df$time_quit) <- "Time since quitting smoking"
levels(df$agegroup) <- c("<45", "45–49", "50–54", "55–59", "60–64", "65–69", "70–74", ">74")


# Check correct mapping 
# df %>% distinct(study_name, source_controls) 

# Load additional data 
studies_SYNERGY <- read.csv(file.path(psfolder, "SYNERGY_study_characteristics.csv"))
study_rates <- read_csv(file.path(psfolder, "incidence_rates.csv"))

studies_SYNERGY <- studies_SYNERGY %>% 
  mutate(
    source_controls = case_when(
      # Population-based controls (P)
      study_name %in% c(
        "AUT", "EAGLE", "HdA", "ICARE", "INCO_UK", 
        "LUCAS", "MONTREAL", "TURIN"
      ) ~ "P",
      
      # Hospital-based controls (H)
      study_name %in% c(
        "CAPUA", "LUCA", "PARIS", "ROME", 
        "INCO_Czech Republic", "INCO_Hungary", 
        "INCO_Romania", "INCO_Russia", "INCO_Slovakia"
      ) ~ "H",
      
      # Both population & hospital controls (P & H)
      study_name %in% c(
        "INCO_Poland", "Toronto"
      ) ~ "H", # Considered Hospital-based on prior SYNERGY studies
      
      # Nested case-control study.
      study_name == "MORGEN" ~ "P", 
      # IRR estimates compatible with RR from other population-based studies. 
      
      TRUE ~ NA_character_
    ) 
  )

study_rates <- study_rates %>%
  left_join(studies_SYNERGY %>% select(study_name, source_controls), 
            by = "study_name")

df <- df %>%
  left_join(study_rates, by = c("study_name", "sex", "agegroup")) %>% 
  left_join(studies_SYNERGY %>%
              select(study_name, case_control_prop, response_cases, 
                     response_controls, sampling, duration_T = year_period),
            by = "study_name")