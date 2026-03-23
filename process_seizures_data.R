# process_anxiety_data.R


setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")

# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Main ------------------------------------------------------------------------

# Paths to anxiety directories
input_dir <- "data/raw/Seizures/"
output_dir <- "data/processed/Seizures/"

file <- "co_occurring_condition_characteristics___details_2025-12-10T18_56_00.287411Z.xlsx"
file <- file.path(input_dir, file)
df_seizures_init <- read_excel(file) %>% 
  filter(condition == "Seizures")

codes <- sort(unique(df_seizures_init$code))
codes

df_measures <- tibble(code = codes,
                      threshold = c(2, 0, 1, 1, 1, 1, 1, 1, 1, 1),
                      comparison = c("=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">="),
                      colname = c("ADIR", "AGRE_AGE", "AGRE_FEBRILE", "AGRE_INTRACT", "AGRE_NUM",
                                  "AGRE_OTHER", "AGRE_MEDS", "AGRE_TYPE", "ASDEPILEP", "ASDSEIZ")) %>% 
  mutate(colname = paste0(colname, "_PASS"))


for (i in 1:nrow(df_measures)) {
  
  df <- df_seizures_init %>% 
    filter(code == codes[i]) %>% 
    select(ID = indexid, score = raw_value, testdate) %>% 
    mutate(score = as.numeric(score))
  
  if (df_measures[[i, "comparison"]] == "=") {
    df <- df %>% 
      mutate(PASS = score == df_measures[[i, "threshold"]]) 
  } else if (df_measures[[i, "comparison"]] == ">=") {
    df <- df %>% 
      mutate(PASS = score >= df_measures[[i, "threshold"]]) 
  } else {
    stop()
  }
  

  
  df <- df %>% 
    group_by(ID) %>% 
    summarise(PASS = any(PASS)) %>% 
    ungroup() 
  
  colnames(df)[2] <- df_measures[[i, "colname"]]
  
  if (i == 1) {
    df_seizures <- df
  } else {
    df_seizures <- full_join(df_seizures, df, by = "ID")
  }
  
}

# Calculate combined seizures score
df_seizures_combined <- df_seizures %>% 
  column_to_rownames("ID") %>% 
  apply(1, function(x) {any(as.logical(x), na.rm = TRUE)}) %>% 
  enframe(name = "ID", value = "Seizure_PASS")

# Join combined score to individual scores
df_seizures <- df_seizures %>% 
  left_join(df_seizures_combined, by = "ID")


## Processing dates -----------------------------------------------------------

# Extract initial dates information
df_seizures_dates_init <- df_seizures_init %>%
  select(ID = indexid, Seizure_Date = testdate, code) %>% 
  mutate(Seizure_Date = as.Date(Seizure_Date)) %>% 
  mutate(Seizure_Date = ifelse(Seizure_Date < "1970-01-01", NA, Seizure_Date)) %>% 
  mutate(Seizure_Date = as.Date(Seizure_Date)) %>% 
  filter(!is.na(Seizure_Date)) %>% 
  distinct() 

# Identify participants with multiple dates
ids_multiple_dates <- df_seizures_dates_init %>% 
  select(ID, Seizure_Date) %>% 
  distinct() %>% 
  group_by(ID) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(ID)

# For participants with unique dates, use those
df_seizures_dates_unique <- df_seizures_dates_init %>% 
  filter(!(ID %in% ids_multiple_dates)) %>% 
  select(ID, Seizure_Date) %>% 
  distinct() %>% 
  arrange(ID) 

# Convert seizure data to long format and merge code information
df_seizures_long <- df_seizures %>% 
  select(-Seizure_PASS) %>% 
  pivot_longer(cols = -ID, names_to = "colname", values_to = "PASS") %>% 
  left_join(df_measures %>% select(colname, code), by = "colname")

# Filter for participants with multiple dates
df_seizures_dates_multiple <- df_seizures_dates_init %>% 
  filter(ID %in% ids_multiple_dates) %>% 
  left_join(df_seizures_long, by = c("ID", "code")) %>% 
  arrange(ID)

# If participants pass on a measure, use the earliest date for passing measures
df_seizures_dates_multiple_pass <- df_seizures_dates_multiple %>%
  filter(PASS) %>% 
  group_by(ID) %>% 
  filter(Seizure_Date == min(Seizure_Date)) %>%
  select(ID, Seizure_Date) %>% 
    distinct()

# For remaining participants who don't pass anything, use the earliest date
df_seizures_dates_multiple_rest <- df_seizures_dates_multiple %>% 
  anti_join(df_seizures_dates_multiple_pass, by = "ID") %>% 
  group_by(ID) %>% 
  filter(Seizure_Date == min(Seizure_Date)) %>%
  select(ID, Seizure_Date) %>% 
  distinct()

# Combine all dates information
df_seizures_dates <- bind_rows(df_seizures_dates_unique,
                               df_seizures_dates_multiple_pass,
                               df_seizures_dates_multiple_rest)

# Join dates information to main data
df_seizures <- df_seizures %>% 
  left_join(df_seizures_dates, by = "ID")

# Export processed data
outfile <- "seizures.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_seizures, file = outfile)


