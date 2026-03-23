# process_anxiety_data.R


setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")

# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------


# Main ------------------------------------------------------------------------

# Paths to anxiety directories
input_dir <- "data/raw/Seizures/"
output_dir <- "data/processed/Seizures/"

# Data frame containing files and threshold info
df_files <- tibble(file = c("ADI-R 85 ADIWPS.xlsx",
                            "AGRE_AFFCHILD1_AGE_OF_SEIZURE_ONSET_YEARS.xlsx",
                            "AGRE_AFFCHILD1_INTRACTABILITY_OF_SEIZURES.xlsx",
                            "AGRE_AFFCHILD1_NUMBER_OF_SEIZURES.xlsx",
                            "AGRE_AFFCHILD1_OTHER_SEIZURES.xlsx",
                            "AGRE_AFFCHILD1_SEIZR_REQ_TREATMENT_WITH_MEDS.xlsx",
                            "AGRE_AFFCHILD1_SEIZURE_TYPE.xlsx",
                            "CLINICALINFO_ASDEPILEP.xlsx",
                            "CLINICALINFO_ASDSEIZ (FS excluded) (Feb 5, 2026 9-45-09 AM).xlsx"),
                   threshold = c(2, 1, 1, 1, 1, 1, 1, 1, 1),
                   comparison = c("=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">="),
                   colname = c("ADIR", "AGRE_AGE", "AGRE_INTRACT", "AGRE_NUM", "AGRE_OTHER",
                               "AGRE_MEDS", "AGRE_TYPE", "ASDEPILEP", "ASDSEIZ")) %>% 
  mutate(colname = paste0(colname, "_PASS"))

for (i in 1:nrow(df_files)) {

  file <- df_files[[i, "file"]]  
  file <- file.path(input_dir, file)
  df <- read_excel(file) %>% 
    select(ID = indexid, score = numeric_value) %>% 
    filter(!is.na(ID), !is.na(score)) %>% 
    group_by(ID) %>% 
    mutate(score_max = max(score, na.rm = TRUE)) %>% 
    ungroup() 
    
  if (df_files[[i, "comparison"]] == "=") {
    df <- df %>% 
      mutate(PASS = score_max == df_files[[i, "threshold"]]) 
  } else if (df_files[[i, "comparison"]] == ">=") {
    df <- df %>% 
      mutate(PASS = score_max >= df_files[[i, "threshold"]]) 
  } else {
    stop()
  }

  df <- df %>%     
    select(ID, PASS) %>% 
    distinct()
  
  colnames(df)[2] <- df_files[[i, "colname"]]
  
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

# Import seizure dates data
file <- "mssng sorted epilepsy.xlsx"
file <- file.path(input_dir, file)
df_dates <- read_excel(file, sheet = "MSSNG co-occuring sorted Epilep") %>% 
  select(ID = indexid, Seizure_Date = testdate) %>% 
  mutate(Seizure_Date = as.Date(Seizure_Date))

df_seizures <- df_seizures %>% 
  left_join(df_dates, by = "ID")

# Export processed data
outfile <- "seizures.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_seizures, file = outfile)


# Note that false here doesn't mean they don't have ADHD. 

# No participants have all variables, so can't define a proper negative case
# mat_ADHD <- as.matrix(df_ADHD)
# mat_ADHD_na <- is.na(mat_ADHD)
# which(rowSums(mat_ADHD_na) == 0)

