# process_depression_data.R


setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")

# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------


# Main ------------------------------------------------------------------------

# Paths to anxiety directories
input_dir <- "data/raw/Depression/"
output_dir <- "data/processed/Depression/"

# Data frame containing files and threshold info
df_files <- tibble(file = c("AGRE_AFFCHILD1_DEPRESSION_AGE_AT_DIAGNOSIS.xlsx", 
                             "AGRE_AFFCHILD1_DEPRESSION_DIAGNOSIS.xlsx",
                             "AGRE_AFFCHILD1_DEPRESSIVE_SYMPTOMS.xlsx",
                            "CBCL1_CB1TS 1-5yold.xlsx",
                            "CBCL618_CB681TS 6-18y old Affective Problems.xlsx",
                            "CCDC_CDCOT2SY Depression Diagnosis.xlsx",
                            "RCADSP_D_TSCORE.xlsx"),
                   threshold = c(0, 1, 1, 70, 70, 1, 70),
                   comparison = c(">=", "=", "=", ">=", ">=", "=", ">="),
                   colname = c("AGRE_AGE", "AGRE_DX", "AGRE_SY",
                               "CBCL1", "CBCL618", "CCDC","RCADSP")) %>% 
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
    df_depression <- df
  } else {
    df_depression <- full_join(df_depression, df, by = "ID")
  }
  
}

# Calculate combined anxiety score
df_depression_combined <- df_depression %>% 
  column_to_rownames("ID") %>% 
  apply(1, function(x) {any(as.logical(x), na.rm = TRUE)}) %>% 
  enframe(name = "ID", value = "Depression_PASS")

# Join combined score to individual scores
df_depression <- df_depression %>% 
  left_join(df_depression_combined, by = "ID")

# Export processed data
outfile <- "depression.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_depression_combined, file = outfile)

