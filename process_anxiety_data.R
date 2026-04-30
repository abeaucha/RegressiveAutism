# process_anxiety_data.R



# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------


# Main ------------------------------------------------------------------------

# Paths to anxiety directories
input_dir <- "data/raw/Anxiety/"
output_dir <- "data/processed/Anxiety/"

# Data frame containing files and threshold info
df_files <- tibble(file = c("AGRE_AFFCHILD1_ANXIETY_AGE_AT_DIAGNOSIS.xlsx",
                            "AGRE_AFFCHILD1_ANXIETY_SYMPTOMS.xlsx",
                            "CBCL1_CB2TS.xlsx",
                            "CBCL618_CB682TS; Tscores.xlsx",
                            "CCDC_CDCDBOSY.xlsx",
                            "CCDC_CDCDPDX.xlsx",
                            "CLINICALINFO_ASDANXIET.xlsx",
                            "RCADSP_GA_TSCORE.xlsx",
                            "SPENCEP_SPENCE_TOT_SCORE; SCAS (Jan 29, 2026 12-16-51 PM).xlsx"),
                   threshold = c(0, 1, 65, 65, 1, 1, 1, 65, 65),
                   comparison = c(">=", "=", ">=", ">=", "=", "=", "=", ">=", ">="),
                   colname = c("AGRE_AGE", "AGRE", "CBCL1", "CBCL618", "CDCDBOSY",
                               "CDCDPDX", "ASDANXIETY", "RCADSP", "SPENCEP")) %>% 
  mutate(colname = paste0(colname, "_PASS"))

# Remove 
df_files <- df_files %>%
  filter(!(colname %in% c("CDCDBOSY", "CDCDPDX")))

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
    df_anxiety <- df
  } else {
    df_anxiety <- full_join(df_anxiety, df, by = "ID")
  }
    
}

# Calculate combined anxiety score
df_anxiety_combined <- df_anxiety %>% 
  column_to_rownames("ID") %>% 
  apply(1, function(x) {any(as.logical(x), na.rm = TRUE)}) %>% 
  enframe(name = "ID", value = "Anxiety_PASS")

# Join combined score to individual scores
df_anxiety <- df_anxiety %>% 
  left_join(df_anxiety_combined, by = "ID")

# Export processed data
outfile <- "anxiety.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_anxiety_combined, file = outfile)



# Continuous scores

file <- "CBCL618_CB682TS; Tscores.xlsx"
file <- file.path(input_dir, file)
df_anxiety_cbcl_618 <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value)  

file <- "CBCL1_CB2TS.xlsx"
file <- file.path(input_dir, file)
df_anxiety_cbcl_15 <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value) %>% 
  anti_join(df_anxiety_cbcl_618, by = "ID")

df_anxiety_cbcl <- bind_rows(df_anxiety_cbcl_618, df_anxiety_cbcl_15) %>% 
  rename(CBCL_AP_TS = score)

# Export processed data
outfile <- "anxiety_cbcl.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_anxiety_cbcl, file = outfile)


file <- "RCADSP_GA_TSCORE.xlsx"
file <- file.path(input_dir, file)
df_anxiety_rcads <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value) 

# Export processed data
outfile <- "anxiety_rcads.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_anxiety_rcads, file = outfile)



file <- "SPENCEP_SPENCE_TOT_SCORE; SCAS (Jan 29, 2026 12-16-51 PM).xlsx"
file <- file.path(input_dir, file)
df_anxiety_spencep <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value) 

# Export processed data
outfile <- "anxiety_spencep.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_anxiety_spencep, file = outfile)