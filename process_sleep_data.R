# process_sleep_data.R



# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------


# Main ------------------------------------------------------------------------

# Paths to anxiety directories
input_dir <- "data/raw/Sleep/"
output_dir <- "data/processed/Sleep/"

# Data frame containing files and threshold info
df_files <- tibble(file = c("AGRE_AFFCHILD1_DIFFICULTY_FALLING_ASLEEP.xlsx",
                            "AGRE_AFFCHILD1_INTERUPTED_SLEEP 30of80participants.xlsx",
                            "AGRE_AFFCHILD1_NIGHT_TERRORS.xlsx",
                            "AGRE_AFFCHILD1_SLEEP_DISORDER_AGE_AT_DIAGNOSIS.xlsx",
                            "AGRE_AFFCHILD1_SLEEP_DISORDER_AGE_OF_ONSET.xlsx",
                            "AGRE_AFFCHILD1_SLEEP_DISORDER_COURSE.xlsx",
                            "AGRE_AFFCHILD1_SLEEP_DISORDER_DIAGNOSIS.xlsx",
                            "AGRE_AFFCHILD1_SLEEP_DISORDER_SYMPTOMS.xlsx",
                            "CBCL1_CBVTOT sleep problem total score.xlsx",
                            "CBCL1_CBVTS Sleep problem T-score 65outof410participants.xlsx",
                            "CLINICALINFO_SLEEP& Sorted sleep problems.xlsx",
                            "CSHQ TotalScore 45from63participants.xlsx",
                            "PSQI Total scores 18 from28participants.xlsx",
                            "RCADSP_RCADSP11  sleep problem 44outof 480participants.xlsx"),
                   threshold = c(1, 1, 1, 0, 0, 1, 1, 1, 65, 65, 1, 41, 6, 2),
                   comparison = c("=", "=", "=", ">=", ">=", ">=", "=", "=", ">=", ">=", "=", ">=", ">=", ">="),
                   colname = c("AGRE_DIFF", "AGRE_INT", "AGRE_TERR", "AGRE_AGE_DX",
                               "AGRE_AGE_ONSET", "AGRE_COURSE", "AGRE_DX", "AGRE_SY",
                               "CBCL1", "CBCL618", "CLINICALINFO", "CSHQ", "PSQI", "RCADSP")) %>% 
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
    df_sleep <- df
  } else {
    df_sleep <- full_join(df_sleep, df, by = "ID")
  }
  
}

# Calculate combined anxiety score
df_sleep_combined <- df_sleep %>% 
  column_to_rownames("ID") %>% 
  apply(1, function(x) {any(as.logical(x), na.rm = TRUE)}) %>% 
  enframe(name = "ID", value = "Sleep_PASS")

# Join combined score to individual scores
df_sleep <- df_sleep %>% 
  left_join(df_sleep_combined, by = "ID")

# Export processed data
outfile <- "sleep.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_sleep_combined, file = outfile)
