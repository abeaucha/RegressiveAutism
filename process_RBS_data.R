# process_RBS_data.R

setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")


# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------

import_rbs_file <- function(x) {
  select(read_excel(x), code, ID = indexid, score = numeric_value)
}


# Main ------------------------------------------------------------------------

# Paths to SSP directories
input_dir <- "data/raw/RBS-R/"
output_dir <- "data/processed/RBS-R/"

# Files to import
files <- c("RBSR_RBSALLT Overall Score.xlsx",
           "RBSR_RBSCPT Compulsive Behavior Tot Score.xlsx",
           "RBSR_RBSRST Restricted Behavior Tot Score.xlsx",
           "RBSR_RBSRTT Ritual Behavior Tot Score.xlsx",
           "RBSR_RBSSIT Self Injurious Behavior Tot Score.xlsx",
           "RBSR_RBSSMT  Sameness Behavior Tot Scor.xlsx",
           "RBSR_RBSSTT Sterotyped Tot Score.xlsx")

# Prepend input directory path
files <- file.path(input_dir, files)

# Import RBS files
df_rbs_init <- files %>% 
  map_dfr(.f = import_rbs_file)

# Identify participants with multiple data
ids_w_multiple <- df_rbs_init %>% 
  group_by(ID, code) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  distinct() %>% 
  pull(ID) %>% 
  unique()

# Subset participants without multiple data
df_rbs_unique <- df_rbs_init %>% 
  filter(!(ID %in% ids_w_multiple)) %>% 
  mutate(KEY = 1)

# For participants with multiple data, spreadsheets have been downloaded
# separately
# Directories to these participants
dirs_multiple <- list.files(file.path(input_dir, "IDS_multiple"))

# RBS sub-domain codes
codes <- c("ALLT", "CPT", "RST", "RTT", "SIT", "SMT", "STT")
codes <- paste0("RBSR_RBS", codes)

# IDs to omit (issues with spreadsheets)
ids_omit <- c("1-0344-003", "1-0458-004", "2-1359-003")
dirs_multiple <- dirs_multiple[!(dirs_multiple %in% ids_omit)]

# Iterate over participants with multiple data
for (i in 1:length(dirs_multiple)) {
  
  # Import participant data
  path <- file.path(input_dir, "IDs_multiple", dirs_multiple[i])
  file <- list.files(path)
  file <- file.path(path, file)
  df <- read_excel(file) 
  
  # Filter for RBS data
  df <- df %>% 
    filter(code %in% codes) %>% 
    select(ID = indexid, code, Date = testdate, score = numeric_value) %>% 
    mutate(Date = as.Date(Date))
  
  # Build data frame
  if (i == 1) {
    df_rbs_multiple <- df
  } else {
    df_rbs_multiple <- bind_rows(df_rbs_multiple, df)
  }
  
}

# Create a key for each participant to track 
df_rbs_multiple <- df_rbs_multiple %>% 
  arrange(ID, code, Date) %>% 
  group_by(ID, code) %>% 
  mutate(KEY = 1:n()) %>% 
  ungroup()
  
# Convert multiple data to wide format
df_rbs_multiple_wide <- df_rbs_multiple %>% 
  pivot_wider(id_cols = c("ID", "Date", "KEY"), names_from = "code", values_from = "score")

# Convert unique data to wide format
df_rbs_unique_wide <- df_rbs_unique %>% 
  pivot_wider(id_cols = c("ID", "KEY"), names_from = "code", values_from = "score") %>% 
  mutate(Date = NA)

# Bind unique and multiple data
df_rbs <- bind_rows(df_rbs_unique_wide, df_rbs_multiple_wide)

# Export CSV
outfile <- "RBS.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_rbs, outfile)