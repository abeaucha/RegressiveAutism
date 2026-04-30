# process_sleep_data.R



# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------


# Main ------------------------------------------------------------------------

# Paths to anxiety directories
input_dir <- "data/raw/Prematurity/"
output_dir <- "data/processed/Prematurity/"

file <- "CLINICALINFO_ASDPREMAT.xlsx"
file <- file.path(input_dir, file)
df_prematurity <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value) %>% 
  filter(!is.na(ID), !is.na(score)) %>% 
  group_by(ID) %>% 
  mutate(score_max = max(score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(CLINICALINFO_PASS = score_max == 1) %>% 
  select(ID, CLINICALINFO_PASS) %>% 
  distinct()

file <- "Gestational age at delivery.xlsx"
file <- file.path(input_dir, file)
df_gestational <- read_excel(file) %>% 
  select(ID = indexid, age = Fact__raw_value) %>% 
  mutate(age = as.numeric(age)) %>% 
  filter(age != -1)  %>% 
  mutate(GEST_PASS = age <= 36) %>% 
  select(ID, GEST_PASS)

df_prematurity <- full_join(df_prematurity, df_gestational, by = "ID")

# Calculate combined prematurity score
df_prematurity_combined <- df_prematurity %>% 
  column_to_rownames("ID") %>% 
  apply(1, function(x) {any(as.logical(x), na.rm = TRUE)}) %>% 
  enframe(name = "ID", value = "Prematurity_PASS")

# Join combined score to individual scores
df_prematurity <- df_prematurity %>% 
  left_join(df_prematurity_combined, by = "ID")

# Export processed data
outfile <- "prematurity.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_prematurity_combined, file = outfile)
