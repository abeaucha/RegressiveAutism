# process_onset_data.R


setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")

# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------


# Main ------------------------------------------------------------------------

# Paths to onset directories
input_dir <- "data/raw/Onset/"
output_dir <- "data/processed/Onset/"

file <- "Age Language RegressionADI_R WPS_2025-12-12T14_14_14.807924Z.xlsx"
file <- file.path(input_dir, file)
df_onset_wps <- read_excel(file) %>% 
  select(ID = indexid, age = numeric_value)

df_onset_wps <- df_onset_wps %>% 
  group_by(ID) %>% 
  summarise(age = min(age)) %>% 
  ungroup()

file <- "Age of main skill regression onset ADI1995 _2025-12-12T14_37_12.773403Z.xlsx"
file <- file.path(input_dir, file)
df_onset_1995 <- read_excel(file) %>% 
  select(ID = indexid, age = numeric_value) %>% 
  anti_join(df_onset_wps, by = "ID")

df_onset_1995 <- df_onset_1995 %>% 
  group_by(ID) %>% 
  summarise(age = min(age)) %>% 
  ungroup()

df_onset <- bind_rows(df_onset_wps, df_onset_1995)

df_onset <- df_onset %>% 
  filter(age > 0)

df_onset <- df_onset %>% 
  rename(Age_Onset = age)

outfile <- "age_of_onset.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_onset, outfile)

