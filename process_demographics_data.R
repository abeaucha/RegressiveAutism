# process_demographics_data.R



# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Main ------------------------------------------------------------------------

# Paths to ADI-R directories
input_dir <- "data/raw/"
output_dir <- "data/processed/"

input_file <- "subjects_with_adir__ados__scq_assessments_2025-12-19T15_41_03.583656Z.xlsx"
input_file <- file.path(input_dir, input_file)
df <- read_excel(input_file)

df <- df %>% 
  select(ID = indexid, DOB = dob, Sex = sex) %>% 
  mutate(DOB = as.Date(DOB)) %>% 
  distinct()

output_file <- "demographics.csv"
output_file <- file.path(output_dir, output_file)
write_csv(x = df, file = output_file)