# process_ADIR_data.R

setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")

# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------

import_adir <- function(x, input_dir = "data/raw/ADI-R") {
  out <- read_excel(file.path(input_dir, x[["file"]])) %>% 
    select(ID = indexid, code, score = numeric_value) %>% 
    mutate(questionnaire = x[["questionnaire"]],
           question = x[["question"]],
           measure = x[["measure"]],
           regressive = score == x[["positive"]]) 
  return(out)
}


# Main ------------------------------------------------------------------------

# Paths to ADI-R directories
adir_input_dir <- "data/raw/ADI-R/"
adir_output_dir <- "data/processed/ADI-R/"

# List of files from which we're pulling regression scores
list_adir_files <- list(
  list(file = "11_languagelossmeasure_values_2025-08-08T18_23_22.684013Z.xlsx",
       questionnaire = "ADI-WPS",
       question = "ADI11",
       measure = "Language",
       positive = 1),
  list(file = "ADIR_20_measure_values_2025-09-05T14_57_43.429564Z.xlsx",
       questionnaire = "ADI-WPS",
       question = "ADI20",
       measure = "Other",
       positive = 2),
  list(file = "39lnguage loss_measure_values_2025-08-08T18_27_27.460797Z.xlsx",
       questionnaire = "ADI-1995",
       question = "ADI39E",
       measure = "Language",
       positive = 2),
  list(file = "ADI 1995 _95_95B5 other loss under 5_measure_values_2025-08-08T18_31_50.351835Z.xlsx",
       questionnaire = "ADI-1995",
       question = "ADI95B5",
       measure = "Other",
       positive = 2),
  list(file = "ADI 1995_95A5_  other loss over 5_2025-09-29T12_48_13.481482Z.xlsx",
       questionnaire = "ADI-1995",
       question = "ADI95A5",
       measure = "Other",
       positive = 2)
)

# Import the data from the various questionnaires
df_adir_all <- map_dfr(.x = list_adir_files, .f = import_adir)

# Get distinct entries
df_adir_all <- df_adir_all %>% 
  distinct()


## Participants with ADI-1995 and ADI-WPS -------------------------------------
#
# Some participants have results from both ADI-1995 and ADI-WPS.
# In these cases, we want to select ADI-1995 since it's earlier and probably
# has better recall.

# Identify participants with entries for both ADI-WPS and ADI-1995
ids_w_multiple_questionnaires <- df_adir_all %>% 
  select(ID, questionnaire) %>% 
  distinct() %>% 
  group_by(ID) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(ID)

# Extract participants with ADI-1995
df_adir_1995 <- bind_rows(df_adir_all %>% 
                            filter(ID %in% ids_w_multiple_questionnaires,
                                   questionnaire == "ADI-1995"),
                          df_adir_all %>% 
                            filter(!(ID %in% ids_w_multiple_questionnaires),
                                   questionnaire == "ADI-1995"))

# Extract participants with ADI-WPS 
df_adir_wps <- df_adir_all %>% 
  filter(!(ID %in% ids_w_multiple_questionnaires),
         questionnaire == "ADI-WPS")


## Participants with ADI-WPS only ---------------------------------------------
#
# Some participants with only ADI-WPS scores have multiple instances of the 
# questionnaire. We want to handle these participants.

# Identify participants with multiple instances of ADI-WPS
ids_wps_multiple <- df_adir_wps %>% 
  group_by(ID, question) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(ID) %>% 
  unique()

# Filter for participants with only one instance of ADI-WPS
df_adir_wps_unique <- df_adir_wps %>% 
  filter(!(ID %in% ids_wps_multiple)) %>% 
  pivot_wider(id_cols = "ID",
              names_from = "measure",
              values_from = "regressive")

# Excel file containing information about participants with multiple ADI-R WPS
# (Curated by Hassan)
file_wps_multiple <- "ADIWPS_multiple.xlsx"
df_adir_wps_multiple <- read_excel(file.path(adir_input_dir, file_wps_multiple))

# Regression statuses for participants with ADI-R WPS
df_adir_wps_bool <- bind_rows(df_adir_wps_unique,
                              df_adir_wps_multiple)


## Participants with ADI-1995 only --------------------------------------------
#
# Some participants with only ADI-1995 scores have multiple instances of the 
# questionnaire. We want to handle these participants.

# Identify participants with multiple instances of ADI-1995
ids_1995_multiple <- df_adir_1995 %>% 
  group_by(ID, question) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(ID) %>% 
  unique()

# Filter for participants with only one instance of ADI-1995
df_adir_1995_unique <- df_adir_1995 %>%
  filter(!(ID %in% ids_1995_multiple)) %>% 
  pivot_wider(id_cols = "ID",
              names_from = "question", 
              values_from = "regressive") %>% 
  mutate(Language = ADI39E,
         Other = case_when(is.na(ADI95B5) & is.na(ADI95A5) ~ NA,
                           is.na(ADI95B5) & !is.na(ADI95A5) ~ ADI95A5,
                           !is.na(ADI95B5) & is.na(ADI95A5) ~ ADI95B5,
                           !is.na(ADI95B5) & !is.na(ADI95A5) ~ ADI95B5 | ADI95A5)) %>% 
  select(ID, Language, Other)

# Excel file containing information about participants with multiple ADI-R 1995
# (Curated by Hassan)
file_1995_multiple <- "ADI1995_multiple.xlsx"
df_adir_1995_multiple <- read_excel(file.path(adir_input_dir, file_1995_multiple))

# Regression statuses for participants with ADI-R 1995
df_adir_1995_bool <- bind_rows(df_adir_1995_unique,
                         df_adir_1995_multiple)


## Combining all ADI-R regression statuses ------------------------------------

# Combine data from ADI-R WPS and 1995
df_regression_bool <- bind_rows(df_adir_wps_bool, df_adir_1995_bool)

# Arrange by participant ID
df_regression_bool <- df_regression_bool %>% 
  arrange(ID)

# Get age at test data
input_file <- "subjects_with_adir__ados__scq_assessments_2025-12-19T15_41_03.583656Z.xlsx"
input_file <- file.path("data/raw/", input_file)
df_adir_age <- read_excel(input_file) %>% 
  filter(`Tests Classification__family` == "ADIR") %>% 
  select(indexid, dob, testdate, age_at_assessment_in_months) %>% 
  filter(!is.na(testdate)) %>% 
  mutate(dob = as.Date(dob),
         testdate = as.Date(testdate)) %>% 
  mutate(ADIR_Age = time_length(interval(dob, testdate), "years")) %>% 
  select(ID = indexid, ADIR_Age) %>% 
  group_by(ID) %>% 
  filter(ADIR_Age == min(ADIR_Age)) %>% 
  ungroup()

df_regression_bool <- df_regression_bool %>% 
  left_join(df_adir_age, by = "ID")

# Export ADI-R regression statuses
outfile_adir <- "ADIR_regression.csv"
outfile_adir <- file.path(adir_output_dir, outfile_adir)
write_csv(x = df_regression_bool, file = outfile_adir)
