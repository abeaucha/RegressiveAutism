# process_data.R


# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------

source("functions.R")


# Main ------------------------------------------------------------------------

# Directories
input_dir <- "data/raw/"
output_dir <- "data/processed/"


## Demographics ---------------------------------------------------------------

file_demographics <- "subjects_with_adir__ados__scq_assessments_2025-12-19T15_41_03.583656Z.xlsx"
file_demographics <- file.path(input_dir, file_demographics)
df_demographics <- process_demographics(file_demographics)


## ADI-R ----------------------------------------------------------------------

list_ADIR_files <- list(
  list(file = "WPS ADI11 Language regression.xlsx",
       questionnaire = "ADI-WPS",
       question = "ADI11",
       measure = "Language",
       positive = 1),
  list(file = "WPS ADI20 other regression.xlsx",
       questionnaire = "ADI-WPS",
       question = "ADI20",
       measure = "Other",
       positive = 2),
  list(file = "ADI1995ADI39E.xlsx",
       questionnaire = "ADI-1995",
       question = "ADI39E",
       measure = "Language",
       positive = 2),
  list(file = "ADI1995 ADI95B.xlsx",
       questionnaire = "ADI-1995",
       question = "ADI95B5",
       measure = "Other",
       positive = 2),
  list(file = "ADI1995ADI95A.xlsx",
       questionnaire = "ADI-1995",
       question = "ADI95A5",
       measure = "Other",
       positive = 2)
)

df_ADIR <- process_ADIR(inputs = list_ADIR_files, 
                        input_dir = file.path(input_dir, "ADI-R"))


## ADOS -----------------------------------------------------------------------

files_ADOS <- c("OTS" = "ADOS_COMPUTED (calibrated) scores_2025-ALL (Dec 9, 2025 3-02-02 PM) (Dec 10, 2025 8-55-45 AM).xlsx",
                "CSS" = "ADOSresults.xlsx")

df_ADOS <- process_ADOS_data(files = files_ADOS, 
                             input_dir = file.path(input_dir, "ADOS"))

df_ADOS <- df_ADOS %>% 
  select(ID, ADOS_OTS = OTS, ADOS_CSS = CSS, 
         ADOS_Age = Age, ADOS_Date = Date)

