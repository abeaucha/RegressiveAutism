# process_SRS_data.R

setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")


# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------

import_srs_file <- function(x) {
  select(read_excel(x), ID = indexid, score = numeric_value)
}


# Main ------------------------------------------------------------------------

# Paths to SSP directories
input_dir <- "data/raw/SRS/"
output_dir <- "data/processed/SRS/"

# Data file
files <- c("SRSPARENTREP_Total_score_SRSTOTT.xlsx",
           "SRSPARENTREP.Social Cognition_SRSCGT.xlsx",
           "SRSPARENTREP.Social Communication_SRSCMT.xlsx",
           "SRSPARENTREP.Social Motivation_SRSMTT.xlsx",
           "SRSPARENTREP.SocialAwarness_SRSAWT.xlsx")

# Prepend input directory path
files <- file.path(input_dir, files)

# Variable names
names(files) <- c("SRS_Total", "SRS_SocialCog", "SRS_SocialComm",
                  "SRS_SocialMotiv", "SRS_SocialAware")

# Output file
outfile <- "SRS.csv"
outfile <- file.path(output_dir, outfile)

# Import SRS files and export data frame
files %>% 
  map_dfr(.f = import_srs_file, .id = "code") %>% 
  group_by(ID, code) %>% 
  summarise(score = mean(score), .groups = "drop") %>% 
  pivot_wider(id_cols = "ID", names_from = "code", values_from = "score") %>% 
  write_csv(file = outfile)
