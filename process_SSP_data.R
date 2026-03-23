# process_SSP_data.R

setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")


# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------

import_ssp_file <- function(x) {
    select(read_excel(x), ID = indexid, score = numeric_value)
}


# Main ------------------------------------------------------------------------

# Paths to SSP directories
input_dir <- "data/raw/SSP/"
output_dir <- "data/processed/SSP/"

# Files to import
files <- c("SSP_AUD_FILTER" = "SSP_SSP_AUD_FILTER_RS.xlsx",
          "SSP_LOW_ENRGY_WEAK" = "SSP_SSP_LOW_ENRGY_WEAK_RS.xlsx",
          "SSP_MOVEMENT" = "SSP_SSP_MOVEMENT_RS.xlsx",
          "SSP_TACTILE" = "SSP_SSP_TACTILE_RS.xlsx",
          "SSP_TASTE_SMELL" = "SSP_SSP_TASTE_SMELL_RS.xlsx",
          "SSP_UNDERRESP_SEEKS" = "SSP_SSP_UNDERRESP_SEEKS_RS.xlsx",
          "SSP_VIS_AUD" = "SSP_SSP_VIS_AUD_RS.xlsx",
          "SSP_TOTAL" = "SSP_SSP_TOTAL_RS (Feb 5, 2026 1-53-02 PM).xlsx")

# Prepend input directory path
files <- file.path(input_dir, files)

# Variable names
names(files) <- c("SSP_AUD_FILTER", "SSP_LOW_ENRGY_WEAK", "SSP_MOVEMENT",
                  "SSP_TACTILE", "SSP_TASTE_SMELL", "SSP_UNDERRESP_SEEKS",
                  "SSP_VIS_AUD", "SSP_TOTAL")

# Output file
outfile <- "SSP.csv"
outfile <- file.path(output_dir, outfile)

# Import SSP files and export data frame
files %>% 
  map_dfr(.f = import_ssp_file, .id = "code") %>% 
  pivot_wider(id_cols = "ID", names_from = "code", values_from = "score") %>% 
  write_csv(file = outfile)