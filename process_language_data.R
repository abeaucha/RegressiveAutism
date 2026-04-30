# process_language_data.R


# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------

import_ccc_file <- function(x) {
  select(read_excel(x), ID = indexid, score = numeric_value)
}


# Main ------------------------------------------------------------------------

# Paths to SSP directories
input_dir <- "data/raw/Language/"
output_dir <- "data/processed/Language/"

# Files to import
files <- c("CCC2 AS Speech-Scaled Score.xlsx",
           "CCC2CHILD_CCCBSCAL Syntax Scaled Score.xlsx",
           "CCC2CHILD_CCCCSCAL SemanticScaledScore CS.xlsx",
           "CCC2CHILD_CCCDSCAL CoherenceScaledScore DS.xlsx",
           "CCC2CHILD_CCCESCAL InappInitiationScaledScore ES.xlsx",
           "CCC2CHILD_CCCFSCAL StereotypedScaledScore FS.xlsx",
           "CCC2CHILD_CCCGSCAL UseofContextScaledScore GS.xlsx",
           "CCC2CHILD_CCCHSCAL NonVerbalCommScaledScore HS.xlsx",
           "CCC2CHILD_CCCSIDC Social Interaction Deviance Composite.xlsx")

# Prepend input directory path
files <- file.path(input_dir, files)

names(files) <- c("Speech", "Syntax", "Semantics", "Coherence", "Inapp_Init",
                  "Stereotyped", "Context", "NonVerbalComm", "SIDC")
names(files) <- paste0("CCC_", names(files))


# Output file
outfile <- "CCC.csv"
outfile <- file.path(output_dir, outfile)

files %>%
  map_dfr(.f = import_ccc_file, .id = "code") %>% 
  distinct() %>% 
  group_by(ID, code) %>% 
  summarise(score = mean(score), .groups = "drop") %>% 
  pivot_wider(id_cols = "ID", names_from = "code", values_from = "score") %>% 
  write_csv(file = outfile)

