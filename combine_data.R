# combine_data.R

setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")

# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Main ------------------------------------------------------------------------

# Directories
data_dir_raw <- "data/raw/"
data_dir_proc <- "data/processed/"

## Import demographics --------------------------------------------------------

file_demographics <- "demographics.csv"
file_demographics <- file.path(data_dir_proc, file_demographics)
df_demographics <- read_csv(file_demographics, show_col_types = FALSE)


## Import adaptive function scores --------------------------------------------

# Import overall adaptive function scores
file_adaptive <- "adaptive_behaviour_standard_score_2025-10-16T19_30_47.438825Z.xlsx"
file_adaptive <- file.path(data_dir_raw, "AdaptiveFunction", file_adaptive)
df_adaptive <- read_excel(file_adaptive) %>% 
  select(ID = IndexId, AdaptiveScore = `Standard Score`)

# Import social adaptive function scores
file_adaptive_social <- "social_adaptive_function___raw_values_by_test_2025-10-17T18_04_47.576986Z.xlsx"
file_adaptive_social <- file.path(data_dir_raw, "AdaptiveFunction", file_adaptive_social)
df_adaptive_social <- read_excel(file_adaptive_social) %>% 
  select(ID = indexid, SocialAdaptiveScore = numeric_value)


## Import IQ scores -----------------------------------------------------------

# Import full-scale IQ scores
file_IQ_fsiq <- "full_scale_iq_2025-10-16T17_23_31.439311Z.xlsx"
file_IQ_fsiq <- file.path(data_dir_raw, "IQ", file_IQ_fsiq)
df_IQ_fsiq <- read_excel(file_IQ_fsiq) %>% 
  select(ID = IndexId, FSIQ = `Standard Score`)

# Import aggregate full-scale IQ scores
file_IQ_aggregate <- "aggregate_full_scale_iq_standard_score___all_values_by_test_2025-10-09T17_23_41.947473Z.xlsx"
file_IQ_aggregate <- file.path(data_dir_raw, "IQ", file_IQ_aggregate)
df_IQ_aggregate <- read_excel(file_IQ_aggregate) %>% 
  select(ID = indexid, AggregateIQ = numeric_value)

# Combine FSIQ and aggregate IQ, prioritizing FSIQ
df_IQ <- full_join(df_IQ_fsiq, df_IQ_aggregate, by = "ID") %>% 
  mutate(IQ = ifelse(is.na(FSIQ), AggregateIQ, FSIQ)) %>% 
  select(ID, IQ)

# Import global ability composite estimates
file_global_ability <- "global_ability_composite_estimate_2025-10-16T19_05_36.789126Z.xlsx"
file_global_ability <- file.path(data_dir_raw, "IQ", file_global_ability)
df_global_ability <- read_excel(file_global_ability) %>% 
  select(ID = IndexId, GlobalAbilityScore = `Standard Score`)


# Import ADOS scores ----------------------------------------------------------

file_ADOS <- "ADOS.csv"
file_ADOS <- file.path(data_dir_proc, "ADOS", file_ADOS)
df_ADOS <- read_csv(file_ADOS, show_col_types = FALSE) %>% 
  select(ID, ADOS_Total = Score, ADOS_CSS = CSS, 
         ADOS_Age = Age, ADOS_Date = Date)


# Import ADHD scores ----------------------------------------------------------

file_ADHD <- "ADHD.csv"
file_ADHD <- file.path(data_dir_proc, "ADHD", file_ADHD)
df_ADHD <- read_csv(file_ADHD, show_col_types = FALSE) %>% 
  select(ID, ADHD = ADHD_PASS)


# Import anxiety scores -------------------------------------------------------

file_anxiety <- "anxiety.csv"
file_anxiety <- file.path(data_dir_proc, "Anxiety", file_anxiety)
df_anxiety <- read_csv(file_anxiety, show_col_types = FALSE) %>% 
  select(ID, Anxiety = Anxiety_PASS)


# Import seizures scores ------------------------------------------------------

file_seizures <- "seizures.csv"
file_seizures <- file.path(data_dir_proc, "Seizures", file_seizures)
df_seizures <- read_csv(file_seizures, show_col_types = FALSE) %>% 
  select(ID, Seizure = Seizure_PASS, Seizure_Date)


# Import SSP data -------------------------------------------------------------

file_ssp <- "SSP.csv"
file_ssp <- file.path(data_dir_proc, "SSP", file_ssp)
df_ssp <- read_csv(file_ssp, show_col_types = FALSE) 


# Import SRS data -------------------------------------------------------------

file_srs <- "SRS.csv"
file_srs <- file.path(data_dir_proc, "SRS", file_srs)
df_srs <- read_csv(file_srs, show_col_types = FALSE) 


# Import RBS-R data -----------------------------------------------------------
# RBS not combined because there are repeated measures per participant

# file_rbs <- "RBS.csv"
# file_rbs <- file.path(data_dir_proc, "RBS", file_rbs)
# df_rbs <- read_csv(file_rbs, show_col_types = FALSE) 


# Import CCC data -------------------------------------------------------------

file_ccc <- "CCC.csv"
file_ccc <- file.path(data_dir_proc, "Language", file_ccc)
df_ccc <- read_csv(file_ccc, show_col_types = FALSE)


# Combine data ----------------------------------------------------------------

# Join data to ADI-R regression statuses
file_adir <- "ADIR_regression.csv"
outfile <- "regressive_autism_data.csv"

# Import ADI-R data
file_adir <- file.path(data_dir_proc, "ADI-R", file_adir)
df_adir <- read_csv(file_adir, show_col_types = FALSE)

# Join with other data scores
df_adir <- df_adir %>% 
  left_join(df_demographics, by = "ID") %>% 
  left_join(df_adaptive, by = "ID") %>% 
  left_join(df_adaptive_social, by = "ID") %>% 
  left_join(df_IQ, by = "ID") %>% 
  left_join(df_global_ability, by = "ID") %>% 
  left_join(df_ADOS, by = "ID") %>% 
  left_join(df_ADHD, by = "ID") %>% 
  left_join(df_anxiety, by = "ID") %>% 
  left_join(df_seizures, by = "ID") %>% 
  left_join(df_ssp, by = "ID") %>% 
  left_join(df_srs, by = "ID") %>%  
  left_join(df_ccc, by = "ID")

# Export
outfile <- file.path(data_dir_proc, outfile)
write_csv(x = df_adir, file = outfile)
