# process_ADOS_data.R

setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")

# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Main ------------------------------------------------------------------------

# Paths to ADOS directory
ados_input_dir <- "data/raw/ADOS/"
ados_output_dir <- "data/processed/ADOS/"

# Import ADOS calibrated scores
ados_input_file <- "ADOS_COMPUTED (calibrated) scores_2025-ALL (Dec 9, 2025 3-02-02 PM) (Dec 10, 2025 8-55-45 AM).xlsx"
ados_input_file <- file.path(ados_input_dir, ados_input_file)
df_ados_all <- read_excel(path = ados_input_file, sheet = "Query result")

# Rename columns for ease and subset
cols_new <- c("Score", "Date", "ID", "Code", "Test", "Family", "Category", "Notes")
colnames(df_ados_all) <- cols_new
df_ados_all <- df_ados_all %>% 
  select(ID, Date, Test, Code, Score)

# Filter data for overall total scores
df_ados_totals <- df_ados_all %>% 
  filter(str_detect(Code, "_OT"))

# Convert dates to date format
df_ados_totals <- df_ados_totals %>% 
  mutate(Date = as.Date(Date))

# Some participants have a test date of 1900-01-01 and one participant has
# a date of 1970-01-02. Remove these.
# Earliest date beyond that is in 1996.
df_ados_totals <- df_ados_totals %>% 
  filter(Date > "1980-01-01") 

# Identify participants with multiple scores
ids_multiple <- df_ados_totals %>% 
  group_by(ID) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(ID) %>% 
  unique()

# Subset participants with unique scores
df_ados_unique <- df_ados_totals %>% 
  filter(!(ID %in% ids_multiple)) %>% 
  mutate(Module = Test %>% 
           str_extract("Module [1-4]") %>% 
           str_remove("Module ") %>% 
           as.numeric())

# Subset participants with multiple scores
df_ados_multiple <- df_ados_totals %>% 
  filter(ID %in% ids_multiple)

# Create a variable for ADOS module
df_ados_multiple <- df_ados_multiple %>% 
  mutate(Module = Test %>% 
           str_extract("Module [1-4]") %>% 
           str_remove("Module ") %>% 
           as.numeric())


# 186 participants have multiple scores. How do we pick these?
# Scenarios:
# - Participants have multiple modules on the same date (Use lowest module?)
# - Participants have the same module on different dates
# - Participants have different modules on different dates (e.g. mod 3 in 2007, mod 1 in 2004)
# - Participants may have 0 score on date and non-zero on another (Take the non-zero?)
# df_ados_multiple %>% 
#   arrange(ID, Date) %>% 
#   View()

df_ados_multiple_consolidated <- tibble()
for (i in 1:length(ids_multiple)) {
  
  # Filter for participant i
  df_ados_multiple_i <- df_ados_multiple %>% 
    filter(ID == ids_multiple[i])
  
  # Create a column identifying zero scores
  df_ados_multiple_i <- df_ados_multiple_i %>% 
    mutate(nonzero = Score > 0)
  
  # Test whether all scores are 0
  test_all_zero <- sum(!(df_ados_multiple_i$nonzero)) == nrow(df_ados_multiple_i)
  if (test_all_zero) {
    
    # If all entries are 0, filter for the earliest date
    df_ados_multiple_i <- df_ados_multiple_i %>% 
      filter(Date == min(Date))
    
    # If there are multiple entries at the earliest date... pick one at random?
    # This doesn't seem to actually occur in the dataset
    if (nrow(df_ados_multiple_i) > 1) {
      print(paste(i, ": multiple zeros"))
      stop()
    }
    
  } else {
    
    # If not all entries are zero, focus on non-zero entries
    df_ados_multiple_i <- df_ados_multiple_i %>% 
      filter(nonzero)
    
    # Among non-zero entries, filter for the earliest date
    df_ados_multiple_i <- df_ados_multiple_i %>% 
      filter(Date == min(Date))
    
    # If there are multiple entries at the earliest date... what do we do?
    if (nrow(df_ados_multiple_i) > 1) {
      df_ados_multiple_i <- df_ados_multiple_i[1,]
    } 
  }
  
  # Remove zero score identifier column
  df_ados_multiple_i <- df_ados_multiple_i %>% 
    select(-nonzero)
  
  # Append row to growing data frame
  df_ados_multiple_consolidated <- bind_rows(df_ados_multiple_consolidated,
                                             df_ados_multiple_i)
  
}

df_ados <- bind_rows(df_ados_unique, df_ados_multiple_consolidated)


# Import calibrated scores
ados_css_file <- "ADOSresults.xlsx"
ados_css_file <- file.path(ados_input_dir, ados_css_file)
df_ados_css <- read_excel(path = ados_css_file)
df_ados_css <- df_ados_css %>% 
  select(ID = indexid, Date = testdate, Age = age_at_test, Code = code, Score = value, CSS = css) %>% 
  mutate(Date = as.Date(Date))
  
# Join calibrated scores to full data
df_ados <- df_ados %>% 
  left_join(df_ados_css, by = c("ID", "Date", "Code", "Score")) 

# Export ADOS scores
outfile_ados <- "ADOS.csv"
outfile_ados <- file.path(ados_output_dir, outfile_ados)
write_csv(x = df_ados, file = outfile_ados)
