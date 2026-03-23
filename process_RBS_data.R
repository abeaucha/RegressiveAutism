# process_RBS_data.R

setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")


# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------

import_rbs_file <- function(x) {
  # select(read_excel(x), ID = indexid, score = numeric_value)
  select(read_excel(x), code, ID = indexid, score = numeric_value)
}


# Main ------------------------------------------------------------------------


# Paths to SSP directories
input_dir <- "data/raw/RBS-R/"
output_dir <- "data/processed/RBS-R/"

# Files to import
files <- c("RBSR_RBSALLT Overall Score.xlsx",
           "RBSR_RBSCPT Compulsive Behavior Tot Score.xlsx",
           "RBSR_RBSRST Restricted Behavior Tot Score.xlsx",
           "RBSR_RBSRTT Ritual Behavior Tot Score.xlsx",
           "RBSR_RBSSIT Self Injurious Behavior Tot Score.xlsx",
           "RBSR_RBSSMT  Sameness Behavior Tot Scor.xlsx",
           "RBSR_RBSSTT Sterotyped Tot Score.xlsx")

# Prepend input directory path
files <- file.path(input_dir, files)

names(files) <- c("Overall", "Comp_Behv", "Rest_Behv", "Rit_Behv", 
                  "Self_Inj_Behav", "Same_Behv", "Stereo_Behv")
names(files) <- paste0("RBS_", names(files))


# Output file
outfile <- "RBS.csv"
outfile <- file.path(output_dir, outfile)

# Import SSP files and export data frame
df_rbs <- files %>% 
  map_dfr(.f = import_rbs_file, .id = "code_new")

write_csv(df_rbs, file = outfile)

df_ids_w_multiple <- df_rbs %>% 
  group_by(ID, code) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  distinct() %>% 
  select(ID, code)

df_rbs_unique <- df_rbs %>% 
  anti_join(df_ids_w_multiple, by = "ID")

df_rbs_unique<- df_rbs_unique %>% 
  group_by(code, ID) %>% 
  mutate(key = 1:n()) %>% 
  ungroup()

ids_unique <- df_rbs_unique %>% 
  pull(ID) %>% 
  unique()

df_rbs_multiple <- df_rbs %>% 
  semi_join(df_ids_w_multiple, by = c("ID", "code")) %>% 
  arrange(code, ID, score)


file <- "data/raw/date of tests.xlsx"
df_tests <- read_excel(file)
df_tests <- df_tests %>% 
  select(code = Code, ID = `Index ID`, date = `Test Date`, score = `Numeric Value`)

df_tests %>% 
  select(code, ID, date, score) %>% 
  filter(code == "RBSR_RBSAGE") %>% 
  View()

df_rbs_multiple <- df_rbs_multiple %>% 
  left_join(df_tests, by = c("code", "ID", "score"))

ids_multiple <- df_rbs_multiple %>% 
  pull(ID) %>% 
  unique()

df_rbs_multiple %>% 
  filter(!is.na(date)) %>% 
  group_by(ID, date) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  View()

id <- "2-1089-004"
tmp_id <- df_rbs_multiple %>% 
  filter(ID == id) 
tmp_id
tmp_id_split <- split(x = tmp_id, f = tmp_id$code)
tmp_id_split

list_tmp_id_scores <- map(tmp_id_split, .f = function(x) {x$score})

df_grid <- as_tibble(expand.grid(list_tmp_id_scores))

df_grid %>%
  mutate(total = rowSums(across(RBSR_RBSCPT:RBSR_RBSSTT), na.rm = TRUE)) %>% 
  filter(total == RBSR_RBSALLT)

df_rbs %>% 
  filter(code == "RBSR_RBSALLT", ID == "2-1213-003")

df_rbs %>% 
  filter(ID == "2-1213-003")

df_tests %>% 
  filter(code == "RBSR_RBSALLT", ID == "2-1213-003")

df_rbs_multiple 

outfile <- "RBS_IDs_multiple.csv"
outfile <- file.path(output_dir, outfile)

df_ids_w_multiple <- df_rbs %>% 
  group_by(ID, code) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  select(ID) %>% 
  distinct() %>% 
  arrange()

df_rbs %>% filter(str_detect(ID, "AU"))

write_csv(df_ids_w_multiple, outfile)

tmp <- import_rbs_file(files[1])
tmp %>% 
  group_by(ID) %>% 
  count() %>% 
  filter(n > 1)

tmp %>% 
  filter(ID == "1-0004-003")

tmp <- df_rbs %>% 
  filter(code == "RBS_Stereo_Behv") %>% 
  distinct() 

vars <- names(files)
vars <- vars[2:length(vars)]
ids_multiple <- c()
for (var in vars) {
  
  tmp <- df_rbs %>% 
    filter(code == var) %>% 
    distinct()
  
  ids_multiple_var <- tmp%>% 
    group_by(ID) %>% 
    count() %>% 
    filter(n > 1) %>% 
    pull(ID)
  
  print(var)
  print(length(ids_multiple_var))
  
  ids_multiple <- c(ids_multiple, ids_multiple_var)
}

ids_mult_tab <- table(ids_multiple)
sum(ids_mult_tab == 6)

ids_multiple <- tmp %>% 
  group_by(ID) %>% 
  count() %>% 
  filter(n > 1) %>% 
  pull(ID)

length(ids_multiple)

#%>% 
 # pivot_wider(id_cols = "ID", names_from = "code", values_from = "score") 

#%>% 
  # write_csv(file = outfile)
