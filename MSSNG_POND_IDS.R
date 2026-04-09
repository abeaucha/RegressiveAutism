
library(tidyverse)
library(readxl)

# Import MSSNG metadata information
mssng_file <- "data/MSSNG_DB7_metadata.tsv"
df_mssng <- as_tibble(read.csv(mssng_file, sep = "\t"))

# Extract POND sample information and POND IDs
df_mssng <- df_mssng %>% 
  mutate(POND_sample = Other.information %>% 
           str_extract("POND_sample=[a-z]+") %>% 
           str_remove("POND_sample="),
         POND_sample = POND_sample == "true") %>% 
  mutate(POND_ID = Other.information %>% 
           str_extract("POND_ID=[0-9]+") %>% 
           str_remove("POND_ID=") %>% 
           as.numeric()) 

# Filter for subset of MSSNG that are POND
df_mssng_pond <- df_mssng %>% 
  filter(POND_sample) 

# Import MSSNG ADI-R regression phenotype information
# regression_file <- "data/processed/regressive_autism_data.csv"
regression_file <- "data/processed/ADI-R/ADIR_regression.csv"
df_regression <- read_csv(regression_file, show_col_types = FALSE)
df_regression <- df_regression %>% 
  select(ID, Language, Other)

# Filter MSSNG POND participants with no ADI-R information
df_mssng_pond_no_pheno <- df_mssng_pond %>% 
  anti_join(df_regression, by = c("Individual.ID" = "ID")) 

nrow(df_mssng_pond_no_pheno)

df_mssng_pond_no_pheno <- df_mssng_pond_no_pheno %>% 
  filter(!is.na(POND_ID))  

nrow(df_mssng_pond_no_pheno)


# Import POND phenotyping database
pond_database <- "data/PND01Database_DATA_2025-12-09_1553.csv"
df_pond <- read_csv(pond_database, show_col_types = FALSE)

# Extract ADI-R regression information and clean up
df_pond_adir <- df_pond %>% 
  select(subject_id, sub_id, site, redcap_event_name, redcap_repeat_instance, adi11, adi20) %>% 
  filter(str_detect(subject_id, "deprecated", negate = TRUE)) %>% 
  filter(!is.na(adi11) | !is.na(adi20)) %>% 
  mutate(site_code = subject_id %>%
           str_remove("PND01_") %>% 
           str_extract("[A-Z]+")) 

# Create a dictionary of site codes
df_site_codes <- df_pond_adir %>% 
  select(site, site_code) %>% 
  distinct() %>% 
  filter(!is.na(site))

# Fill site codes for those missing
df_pond_adir <- df_pond_adir %>% 
  left_join(df_site_codes, by = "site_code") %>% 
  mutate(site = ifelse(is.na(site.x), site.y, site.x)) %>% 
  select(-site.x, -site.y)

# Fix subject IDs 
df_pond_adir <- df_pond_adir %>% 
  mutate(sub_id_suffix = subject_id %>% str_remove("PND01") %>% str_extract("[0-9]+")) %>% 
  mutate(sub_id_new = str_c(site, sub_id_suffix)) %>% 
  mutate(sub_id = ifelse(!is.na(sub_id), sub_id, sub_id_new)) %>% 
  mutate(sub_id = ifelse(str_detect(sub_id, "PND01"), sub_id_new, sub_id)) %>% 
  select(subject_id, redcap_event_name, redcap_repeat_instance, sub_id, adi11, adi20)

# Binarize ADI-R regression scores
df_pond_adir <- df_pond_adir %>% 
  mutate(Language = adi11 == 1,
         Other = adi20 == 2) 

# Clean up data frame
df_pond_adir <- df_pond_adir %>% 
  select(POND_ID = sub_id, Language, Other) %>% 
  mutate(POND_ID = as.numeric(POND_ID))

# For the participants with missing ADI-R information, add the information
# from the POND database
df_mssng_pond_no_pheno_regression <- df_mssng_pond_no_pheno %>% 
  select(ID = Individual.ID, POND_ID) %>% 
  left_join(df_pond_adir, by = "POND_ID")


df_regression_all <- bind_rows(df_regression,
                               df_mssng_pond_no_pheno_regression %>% 
                                 select(-POND_ID))

df_regression_all <- df_regression_all %>% 
  filter(!is.na(Language), !is.na(Other))

outfile <- "MSSNG_regressive_autism.csv"
outfile <- file.path("data", outfile)
write_csv(df_regression_all, outfile)


