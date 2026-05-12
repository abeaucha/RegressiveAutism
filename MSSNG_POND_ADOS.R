library(tidyverse)
library(readxl)

df_new_ids <- read_csv("ADIR_DB7_new.csv")

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

df_regressive_autism <- read_csv("data/processed/regressive_autism_data.csv")

df_regressive_autism_new <- df_regressive_autism %>% 
  semi_join(df_new_ids, by = "ID")

df_mssng_pond_ids <- df_mssng_pond %>% 
  select(ID = Individual.ID, POND_ID)

df_regressive_autism_new <- df_regressive_autism_new %>% 
  left_join(df_mssng_pond_ids, by = "ID")


pond_database <- "data/PND01Database_DATA_2025-12-09_1553.csv"
df_pond <- read_csv(pond_database, show_col_types = FALSE)


# Extract ADI-R regression information and clean up
df_pond_ados <- df_pond %>% 
  select(subject_id, sub_id, site, redcap_event_name, redcap_repeat_instance, adiitdate, adii1date, adii2date, adii3date, adii4date) %>% 
  filter(str_detect(subject_id, "deprecated", negate = TRUE)) %>% 
  filter(!is.na(adiitdate) | !is.na(adii1date) | !is.na(adii2date) | !is.na(adii3date) | !is.na(adii4date)) %>% 
  mutate(site_code = subject_id %>%
           str_remove("PND01_") %>% 
           str_extract("[A-Z]+")) 

# Create a dictionary of site codes
df_site_codes <- df_pond_ados %>% 
  select(site, site_code) %>% 
  distinct() %>% 
  filter(!is.na(site))

# Fill site codes for those missing
df_pond_ados <- df_pond_ados %>% 
  left_join(df_site_codes, by = "site_code") %>% 
  mutate(site = ifelse(is.na(site.x), site.y, site.x)) %>% 
  select(-site.x, -site.y)

# Fix subject IDs 
df_pond_ados <- df_pond_ados %>% 
  mutate(sub_id_suffix = subject_id %>% str_remove("PND01") %>% str_extract("[0-9]+")) %>% 
  mutate(sub_id_new = str_c(site, sub_id_suffix)) %>% 
  mutate(sub_id = ifelse(!is.na(sub_id), sub_id, sub_id_new)) %>% 
  mutate(sub_id = ifelse(str_detect(sub_id, "PND01"), sub_id_new, sub_id)) %>% 
  select(subject_id, redcap_event_name, redcap_repeat_instance, POND_ID = sub_id, contains("adii")) %>% 
  mutate(POND_ID = as.numeric(POND_ID))


df_no_ados_in_mssng <- df_regressive_autism_new %>% 
  filter(is.na(ADOS_OTS)) %>% 
  select(ID, POND_ID, ADOS_OTS) %>% 
  inner_join(df_pond_ados, by = "POND_ID") %>% 
  select(MSSNG_ID = ID, POND_ID, 
         ADOS_Toddler_Date = adiitdate, 
         ADOS_MOD1_DATE = adii1date,
         ADOS_MOD2_DATE = adii2date,
         ADOS_MOD3_DATE = adii3date,
         ADOS_MOD4_DATE = adii4date) %>% 
  arrange(MSSNG_ID)

outfile <- "participants_missing_ADOS.csv"
write_csv(df_no_ados_in_mssng, outfile)
