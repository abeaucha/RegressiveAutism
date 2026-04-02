
library(tidyverse)
library(readxl)

mssng_file <- "data/MSSNG_DB7_metadata.tsv"
df_mssng <- read.csv(mssng_file, sep = "\t")

df_mssng <- df_mssng %>% 
  mutate(POND_sample = Other.information %>% 
           str_extract("POND_sample=[a-z]+") %>% 
           str_remove("POND_sample="),
         POND_sample = POND_sample == "true") %>% 
  mutate(POND_ID = Other.information %>% 
           str_extract("POND_ID=[0-9]+") %>% 
           str_remove("POND_ID=") %>% 
           as.numeric()) 

df_mssng_pond <- df_mssng %>% 
  filter(POND_sample) 

regression_file <- "data/processed/regressive_autism_data.csv"
df_regression <- read_csv(regression_file, show_col_types = FALSE)


df_mssng_pond_no_pheno <- df_mssng_pond %>% 
  anti_join(df_regression, by = c("Individual.ID" = "ID")) %>% 
  filter(!is.na(POND_ID))  

pond_database <- ""
