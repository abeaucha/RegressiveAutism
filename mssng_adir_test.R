library(tidyverse)
library(readxl)

mssng_metadata <- read_tsv("data/MSSNG_DB7_metadata.tsv")

adir <- read_csv("data/processed/ADI-R/ADIR_regression_bool.csv")

View(mssng_metadata)

sum(!(adir$ID %in% mssng_metadata$`Sample ID`))

adir_not_mssng <- adir %>% 
  anti_join(mssng_metadata, by = c("ID" = "Individual ID"))

mssng_metadata %>% 
  filter(str_detect(`Other information`, "POND_sample=true"))


adir_not_mssng

mssng_metadata %>% 
  filter(str_detect(`Sample ID`, "AU"))

df_adir_all %>% 
  semi_join(adir_not_mssng, by = "ID") %>% 
  group_by(code) %>% 
  count()

dim(mssng_metadata)

mssng_metadata$
