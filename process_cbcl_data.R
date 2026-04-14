# process_cbcl_data.R


setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")

# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------


# Main ------------------------------------------------------------------------

# Paths to CBCL externalizing directories
input_dir <- "data/raw/Externalizing/"
output_dir <- "data/processed/Externalizing/"

file <- "CBCL618_CB68EPTS externalizing Tscores.xlsx"
file <- file.path(input_dir, file)
df_ep_618 <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value)  

file <- "CBCL1_CBEPTS Externalizing T-score.xlsx"
file <- file.path(input_dir, file)
df_ep_15 <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value) %>% 
  anti_join(df_ep_618, by = "ID")

df_ep <- bind_rows(df_ep_618, df_ep_15) %>% 
  rename(CBCL_EP_TS = score)

# Export processed data
outfile <- "externalizing.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_ep, file = outfile)


# Paths to CBCL internalizing directories
input_dir <- "data/raw/Internalizing/"
output_dir <- "data/processed/Internalizing/"

file <- "CBCL618_CB68IPTS 6-18y old.xlsx"
file <- file.path(input_dir, file)
df_ip_618 <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value)  

file <- "CBCL1_CBIPTS 1-5y old.xlsx"
file <- file.path(input_dir, file)
df_ip_15 <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value) %>% 
  anti_join(df_ip_618, by = "ID")

df_ip <- bind_rows(df_ip_618, df_ip_15) %>% 
  rename(CBCL_IP_TS = score)

# Export processed data
outfile <- "internalizing.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_ip, file = outfile)
