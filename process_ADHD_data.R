# process_ADHD_data.R


setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")

# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------


# Main ------------------------------------------------------------------------

# Paths to ADHD directories
input_dir <- "data/raw/ADHD/"
output_dir <- "data/processed/ADHD/"

# Data frame containing files and threshold info
df_files <- tibble(file = c("AGRE_AFFCHILD1_ADHD_DIAGNOSIS.xlsx",
                            "CBCL1_CB4RNG.xlsx",
                            "CBCL618_CB684RNG.xlsx",
                            "CCDC_CDCAUTSY.xlsx",
                            "CCDC_CDCCDDX.xlsx",
                            "CCDC_CDCCOMDX.xlsx",
                            "CLINICALINFO_ASDADHD.xlsx",
                            "CONNERSREVISED_CONHT.xlsx",
                            "SWANSCALE_ADHD_DX.xlsx",
                            "SWANRATINGSCALE_ADHD_I_SUB.xlsx",
                            "SWANRATINGSCALE_ADHD_HI_SUB.xlsx"),
                   threshold = c(1, 2, 2, 1, 1, 1, 1, 65, 1, 6, 6),
                   comparison = c("=", "=", "=", "=", "=", "=", "=", ">=", "=", ">=", ">="),
                   colname = c("AGRE", "CBCL1", "CBCL618", "CDCAUTSY",
                               "CDCCDDX", "CDCCOMDX", "ASDADHD", "CONNERS",
                               "SWAN_DX", "SWAN_I", "SWAN_HI")) %>% 
  mutate(colname = paste0(colname, "_PASS"))

# Remove 
# df_files <- df_files %>% 
#   filter(!(colname %in% c("CDCAUTSY_PASS", "CDCCDDX_PASS")))

for (i in 1:nrow(df_files)) {

  file <- df_files[[i, "file"]]  
  file <- file.path(input_dir, file)
  df <- read_excel(file) %>% 
    select(ID = indexid, score = numeric_value) %>% 
    filter(!is.na(ID), !is.na(score)) %>% 
    group_by(ID) %>% 
    mutate(score_max = max(score, na.rm = TRUE)) %>% 
    ungroup() 
    
  if (df_files[[i, "comparison"]] == "=") {
    df <- df %>% 
      mutate(PASS = score_max == df_files[[i, "threshold"]]) 
  } else if (df_files[[i, "comparison"]] == ">=") {
    df <- df %>% 
      mutate(PASS = score_max >= df_files[[i, "threshold"]]) 
  } else {
    stop()
  }

  df <- df %>%     
    select(ID, PASS) %>% 
    distinct()
  
  colnames(df)[2] <- df_files[[i, "colname"]]
  
  if (i == 1) {
    df_ADHD <- df
  } else {
    df_ADHD <- full_join(df_ADHD, df, by = "ID")
  }
    
}

# Combine SWAN I and HI sub-domains
df_ADHD <- df_ADHD %>% 
  mutate(SWAN_I_HI_PASS = SWAN_I_PASS & SWAN_HI_PASS) %>% 
  select(-SWAN_I_PASS, -SWAN_HI_PASS)

# Calculate combined ADHD score
df_ADHD_combined <- df_ADHD %>% 
  column_to_rownames("ID") %>% 
  apply(1, function(x) {any(as.logical(x), na.rm = TRUE)}) %>% 
  enframe(name = "ID", value = "ADHD_PASS")

# Join combined score to individual scores
df_ADHD <- df_ADHD %>% 
  left_join(df_ADHD_combined, by = "ID")

# Export processed data
outfile <- "ADHD.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_ADHD_combined, file = outfile)

df_ADHD_combined %>% 
  group_by(ADHD_PASS) %>% 
  count()

# Note that false here doesn't mean they don't have ADHD. 

# No participants have all variables, so can't define a proper negative case
# mat_ADHD <- as.matrix(df_ADHD)
# mat_ADHD_na <- is.na(mat_ADHD)
# which(rowSums(mat_ADHD_na) == 0)

