library(tidyverse)
library(readxl)

# ADHD

file <- "data/raw/ADHD/mssng ADHD list.xlsx"
df_ADHD_mssng <- read_excel(file, sheet = "mssng ", col_names = FALSE)
colnames(df_ADHD_mssng) <- c("ID", "ADHD")
df_ADHD_mssng %>% nrow()
df_ADHD_mssng %>% distinct() %>% nrow()
 
file <- "data/raw/ADHD/mssng ADHD list.xlsx"
df_ADHD_Hassan <- read_excel(file, sheet = "Hassan sorted", col_names = FALSE)
colnames(df_ADHD_Hassan) <- c("code", "ID", "numeric_value", "text_value", "raw_value", "type")
nrow(df_ADHD_Hassan)
df_ADHD_Hassan %>% nrow()
df_ADHD_Hassan %>% distinct() %>% nrow()

sum(!(df_ADHD_Hassan$ID %in% df_ADHD_mssng$ID))
sum(!(df_ADHD_mssng$ID %in% df_ADHD_Hassan$ID))

file <- "data/processed/ADHD/ADHD.csv"
df_ADHD_Antoine <- read_csv(file)
df_ADHD_Antoine %>% filter(ADHD_PASS) %>% nrow()
df_ADHD_Antoine %>% filter(ADHD_PASS) %>% distinct() %>% nrow()


# Anxiety

file <- "data/raw/Anxiety/MSSNG provided coccur_cond_Anxiety.xlsx"
df_anxiety_mssng <- read_excel(file, sheet = "Anxiety sorted", col_names = FALSE)
colnames(df_anxiety_mssng) <- c("ID", "Anxiety")
df_anxiety_mssng %>% nrow()
df_anxiety_mssng %>% distinct() %>% nrow()

file <- "data/raw/Anxiety/Hassan All Anxiety sorted.xlsx"
df_anxiety_Hassan <- read_excel(file, col_names = TRUE)
df_anxiety_Hassan %>% nrow()
df_anxiety_Hassan %>% distinct() %>% nrow()

sum(!(df_anxiety_Hassan$indexid %in% df_anxiety_mssng$ID))
sum(!(df_anxiety_mssng$ID %in% df_anxiety_Hassan$indexid))


file <- "data/processed/Anxiety/anxiety.csv"
df_anxiety_Antoine <- read_csv(file)
df_anxiety_Antoine %>% filter(Anxiety_PASS) %>% nrow()
df_anxiety_Antoine %>% filter(Anxiety_PASS) %>% distinct() %>% nrow()

