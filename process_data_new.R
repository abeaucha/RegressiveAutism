# process_data.R


# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions -------------------------------------------------------------------

source("functions.R")


# Main ------------------------------------------------------------------------

# Directories
input_dir <- "data/raw/"
output_dir <- "data/processed/"


## Demographics data ----------------------------------------------------------

message("Importing and processing demographics data...")

file_demographics <- "subjects_with_adir__ados__scq_assessments_2025-12-19T15_41_03.583656Z.xlsx"
file_demographics <- file.path(input_dir, file_demographics)
df_demographics <- process_demographics(file_demographics)


## ADI-R data -----------------------------------------------------------------

message("Importing and processing ADI-R data...")

# List containing files and information
list_ADIR_files <- list(
  list(file = "WPS ADI11 Language regression.xlsx",
       questionnaire = "ADI-WPS",
       question = "ADI11",
       measure = "Language",
       positive = 1),
  list(file = "WPS ADI20 other regression.xlsx",
       questionnaire = "ADI-WPS",
       question = "ADI20",
       measure = "Other",
       positive = 2),
  list(file = "ADI1995ADI39E.xlsx",
       questionnaire = "ADI-1995",
       question = "ADI39E",
       measure = "Language",
       positive = 2),
  list(file = "ADI1995 ADI95B.xlsx",
       questionnaire = "ADI-1995",
       question = "ADI95B5",
       measure = "Other",
       positive = 2),
  list(file = "ADI1995ADI95A.xlsx",
       questionnaire = "ADI-1995",
       question = "ADI95A5",
       measure = "Other",
       positive = 2)
)

# Process ADI-R data
df_ADIR <- process_ADIR(inputs = list_ADIR_files, 
                        input_dir = file.path(input_dir, "ADI-R"))


## Age of regression onset data -----------------------------------------------

# Files containing age of onset data
files_onset <- c("ADIWPS" = "Age Language RegressionADI_R WPS_2025-12-12T14_14_14.807924Z.xlsx",
                 "ADI1995" = "Age of main skill regression onset ADI1995 _2025-12-12T14_37_12.773403Z.xlsx")

df_onset <- process_onset_data(files = files_onset,
                               input_dir = file.path(input_dir, "Onset"))

outfile <- "age_of_onset.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_onset, outfile)


## Adaptive function data -----------------------------------------------------

# Import overall adaptive function scores
file_adaptive <- "adaptive_behaviour_standard_score_2025-10-16T19_30_47.438825Z.xlsx"
file_adaptive <- file.path(input_dir, "AdaptiveFunction", file_adaptive)
df_adaptive <- read_excel(file_adaptive) %>% 
  select(ID = IndexId, AdaptiveScore = `Standard Score`)

# Import social adaptive function scores
file_adaptive_social <- "social_adaptive_function___raw_values_by_test_2025-10-17T18_04_47.576986Z.xlsx"
file_adaptive_social <- file.path(input_dir, "AdaptiveFunction", file_adaptive_social)
df_adaptive_social <- read_excel(file_adaptive_social) %>% 
  select(ID = indexid, SocialAdaptiveScore = numeric_value)

## IQ data --------------------------------------------------------------------

# Import full-scale IQ scores
file_IQ_fsiq <- "full_scale_iq_2025-10-16T17_23_31.439311Z.xlsx"
file_IQ_fsiq <- file.path(input_dir, "IQ", file_IQ_fsiq)
df_IQ_fsiq <- read_excel(file_IQ_fsiq) %>% 
  select(ID = IndexId, FSIQ = `Standard Score`)

# Import aggregate full-scale IQ scores
file_IQ_aggregate <- "aggregate_full_scale_iq_standard_score___all_values_by_test_2025-10-09T17_23_41.947473Z.xlsx"
file_IQ_aggregate <- file.path(input_dir, "IQ", file_IQ_aggregate)
df_IQ_aggregate <- read_excel(file_IQ_aggregate) %>% 
  select(ID = indexid, AggregateIQ = numeric_value)

# Combine FSIQ and aggregate IQ, prioritizing FSIQ
df_IQ <- full_join(df_IQ_fsiq, df_IQ_aggregate, by = "ID") %>% 
  mutate(IQ = ifelse(is.na(FSIQ), AggregateIQ, FSIQ)) %>% 
  select(ID, IQ)

# Import global ability composite estimates
file_global_ability <- "global_ability_composite_estimate_2025-10-16T19_05_36.789126Z.xlsx"
file_global_ability <- file.path(input_dir, "IQ", file_global_ability)
df_global_ability <- read_excel(file_global_ability) %>% 
  select(ID = IndexId, GlobalAbilityScore = `Standard Score`)

## ADOS data ------------------------------------------------------------------

message("Importing and processing ADOS data...")

files_ADOS <- c("OTS" = "ADOS_COMPUTED (calibrated) scores_2025-ALL (Dec 9, 2025 3-02-02 PM) (Dec 10, 2025 8-55-45 AM).xlsx",
                "CSS" = "ADOSresults.xlsx")

df_ADOS <- process_ADOS_data(files = files_ADOS, 
                             input_dir = file.path(input_dir, "ADOS"))

df_ADOS <- df_ADOS %>% 
  select(ID, ADOS_OTS = OTS, ADOS_CSS = CSS, 
         ADOS_Age = Age, ADOS_Date = Date)


## ADHD data ------------------------------------------------------------------

message("Importing and processing ADHD data...")

df_ADHD_files <- tibble(
  file = c("AGRE_AFFCHILD1_ADHD_DIAGNOSIS.xlsx",
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
              "SWAN_DX", "SWAN_I", "SWAN_HI")
) %>% 
  mutate(colname = paste0(colname, "_PASS"))

df_ADHD <- process_ADHD_data(inputs = df_ADHD_files,
                             input_dir = file.path(input_dir, "ADHD"))

df_ADHD <- df_ADHD %>% 
  select(ID, ADHD = ADHD_PASS)


## Anxiety data ---------------------------------------------------------------

message("Importing and processing anxiety data...")

# Data frame containing files and threshold info
df_anxiety_files <- tibble(
  file = c("AGRE_AFFCHILD1_ANXIETY_AGE_AT_DIAGNOSIS.xlsx",
           "AGRE_AFFCHILD1_ANXIETY_SYMPTOMS.xlsx",
           "CBCL1_CB2TS.xlsx",
           "CBCL618_CB682TS; Tscores.xlsx",
           "CCDC_CDCDBOSY.xlsx",
           "CCDC_CDCDPDX.xlsx",
           "CLINICALINFO_ASDANXIET.xlsx",
           "RCADSP_GA_TSCORE.xlsx",
           "SPENCEP_SPENCE_TOT_SCORE; SCAS (Jan 29, 2026 12-16-51 PM).xlsx"),
  threshold = c(0, 1, 65, 65, 1, 1, 1, 65, 65),
  comparison = c(">=", "=", ">=", ">=", "=", "=", "=", ">=", ">="),
  colname = c("AGRE_AGE", "AGRE", "CBCL1", "CBCL618", "CDCDBOSY",
              "CDCDPDX", "ASDANXIETY", "RCADSP", "SPENCEP")
) %>% 
  mutate(colname = paste0(colname, "_PASS"))

# Remove
df_anxiety_files <- df_anxiety_files %>%
  filter(!(colname %in% c("CDCDBOSY", "CDCDPDX")))

# Process anxiety data
df_anxiety <- process_dichotomized_data(inputs = df_anxiety_files,
                                            input_dir = file.path(input_dir, "Anxiety"))

# Subset participant ID and dichotomous anxiety status
df_anxiety <- df_anxiety %>% 
  select(ID, Anxiety = PASS)


### Anxiety continuous CBCL scores --------------------------------------------

file <- "CBCL618_CB682TS; Tscores.xlsx"
file <- file.path(input_dir, "Anxiety", file)
df_anxiety_cbcl_618 <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value)  

file <- "CBCL1_CB2TS.xlsx"
file <- file.path(input_dir, "Anxiety", file)
df_anxiety_cbcl_15 <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value) %>% 
  anti_join(df_anxiety_cbcl_618, by = "ID")

df_anxiety_cbcl <- bind_rows(df_anxiety_cbcl_618, df_anxiety_cbcl_15) %>% 
  rename(CBCL_AP_TS = score)


### Anxiety continuous RCADS scores -------------------------------------------

file <- "RCADSP_GA_TSCORE.xlsx"
file <- file.path(input_dir, "Anxiety", file)
df_anxiety_rcads <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value) 


### Anxiety continuous SPENCE scores ------------------------------------------

file <- "SPENCEP_SPENCE_TOT_SCORE; SCAS (Jan 29, 2026 12-16-51 PM).xlsx"
file <- file.path(input_dir, "Anxiety", file)
df_anxiety_spencep <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value) 


## Depression data ------------------------------------------------------------

message("Importing and processing depression data...")

# Data frame containing files and threshold info
df_depression_files <- tibble(
  file = c("AGRE_AFFCHILD1_DEPRESSION_AGE_AT_DIAGNOSIS.xlsx", 
           "AGRE_AFFCHILD1_DEPRESSION_DIAGNOSIS.xlsx",
           "AGRE_AFFCHILD1_DEPRESSIVE_SYMPTOMS.xlsx",
           "CBCL1_CB1TS 1-5yold.xlsx",
           "CBCL618_CB681TS 6-18y old Affective Problems.xlsx",
           "CCDC_CDCOT2SY Depression Diagnosis.xlsx",
           "RCADSP_D_TSCORE.xlsx"),
  threshold = c(0, 1, 1, 70, 70, 1, 70),
  comparison = c(">=", "=", "=", ">=", ">=", "=", ">="),
  colname = c("AGRE_AGE", "AGRE_DX", "AGRE_SY",
              "CBCL1", "CBCL618", "CCDC","RCADSP")
) %>% 
  mutate(colname = paste0(colname, "_PASS"))

# Process depression data
df_depression <- process_dichotomized_data(inputs = df_depression_files,
                                         input_dir = file.path(input_dir, "Depression"))

# Subset participant ID and dichotomous depression status
df_depression <- df_depression %>% 
  select(ID, Depression = PASS)


### Depression continuous CBCL scores -----------------------------------------

file <- "CBCL618_CB681TS 6-18y old Affective Problems.xlsx"
file <- file.path(input_dir, "Depression", file)
df_depression_cbcl_618 <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value)  

file <- "CBCL1_CB1TS 1-5yold.xlsx"
file <- file.path(input_dir, "Depression", file)
df_depression_cbcl_15 <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value) %>% 
  anti_join(df_depression_cbcl_618, by = "ID")

df_depression_cbcl <- bind_rows(df_depression_cbcl_618, df_depression_cbcl_15) %>% 
  rename(CBCL_AP_TS = score)


### Depression continuous RCADS scores ----------------------------------------

file <- "RCADSP_D_TSCORE.xlsx"
file <- file.path(input_dir, "Depression", file)
df_depression_rcads <- read_excel(file) %>% 
  select(ID = indexid, score = numeric_value) 


## Seizures data --------------------------------------------------------------

message("Importing and processing seizures data...")

# Data frame containing files and threshold info
df_seizures_files <- tibble(
  file = c("ADIWPS-ADI85.xlsx",
           "AGRE_AFFCHILD1_AGE_OF_SEIZURE_ONSET_YEARS.xlsx",
           "AGRE_AFFCHILD1_INTRACTABILITY_OF_SEIZURES.xlsx",
           "AGRE_AFFCHILD1_NUMBER_OF_SEIZURES.xlsx",
           "AGRE_AFFCHILD1_OTHER_SEIZURES.xlsx",
           "AGRE_AFFCHILD1_SEIZR_REQ_TREATMENT_WITH_MEDS.xlsx",
           "AGRE_AFFCHILD1_SEIZURE_TYPE.xlsx",
           "CLINICALINFO_ASDEPILEP.xlsx",
           "CLINICALINFO_ASDSEIZ (FS excluded) (Feb 5, 2026 9-45-09 AM).xlsx"),
  threshold = c(2, 1, 1, 1, 1, 1, 1, 1, 1),
  comparison = c("=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", ">="),
  colname = c("ADIR", "AGRE_AGE", "AGRE_INTRACT", "AGRE_NUM", "AGRE_OTHER",
              "AGRE_MEDS", "AGRE_TYPE", "ASDEPILEP", "ASDSEIZ")
) %>% 
  mutate(colname = paste0(colname, "_PASS"))

# Process seizures data
df_seizures <- process_dichotomized_data(inputs = df_seizures_files,
                                         input_dir = file.path(input_dir, "Seizures")) 

# Import seizure dates data
file <- "mssng sorted epilepsy.xlsx"
file <- file.path(input_dir, "Seizures", file)
df_seizures_dates <- read_excel(file, sheet = "MSSNG co-occuring sorted Epilep") %>% 
  select(ID = indexid, Seizure_Date = testdate) %>% 
  mutate(Seizure_Date = as.Date(Seizure_Date))

# Join seizures data with dates
df_seizures <- df_seizures %>% 
  left_join(df_seizures_dates, by = "ID") %>% 
  select(ID, Seizure = PASS, Seizure_Date)


## Sleep data -----------------------------------------------------------------

message("Importing and processing sleep data...")

# Data frame containing files and threshold info
df_sleep_files <- tibble(
  file = c("AGRE_AFFCHILD1_DIFFICULTY_FALLING_ASLEEP.xlsx",
           "AGRE_AFFCHILD1_INTERUPTED_SLEEP 30of80participants.xlsx",
           "AGRE_AFFCHILD1_NIGHT_TERRORS.xlsx",
           "AGRE_AFFCHILD1_SLEEP_DISORDER_AGE_AT_DIAGNOSIS.xlsx",
           "AGRE_AFFCHILD1_SLEEP_DISORDER_AGE_OF_ONSET.xlsx",
           "AGRE_AFFCHILD1_SLEEP_DISORDER_COURSE.xlsx",
           "AGRE_AFFCHILD1_SLEEP_DISORDER_DIAGNOSIS.xlsx",
           "AGRE_AFFCHILD1_SLEEP_DISORDER_SYMPTOMS.xlsx",
           "CBCL1_CBVTOT sleep problem total score.xlsx",
           "CBCL1_CBVTS Sleep problem T-score 65outof410participants.xlsx",
           "CLINICALINFO_SLEEP& Sorted sleep problems.xlsx",
           "CSHQ TotalScore 45from63participants.xlsx",
           "PSQI Total scores 18 from28participants.xlsx",
           "RCADSP_RCADSP11  sleep problem 44outof 480participants.xlsx"),
  threshold = c(1, 1, 1, 0, 0, 1, 1, 1, 65, 65, 1, 41, 6, 2),
  comparison = c("=", "=", "=", ">=", ">=", ">=", "=", "=", ">=", ">=", "=", ">=", ">=", ">="),
  colname = c("AGRE_DIFF", "AGRE_INT", "AGRE_TERR", "AGRE_AGE_DX",
              "AGRE_AGE_ONSET", "AGRE_COURSE", "AGRE_DX", "AGRE_SY",
              "CBCL1", "CBCL618", "CLINICALINFO", "CSHQ", "PSQI", "RCADSP")
) %>% 
  mutate(colname = paste0(colname, "_PASS"))

# Process sleep data
df_sleep <- process_dichotomized_data(inputs = df_sleep_files,
                                      input_dir = file.path(input_dir, "Sleep"))  

# Subset participant ID and dichotomous sleep status
df_sleep <- df_sleep %>% 
  select(ID, Sleep = PASS)


## Prematurity data -----------------------------------------------------------

message("Importing and processing prematurity data...")

files_prematurity <- c("prematurity" = "CLINICALINFO_ASDPREMAT.xlsx",
                       "gestational" = "Gestational age at delivery.xlsx")

df_prematurity <- process_prematurity_data(files = files_prematurity)

df_prematurity <- df_prematurity %>% 
  select(ID, Prematurity = PASS)


## SSP sensory data -----------------------------------------------------------

message("Importing and processing SSP data...")

# SSP files to import
files_SSP <- c("SSP_AUD_FILTER" = "SSP_SSP_AUD_FILTER_RS.xlsx",
               "SSP_LOW_ENRGY_WEAK" = "SSP_SSP_LOW_ENRGY_WEAK_RS.xlsx",
               "SSP_MOVEMENT" = "SSP_SSP_MOVEMENT_RS.xlsx",
               "SSP_TACTILE" = "SSP_SSP_TACTILE_RS.xlsx",
               "SSP_TASTE_SMELL" = "SSP_SSP_TASTE_SMELL_RS.xlsx",
               "SSP_UNDERRESP_SEEKS" = "SSP_SSP_UNDERRESP_SEEKS_RS.xlsx",
               "SSP_VIS_AUD" = "SSP_SSP_VIS_AUD_RS.xlsx",
               "SSP_TOTAL" = "SSP_SSP_TOTAL_RS (Feb 5, 2026 1-53-02 PM).xlsx")

# Process SSP files
df_SSP <- process_subdomain_data(files = files_SSP, 
                                 input_dir = file.path(input_dir, "SSP")) 


## SRS socialization data -----------------------------------------------------

message("Importing and processing SRS data...")

# SRS files to import
files_SRS <- c("SRS_Total" = "SRSPARENTREP_Total_score_SRSTOTT.xlsx",
               "SRS_SocialCog" = "SRSPARENTREP.Social Cognition_SRSCGT.xlsx",
               "SRS_SocialComm" = "SRSPARENTREP.Social Communication_SRSCMT.xlsx",
               "SRS_SocialMotiv" = "SRSPARENTREP.Social Motivation_SRSMTT.xlsx",
               "SRS_SocialAware" = "SRSPARENTREP.SocialAwarness_SRSAWT.xlsx")

# Process SRS files
df_SRS <- process_subdomain_data(files = files_SRS, 
                                 input_dir = file.path(input_dir, "SRS")) 


## CCC language data ----------------------------------------------------------

message("Importing and processing CCC data...")

files_CCC <- c("CCC2 AS Speech-Scaled Score.xlsx",
               "CCC2CHILD_CCCBSCAL Syntax Scaled Score.xlsx",
               "CCC2CHILD_CCCCSCAL SemanticScaledScore CS.xlsx",
               "CCC2CHILD_CCCDSCAL CoherenceScaledScore DS.xlsx",
               "CCC2CHILD_CCCESCAL InappInitiationScaledScore ES.xlsx",
               "CCC2CHILD_CCCFSCAL StereotypedScaledScore FS.xlsx",
               "CCC2CHILD_CCCGSCAL UseofContextScaledScore GS.xlsx",
               "CCC2CHILD_CCCHSCAL NonVerbalCommScaledScore HS.xlsx",
               "CCC2CHILD_CCCSIDC Social Interaction Deviance Composite.xlsx")

names(files_CCC) <- c("Speech", "Syntax", "Semantics", "Coherence", "Inapp_Init",
                      "Stereotyped", "Context", "NonVerbalComm", "SIDC")
names(files_CCC) <- paste0("CCC_", names(files_CCC))

df_CCC <- process_subdomain_data(files = files_CCC,
                                 input_dir = file.path(input_dir, "Language"))


## RBS repetitive behaviour data ----------------------------------------------

message("Importing and processing RBS data...")

# Files to import
files_RBS <- c("RBSR_RBSALLT Overall Score.xlsx",
               "RBSR_RBSCPT Compulsive Behavior Tot Score.xlsx",
               "RBSR_RBSRST Restricted Behavior Tot Score.xlsx",
               "RBSR_RBSRTT Ritual Behavior Tot Score.xlsx",
               "RBSR_RBSSIT Self Injurious Behavior Tot Score.xlsx",
               "RBSR_RBSSMT  Sameness Behavior Tot Scor.xlsx",
               "RBSR_RBSSTT Sterotyped Tot Score.xlsx")

# RBS sub-domain codes
codes <- c("ALLT", "CPT", "RST", "RTT", "SIT", "SMT", "STT")
codes <- paste0("RBSR_RBS", codes)

# Add codes to file names
names(files_RBS) <- codes

# Process RBS data
df_RBS <- process_RBS_data(files = files_RBS, 
                           input_dir = file.path(input_dir, "RBS-R"))

# Export CSV
outfile <- "RBS.csv"
outfile <- file.path(output_dir, outfile)
write_csv(df_RBS, outfile)


## CBCL externalizing and internalizing data ----------------------------------

message("Importing and processing CBCL data...")

# Process CBCL data
list_cbcl <- process_CBCL_data(input_dir = file.path(input_dir, "CBCL"))

# Export CBCL externalizing data
outfile <- "CBCL_Externalizing.csv"
outfile <- file.path(output_dir, outfile)
write_csv(list_cbcl[["EP"]], file = outfile)

# Export CBCL internalizing data
outfile <- "CBCL_Internalizing.csv"
outfile <- file.path(output_dir, outfile)
write_csv(list_cbcl[["IP"]], file = outfile)


## Combine data ---------------------------------------------------------------

# Join ADI-R data with other measures
df_ADIR <- df_ADIR %>% 
  left_join(df_demographics, by = "ID") %>% 
  left_join(df_adaptive, by = "ID") %>% 
  left_join(df_adaptive_social, by = "ID") %>% 
  left_join(df_IQ, by = "ID") %>% 
  left_join(df_global_ability, by = "ID") %>% 
  left_join(df_ADOS, by = "ID") %>% 
  left_join(df_ADHD, by = "ID") %>% 
  left_join(df_anxiety, by = "ID") %>% 
  left_join(df_depression, by = "ID") %>% 
  left_join(df_seizures, by = "ID") %>% 
  left_join(df_sleep, by = "ID") %>% 
  left_join(df_prematurity, by = "ID") %>% 
  left_join(df_SSP, by = "ID") %>% 
  left_join(df_SRS, by = "ID") %>%  
  left_join(df_CCC, by = "ID")

# Export
outfile <- outfile <- "regressive_autism_data_new.csv"
outfile <- file.path(output_dir, outfile)
write_csv(x = df_ADIR, file = outfile)

