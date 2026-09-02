
# Packages --------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))


# Functions ------------------------------------------------------------------- 

#' Process demographics data
#'
#' @param file (character scalar) File (.xlsx) containing demographics DOB and 
#' sex information
#'
#' @returns (tibble)
process_demographics <- function(file) {
  
  df <- read_excel(file) %>% 
    select(ID = indexid, DOB = dob, Sex = sex) %>% 
    mutate(DOB = as.Date(DOB)) %>% 
    distinct()
  
  return(df)
}


#' Import ADI-R files
#'
#' @param file (character scalar) 
#' @param questionnaire (character scalar)
#' @param question (character scalar)
#' @param measure (character scalar)
#' @param positive (character scalar)
#'
#' @returns (tibble) 
import_ADIR <- function(file, questionnaire, question, measure, positive) {
  out <- read_excel(file) %>% 
    select(ID = indexid, code, score = numeric_value) %>% 
    mutate(questionnaire = questionnaire,
           question = question,
           measure = measure,
           regressive = score == positive) 
  return(out)
}


#' Process ADI-R data
#'
#' @param inputs (list)
#' @param input_dir (character scalar) Path to directory containing ADI-R input
#' files
#'
#' @returns (tibble) Data frame with language regression status, other skill
#' regression status, and age at ADI-R 
#' 
process_ADIR <- function(inputs, input_dir = "data/raw/ADI-R") {
  
  # Import the data from the various questionnaires
  list_ADIR <- vector(mode = "list", length = length(inputs))
  for (i in 1:length(list_ADIR)) {
    list_ADIR[[i]] <- import_ADIR(
      file = file.path(input_dir, inputs[[i]][["file"]]),
      questionnaire = inputs[[i]][["questionnaire"]],
      question = inputs[[i]][["question"]],
      measure = inputs[[i]][["measure"]],
      positive = inputs[[i]][["positive"]]
    )
  }
  
  # Combine all data in a data frame
  df_adir_all <- bind_rows(list_ADIR)
  
  # Get distinct entries
  df_adir_all <- distinct(df_adir_all)
  
  
  # Participants with ADI-1995 and ADI-WPS ------------------------------------
  #
  # Some participants have results from both ADI-1995 and ADI-WPS.
  # In these cases, we want to select ADI-1995 since it's earlier and probably
  # has better recall.
  
  # Identify participants with entries for both ADI-WPS and ADI-1995
  ids_w_multiple_questionnaires <- df_adir_all %>% 
    select(ID, questionnaire) %>% 
    distinct() %>% 
    group_by(ID) %>% 
    count() %>% 
    filter(n > 1) %>% 
    pull(ID)
  
  # Extract participants with ADI-1995
  df_adir_1995 <- bind_rows(df_adir_all %>% 
                              filter(ID %in% ids_w_multiple_questionnaires,
                                     questionnaire == "ADI-1995"),
                            df_adir_all %>% 
                              filter(!(ID %in% ids_w_multiple_questionnaires),
                                     questionnaire == "ADI-1995"))
  
  # Extract participants with ADI-WPS 
  df_adir_wps <- df_adir_all %>% 
    filter(!(ID %in% ids_w_multiple_questionnaires),
           questionnaire == "ADI-WPS")
  
  
  # Participants with ADI-WPS only --------------------------------------------
  #
  # Some participants with only ADI-WPS scores have multiple instances of the 
  # questionnaire. We want to handle these participants.
  
  # Identify participants with multiple instances of ADI-WPS
  ids_wps_multiple <- df_adir_wps %>% 
    group_by(ID, question) %>% 
    count() %>% 
    filter(n > 1) %>% 
    pull(ID) %>% 
    unique()
  
  # Filter for participants with only one instance of ADI-WPS
  df_adir_wps_unique <- df_adir_wps %>% 
    filter(!(ID %in% ids_wps_multiple)) %>% 
    pivot_wider(id_cols = "ID",
                names_from = "measure",
                values_from = "regressive")
  
  # Excel file containing information about participants with multiple ADI-R WPS
  # (Curated by Hassan)
  file_wps_multiple <- "ADIWPS_multiple.xlsx"
  df_adir_wps_multiple <- read_excel(file.path(input_dir, file_wps_multiple))
  
  # Regression statuses for participants with ADI-R WPS
  df_adir_wps_bool <- bind_rows(df_adir_wps_unique,
                                df_adir_wps_multiple)
  
  
  # Participants with ADI-1995 only -------------------------------------------
  #
  # Some participants with only ADI-1995 scores have multiple instances of the 
  # questionnaire. We want to handle these participants.
  
  # Identify participants with multiple instances of ADI-1995
  ids_1995_multiple <- df_adir_1995 %>% 
    group_by(ID, question) %>% 
    count() %>% 
    filter(n > 1) %>% 
    pull(ID) %>% 
    unique()
  
  # Filter for participants with only one instance of ADI-1995
  df_adir_1995_unique <- df_adir_1995 %>%
    filter(!(ID %in% ids_1995_multiple)) %>% 
    pivot_wider(id_cols = "ID",
                names_from = "question", 
                values_from = "regressive") %>% 
    mutate(Language = ADI39E,
           Other = case_when(
             is.na(ADI95B5) & is.na(ADI95A5) ~ NA,
             is.na(ADI95B5) & !is.na(ADI95A5) ~ ADI95A5,
             !is.na(ADI95B5) & is.na(ADI95A5) ~ ADI95B5,
             !is.na(ADI95B5) & !is.na(ADI95A5) ~ ADI95B5 | ADI95A5
           )) %>% 
    select(ID, Language, Other)
  
  # Excel file containing information about participants with multiple ADI-R 1995
  # (Curated by Hassan)
  file_1995_multiple <- "ADI1995_multiple.xlsx"
  df_adir_1995_multiple <- read_excel(file.path(input_dir, file_1995_multiple))
  
  # Regression statuses for participants with ADI-R 1995
  df_adir_1995_bool <- bind_rows(df_adir_1995_unique,
                                 df_adir_1995_multiple)
  
  # Combining all ADI-R regression statuses ------------------------------------
  
  # Combine data from ADI-R WPS and 1995
  df_regression_bool <- bind_rows(df_adir_wps_bool, df_adir_1995_bool)
  
  # Arrange by participant ID
  df_regression_bool <- df_regression_bool %>% 
    arrange(ID)
  
  # Get age at test data
  input_file <- "subjects_with_adir__ados__scq_assessments_2025-12-19T15_41_03.583656Z.xlsx"
  input_file <- file.path("data/raw/", input_file)
  df_adir_age <- read_excel(input_file) %>% 
    filter(`Tests Classification__family` == "ADIR") %>% 
    select(indexid, dob, testdate, age_at_assessment_in_months) %>% 
    filter(!is.na(testdate)) %>% 
    mutate(dob = as.Date(dob),
           testdate = as.Date(testdate)) %>% 
    mutate(ADIR_Age = time_length(interval(dob, testdate), "years")) %>% 
    select(ID = indexid, ADIR_Age) %>% 
    group_by(ID) %>% 
    filter(ADIR_Age == min(ADIR_Age)) %>% 
    ungroup()
  
  df_regression_bool <- df_regression_bool %>% 
    left_join(df_adir_age, by = "ID")
  
  return(df_regression_bool)
  
}


#' Process dichotomized data
#'
#' @param inputs (tibble) Data frame containing columns indicating input file, 
#' threshold for binarization, comparison direction, and column name
#' @param input_dir (character scalar) Path to directory containing input files
#'
#' @returns (tibble) Data frame with dichotomous trait status across multiple 
#' measures
process_dichotomized_data <- function(inputs, input_dir) {
  
  for (i in 1:nrow(inputs)) {
    
    file <- inputs[[i, "file"]]  
    file <- file.path(input_dir, file)
    df <- read_excel(file) %>% 
      select(ID = indexid, score = numeric_value) %>% 
      filter(!is.na(ID), !is.na(score)) %>% 
      group_by(ID) %>% 
      mutate(score_max = max(score, na.rm = TRUE)) %>% 
      ungroup() 
    
    if (inputs[[i, "comparison"]] == "=") {
      df <- df %>% 
        mutate(PASS = score_max == inputs[[i, "threshold"]]) 
    } else if (inputs[[i, "comparison"]] == ">=") {
      df <- df %>% 
        mutate(PASS = score_max >= inputs[[i, "threshold"]]) 
    } else {
      stop()
    }
    
    df <- df %>%     
      select(ID, PASS) %>% 
      distinct()
    
    colnames(df)[2] <- inputs[[i, "colname"]]
    
    if (i == 1) {
      df_out <- df
    } else {
      df_out <- full_join(df_out, df, by = "ID")
    }
    
  }
  
  # Calculate combined anxiety score
  df_out_combined <- df_out %>% 
    column_to_rownames("ID") %>% 
    apply(1, function(x) {any(as.logical(x), na.rm = TRUE)}) %>% 
    enframe(name = "ID", value = "PASS")
  
  # Join combined score to individual scores
  df_out <- df_out %>% 
    left_join(df_out_combined, by = "ID")
  
  return(df_out)
  
}


#' Process sub-domain data
#'
#' @param files (character vector) File names for sub-domain files
#' @param input_dir (character scalar) Path to directory containing input
#' files
#'
#' @returns (tibble) Data frame containing scores for sub-domains
process_subdomain_data <- function(files, input_dir) {
  
  # Define an import function
  import_file <- function(x) {
    select(read_excel(x), ID = indexid, score = numeric_value)
  }
  
  # Extract sub-domain labels
  labels <- names(files)
  
  # Prepend input directory path
  files <- file.path(input_dir, files)
  
  # Re-add sub-domain labels
  names(files) <- labels
  
  # Import sub-domain files
  df_out <- files %>% 
    map_dfr(.f = import_file, .id = "code") %>% 
    group_by(ID, code) %>% 
    summarise(score = mean(score), .groups = "drop") %>% 
    pivot_wider(id_cols = "ID", names_from = "code", values_from = "score") 
  
  return(df_out)
  
}


#' Process ADOS data
#'
#' @param files (character vector) File names for ADOS OTS and CSS scores
#' @param input_dir (character scalar) Path to directory containing ADOS input 
#' files
#'
#' @returns (tibble) Data frame with ADOS OTS and CSS scores
#' 
process_ADOS_data <- function(files, input_dir = "data/raw/ADOS") {
  
  ados_input_file <- file.path(input_dir, files[["OTS"]])
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
  
  ados_css_file <- file.path(input_dir, files[["CSS"]])
  df_ados_css <- read_excel(path = ados_css_file)
  df_ados_css <- df_ados_css %>% 
    select(ID = indexid, Date = testdate, Age = age_at_test, Code = code,
           Score = value, CSS = css) %>% 
    mutate(Date = as.Date(Date))
  
  # Join calibrated scores to full data
  df_ados <- df_ados %>% 
    left_join(df_ados_css, by = c("ID", "Date", "Code", "Score")) %>% 
    rename(OTS = Score)
  
  return(df_ados)
}


#' Process ADHD data
#'
#' @param inputs (tibble) 
#' @param input_dir (character scalar) Path to directory containing ADHD input
#' files
#'
#' @returns (tibble) Data frame with ADHD status across multiple measures
process_ADHD_data <- function(inputs, input_dir = "data/raw/ADHD") {
  
  for (i in 1:nrow(inputs)) {
    
    file <- inputs[[i, "file"]]  
    file <- file.path(input_dir, file)
    df <- read_excel(file) %>% 
      select(ID = indexid, score = numeric_value) %>% 
      filter(!is.na(ID), !is.na(score)) %>% 
      group_by(ID) %>% 
      mutate(score_max = max(score, na.rm = TRUE)) %>% 
      ungroup() 
    
    if (inputs[[i, "comparison"]] == "=") {
      df <- df %>% 
        mutate(PASS = score_max == inputs[[i, "threshold"]]) 
    } else if (inputs[[i, "comparison"]] == ">=") {
      df <- df %>% 
        mutate(PASS = score_max >= inputs[[i, "threshold"]]) 
    } else {
      stop()
    }
    
    df <- df %>%     
      select(ID, PASS) %>% 
      distinct()
    
    colnames(df)[2] <- inputs[[i, "colname"]]
    
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
  
  return(df_ADHD)
  
}


#' Process prematurity data
#'
#' @param files (character vector) File names for prematurity files
#' @param input_dir (character scalar)  Path to directory containing prematurity 
#' input files
#'
#' @returns (tibble) Data frame with prematurity status across multiple measures
process_prematurity_data <- function(files, input_dir = "data/raw/Prematurity") {
  
  file <- file.path(input_dir, files[["prematurity"]])
  df_prematurity <- read_excel(file) %>% 
    select(ID = indexid, score = numeric_value) %>% 
    filter(!is.na(ID), !is.na(score)) %>% 
    group_by(ID) %>% 
    mutate(score_max = max(score, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(CLINICALINFO_PASS = score_max == 1) %>% 
    select(ID, CLINICALINFO_PASS) %>% 
    distinct()
  
  file <- file.path(input_dir, files[["gestational"]])
  df_gestational <- read_excel(file) %>% 
    select(ID = indexid, age = Fact__raw_value) %>% 
    mutate(age = as.numeric(age)) %>% 
    filter(age != -1)  %>% 
    mutate(GEST_PASS = age <= 36) %>% 
    select(ID, GEST_PASS)
  
  df_prematurity <- full_join(df_prematurity, df_gestational, by = "ID")
  
  # Calculate combined prematurity score
  df_prematurity_combined <- df_prematurity %>% 
    column_to_rownames("ID") %>% 
    apply(1, function(x) {any(as.logical(x), na.rm = TRUE)}) %>% 
    enframe(name = "ID", value = "PASS")
  
  # Join combined score to individual scores
  df_prematurity <- df_prematurity %>% 
    left_join(df_prematurity_combined, by = "ID")
  
  return(df_prematurity)
  
}


#' Process RBS data
#'
#' @param files (character vector) File names for RBS sub-domain files
#' @param input_dir (character scalar) Path to directory containing input
#' files
#'
#' @returns (tibble) Data frame containing scores for RBS sub-domains
process_RBS_data <- function(files, input_dir = "data/raw/RBS-R") {
  
  # Define an import function
  import_RBS_file <- function(x) {
    select(read_excel(x), code, ID = indexid, score = numeric_value)
  }
  
  # Extract codes
  codes <- names(files)
  
  # Prepend input directory path
  files <- file.path(input_dir, files)
  
  # Import RBS files
  df_rbs_init <- files %>% 
    map_dfr(.f = import_RBS_file)
  
  # Identify participants with multiple data
  ids_w_multiple <- df_rbs_init %>% 
    group_by(ID, code) %>% 
    count() %>% 
    ungroup() %>% 
    filter(n > 1) %>% 
    distinct() %>% 
    pull(ID) %>% 
    unique()
  
  # Subset participants without multiple data
  df_rbs_unique <- df_rbs_init %>% 
    filter(!(ID %in% ids_w_multiple)) %>% 
    mutate(KEY = 1)
  
  # For participants with multiple data, spreadsheets have been downloaded
  # separately
  # Directories to these participants
  dirs_multiple <- list.files(file.path(input_dir, "IDS_multiple"))
  
  # IDs to omit (issues with spreadsheets)
  ids_omit <- c("1-0344-003", "1-0458-004", "2-1359-003")
  dirs_multiple <- dirs_multiple[!(dirs_multiple %in% ids_omit)]
  
  # Iterate over participants with multiple data
  for (i in 1:length(dirs_multiple)) {
    
    # Import participant data
    path <- file.path(input_dir, "IDs_multiple", dirs_multiple[i])
    file <- list.files(path)
    file <- file.path(path, file)
    df <- read_excel(file) 
    
    # Filter for RBS data
    df <- df %>% 
      filter(code %in% codes) %>% 
      select(ID = indexid, code, Date = testdate, score = numeric_value) %>% 
      mutate(Date = as.Date(Date))
    
    # Build data frame
    if (i == 1) {
      df_rbs_multiple <- df
    } else {
      df_rbs_multiple <- bind_rows(df_rbs_multiple, df)
    }
    
  }
  
  # Create a key for each participant to track 
  df_rbs_multiple <- df_rbs_multiple %>% 
    arrange(ID, code, Date) %>% 
    group_by(ID, code) %>% 
    mutate(KEY = 1:n()) %>% 
    ungroup()
  
  # Convert multiple data to wide format
  df_rbs_multiple_wide <- df_rbs_multiple %>% 
    pivot_wider(id_cols = c("ID", "Date", "KEY"),
                names_from = "code", 
                values_from = "score")
  
  # Convert unique data to wide format
  df_rbs_unique_wide <- df_rbs_unique %>% 
    pivot_wider(id_cols = c("ID", "KEY"), 
                names_from = "code", 
                values_from = "score") %>% 
    mutate(Date = NA)
  
  # Bind unique and multiple data
  df_rbs <- bind_rows(df_rbs_unique_wide, df_rbs_multiple_wide)
  
  return(df_rbs)
  
}

