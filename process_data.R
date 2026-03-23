# process_data.R

setwd("/Users/abeauchamp/Documents/Projects/RegressiveAutism")

domains <- c("demographics", "ADIR", "ADOS", "ADHD", "anxiety",
             "seizure", "SSP", "SRS", "language")
srcfiles <- paste0("process_", domains, "_data.R")
for (srcfile in srcfiles) {
  message(paste("Executing", srcfile, "..."))
  source(srcfile)
}

message("Executing combine_data.R ...")
source("combine_data.R")