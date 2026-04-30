# process_data.R


domains <- c("demographics", "ADIR", "ADOS", 
             "ADHD", "anxiety", "depression", "seizures", "sleep", "prematurity",
             "SSP", "SRS", "language", "RBS")
srcfiles <- paste0("process_", domains, "_data.R")
for (srcfile in srcfiles) {
  message(paste("Executing", srcfile, "..."))
  source(srcfile)
}

message("Executing combine_data.R ...")
source("combine_data.R")