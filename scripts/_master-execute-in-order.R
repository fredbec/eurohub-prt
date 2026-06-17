library(here)

cat("Running 00_download-data/master-get-data.R...\n")
source(here("scripts", "00_download-data", "master-get-data.R"))
cat("00_download-data/master-get-data.R successfully run\n\n")

#cat("Running 01_load-data.R...\n")
#source(here("scripts", "01_load-data.R"))
#cat("01_load-data.R successfully run\n\n")

#cat("Running 02_score-component-models.R...\n")
#source(here("scripts", "02_score-component-models.R"))
#cat("02_score-component-models.R successfully run\n\n")

#cat("Running 03_run-ens-combs.R...\n")
#source(here("scripts", "03_run-ens-combs.R"))
#cat("03_run-ens-combs.R successfully run\n\n")

#cat("Running 04_run-selection-ensemble.R...\n")
#source(here("scripts", "04_run-selection-ensemble.R"))
#cat("04_run-selection-ensemble.R successfully run\n\n")

#cat("Running 05_run-ensemble-size.R...\n")
#source(here("scripts", "05_run-ensemble-size.R"))
#cat("05_run-ensemble-size.R successfully run\n\n")

cat("Running 06_run-ensemble-diversity.R...\n")
source(here("scripts", "06_run-ensemble-diversity.R"))
cat("06_run-ensemble-diversity.R successfully run\n\n")

cat("Running 07_produce-plots.R...\n")
source(here("scripts", "07_produce-plots.R"))
cat("07_produce-plots.R successfully run\n\n")

cat("All scripts completed successfully.\n")
