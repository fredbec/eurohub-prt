# Load packages
library(data.table)
library(here)
library(readr)

# Pinned to a specific commit for reproducibility (was the floating `main` branch).
class_commit <- "55b8c1e9e9758a54945354620c4635519f6a7a45"
model_class <- read_csv(paste0(
  "https://raw.githubusercontent.com/epiforecasts/eval-by-method/",
  class_commit, "/data/model-classification.csv"))


dir.create(here("data", "raw-downloads"), recursive = TRUE, showWarnings = FALSE)
fwrite(model_class, here("data", "raw-downloads", "model-classifications.csv"))
