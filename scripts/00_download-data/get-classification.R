# Load packages
library(data.table)
library(here)


model_class <- read_csv("https://raw.githubusercontent.com/epiforecasts/eval-by-method/main/data/model-classification.csv")


fwrite(model_class, here("data", "raw-downloads", "model-classifications.csv"))
