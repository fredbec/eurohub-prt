library(knitr)
library(here)
library(arrow)
library(purrr)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

source(here("ensvssize", "specs.R"))


# Get targets  ---------------------------------------------------
# -- keep only deaths as we only classified models forecasting deaths
targets <- crossing("loc" = enscomb_specs$loctargets) |>
  mutate(target = loc) |> #for compatibility with merging later on
  filter(grepl("Deaths", target)) |>
  pull(target)

# get model classifications
model_class <- read_csv("https://raw.githubusercontent.com/epiforecasts/eval-by-method/main/data/model-classification.csv") |>
  select(model, classification)

# Ensemble combinations ---------------------------------------------------
# get ensemble component combinations
#component combinations have k in file name (pwscores don't), so need another
#version of targets here
targets_comp_combinations <- crossing("loc" = enscomb_specs$loctargets,
                                      "k" = enscomb_specs$ks) |>
  mutate(target = paste(loc, k, sep = "_k")) |>
  filter(grepl("Deaths", target)) |>
  pull(target)

models <- map_dfr(.x = targets_comp_combinations,
                  ~ read_parquet(here("enscomb-data",
                                      paste0("enscomb_",
                                             .x, ".parquet"))) |>
                    mutate(target = .x))

# Join to model classification
# - restrict to deaths as we only classified models with death forecasts
models <- models |>
  left_join(model_class, by = "model")

# count model classifications in each ensemble
ensemble_mix <- models |>
  group_by(ensid, target, k, classification) |>
  count() |>
  mutate(homog = k==n,
         classification = ifelse(!homog, "Heterogeneous",
                                 classification)) |>
  select(-n) |>
  distinct()

# Get scores  ---------------------------------------------------
scores <- map_dfr(.x = targets,
                  ~ read_parquet(here("enscomb-data",
                                      "pwscores",
                                      paste0("ens_comb_pwscores",
                                             .x, ".parquet"))) |>
                    mutate(target = paste0(.x, "_k", str_extract(model, "(?<=_k)\\d+")))) #for merging compatibility

scores <- scores |>
  #only keep rows with reference to full ensemble
  #and kick out data for full ensemble itself (only interested in ensemble recombinations)
  filter(compare_against == "median-hubreplica" &
           model != "median-hubreplica") |>
  #remove "median_ensemble" from ensid, as well as k (info for this is in target already), for merging
  mutate(ensid = str_remove_all(model, "median_ensemble")) |>
  mutate(ensid = gsub("_k[0-9]*", "", ensid)) |> #remove k number from ensid
  select(target, ensid, horizon, scaled_rel_skill)

# Join scores and model type ----------------------------------------------
ensemble_scores <- left_join(scores, ensemble_mix,
                             by = c("ensid", "target")) |>
  filter(!is.na(classification))


# restrict to k < possible single-type ensembles
k_max <- filter(ensemble_scores, homog) |>
  group_by(target, horizon) |>
  summarise(k = max(k))

ensemble_scores <- filter(ensemble_scores,
                          target %in% k_max$target) |>
  # clean variables
  mutate(location = str_remove_all(target, "Deaths_k[:digit:]"),
         location = fct_infreq(location),
         horizon = ordered(horizon, levels = c(1,2),
                           labels = c("1 week", "2 week")),
         classification = fct_infreq(classification),
         homog = factor(homog,
                        levels = c(FALSE, TRUE),
                        labels = c("Heterogeneous", "Homogeneous")))

#save data
arrow::write_parquet(ensemble_scores, here("model-diversity", "enscomb_scores_with_classification.parquet"))
arrow::write_parquet(model_class, here("model-diversity", "component_model_classification.parquet"))
arrow::write_parquet(models, here("model-diversity", "enscomb_with_classification.parquet"))
