library(here)
library(arrow)
library(purrr)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(data.table)

source(here("specs", "specs.R"))
source(here("R", "functions-ensemble-diversity.R"))
source(here("R", "utils-ext.R"))



# Get targets  ---------------------------------------------------
targets <- crossing("loc" = specs$loctargets) |>
  mutate(target = loc) |> #for compatibility with merging later on
  #filter(grepl("Deaths", target)) |>
  pull(target)

# get model classifications
model_class <- fread(here("data", "raw-downloads", "model-classifications.csv")) |>
  classify_models() |> #applies majority vote; from Kath's project, here locally saved in R/functions-ensemble-diversity
  filter(!is.na(classification)) |> #all models used here are classified
  select(model, classification) |>
  mutate(classification = as.character(classification)) #needed for function later on

# Ensemble combinations ---------------------------------------------------
# get ensemble component combinations
#component combinations have k in file name (pwscores don't), so need another
#version of targets here
targets_comp_combinations <- crossing("loc" = specs$loctargets,
                                      "k" = specs$ks) |>
  mutate(target = paste(loc, k, sep = "_k")) |>
  #filter(grepl("Deaths", target)) |>
  pull(target)

models <- map_dfr(.x = targets_comp_combinations,
                  ~ read_parquet(here("output",
                                      "ensemble-size",
                                      "ensemble-combinations",
                                      paste0("enscomb_",
                                             .x, ".parquet"))) |>
                    mutate(target = .x))

# Join to model classification
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
                  ~ read_parquet(here("output",
                                      "ensemble-size",
                                      "pwscores-median_ensemble",
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


# restrict to k <= the largest k at which a single-type (homogeneous) ensemble
# is possible for that location-target. target encodes k (loc_kN), so strip it
# before taking the max, otherwise every (target, horizon) group holds a single k.
k_max <- ensemble_scores |>
  filter(homog) |>
  mutate(loctarget = sub("_k[0-9]+$", "", target)) |>
  group_by(loctarget, horizon) |>
  summarise(k_max = max(k), .groups = "drop")

ensemble_scores <- ensemble_scores |>
  mutate(loctarget = sub("_k[0-9]+$", "", target)) |>
  left_join(k_max, by = c("loctarget", "horizon")) |>
  filter(!is.na(k_max), k <= k_max) |>
  select(-loctarget, -k_max) |>
  # clean variables
  mutate(location = str_remove_all(target, "Deaths_k[:digit:]"),
         location = str_remove_all(target, "Cases_k[:digit:]"),
         location = fct_infreq(location),
         horizon = ordered(horizon, levels = c(1,2),
                           labels = c("1 week", "2 week")),
         classification = fct_infreq(classification),
         homog = factor(homog,
                        levels = c(FALSE, TRUE),
                        labels = c("Heterogeneous", "Homogeneous")))

#save data
dir.create(here("output", "ensemble-diversity"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "ensemble-diversity", "distance-data"), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(ensemble_scores, here("output", "ensemble-diversity", "enscomb_scores_with_classification.parquet"))
arrow::write_parquet(model_class, here("output", "ensemble-diversity", "component_model_classification.parquet"))
arrow::write_parquet(models, here("output", "ensemble-diversity", "enscomb_with_classification.parquet"))



####calculate ensemble pairwise distances
start_date <- specs$start_date
end_date <- specs$end_date
ks <- specs$ks
loctargets <- specs$loctargets

fcdat <- arrow::read_parquet(here("data", "processed", "fcdat.parquet")) |>
  filter(forecast_date >= as.IDate(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.IDate(end_date))

for(k in ks){

  lapply(as.list(loctargets), function(loctarg){
    loc <- substr(loctarg, 0, 2)
    targ <- substr(loctarg, 3, 100)

    #READ in data
    ens_unavail_dat <- arrow::read_parquet(here("output", "ensemble-size", "ensemble-combinations", paste0("ens_unavail_bydate_", loctarg, "_k", k, ".parquet")))
    enscombdat <- arrow::read_parquet(here("output", "ensemble-size", "ensemble-combinations", paste0("enscomb_", loctarg, "_k", k, ".parquet")))
    #make function call
    distances <- pairwise_distance(enscombdat = enscombdat,
                                   ens_unavail_dat = ens_unavail_dat,
                                   fcdat = fcdat)

    #write data
    arrow::write_parquet(distances, here("output", "ensemble-diversity", "distance-data", paste0("distances", loctarg, "_k", k, ".parquet")))
  }
  )
}



scores_distances <- map(loctargets, \(loctarg) {
  cat(loctarg, "\n")
  scores <- arrow::read_parquet(
    here(
      "output", "ensemble-size", "pwscores-median_ensemble",
      paste0("ens_comb_pwscores", loctarg, ".parquet")
    )
  )
  comparison <- map(ks, \(k) {
    cat(k, "\n")
    distances <- arrow::read_parquet(
      here(
        "output", "ensemble-diversity", "distance-data",
        paste0("distances", loctarg, "_k", k, ".parquet")
      )
    ) |>
      scoringutils:::as_scores(metrics = "mean_distance") |>
      DT(, model := sub("mean_ensemble", "median_ensemble", model)) |>
      DT(, model := paste0(model, "_k", k))
    if (nrow(distances) > 0 & length(unique(distances$model)) > 1) {
      pw_distances <- scoringutils::get_pairwise_comparisons(
        distances, metric = "mean_distance", by = "horizon"
      ) |>
        DT(, list(model, horizon, mean_distance_relative_skill)) |>
        unique()
      k_scores <- scores |>
        DT(grepl(paste0("_k", k, "$"), model))
      merge.data.table(k_scores, pw_distances, by = c("model", "horizon")) |>
        DT(, k := k) |>
        DT(, loctarg := loctarg)
    } else {
      NULL
    }
  }) |>
    rbindlist(fill = TRUE)
})

scores_distances <- rbindlist(scores_distances) |>
  DT(, location := substr(loctarg, 0, 2)) |>
  DT(, target_type := substr(loctarg, 3, 8))

scores_distances <- scores_distances |>
  #DT(, horizon := ifelse(horizon == 1, "1-week horizon", "2-week horizon")) |>
  DT(, location := factor(location,
                          levels = c("DE", "PL", "CZ", "FR", "GB"),
                          labels = c("Germany", "Poland", "Czech Rep.", "France", "United Kingd."))
  )
arrow::write_parquet(scores_distances, here("output", "ensemble-diversity", "scores-distances.parquet"))
