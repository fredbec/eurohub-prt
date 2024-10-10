library(here)
library(scoringutils)
library(dplyr)
library(data.table)


source(here("ensvssize", "specs.R"))
source(here("R", "utils-ext.R"))

su_cols <- c("model", "forecast_date", "quantile", "horizon",
             "target_type", "location", "target_end_date",
             "prediction", "true_value")
start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date
loctargets <- enscomb_specs$loctargets

fcdat <- data.table::fread(here("data", "depldat.csv")) |>
  filter(forecast_date >= data.table::as.IDate(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= data.table::as.IDate(end_date))

hubdat <- data.table::fread(here("data", "hubensemble.csv")) |>
  filter(forecast_date >= data.table::as.IDate(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= data.table::as.IDate(end_date))


#also need target end date after scoring
tg_end_map <- fcdat |>
  select(forecast_date, horizon, target_end_date) |>
  distinct()

###############################Scoring##########################
#score all models
score_all_mods <- fcdat |>
  select(su_cols) |>
  score() |>
  summarise_scores(by = c("model", "location", "target_type",
                          "forecast_date", "horizon")) |>
  left_join(tg_end_map, by = c("forecast_date", "horizon"))

#Version with relative skill
score_all_mods_with_relwis <- score_all_mods |>
  summarise_scores(by = c("model", "target_type", "forecast_date",
                          "location"),
                   relative_skill = TRUE,
                   baseline = "EuroCOVIDhub-baseline")

#Version with relative skill, by horizon
score_all_mods_with_relwis_avg <- score_all_mods |>
  summarise_scores(by = c("model", "target_type",
                          "location", "horizon"),
                   relative_skill = TRUE,
                   baseline = "EuroCOVIDhub-baseline")


#Score unweighted mean and median ensemble
mean_ens <- fcdat |>
  make_ensemble(summary_function = mean, old_call = TRUE) |>
  filter(model == "mean_ensemble")

median_ens <- fcdat |>
  make_ensemble(summary_function = median, old_call = TRUE) |>
  filter(model == "median_ensemble")

mean_ens_scores <- mean_ens |>
  select(su_cols) |>
  score() |>
  summarise_scores(by = c("model", "location", "target_type",
                          "forecast_date", "horizon")) |>
  left_join(tg_end_map, by = c("forecast_date", "horizon"))

median_ens_scores <- median_ens |>
  select(su_cols) |>
  score() |>
  summarise_scores(by = c("model", "location", "target_type",
                          "forecast_date", "horizon")) |>
  left_join(tg_end_map, by = c("forecast_date", "horizon"))

tg_end_map <- tg_end_map |>
  mutate(forecast_date = as.Date(forecast_date))

hub_scores <- hubdat |>
  select(su_cols) |>
  score() |>
  summarise_scores(by = c("model", "location", "target_type",
                          "forecast_date", "horizon")) |>
  mutate(forecast_date = as.Date(forecast_date)) |>
  left_join(tg_end_map, by = c("forecast_date", "horizon"))



data.table::fwrite(score_all_mods, here("bestperformers-data","scores", "score_all_models.csv"))
data.table::fwrite(score_all_mods_with_relwis, here("bestperformers-data","scores", "score_all_mods_with_relwis.csv"))
data.table::fwrite(score_all_mods_with_relwis_avg, here("bestperformers-data","scores", "score_all_mods_with_relwis_avg.csv"))
data.table::fwrite(mean_ens_scores, here("bestperformers-data","scores", "mean_ensemble_scores.csv"))
data.table::fwrite(median_ens_scores, here("bestperformers-data","scores", "median_ensemble_scores.csv"))
data.table::fwrite(hub_scores, here("bestperformers-data","scores", "hubensemble_scores.csv"))


data.table::fwrite(mean_ens, here("bestperformers-data", "mean_ensemble.csv"))
data.table::fwrite(median_ens, here("bestperformers-data", "median_ensemble.csv"))

data.table::fwrite(median_ens |> mutate(model = "median-hubreplica"), here("data", "median_hubreplica_ensemble.csv"))
