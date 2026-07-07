library(data.table)
library(dplyr)
library(here)
library(ggplot2)
library(arrow)
DT <- `[`

nfcsts <- 50
plength <- 10
model_avail <- 0.7

source(here("R", "utils.R"))
source(here("R", "utils-ext.R"))
source(here("specs", "specs.R"))

su_cols <- c("model", "forecast_date", "quantile", "horizon",
             "target_type", "location", "target_end_date",
             "prediction", "true_value")

fcdat <- fread(here("data", "raw-downloads", "forecasts.csv"))
truth <- fread(here("data", "raw-downloads", "truth.csv"))

loctargets <- specs$loctargets
locs <- unique(substr(loctargets, 1, 2))

# Load observations defined as anomalies
anomalies <- fread(here("data", "raw-downloads",  "anomalies.csv")) |>
  DT(, location_name := NULL)

# Load population data
population <- fread(here("data", "raw-downloads", "population.csv"))

period_cats <- fread(here("data", "auxiliary", "period_cats.csv"))

combdat <- fcdat |>
  rename(prediction = value) |>
  filter(!location %in% locs) |>
  mutate(forecast_date =
           lubridate::ceiling_date(forecast_date,
                                   change_on_boundary = FALSE,
                                   unit = "week",
                                   week_start = getOption("lubridate.week.start", 1)) #round down to Saturday
  ) |>
  filter(!model == "EuroCOVIDhub-ensemble") |>
  filter(forecast_date >= as.Date(specs$start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(specs$end_date)) |>
  DT(prediction<0, prediction := 0) |>
  mutate(forecast_date = as.IDate(forecast_date)) |>
  merge_forecasts_with_truth(truth) |>
  DT(, location_name := NULL) |>
  DT(, scenario_id := NULL) |>
  DT(anomalies, on = c("location", "target_end_date", "target_type"), anomaly_code := i.anomaly) |>
  DT(, anomaly := !is.na(anomaly_code)) |>
  DT(, anomaly_code := NULL) |>
  rescale_to_incidence_rate(population) |>
  DT(order(location, target_type, forecast_date, horizon, model, quantile)) |>
  setcolorder(c("model", "location", "target_type",
                "forecast_date", "horizon", "target_end_date",
                "quantile", "prediction", "true_value",
                "prediction_pop", "true_value_pop",
                "anomaly")) |>
  DT(period_cats, on = c("forecast_date")) |>
  filter(!is.na(quantile))

fcdat_allcountries <- combdat |>
  copy() |>
  DT(, n_models := uniqueN(model), by = .(location, target_type, forecast_date)) |>
  DT(, min_models := min(n_models), by = .(location, target_type)) |>
  DT(, min_models := min_models - 1) |> #subtract baseline model from count
  DT(min_models >= 4) |>
  DT(, c("n_models", "min_models") := NULL)

#make hub replica ensemble
median_ens <- fcdat_allcountries |>
  make_ensemble(summary_function = median,
                model_name = "median_ensemble", old_call = TRUE) |>
  filter(model == "median_ensemble")

#score all models
#also need target end date after scoring
tg_end_map <- fcdat_allcountries |>
  select(forecast_date, horizon, target_end_date) |>
  distinct() |>
  mutate(forecast_date = as.Date(forecast_date))

score_all_mods <- fcdat_allcountries |>
  select(all_of(su_cols)) |>
  scoringutils::as_forecast_quantile(predicted = "prediction",
                                     observed = "true_value",
                                     quantile_level = "quantile") |>
  scoringutils::score() |>
  scoringutils::summarise_scores(by = c("model", "location", "target_type",
                          "forecast_date", "horizon")) |>
  mutate(forecast_date = as.Date(forecast_date)) |>
  left_join(tg_end_map, by = c("forecast_date", "horizon"))


dir.create(here("revision", "data"), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(fcdat_allcountries, here("revision", "data", "fcdat_allcountries_filtered.parquet"))
fwrite(median_ens, here("revision", "data", "hubreplica-ensemble.csv"))
fwrite(score_all_mods, here("revision", "data", "component-model-scores.csv"))



