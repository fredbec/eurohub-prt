library(scoringutils)
library(data.table)
library(here)
library(dplyr)
library(purrr)
library(arrow)

DT <- `[`
source(here("specs", "specs.R"))

#allow a single loc-target combination to be passed as a command-line argument
#(useful when running each loc-target as a separate job on a cluster);
#otherwise process all loc-targets defined in specs.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && nzchar(args[1])) {
  loctargets <- as.list(args[1])
} else {
  loctargets <- specs$loctargets
}

ks <- specs$ks
start_date <- specs$start_date
end_date <- specs$end_date
score_horizon <- specs$horizon
with_anomalies <- specs$with_anomalies
ensemble_type <- specs$ensemble_type


ensdat <- fread(here("data", "processed", "hubreplica-ensemble.csv")) |>
  filter(forecast_date >= as.Date(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(end_date)) |>
  DT(horizon %in% score_horizon) |>
  DT(, availability := NULL) |>
  DT(, model_type := NULL)

baselinedat <- read_parquet(here("data", "processed", "fcdat.parquet")) |>
  filter(forecast_date >= as.Date(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(end_date)) |>
  DT(model == "EuroCOVIDhub-baseline") |>
  DT(horizon %in% score_horizon) |>
  DT(, prediction_pop := NULL) |>
  DT(, true_value_pop := NULL) |>
  DT(, anomaly := NULL) |>
  DT(, anomaly_code := NULL)


# set k = 0 for hubensemble and baseline
ensdat[, k := 0]
baselinedat[, k := 0]

ensdat <- rbind(ensdat, baselinedat)

if(with_anomalies){
  #read in anomalies
  anoms <- data.table::fread(here("data", "raw-downloads", "anomalies.csv")) |>
    DT(location %in% c("DE", "PL")) |>
    DT(target_variable %in% c("inc case", "inc death")) |>
    DT(, target_type := ifelse(target_variable == "inc case", "Cases", "Deaths")) |>
    DT(, c("location", "target_type","target_end_date")) |>
    DT(, anom := 1)

  #join with anomalies and filter for instances that don't exist in anomaly data
  #i.e. these are the instances that aren't an anomaly
  ensdat <- anoms |>
    DT(ensdat, on = c("location", "target_type", "target_end_date"))|>
    DT(is.na(anom)) |>
    DT(, anom := NULL)
}

all_data <- map(as.list(loctargets), \(loctarg) {
  dattoscore <- map(ks, \(k) {
    #read in recombined ensemble data for given loc-targ and k
    dt <- read_parquet(here("output", "ensemble-size", "ensemble-forecasts", paste0("predictions_enscomb", loctarg, "_k", k, ".parquet")))
    if (nrow(dt) == 0) return(NULL)

    dt |>
      DT(, k := k) |>
      DT(model %in% ensemble_type) |>
      DT(horizon %in% score_horizon)
  })
  return(rbindlist(dattoscore))
})

dattoscore <- rbindlist(all_data) |>
  DT(, model := paste0(model, ensid))

if(with_anomalies){
  #join with anomalies and filter for instances that don't exist in anomaly data
  #i.e. these are the instances that aren't an anomaly
  dattoscore <- anoms |>
    DT(dattoscore, on = c("location", "target_type", "target_end_date"))|>
    DT(is.na(anom)) |>
    DT(, anom := NULL)
}

#append hub ensemble data to recombined ensemble data (data have the same format)
#score and do pairwise comparisons
dattoscore <- dattoscore |>
  DT(, c("location", "forecast_date", "k", "quantile", "horizon", "target_type", "model", "prediction", "true_value")) |>
  rbind(ensdat, fill = TRUE) |>
  DT(, c("location", "forecast_date", "k", "quantile", "horizon", "target_type", "model", "prediction", "true_value")) |>
  DT(, target_end_date := forecast_date + (horizon-1)*7 + 5) |>
  DT(, model := paste0(model, "_k", k)) |>
  DT(, forecast_date := NULL)

## score by location / target
dir.create(here("output", "ensemble-size", paste0("pwscores-", ensemble_type)),
           recursive = TRUE, showWarnings = FALSE)

walk(loctargets, \(loctarg) {
  loc <- substr(loctarg, 0, 2)
  targ <- substr(loctarg, 3, 100)
  scores <- dattoscore |>
    DT(location == loc) |>
    DT(target_type == targ) |>
    DT(,location := NULL) |>
    DT(,target_type := NULL) |>
    DT(, k := NULL)  |>
    DT(, model := ifelse(model == "median-hubreplica_k0", "median-hubreplica", model)) |>
    as_forecast_quantile(observed = "true_value", predicted = "prediction",
                         quantile_level = "quantile") |>
    score() |>
    get_pairwise_comparisons(compare = "model",
                             by = c("horizon"),
                             metric = "wis",
                             baseline = "median-hubreplica") |>
    DT() |>
    setnames(c("wis_relative_skill", "wis_scaled_relative_skill"),
             c("relative_skill", "scaled_rel_skill")) |>
    DT(compare_against == "median-hubreplica")

  arrow::write_parquet(
    scores,
    sink = here("output", "ensemble-size",
                paste0("pwscores-", ensemble_type),
                paste0("ens_comb_pwscores", loctarg, ".parquet")))
})
