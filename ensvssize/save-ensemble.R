library(scoringutils)
library(data.table)
library(here)
library(dplyr)
library(purrr)
DT <- `[`
source(here("ensvssize", "specs.R"))

#if running code on server
if(grepl("*kit*", getwd())){
  args=(commandArgs(TRUE))
    loctargets <- as.list(as.character(args[[1]]))

    #if sampling ensembles (to make code run faster in testing situations)
    #this always assumes that something is passed
    #if not wanting to sample, just provide anything for rdseed and set propens
    #to 1, then nothing is sampled
    rdseed <- as.numeric(args[[2]])
    #proportion of ensembles to sample
    propens <- as.numeric(args[[3]])

} else { # if running locally
  loctargets <- enscomb_specs$loctargets
  rdseed <- Sys.time()
  propens <- 1
}

ks <- enscomb_specs$ks
start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date
score_horizon <- enscomb_specs$horizon
with_anomalies <- enscomb_specs$with_anomalies
#which ensemble type to run pairwise comparisons on
#either median_ensemble or mean_ensemble
model_types <- c("median_ensemble")

#set random seed
set.seed(rdseed)


ensdat <- fread(here("data", "median_hubreplica_ensemble.csv")) |>
  filter(forecast_date >= as.Date(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(end_date)) |>
  DT(horizon %in% score_horizon) |>
  DT(, availability := NULL) |>
  DT(, model_type := NULL)

baselinedat <- fread(here("data", "depldat.csv")) |>
  filter(forecast_date >= as.Date(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(end_date)) |>
  DT(model == "EuroCOVIDhub-baseline") |>
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
  anoms <- data.table::fread(here("anomalies.csv")) |>
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
    dt <- data.table::fread(here("enscomb-data", paste0("predictions_enscomb", loctarg, "_k", k, ".csv")))
    if (nrow(dt) == 0) return(NULL)

    if(k %in% 3:8){ #sample in these ranges of k, where the number of recombinations is highest
      prop_ensids <- unique(dt$ensid)
      keep_ensids <- sample(prop_ensids, ceiling(propens*length(prop_ensids)))
    } else { #keep all ensids
      keep_ensids <- unique(dt$ensid)
    }

    dt |>
      DT(, k := k) |>
      DT(model %in% model_types) |>
      DT(ensid %in% keep_ensids) |>
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
scores <- map(loctargets, \(loctarg) {
  loc <- substr(loctarg, 0, 2)
  targ <- substr(loctarg, 3, 100)
  dattoscore |>
    DT(location == loc) |>
    DT(target_type == targ) |>
    #remove unnecessary columns
    #this only leaves horizon, model, target_end_date (for identifying instances), as well as quantile, prediction, true_value
    DT(,location := NULL) |>
    DT(,target_type := NULL) |>
    DT(, k := NULL)  |>
    DT(, model := ifelse(model == "median-hubreplica_k0", "median-hubreplica", model)) |>
    score() |>
    pairwise_comparison(score,
                        by = c("model", "horizon"),
                        metric = "interval_score",
                        baseline = "median-hubreplica")
}) |>
  rbindlist() |>
  DT()


if(propens == 1){ #leave out random seed from filename, since no randomness is happening
  data.table::fwrite(scores, file = here("enscomb-data", paste0("ens_comb_pwscores", loctargets[[1]], ".csv")))
} else {
  data.table::fwrite(scores, file = here("enscomb-data", paste0("ens_comb_pwscores", loctargets[[1]], "rdseed", rdseed, "propens", propens, ".csv")))
}

