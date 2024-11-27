library(scoringutils)
library(data.table)
library(here)
library(dplyr)
library(purrr)
DT <- `[`
source(here("ensvssize", "specs.R"))

###################Serverpart###################
if(grepl("*kit*", getwd())){ #if running code on server
  args=(commandArgs(TRUE))
  ind <- as.character(args[[1]])

  if(ind == "multiple"){ #this is an adhoc solution to run all smaller locations in a single script
    loctargets <- as.list(
      c(as.character(args[[2]]),
        as.character(args[[3]]),
        as.character(args[[4]]),
        as.character(args[[5]]),
        as.character(args[[6]]),
        as.character(args[[7]])))

    ks <- c(
      as.numeric(args[[8]]),
      as.numeric(args[[9]]),
      as.numeric(args[[10]]),
      as.numeric(args[[11]]),
      as.numeric(args[[12]]),
      as.numeric(args[[13]]),
      as.numeric(args[[14]]),
      as.numeric(args[[15]]))
  } else { #for Germany and Poland, running one k and loctarget at a time
    loctargets <- as.list(as.character(args[[1]]))
    rdseed <- as.numeric(args[[2]])
    propens <- as.numeric(args[[3]])
    #ks <- c(as.numeric(args[[2]]))
  }

} else { # if running locally
  #ks <- enscomb_specs$ks
  loctargets <- enscomb_specs$loctargets
}

ks <- enscomb_specs$ks
start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date
score_horizon <- enscomb_specs$horizon
model_types <- c("median_ensemble")
with_anomalies <- enscomb_specs$with_anomalies


set.seed(rdseed)


ensdat <- fread(here("data", "median_hubreplica_ensemble.csv")) |>
  filter(forecast_date >= as.Date(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(end_date)) |>
  DT(horizon %in% score_horizon)
# set k = 0 for hubensemble
ensdat[, k := 0]

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

    if(k %in% 3:8){
      prop_ensids <- unique(dt$ensid)
      keep_ensids <- sample(prop_ensids, ceiling(propens*length(prop_ensids)))
    } else {
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
                        baseline = "median-hubreplica")|>
    DT(compare_against == "median-hubreplica")
}) |>
  rbindlist() |>
  DT()


data.table::fwrite(scores, file = here("enscomb-data", paste0("ens_comb_pwscores", loctargets[[1]], "allk", "rdseed", rdseed, "propens", propens, ".csv")))

