library(scoringutils)
library(data.table)
library(here)
library(dplyr)
DT <- `[`
source(here("ensvssize", "specs.R"))

start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date
ks <- enscomb_specs$ks
loctargets <- enscomb_specs$loctargets
maxens <- enscomb_specs$maxens
score_horizon <- enscomb_specs$horizon
model_types <- c("median_ensemble")


ensdat <- fread(here("data", "hubensemble.csv")) |>
  filter(forecast_date >= as.IDate(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.IDate(end_date)) |>
  DT(horizon == score_horizon) |>
  DT(, c("location", "target_type", "model", "target_end_date", "quantile", "prediction", "true_value"))

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



for(k in c(12,13)){

  print(k)
  lapply(as.list(loctargets), function(loctarg){
    loc <- substr(loctarg, 0, 2)
    targ <- substr(loctarg, 3, 100)

    #filter hub ensemble data for the given location-target combination
    #and only keep relevant variables
    enssubdat <- ensdat |>
      setDT() |>
      DT(location == loc) |>
      DT(target_type == targ) |>
      DT(, c("model", "target_end_date", "quantile", "prediction", "true_value"))

    #read in recombined ensemble data for given loc-targ and k
    dattoscore <- data.table::fread(here("enscomb-data", paste0("predictions_enscomb", loctarg, "_k", k, ".csv"))) |>
      DT(model %in% model_types) |>
      DT(horizon == score_horizon) |>
      DT(, model := paste0(model, ensid))

    #join with anomalies and filter for instances that don't exist in anomaly data
    #i.e. these are the instances that aren't an anomaly
    #only keep relevant variables
    dattoscore <- anoms |>
      DT(dattoscore, on = c("location", "target_type", "target_end_date"))|>
      DT(is.na(anom)) |>
      DT(, anom := NULL) |>
      DT(, c("model", "target_end_date", "quantile", "prediction", "true_value"))


    #append hub ensemble data to recombined ensemble data (data have the same format)
    #score and do pairwise comparisons
    pwscores <- dattoscore |>
      rbind(enssubdat) |>
      score() |>
      pairwise_comparison(metric = "interval_score",
                          baseline = "EuroCOVIDhub-ensemble")

    if(!is.null(pwscores)){

      data.table::fwrite(pwscores, here("enscomb-data", paste0("ens_comb_pwscores", loctarg, "_k", k, ".csv")))
    }
  }
  )
}
