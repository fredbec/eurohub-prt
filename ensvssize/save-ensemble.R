library(scoringutils)
library(data.table)
library(here)
library(dplyr)
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
    ks <- c(as.numeric(args[[2]]))
  }

} else { # if running locally
  ks <- enscomb_specs$ks
  loctargets <- enscomb_specs$loctargets
}

start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date
score_horizon <- enscomb_specs$horizon
model_types <- c("median_ensemble")
with_anomalies <- enscomb_specs$with_anomalies


ensdat <- fread(here("data", "hubensemble.csv")) |>
  filter(forecast_date >= as.IDate(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.IDate(end_date)) |>
  DT(horizon %in% score_horizon) |>
  DT(, c("location", "target_type", "model", "target_end_date", "quantile", "prediction", "true_value", "horizon"))


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



for(k in ks){

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
      DT(, c("model", "target_end_date", "quantile", "prediction", "true_value", "horizon"))

    #read in recombined ensemble data for given loc-targ and k
    dattoscore <- data.table::fread(here("enscomb-data", paste0("predictions_enscomb", loctarg, "_k", k, ".csv"))) |>
      DT(model %in% model_types) |>
      DT(horizon %in% score_horizon) |>
      DT(, model := paste0(model, ensid))

    if(with_anomalies){
      #join with anomalies and filter for instances that don't exist in anomaly data
      #i.e. these are the instances that aren't an anomaly
      dattoscore <- anoms |>
        DT(dattoscore, on = c("location", "target_type", "target_end_date"))|>
        DT(is.na(anom)) |>
        DT(, anom := NULL)

    }

    #select recombined ensembles for only relevant variables
    dattoscore <- dattoscore |>
      DT(, c("model", "target_end_date", "quantile", "prediction", "true_value", "horizon"))


    #append hub ensemble data to recombined ensemble data (data have the same format)
    #score and do pairwise comparisons
    pwscores <- dattoscore |>
      rbind(enssubdat) |>
      score() |>
      pairwise_comparison(by = c("model", "horizon"),
                          metric = "interval_score",
                          baseline = "EuroCOVIDhub-ensemble")

    if(!is.null(pwscores)){

      data.table::fwrite(pwscores, here("enscomb-data", paste0("ens_comb_pwscores", loctarg, "_k", k, ".csv")))
      rm(pwscores)
    }
  }
  )
}
