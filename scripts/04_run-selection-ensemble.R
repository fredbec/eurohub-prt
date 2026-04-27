library(here)
library(dplyr)
library(data.table)
library(tidyr)

source(here("ensvssize", "specs.R"))
source(here("R", "utils-ext.R"))
source(here("R", "bestperformers-functions.R"))
library(spatstat.geom)

###############################from function call####################################
nmods <- c(3,5,8,10)

su_cols <- c("model", "forecast_date", "quantile", "horizon",
             "target_type", "location", "target_end_date",
             "prediction", "true_value")

start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date
loctargets <- enscomb_specs$loctargets
horizons <- enscomb_specs$horizons

#appendix results
if (length(horizons) == 4){
  suffix <- "_allhor"
} else {
  suffix <- ""
}

fcdat <- arrow::read_parquet(here("data", "processed", "fcdat.parquet")) |>
  filter(forecast_date >= data.table::as.IDate(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= data.table::as.IDate(end_date)) |>
  filter(horizon %in% horizons)

median_ens <-
  data.table::fread(here("data", "processed", "hubreplica-ensemble.csv")) |>
  filter(horizon %in% horizons)

excl_from_bp <-
  data.table::fread(here("data", "auxiliary", "selection-ensemble-exclude-instances.csv"))

su_cols <- c("model", "forecast_date", "quantile", "horizon",
             "target_type", "location", "target_end_date",
             "prediction", "true_value")


data <- fcdat
hub_data <- fcdat
window <- 4

#mydat <- best_performers_ensemble(data,
#                                  su_cols = su_cols)
###########supplying data end####################

###############################end from function call####################################

#library(masterthesis)
#library(here)
#library(spatstat.geom)
#source(here("code", "load_clean_data.R"))
#source(here("specs", "specs.R"))

best_performers_data <- NULL
k <- 1
for(nmod in nmods){

  if(nmod == 8){

    #exclude loc-target combs if they don't have enough models
    #here: only include GB&CZ Deaths, as well as DE&PL both
    bp_data <- hub_data |>
      filter(location %in% c("DE", "PL", "GB", "CZ")) |>
      mutate(excl = ifelse(target_type == "Cases" &
                             location %in% c("GB", "CZ"),
                           1, 0)) |>
      filter(excl == 0) |>
      select(-excl)

    lwr <- 1
    upr <- 6 #was 7 before!
  }
  if(nmod == 10){
    #exclude loc-target combs if they don't have enough models
    #here: only include DE&PL Deaths and Cases
    bp_data <- hub_data |>
      filter(location %in% c("DE", "PL"))
    lwr <- 1
    upr <- 4
  } else {
    bp_data <- hub_data
    lwr <- 1
    upr <- 10
  }

  best_performers_ens <- best_performers_ensemble(bp_data, su_cols = su_cols,
                                                  nmods = nmod,
                                                  #may_miss = 3,
                                                  return_model_data = TRUE)

  best_performers_ensemble_mean <- best_performers_ens[[1]] |>
    filter(model == "mean_ensemble")

  best_performers_ensemble_median <- best_performers_ens[[1]] |>
    filter(model == "median_ensemble")

  remove_empties <- best_performers_ens[[2]] |> lapply(function(nc) ncol(nc)>0) |> unlist()
  best_performers_ens[[2]] <- best_performers_ens[[2]][remove_empties]

  #best performers models are returned as matrices because two months ago me is stupid
  best_performers_data[[k]] <-lapply(best_performers_ens[[2]][lwr:upr], function(mat)
    mat |>
      data.table::data.table(keep.rownames = TRUE) |>
      data.table::melt(id.vars = "rn") |>
      dplyr::filter(value == 1) |>
      dplyr::arrange(rn) |>
      dplyr::select(-value) |>
      rename(forecast_date = rn)) |>
    data.table::rbindlist(idcol = TRUE) |>
    mutate(nmod = nmod) |>
    tidyr::separate(.id, into = c("location", "target_type"), sep = "[.]") |>
    rename(model = variable)


  data.table::fwrite(best_performers_ensemble_mean,
                     here("output", "selection-ensemble", "forecasts",
                          paste0("best_performers_ensemble_mean_nmod", nmod, suffix, ".csv")))
  data.table::fwrite(best_performers_ensemble_median,
                     here("output", "selection-ensemble", "forecasts",
                          paste0("best_performers_ensemble_median_nmod", nmod, suffix, ".csv")))


  k <- k + 1
}

data.table::fwrite(rbindlist(best_performers_data),
                   here("output", "selection-ensemble", "weights", paste0("best_performers_incl_mods", suffix, ".csv")))




####################Calculate corresponding inverse score weights####################################
#inverse score weights
best_performers <- fread(here("output", "selection-ensemble", "weights", paste0("best_performers_incl_mods", suffix, ".csv")))
#score data
score_data <- fread(here("data","processed", "component-model-scores.csv")) |>
  filter(horizon %in% horizons) |>
  mutate(target_end_date = as.Date(target_end_date))

fc_dates <- sort(unique(hub_data$forecast_date))[-(1:window)] |>
  as.list()


all_inv_score_weights <- vector(mode = "list", length = length(nmods))
k <- 1
for(nm in nmods){

  subset_bp <- best_performers |>
    filter(nmod == nm) |>
    mutate(forecast_date = as.Date(forecast_date))

  #hub data with only best performers at each forecast date
  bp_data <- right_join(hub_data, subset_bp,
                        by = c("location", "target_type", "forecast_date", "model")) |>
    arrange(forecast_date, location, target_type, model)

  #bp_scores <- right_join(score_data, subset_bp,
  #                        by = c("location", "target_type", "forecast_date", "model")) |>
  #  arrange(forecast_date, location, target_type, model)


  #helper function that gets subset of data (both hub and hub score data)
  #with only the best performers of forecast date
  #Different to above in the following way: includes all the same
  #models (best performers at fcdate), needed to score models
  get_correct_subset <- function(bp_data,
                                 all_data,
                                 fcdate){
    bp_models <- bp_data |>
      filter(forecast_date == fcdate) |>
      select(location, target_type, model) |>
      distinct()

    subset_bp <- all_data |>
      right_join(bp_models, by = c("location", "target_type", "model"))

    return(subset_bp)
  }

  ##########remove
  # bp_data <- bp_data |>
  #  filter(location == "DE", target_type == "Cases")
  #
  # hub_data <- hub_data |>
  #  filter(location == "DE", target_type == "Cases")
  ##########remove

  invscore_weights <- lapply(fc_dates, function(fcdat)

    inverse_score_weights(data = get_correct_subset(bp_data, hub_data, fcdat),
                          score_data = get_correct_subset(bp_data, score_data, fcdat),
                          su_cols = NULL,
                          fc_date = fcdat,
                          exp_smooth = NULL,
                          window = window)) |>
    rbindlist() |>
    mutate(nmod = nm)

  all_inv_score_weights[[k]] <- invscore_weights


  ######make according ensembles (weighted mean and median)
  bp_weighted_median_ens <- bp_data |>
    mutate(
      forecast_date = as.Date(forecast_date)
    )|>
    select(-nmod) |>
    left_join(invscore_weights |>
                mutate(
                  forecast_date = as.Date(forecast_date)
                ), by = c("model", "location",
                          "target_type", "forecast_date")) |>
    select(-nmod) |>
    make_ensemble(summary_function = weighted.median,
                  model_name = "weighted.median_ensemble", old_call = TRUE) |>
    filter(model == "weighted.median_ensemble")

  bp_weighted_mean_ens <- bp_data |>
    mutate(
      forecast_date = as.Date(forecast_date)
    )|>
    select(-nmod) |>
    left_join(invscore_weights |>
                mutate(
                  forecast_date = as.Date(forecast_date)
                ), by = c("model", "location",
                          "target_type", "forecast_date")) |>
    select(-nmod) |>
    make_ensemble(summary_function = weighted.mean,
                  model_name = "weighted.mean_ensemble", old_call = TRUE) |>
    filter(model == "weighted.mean_ensemble")


  data.table::fwrite(bp_weighted_mean_ens,
                     here("output", "selection-ensemble", "forecasts",
                          paste0("best_performers_ensemble_invscore_mean_nmod", nm, suffix, ".csv")))
  data.table::fwrite(bp_weighted_median_ens,
                     here("output", "selection-ensemble", "forecasts",
                          paste0("best_performers_ensemble_invscore_median_nmod", nm, suffix, ".csv")))

  k <- k + 1

}

all_inv_score_weights <- rbindlist(all_inv_score_weights)
data.table::fwrite(all_inv_score_weights, here("output", "selection-ensemble", "weights", paste0("best_performers_invscore_weights", suffix, ".csv")))



all_evals <- NULL
for(nm in c(3,5,8,10)){

  bestperforms_mean <- data.table::fread(here("output", "selection-ensemble", "forecasts",
                                              paste0("best_performers_ensemble_mean_nmod", nm, suffix, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)

  bestperforms_median <- data.table::fread(here("output", "selection-ensemble", "forecasts",
                                                paste0("best_performers_ensemble_median_nmod", nm, suffix, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)

  bestperforms_invscore_mean <- data.table::fread(here("output", "selection-ensemble", "forecasts",
                                                       paste0("best_performers_ensemble_invscore_mean_nmod", nm, suffix, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)

  bestperforms_invscore_median <- data.table::fread(here("output", "selection-ensemble", "forecasts",
                                                         paste0("best_performers_ensemble_invscore_median_nmod", nm, suffix, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)



  eval_mean <- fast_eval(bestperforms_mean, median_ens,
                         su_cols = su_cols,
                         strat_by = c("model", "location", "target_type", "forecast_date"),
                         return_eval = TRUE,
                         comp_avg_by = c("forecast_date", "target_type")) |>
    mutate(location = ifelse(is.na(location), "Average", location)) |>
    comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
    mutate(nmod = nm)

  eval_median <- fast_eval(bestperforms_median, median_ens,
                           su_cols = su_cols,
                           strat_by = c("model", "location", "target_type", "forecast_date"),
                           return_eval = TRUE,
                           comp_avg_by = c("forecast_date", "target_type")) |>
    mutate(location = ifelse(is.na(location), "Average", location)) |>
    comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
    mutate(nmod = nm)


  eval_invscore_mean <- fast_eval(bestperforms_invscore_mean, median_ens,
                                  su_cols = su_cols,
                                  strat_by = c("model", "location", "target_type", "forecast_date"),
                                  return_eval = TRUE,
                                  comp_avg_by = c("forecast_date", "target_type")) |>
    mutate(location = ifelse(is.na(location), "Average", location)) |>
    comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
    mutate(nmod = nm)

  eval_invscore_median <- fast_eval(bestperforms_invscore_median, median_ens,
                                    su_cols = su_cols,
                                    strat_by = c("model", "location", "target_type", "forecast_date"),
                                    return_eval = TRUE,
                                    comp_avg_by = c("forecast_date", "target_type")) |>
    mutate(location = ifelse(is.na(location), "Average", location)) |>
    comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
    mutate(nmod = nm)


  all_evals <- all_evals |>
    rbind(eval_mean) |>
    rbind(eval_median) |>
    rbind(eval_invscore_mean) |>
    rbind(eval_invscore_median)
}

scores_individual <- all_evals |>
  filter(!is.na(forecast_date))
scores_average <- all_evals |>
  filter(is.na(forecast_date))
data.table::fwrite(scores_individual, here("output", "selection-ensemble", "forecasts", paste0("scores-selection-ens", suffix, ".csv")))
data.table::fwrite(scores_average, here("output", "selection-ensemble", "forecasts", paste0("avg-scores-selection-ens" , suffix, ".csv")))
