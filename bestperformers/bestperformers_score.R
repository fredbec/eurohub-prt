library(dplyr)
library(knitr)
library(tidyr)

library(here)
#source(here("specs", "specs.R"))

source(here("R", "bestperformers-functions.R"))

horizons <- c(1,2)

median_ens <-
  data.table::fread(here("data", "processed", "hubreplica-ensemble.csv")) |>
  filter(horizon %in% horizons)


excl_from_bp <-
  data.table::fread(here("data", "auxiliary", "selection-ensemble-exclude-instances.csv"))

su_cols <- c("model", "forecast_date", "quantile", "horizon",
             "target_type", "location", "target_end_date",
             "prediction", "true_value")


all_evals <- NULL
for(nm in c(3,5,8,10)){

  bestperforms_mean <- data.table::fread(here("output", "selection-ensemble", "forecasts",
                                              paste0("best_performers_ensemble_mean_nmod", nm, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)

  bestperforms_median <- data.table::fread(here("output", "selection-ensemble", "forecasts",
                                                paste0("best_performers_ensemble_median_nmod", nm, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)

  bestperforms_invscore_mean <- data.table::fread(here("output", "selection-ensemble", "forecasts",
                                                       paste0("best_performers_ensemble_invscore_mean_nmod", nm, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)

  bestperforms_invscore_median <- data.table::fread(here("output", "selection-ensemble", "forecasts",
                                                         paste0("best_performers_ensemble_invscore_median_nmod", nm, ".csv"))) |>
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
data.table::fwrite(scores_individual, here("output", "selection-ensemble", "forecasts", "scores-selection-ens.csv"))
data.table::fwrite(scores_average, here("output", "selection-ensemble", "forecasts", "avg-scores-selection-ens.csv"))
