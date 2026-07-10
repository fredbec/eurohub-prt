library(here)
library(dplyr)
library(data.table)
library(tidyr)

source(here("specs", "specs.R"))
source(here("R", "utils-ext.R"))
source(here("R", "functions-selection-ensemble.R"))
library(spatstat.geom)

nmods <- c(3)
window <- 4
horizon_sets <- specs$horizons


su_cols <- c("model", "forecast_date", "quantile", "horizon",
             "target_type", "location", "target_end_date",
             "prediction", "true_value")




for(horizons in horizon_sets){

  #reset the accumulator for each horizon set, otherwise the _allhor output
  #inherits the rows written during the 2-week run
  best_performers_data <- NULL
  k <- 1

  #appendix results
  if (length(horizons) == 4){
    suffix <- "_allhor"
  } else {
    suffix <- ""
  }

  fcdat_all <- arrow::read_parquet(here("revision", "data", "fcdat_allcountries_filtered.parquet")) |>
    filter(forecast_date >= data.table::as.IDate(specs$start_date)) |> #before: 2021-03-20
    filter(forecast_date <= data.table::as.IDate(specs$end_date)) |>
    filter(horizon %in% horizons)
  hub_data <- fcdat_all


  median_ens <- fread(here("revision", "data", "hubreplica-ensemble.csv")) |>
    filter(horizon %in% horizons)
  for(nmod in nmods){

    best_performers_ens <- best_performers_ensemble(hub_data,
                                                    su_cols = su_cols,
                                                    nmods = nmod,
                                                    return_model_data = TRUE)

    best_performers_ensemble_mean <- best_performers_ens[[1]] |>
      filter(model == "mean_ensemble")

    best_performers_ensemble_median <- best_performers_ens[[1]] |>
      filter(model == "median_ensemble")

    remove_empties <- best_performers_ens[[2]] |>
      lapply(function(nc) ncol(nc) > 0) |>
      unlist()
    best_performers_ens[[2]] <- best_performers_ens[[2]][remove_empties]

    upr <- length(best_performers_ens[[2]])

    best_performers_data[[k]] <- lapply(best_performers_ens[[2]][1:upr], function(mat)
      mat |>
        data.table::data.table(keep.rownames = TRUE) |>
        data.table::melt(id.vars = "rn") |>
        filter(value == 1) |>
        arrange(rn) |>
        select(-value) |>
        rename(forecast_date = rn)) |>
      data.table::rbindlist(idcol = TRUE) |>
      mutate(nmod = nmod) |>
      tidyr::separate(.id, into = c("location", "target_type"), sep = "[.]") |>
      rename(model = variable)

    dir.create(here("revision", "output", "selection-ensemble", "forecasts"),
               recursive = TRUE, showWarnings = FALSE)
    data.table::fwrite(best_performers_ensemble_mean,
                       here("revision", "output", "selection-ensemble", "forecasts",
                            paste0("best_performers_ensemble_mean_nmod", nmod, suffix, ".csv")))
    data.table::fwrite(best_performers_ensemble_median,
                       here("revision", "output", "selection-ensemble", "forecasts",
                            paste0("best_performers_ensemble_median_nmod", nmod, suffix, ".csv")))
    k <- k + 1
  }

  dir.create(here("revision", "output", "selection-ensemble", "weights"),
             recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(rbindlist(best_performers_data),
                     here("revision", "output", "selection-ensemble", "weights",
                          paste0("best_performers_incl_mods",suffix,".csv")))


  ####################Inverse score weights####################################
  best_performers <- fread(here("revision", "output", "selection-ensemble", "weights",
                                paste0("best_performers_incl_mods",suffix,".csv"))) |>
    distinct()

  score_data <- fread(here("revision", "data", "component-model-scores.csv")) |>
    filter(horizon %in% horizons) |>
    mutate(target_end_date = as.Date(target_end_date))

  fc_dates <- sort(unique(hub_data$forecast_date))[-(1:window)] |>
    as.list()

  all_inv_score_weights <- vector(mode = "list", length = length(nmods))
  k <- 1

  for(nm in nmods){

    subset_bp <- best_performers |>
      filter(nmod == nm) |>
      distinct() |>
      mutate(forecast_date = as.Date(forecast_date))

    bp_data <- right_join(hub_data, subset_bp,
                          by = c("location", "target_type", "forecast_date", "model")) |>
      arrange(forecast_date, location, target_type, model)

    get_correct_subset <- function(bp_data, all_data, fcdate){
      bp_models <- bp_data |>
        filter(forecast_date == fcdate) |>
        select(location, target_type, model) |>
        distinct()

      all_data |>
        right_join(bp_models, by = c("location", "target_type", "model"))
    }

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

    bp_weighted_median_ens <- bp_data |>
      mutate(forecast_date = as.Date(forecast_date)) |>
      select(-nmod) |>
      left_join(invscore_weights |> mutate(forecast_date = as.Date(forecast_date)),
                by = c("model", "location", "target_type", "forecast_date")) |>
      select(-nmod) |>
      make_ensemble(summary_function = weighted.median,
                    model_name = "weighted.median_ensemble", old_call = TRUE) |>
      filter(model == "weighted.median_ensemble")

    bp_weighted_mean_ens <- bp_data |>
      mutate(forecast_date = as.Date(forecast_date)) |>
      select(-nmod) |>
      left_join(invscore_weights |> mutate(forecast_date = as.Date(forecast_date)),
                by = c("model", "location", "target_type", "forecast_date")) |>
      select(-nmod) |>
      make_ensemble(summary_function = weighted.mean,
                    model_name = "weighted.mean_ensemble", old_call = TRUE) |>
      filter(model == "weighted.mean_ensemble")

    data.table::fwrite(bp_weighted_mean_ens,
                       here("revision", "output", "selection-ensemble", "forecasts",
                            paste0("best_performers_ensemble_invscore_mean_nmod", nm, suffix, ".csv")))
    data.table::fwrite(bp_weighted_median_ens,
                       here("revision", "output", "selection-ensemble", "forecasts",
                            paste0("best_performers_ensemble_invscore_median_nmod", nm, suffix, ".csv")))
    k <- k + 1
  }

  data.table::fwrite(rbindlist(all_inv_score_weights),
                     here("revision", "output", "selection-ensemble", "weights",
                          "best_performers_invscore_weights.csv"))


  ####################Evaluation####################################
  all_evals <- NULL

  for(nm in nmods){

    bestperforms_mean <- fread(here("revision", "output", "selection-ensemble", "forecasts",
                                    paste0("best_performers_ensemble_mean_nmod", nm,suffix, ".csv")))
    bestperforms_median <- fread(here("revision", "output", "selection-ensemble", "forecasts",
                                      paste0("best_performers_ensemble_median_nmod", nm,suffix, ".csv")))
    bestperforms_invscore_mean <- fread(here("revision", "output", "selection-ensemble", "forecasts",
                                             paste0("best_performers_ensemble_invscore_mean_nmod", nm,suffix, ".csv")))
    bestperforms_invscore_median <- fread(here("revision", "output", "selection-ensemble", "forecasts",
                                               paste0("best_performers_ensemble_invscore_median_nmod", nm,suffix, ".csv")))

    for(ens in list(list(bestperforms_mean, "mean"),
                    list(bestperforms_median, "median"),
                    list(bestperforms_invscore_mean, "invscore_mean"),
                    list(bestperforms_invscore_median, "invscore_median"))){

      eval_res <- fast_eval(ens[[1]], median_ens,
                            su_cols = su_cols,
                            strat_by = c("model", "location", "target_type", "forecast_date"),
                            return_eval = TRUE,
                            comp_avg_by = c("forecast_date", "target_type")) |>
        mutate(location = ifelse(is.na(location), "Average", location)) |>
        comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
        mutate(nmod = nm, ens_type = ens[[2]])

      all_evals <- rbind(all_evals, eval_res)
    }
  }

  scores_individual <- all_evals |> filter(!is.na(forecast_date))
  scores_average    <- all_evals |> filter(is.na(forecast_date))

  dir.create(here("revision", "output", "selection-ensemble", "scores"),
             recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(scores_individual,
                     here("revision", "output", "selection-ensemble", "scores",
                          paste0("scores-selection-ens",suffix,".csv")))
  data.table::fwrite(scores_average,
                     here("revision", "output", "selection-ensemble", "scores",
                          paste0("avg-scores-selection-ens",suffix,".csv")))
}
