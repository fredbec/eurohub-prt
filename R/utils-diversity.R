#this function was copied and slightly adapted (just changing from "file" (to be read in) to "dat" parameter (already loaded))
#from (related project):
#https://github.com/epiforecasts/eval-by-method/blob/f8d0ffc0ff59e079e6815c0c49466a7e1e5f735d/R/prep-data.R#L7

classify_models <- function(dat) {
  methods <- dat |>
    pivot_longer(
      -model,
      names_to = "classifier", values_to = "classification"
    ) |>
    filter(!(is.na(classification) | classification == "#N/A")) |>
    group_by(model) |>
    summarise(
      agreement = (n_distinct(classification) == 1),
      classification = names(
        sort(table(classification), decreasing = TRUE)[1]
      ),
      .groups = "drop"
    ) |>
    mutate(classification = factor(
      classification,
      levels = c(
        "Agent-based", "Mechanistic",
        "Semi-mechanistic", "Statistical",
        "Machine learning", "Qualitative"
      )
    ))
  return(methods)
}


pairwise_distance <- function(
    enscombdat,
    ens_unavail_dat,
    fcdat
){

  DT <- `[`

  ensids <- unique(enscombdat$ensid)

  ens_distances <- lapply(ensids, function(ensidx){

    avail_dates <- ens_unavail_dat |>
      filter(ensid == ensidx, ens_unavail == 0) |>
      select(c("forecast_date", "location", "target_type"))

    ensmods <- enscombdat |>
      filter(ensid == ensidx) |>
      select(c("model"))

    fcdat_avail_dates <- fcdat |>
      setDT() |>
      copy() |>
      inner_join(avail_dates, by = c("forecast_date", "location", "target_type")) |>
      inner_join(ensmods, by = c("model"))

    units <- split(fcdat_avail_dates, by = c("forecast_date", "horizon"))
    ## get all pairs of models
    pairwise_distances <- lapply(units, function(unit) {
      model_split <- split(unit, by = "model")
      combinations <- combn(model_split, 2, simplify = FALSE)
      data.table(
        ensid = ensidx,
        horizon = unique(unit$horizon),
        forecast_date = unique(unit$forecast_date),
        location = unique(unit$location),
        target_type = unique(unit$target_type),
        distances = list(lapply(combinations, cramer_distance_two_models))
      )
    })

    return(rbindlist(pairwise_distances))
  })

  if (length(ens_distances) > 0) {
    allensdistances <- rbindlist(ens_distances) |>
      DT(, list(
        mean_distance = mean(unlist(distances))
      ), by = c("ensid", "horizon", "forecast_date", "location", "target_type"))
    setnames(allensdistances, "ensid", "model")
    allensdistances[, model := paste0("mean_ensemble", model)]
  } else {
    allensdistances <- data.table(
      model = character(0),
      horizon = integer(0),
      forecast_date = as.Date(character(0)),
      location = character(0),
      target_type = character(0)
    )
  }

  return(allensdistances)
}

cramer_distance_two_models <- function(model_pair) {
  covidHubUtils::calc_cramers_dist_one_model_pair(
    model_pair[[1]]$prediction,
    model_pair[[1]]$quantile,
    model_pair[[2]]$prediction,
    model_pair[[2]]$quantile,
    "trapezoid_riemann"
  )
}
