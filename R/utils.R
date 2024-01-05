merge_forecasts_with_truth <- function(forecasts, truth) {
  forecasts <- data.table::copy(forecasts)
  truth <- data.table::copy(truth)

  data.table::setnames(truth, "date", "target_end_date")

  forecasts_with_truth <- truth[
    forecasts, on = c("location", "target_end_date", "target_type")
  ]

  # Remove point forecasts
  forecasts_with_truth <- forecasts_with_truth[!is.na(quantile)]

  # Remove forecasts with no truth data
  forecasts_with_truth <- forecasts_with_truth[!is.na(true_value)]

  return(forecasts_with_truth[])
}

rescale_to_incidence_rate <- function(forecasts, population, scale = 1e5) {
  DT <- `[`

  forecasts <- data.table::copy(forecasts)
  population <- data.table::copy(population)

  forecasts_with_population <- forecasts[
    population, on = c("location"), population := i.population
  ] |>
    DT(, true_value_pop := true_value / population * scale) |>
    DT(, prediction_pop := prediction / population * scale)

  return(forecasts_with_population[])
}

rename_models <- function(forecasts) {
  forecasts <- data.table::copy(forecasts) |>
    data.table::DT(, model := data.table::fcase(
      model %in% "EuroCOVIDhub-ensemble", "Ensemble",
      model %in% "epiforecasts-weeklygrowth", "Surrogate"
    ))
  return(forecasts[])
}

summarise_forecasts_by <- function(forecasts, var = "model", by) {
  forecasts |>
    DT(, c(..var, ..by)) |>
    unique() |>
    DT(, .(n = .N), by = by)
}
