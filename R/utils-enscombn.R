
#This function produces
enscomb_wide <- function(fcdat,
                         start_date,
                         ){


  num_weeks <- fcdat |>
    filter(forecast_date >= as.Date("2021-03-11")) |> #before: 2021-03-20
    filter(forecast_date <= as.IDate(ceilingdate))  |>
    select(forecast_date) |>
    pull() |>
    unique() |>
    length()

  comdat <- fcdat |>
    filter(!model == "EuroCOVIDhub-ensemble") |>
    filter(forecast_date <= as.IDate(ceilingdate))  |>
    select(model, forecast_date, location, target_type) |>
    distinct()|>
    DT(,  n := .N, by = c("model", "location", "target_type")) |>
    filter(n >= model_avail*num_weeks) |>
    select(model, location, target_type) |>
    distinct() |>
    split(by = c("location", "target_type")) |>
    lapply(function(mods) mods$model) |>
    lapply(function(mods) combn(mods, k) |> t()) |>
    #lapply(function(mods) mods[sample(nrow(mods), nfcsts), ]) |>
    lapply(function(mods) as.data.table(mods)) |>
    lapply(function(mods) mods[, ensid := 1:.N])


  return(comdat)
  #rbindlist(idcol = "idcol")
}
