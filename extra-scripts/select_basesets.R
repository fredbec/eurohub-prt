library(data.table)
library(dplyr)
library(here)
library(ggplot2)
DT <- `[`

#nfcsts <- 50
#plength <- 10
model_avail <- 0.95



fcdat <- fread(here("data", "forecasts.csv"))
comdat <- fcdat |>
  filter(location %in% c("PL", "DE")) |>
  mutate(forecast_date2 =
           lubridate::ceiling_date(forecast_date,
                                   change_on_boundary = FALSE,
                                   unit = "week",
                                   week_start = getOption("lubridate.week.start", 1)) #round down to Saturday
  ) |>
  filter(!model == "EuroCOVIDhub-ensemble") |>
  filter(forecast_date >= as.Date("2021-03-11")) |> #before: 2021-03-20
  filter(forecast_date <= as.Date("2022-06-01"))  |>
  mutate(forecast_date = as.IDate(forecast_date))

nwks <- comdat |>
  dplyr::select(forecast_date2) |>
  dplyr::distinct() |>
  dplyr::pull() |>
  length()

fullmodelset <- comdat |>
  dplyr::select(model, forecast_date2, location, target_type) |>
  dplyr::distinct() |>
  dplyr::group_by(model, location, target_type) |>
  dplyr::summarise(n_fcsts = n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(availy = n_fcsts / nwks) |>
  dplyr::filter(availy > model_avail) |>
  setDT() |>
  split(by = c("location", "target_type")) |>
  lapply(function(dat) dat |> select(model) |> pull())


decases <- sort(fullmodelset$DE.Cases)[c(2,3,4,5,6)]
dedeaths <- sort(fullmodelset$DE.Deaths)[c(2,3,4,5,6)]
plcases <- sort(fullmodelset$PL.Cases)
pldeaths <- sort(fullmodelset$PL.Deaths)[c(1,2,3,4,5)]

fullmodelset_red <- list(decases,
                     dedeaths,
                     plcases,
                     pldeaths)

names(fullmodelset_red) <- names(fullmodelset)

saveRDS(fullmodelset_red, here("specs", "basesets.RDS"))
