library(data.table)
library(dplyr)
library(here)
library(ggplot2)
DT <- `[`

nfcsts <- 50
#plength <- 10
ens_avail <- 0.7
ceilingdate <- "2022-03-11" #52 weeks exactly
model_avail <- 0.7
k <- 9

fcdat <- fread(here("data", "depldat.csv")) |>
  filter(location %in% c("DE")) |>
  filter(target_type %in% c("Cases"))
period_cat_dt <- fread(here("data", "period_cats.csv"))

num_weeks <- fcdat |>
  mutate(forecast_date =
           lubridate::ceiling_date(forecast_date,
                                   change_on_boundary = FALSE,
                                   unit = "week",
                                   week_start = getOption("lubridate.week.start", 1)) #round down to Saturday
  ) |>
  filter(forecast_date >= as.Date("2021-03-11")) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(ceilingdate))  |>
  select(forecast_date) |>
  pull() |>
  unique() |>
  length()


comdat <- fcdat |>
  filter(location %in% c("DE")) |>
  filter(target_type %in% c("Cases")) |>
  mutate(forecast_date =
           lubridate::ceiling_date(forecast_date,
                                   change_on_boundary = FALSE,
                                   unit = "week",
                                   week_start = getOption("lubridate.week.start", 1)) #round down to Saturday
  ) |>
  filter(!model == "EuroCOVIDhub-ensemble") |>
  filter(forecast_date >= as.Date("2021-03-11")) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(ceilingdate))  |>
  mutate(forecast_date = as.IDate(forecast_date)) |>
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
  #rbindlist(idcol = "idcol")

comdat1 <- comdat$DE.Cases |>
  split(by = c("ensid"), keep.by = FALSE) |>
  lapply(function(mods) as.data.table(t(mods))) |>
  lapply(function(mods) rename(mods, "models" = "V1")) |>
  lapply(function(mods) mods[, modelid := 1:.N]) |>
  rbindlist(idcol = "ensid")


res <- vector(mode = "list", length = 5)

availmodels <-
for (i in 1:5){

  print(comdat[i, 2:6])

  res[[i]] <- make_ensemble(fcdat |> mutate(period_cat = 1), incl = comdat[i, 2:6])
    #score_ensemble(fcdat |> mutate(period_cat = 1) |> mutate(population = 5000), incl = comdat[i, 2:6])


}


fcdacom


comb1 <- comdat |>
  select(model, forecast_date, target_type, location) |>
  distinct() |>
  DT(,  n := .N, by = c("forecast_date", "target_type", "location")) |>
  select(forecast_date, target_type, location, n) |>
  distinct()
