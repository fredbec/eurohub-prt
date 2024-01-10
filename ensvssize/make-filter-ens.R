library(data.table)
library(dplyr)
library(here)
DT <- `[`

source(here("ensvssize", "specs.R"))
source(here("R", "utils-enscomb.R"))

model_avail <- enscomb_specs$indmodel_avail
start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date
ks <- enscomb_specs$ks
loctargets <- enscomb_specs$loctargets
availpropmods <- enscomb_specs$availpropmods
availproptime <- enscomb_specs$availproptime

fcdat <- fread(here("data", "depldat.csv")) |>
  filter(forecast_date >= as.IDate(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.IDate(end_date))

combdat <-fread(here("enscomb-data", "enscomb_PLCases_k3.csv"))


enscombcheck(fcdat,
             combdat,
             start_date,
             end_date,
             "PLCases",
             0.9,
             0.2)

#availability indicator data
fcdat_tocheck <- fcdat |>
  DT(, c("model", "location", "target_type", "forecast_date")) |>
  unique() |>
  DT(, avail := 1) |>
  #####
  DT(location == "PL") |>
  DT(target_type == "Cases") |>
  DT(, c("model", "forecast_date", "avail"))


fcdates <- unique(fcdat$forecast_date)

testexp <- expand.grid(forecast_date = fcdates,
                       ensid = unique(combdat$ensid),
                       modelid = unique(combdat$modelid)) |>
  left_join(combdat, by = c("ensid", "modelid")) |>
  left_join(fcdat_tocheck, by = c("model", "forecast_date")) |>
  setDT() |>
  DT(, avail := ifelse(is.na(avail), 0, 1)) |>
  #calculate proportion of available models at any given forecast date
  DT(, prop_ensavail := sum(avail)/max(.N), by = c("forecast_date", "ensid")) |>
  DT(, c("ensid", "forecast_date", "prop_ensavail")) |>
  unique() |>
  DT(, ens_unavail := as.numeric(prop_ensavail < availpropmods)) |>
  DT(, proplessthan_apm := sum(ens_unavail)/max(.N), by = c("ensid"))

filterens <- testexp |>
  DT(proplessthan_apm >= availproptime) |>
  DT(, kick := 1) |>
  DT(, c("ensid", "kick")) |>
  unique()

combdat <- filterens[combdat, on = "ensid"] |>
  DT(is.na(kick)) |>
  DT(, kick := NULL)

dateind <- testexp |>
  DT(, c("ensid", "forecast_date", "ens_unavail"))


testexp |> arrange(ensid) |> View()

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
