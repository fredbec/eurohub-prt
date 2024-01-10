
#This function produces a long data.table with all possible combinations
#of models with ensemble size k
enscomb <- function(
    fcdat,
    start_date,
    end_date,
    model_avail,
    k,
    sampleens = NULL,
    mode = "long"){

  DT <- `[`

  num_weeks <- fcdat |>
    filter(forecast_date >= as.IDate(start_date)) |> #before: 2021-03-20
    filter(forecast_date <= as.IDate(end_date))  |>
    select(forecast_date) |>
    pull() |>
    unique() |>
    length()

  combdat <- fcdat |>
    filter(!model == "EuroCOVIDhub-ensemble") |>
    filter(forecast_date >= as.IDate(start_date)) |>
    filter(forecast_date <= as.IDate(end_date))  |>
    select(model, forecast_date, location, target_type) |>
    distinct() |>
    setDT() |>
    DT(,  n := .N, by = c("model", "location", "target_type")) |>
    filter(n >= model_avail*num_weeks) |>
    select(model, location, target_type) |>
    distinct() #|>

  if(nrow(combdat) < k){
    return(NULL)
  } else {
    combdat <- combdat |>
      list() |>
      lapply(function(mods) mods$model) |>
      lapply(function(mods) combn(mods, k) |> t()) |>
      #lapply(function(mods) mods[sample(nrow(mods), nfcsts), ]) |>
      lapply(function(mods) as.data.table(mods)) |>
      lapply(function(mods) mods[, ensid := 1:.N])
  }

  if(!mode == "long"){

    return(combdat[[1]]  |> DT())
  } else {

    combdat <- combdat[[1]] |>
      split(by = c("ensid"), keep.by = FALSE) |>
      lapply(function(mods) as.data.table(t(mods))) |>
      lapply(function(mods) rename(mods, "model" = "V1")) |>
      lapply(function(mods) mods[, modelid := 1:.N]) |>
      lapply(function(mods) mods[, k := .N]) |>
      rbindlist(idcol = "ensid")

    return(combdat)
  }

}


enscombcheck <- function(
    fcdat,
    combdat,
    start_date,
    end_date,
    loctarg,
    availpropmods,
    availproptime
){

  loc <- substr(loctarg, 0, 2)
  targ <- substr(loctarg, 3, 100)


  #availability indicator data
  fcdat_tocheck <- fcdat |>
    filter(forecast_date >= as.IDate(start_date)) |> #before: 2021-03-20
    filter(forecast_date <= as.IDate(end_date))  |>
    DT(, c("model", "location", "target_type", "forecast_date")) |>
    unique() |>
    DT(, avail := 1) |>
    #####
    DT(location == loc) |>
    DT(target_type == targ) |>
    DT(, c("model", "forecast_date", "avail"))

  fcdates <- unique(fcdat_tocheck$forecast_date)

  perfect_avail_dat <- expand.grid(forecast_date = fcdates,
                                   ensid = unique(combdat$ensid),
                                   modelid = unique(combdat$modelid)) |>
    left_join(combdat, by = c("ensid", "modelid")) |>
    left_join(fcdat_tocheck, by = c("model", "forecast_date")) |>
    setDT() |>
    DT(, avail := ifelse(is.na(avail), 0, 1)) |>
    #calculate proportion of available models at any given forecast date, for each ensemble
    DT(, prop_ensavail := sum(avail)/max(.N), by = c("forecast_date", "ensid")) |>
    DT(, c("ensid", "forecast_date", "prop_ensavail")) |>
    unique() |>
    DT(, ens_unavail := as.numeric(prop_ensavail < availpropmods)) |>
    DT(, proplessthan_apm := sum(ens_unavail)/max(.N), by = c("ensid"))

  filterens <- perfect_avail_dat |>
    DT(proplessthan_apm >= availproptime) |>
    DT(, kick := 1) |>
    DT(, c("ensid", "kick")) |>
    unique()

  return(filterens)

  combdat <- filterens[combdat, on = "ensid"] |>
    DT(is.na(kick)) |>
    DT(, kick := NULL)
}
