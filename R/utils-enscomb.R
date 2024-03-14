
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
    return(data.table())
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

  if(nrow(combdat)==0){
    return(list(data.table(), data.table()))
  }

  loc <- substr(loctarg, 0, 2)
  targ <- substr(loctarg, 3, 100)
  k <- unique(combdat$k)

  num_ens <- length(unique(combdat$ensid))
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


  #should better be named avail_dat; perfect_avail_dat is only first step
  perfect_avail_dat <- expand.grid(forecast_date = fcdates,
                                   ensid = unique(combdat$ensid),
                                   modelid = unique(combdat$modelid)) |>
    left_join(combdat, by = c("ensid", "modelid")) |>
    left_join(fcdat_tocheck, by = c("model", "forecast_date")) |>
    setDT() |>
    DT(, avail := ifelse(is.na(avail), 0, 1)) |>
    #calculate number of available models at any given forecast date, for each ensemble
    DT(, num_ensavail := sum(avail), by = c("forecast_date", "ensid")) |>
    #number of total models in each ensemble
    DT(, num_inens := max(.N), by = c("forecast_date", "ensid")) |>
    DT(, c("ensid", "forecast_date", "num_ensavail", "num_inens")) |>
    unique() |>
    DT(, ens_unavail := as.numeric(num_ensavail < num_inens - 1))
    DT(, proplessthan_apm := sum(ens_unavail)/max(.N), by = c("ensid"))

  filterens <- perfect_avail_dat |>
    DT(proplessthan_apm >= 1-availproptime) |>
    DT(, kick := 1) |>
    DT(, c("ensid", "kick")) |>
    unique()

  num_filt <- nrow(filterens)

  combdat_filtered <- filterens[combdat, on = "ensid"] |>
    DT(is.na(kick)) |>
    DT(, kick := NULL) |>
    DT()

  #return(combdat_filtered)
  ens_unavail_bydate <- perfect_avail_dat |>
    DT(, c("ensid", "forecast_date", "ens_unavail")) |>
    DT(, location := loc) |>
    DT(, target_type := targ) |>
    DT(, k := k) |>
    DT()

  return(list(ens_unavail_bydate = ens_unavail_bydate,
              combdat_filtered = combdat_filtered)
         )
}



make_combens <- function(
    enscombdat,
    ens_unavail_dat,
    fcdat
){

  DT <- `[`

  ensids <- unique(enscombdat$ensid)

  enscomb_preds <- lapply(ensids, function(ensidx){

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

    median_ens <- fcdat_avail_dates |>
      make_ensemble(summary_function = median,
                    incl = unique(ensmods$model)) |>
      mutate(ensid = ensidx)

    mean_ens <- fcdat_avail_dates |>
      make_ensemble(summary_function = mean,
                    incl = unique(ensmods$model)) |>
      mutate(ensid = ensidx)


    return(rbind(median_ens, mean_ens))


  })

  allenscombs <- rbindlist(enscomb_preds)

  return(allenscombs)
}

