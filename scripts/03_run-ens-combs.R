library(data.table)
library(dplyr)
library(here)
DT <- `[`

source(here("ensvssize", "specs.R"))

model_avail <- enscomb_specs$indmodel_avail
start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date
ks <- enscomb_specs$ks
loctargets <- enscomb_specs$loctargets
availpropmods <- enscomb_specs$availpropmods
availpropmodsk3 <- availpropmods #same as above (legacy)
availproptime <- enscomb_specs$availproptime



##############################Suggest ensembles################################
fcdat <- arrow::read_parquet(here("data", "processed", "fcdat.parquet"))
for(k in ks){

  combdat <- fcdat |>
    setDT() |>
    DT(, loctarg := paste0(location, target_type)) |>
    split(by = c("loctarg")) |>
    lapply(function(dat) enscomb(dat,
                                 start_date = start_date,
                                 end_date = end_date,
                                 model_avail = model_avail,
                                 k = k))

  #filter out NULLs
  combdat <- combdat[lapply(combdat, function(elem) !is.null(elem)) |> unlist()]
  loctargets_save <- names(combdat)

  sapply(loctargets_save,
         function(loctarg) arrow::write_parquet(combdat[[loctarg]], here("enscomb-data", paste0("enscomb_suggested_", loctarg, "_k", k, ".parquet"))))
}

##############################Filter ensembles################################
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
availpropmodsk3 <- enscomb_specs$availpropmodsk3
availproptime <- enscomb_specs$availproptime

fcdat <- arrow::read_parquet(here("data", "depldat.parquet")) |>
  filter(forecast_date >= as.Date(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(end_date))

rdseeds <- data.table::fread(here("enscomb-data", "rdseeds.csv"))



for(k in ks){

  if(k == 3){
    availpropmods_k <- availpropmodsk3
  } else {
    availpropmods_k <- availpropmods
  }

  lapply(as.list(loctargets), function(loctarg){
    loc <- substr(loctarg, 0, 2)
    targ <- substr(loctarg, 3, 100)

    combdat <- arrow::read_parquet(here("enscomb-data", paste0("enscomb_suggested_", loctarg, "_k", k, ".parquet")))

    results <- enscombcheck(fcdat,
                            combdat,
                            start_date,
                            end_date,
                            loctarg,
                            availpropmods_k,
                            availproptime
    )
    if(!is.null(results)){

      prop_ensids <- results[[2]]$ensid  |> unique()

      arrow::write_parquet(results[[1]], here("enscomb-data", paste0("ens_unavail_bydate_", loctarg, "_k", k, ".parquet")))
      arrow::write_parquet(results[[2]], here("enscomb-data", paste0("enscomb_", loctarg, "_k", k, ".parquet")))
    }
  }
  )
}



#combdat <-fread(here("enscomb-data", "enscomb_PLCases_k3.csv"))

prop_filtered <- vector(mode = "list", length = length(ks))


for(k in ks){

  prop_filtered_ens <- lapply(as.list(loctargets), function(loctarg){
    loc <- substr(loctarg, 0, 2)
    targ <- substr(loctarg, 3, 100)

    sugg <- arrow::read_parquet(here("enscomb-data", paste0("enscomb_suggested_", loctarg, "_k", k, ".parquet")))
    filt <- arrow::read_parquet(here("enscomb-data", paste0("enscomb_", loctarg, "_k", k, ".parquet")))
    nrow_sugg <- length(unique(sugg$ensid))
    nrow_filt <- length(unique(filt$ensid))

    prop_filtered_ens <- data.table(location = loc, target_type = targ, k = k, prop_filt = 1 - nrow_filt/nrow_sugg, num_ens = nrow_filt)

    return(prop_filtered_ens)


  })
  prop_filtered[[k-1]] <- rbindlist(prop_filtered_ens)

}

prop_filtered <- rbindlist(prop_filtered)

data.table::fwrite(prop_filtered, here("enscomb-data", paste0("prop_filterered.csv")))



#########################Make ensemble predictions############################
for(k in ks){

  print(k)
  lapply(as.list(loctargets), function(loctarg){
    loc <- substr(loctarg, 0, 2)
    targ <- substr(loctarg, 3, 100)

    print(loctarg)
    #READ in data
    ens_unavail_dat <- arrow::read_parquet(here("enscomb-data", paste0("ens_unavail_bydate_", loctarg, "_k", k, ".parquet")))
    enscombdat <- arrow::read_parquet(here("enscomb-data", paste0("enscomb_", loctarg, "_k", k, ".parquet")))
    #make function call


    res <- make_combens(enscombdat = enscombdat,
                        ens_unavail_dat = ens_unavail_dat,
                        fcdat = fcdat)

    arrow::write_parquet(res, here("enscomb-data", paste0("predictions_enscomb", loctarg, "_k", k, ".parquet")))
    #write data
  }
  )
}
