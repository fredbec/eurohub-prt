library(data.table)
library(dplyr)
library(here)
DT <- `[`

source(here("specs", "specs.R"))
source(here("R", "functions-ensemble-size.R"))
source(here("R", "utils-ext.R"))

model_avail <- specs$indmodel_avail
start_date <- specs$start_date
end_date <- specs$end_date
ks <- specs$ks
loctargets <- specs$loctargets
availpropmods <- specs$availpropmods
availpropmodsk3 <- availpropmods #same as above (legacy)
availproptime <- specs$availproptime



##############################Suggest ensembles################################
fcdat <- arrow::read_parquet(here("data", "processed", "fcdat.parquet"))|>
  filter(forecast_date >= as.Date(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(end_date))

dir.create(here("output", "ensemble-size", "ensemble-combinations"), recursive = TRUE, showWarnings = FALSE)
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
         function(loctarg) arrow::write_parquet(combdat[[loctarg]], here("output", "ensemble-size", "ensemble-combinations", paste0("enscomb_suggested_", loctarg, "_k", k, ".parquet"))))
}

##############################Filter ensembles################################

for(k in ks){

  if(k == 3){
    availpropmods_k <- availpropmodsk3
  } else {
    availpropmods_k <- availpropmods
  }

  lapply(as.list(loctargets), function(loctarg){
    loc <- substr(loctarg, 0, 2)
    targ <- substr(loctarg, 3, 100)

    combdat <- arrow::read_parquet(here("output", "ensemble-size", "ensemble-combinations", paste0("enscomb_suggested_", loctarg, "_k", k, ".parquet")))

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

      dir.create(here("output", "ensemble-size", "ensemble-combinations"), recursive = TRUE, showWarnings = FALSE)
      arrow::write_parquet(results[[1]], here("output", "ensemble-size", "ensemble-combinations", paste0("ens_unavail_bydate_", loctarg, "_k", k, ".parquet")))
      arrow::write_parquet(results[[2]], here("output", "ensemble-size", "ensemble-combinations", paste0("enscomb_", loctarg, "_k", k, ".parquet")))
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

    sugg <- arrow::read_parquet(here("output", "ensemble-size", "ensemble-combinations",paste0("enscomb_suggested_", loctarg, "_k", k, ".parquet")))
    filt <- arrow::read_parquet(here("output", "ensemble-size", "ensemble-combinations", paste0("enscomb_", loctarg, "_k", k, ".parquet")))
    nrow_sugg <- length(unique(sugg$ensid))
    nrow_filt <- length(unique(filt$ensid))

    prop_filtered_ens <- data.table(location = loc, target_type = targ, k = k, prop_filt = 1 - nrow_filt/nrow_sugg, num_ens = nrow_filt)

    return(prop_filtered_ens)


  })
  prop_filtered[[k-1]] <- rbindlist(prop_filtered_ens)

}

prop_filtered <- rbindlist(prop_filtered)

data.table::fwrite(prop_filtered, here("output", "ensemble-size", "ensemble-combinations", paste0("prop_filterered.csv")))



#########################Make ensemble predictions############################
for(k in ks){

  print(k)
  lapply(as.list(loctargets), function(loctarg){
    loc <- substr(loctarg, 0, 2)
    targ <- substr(loctarg, 3, 100)

    #READ in data
    ens_unavail_dat <- arrow::read_parquet(here("output", "ensemble-size", "ensemble-combinations",paste0("ens_unavail_bydate_", loctarg, "_k", k, ".parquet")))
    enscombdat <- arrow::read_parquet(here("output", "ensemble-size", "ensemble-combinations", paste0("enscomb_", loctarg, "_k", k, ".parquet")))
    #make function call


    res <- make_combens(enscombdat = enscombdat,
                        ens_unavail_dat = ens_unavail_dat,
                        fcdat = fcdat)

    dir.create(here("output", "ensemble-size", "ensemble-forecasts"), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(res, here("output", "ensemble-size", "ensemble-forecasts", paste0("predictions_enscomb", loctarg, "_k", k, ".parquet")))
    #write data
  }
  )
}
