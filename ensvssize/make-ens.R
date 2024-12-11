library(data.table)
library(dplyr)
library(here)
DT <- `[`

source(here("ensvssize", "specs.R"))
source(here("R", "utils-enscomb.R"))
source(here("R", "utils-ext.R"))


start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date
ks <- enscomb_specs$ks
loctargets <- enscomb_specs$loctargets
maxens <- enscomb_specs$maxens

fcdat <- arrow::read_parquet(here("data", "depldat.parquet")) |>
  filter(forecast_date >= as.IDate(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.IDate(end_date))





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


