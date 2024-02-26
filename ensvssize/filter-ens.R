library(data.table)
library(dplyr)
library(here)
DT <- `[`

source(here("ensvssize", "specs.R"))
source(here("R", "utils-enscomb.R"))

#model_avail <- enscomb_specs$indmodel_avail
start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date
ks <- enscomb_specs$ks
loctargets <- enscomb_specs$loctargets
#availpropmods <- enscomb_specs$availpropmods
#availpropmodsk3 <- enscomb_specs$availpropmodsk3
#availproptime <- enscomb_specs$availproptime
#maxens <- enscomb_specs$maxens

fcdat <- fread(here("data", "depldat.csv")) |>
  filter(forecast_date >= as.IDate(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.IDate(end_date))

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

    combdat <- data.table::fread(here("enscomb-data", paste0("enscomb_suggested_", loctarg, "_k", k, ".csv")))

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

      #if "too many" ensembles, samle according to sample data
      if(length(prop_ensids) > maxens){
        rdseed <- rdseeds |>
          DT(location == loc & target_type == targ & kval == k)

        set.seed(rdseed$seed)

        sampled_ensids <- data.table(ensid = sort(sample(prop_ensids, maxens)))

        results[[1]] <- inner_join(results[[1]], sampled_ensids, by = "ensid")
        results[[2]] <- inner_join(results[[2]], sampled_ensids, by = "ensid")
      }

      data.table::fwrite(results[[1]], here("enscomb-data", paste0("ens_unavail_bydate_", loctarg, "_k", k, ".csv")))
      data.table::fwrite(results[[2]], here("enscomb-data", paste0("enscomb_", loctarg, "_k", k, ".csv")))
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

    sugg <- data.table::fread(here("enscomb-data", paste0("enscomb_suggested_", loctarg, "_k", k, ".csv")))
    filt <- data.table::fread(here("enscomb-data", paste0("enscomb_", loctarg, "_k", k, ".csv")))
    nrow_sugg <- length(unique(sugg$ensid))
    nrow_filt <- length(unique(filt$ensid))

    prop_filtered_ens <- data.table(location = loc, target_type = targ, k = k, prop_filt = 1 - nrow_filt/nrow_sugg, num_ens = nrow_filt)

    return(prop_filtered_ens)


  })
  prop_filtered[[k-1]] <- rbindlist(prop_filtered_ens)

}

prop_filtered <- rbindlist(prop_filtered)

data.table::fwrite(prop_filtered, here("enscomb-data", paste0("prop_filterered.csv")))


