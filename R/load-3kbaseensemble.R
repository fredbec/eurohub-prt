library(data.table)
library(dplyr)
library(here)
library(ggplot2)
DT <- `[`

nfcsts <- 50
plength <- 10
model_avail <- 0.7

source(here("ensvssize", "specs.R"))

source(here("R", "utils.R"))

preddat <- vector(mode = "list", length = length(enscomb_specs$loctargets))
k <- 0
for(loctarg in enscomb_specs$loctargets){

  k <- k + 1

  loc <- substr(loctarg, 0, 2)
  targ <- substr(loctarg, 3, 8)

  basesets <- fread(here("specs", "ensemble_basesets.csv")) |>
    DT(location == loc & target_type == targ)

  mods <- basesets$model

  ensdat <- fread(here("enscomb-data", paste0("enscomb_suggested_", loc, targ, "_k3.csv"))) |>
    DT(model == mods[1], present := 1) |>
    DT(model == mods[2], present := 1) |>
    DT(model == mods[3], present := 1) |>
    DT(is.na(present), present := 0) |>
    DT(, numpresent := sum(present), by = "ensid") |>
    DT(numpresent == 3)

  ensid_k3 <- unique(ensdat$ensid)

  if(length(ensid) > 1){
    stop("more than one ensemble selected")
  }


  preddat[[k]] <- fread(here("enscomb-data", paste0("predictions_enscomb", loc, targ, "_k3.csv"))) |>
    DT(ensid == ensid_k3) |>
    DT(model == "median_ensemble") |>
    DT(, model := paste0("stablek3_median_ensemble"))

}

preddat <- data.table::rbindlist(preddat)

data.table::fwrite(preddat, here("data", "stablek3ensemble.csv"))
