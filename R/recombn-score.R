library(data.table)
library(dplyr)
library(here)
library(ggplot2)
DT <- `[`

nfcsts <- 100
plength <- 10
model_avail <- 0.8

ks <- c(4, 5, 6, 7)
loc <- "DE"
tt <- "Cases"

set.seed(2912)

source(here("R", "utils-ext.R"))

fcdat <- fread(here("data", "depldat.csv")) |>
  filter(location == "DE",
         target_type == "Cases")

scoredat <- fread(here("data", "scores_period.csv")) |>
  filter(location == "DE", target_type == "Cases")



allres <- NULL
allpreds <- NULL
for(k in 4:7){

  recens <- data.table::fread(here("recombn-ens", paste0("recombn-ens", "_k", k, ".csv"))) |>
    filter(location == "DE",
           target_type == "Cases")

  scores <- vector(mode = "list", length = nfcsts)


  for(per in 2:5){

    per_score <- scoredat |>
      filter(period_cat == per) |>
      select(interval_score) |>
      pull()

    per_dat <- fcdat |>
      filter(period_cat == per)

    per_recs <- recens |>
      dplyr::filter(period_cat == per) |>
      dplyr::mutate(splitter = 1:n()) |>
      DT(, period_cat := NULL)

    per_list <- per_recs |>
      split(by = "splitter") |>
      lapply(function(elem) elem[,1:k])

    enssets <- lapply(per_list, function(elem) per_dat |> score_ensemble(incl = elem)) |>
      unlist()

    enspreds <- lapply(per_list, function(elem) per_dat |> make_ensemble(incl = elem)) |>
      rbindlist(idcol = "idens") |>
      DT(, k := k)



    resdat <- data.table(
      k = rep(k, nfcsts),
      period_cat = rep(per, nfcsts),
      idens = 1:nfcsts,
      ivscores = enssets,
      rel_ivscores = enssets/per_score
    )

    allres <- rbind(allres, resdat)
    allpreds <- rbind(allpreds, enspreds)
  }

}

data.table::fwrite(allres, here("recombn-ens", "recombn-scores.csv"))
data.table::fwrite(allpreds, here("recombn-ens", "recombn-preds.csv"))


