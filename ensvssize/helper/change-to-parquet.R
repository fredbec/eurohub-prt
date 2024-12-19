library(here)
library(arrow)
library(data.table)
library(stringr)

source(here("ensvssize", "specs.R"))
loctargets <- enscomb_specs$loctargets

DT <- `[`

for(loctarg in loctargets){

  tmp <- data.table::fread(here("enscomb-data", "pwscores", paste0("ens_comb_pwscores", loctarg,".csv")))

  tmp <- tmp |>
    DT(compare_against == "median-hubreplica")
  arrow::write_parquet(tmp, here("enscomb-data", "pwscores", paste0("ens_comb_pwscores", loctarg,".parquet")))

  rm(tmp)

  tmp <- data.table::fread(here("enscomb-data", "pwscores-mean_ensemble", paste0("ens_comb_pwscores", loctarg,".csv")))

  tmp <- tmp |>
    DT(compare_against == "median-hubreplica")
  arrow::write_parquet(tmp, here("enscomb-data", "pwscores-mean_ensemble", paste0("ens_comb_pwscores", loctarg,".parquet")))

  rm(tmp)
}
