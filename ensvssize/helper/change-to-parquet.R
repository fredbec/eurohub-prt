library(here)
library(arrow)
library(data.table)

source(here("ensvssize", "specs.R"))
loctargets <- enscomb_specs$loctargets
ks <- 2:14

for(k in ks){
  for(loctarg in loctargets){

    tmp <- data.table::fread(here("enscomb-data", "median-pwscores", paste0("ens_comb_pwscores", loctarg, "_k", k, ".csv")))
    arrow::write_parquet(tmp, here("enscomb-data", "median-pwscores", paste0("ens_comb_pwscores", loctarg, "_k", k, ".parquet")))


    tmp <- data.table::fread(here("enscomb-data", "mean-pwscores", paste0("ens_comb_pwscores", loctarg, "_k", k, ".csv")))
    arrow::write_parquet(tmp, here("enscomb-data", "mean-pwscores", paste0("ens_comb_pwscores", loctarg, "_k", k, ".parquet")))
  }
}
