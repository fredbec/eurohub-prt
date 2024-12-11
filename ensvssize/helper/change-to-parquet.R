library(here)
library(arrow)
library(data.table)

source(here("ensvssize", "specs.R"))
loctargets <- enscomb_specs$loctargets
ks <- 2:14

for(loctarg in loctargets){

  tmp <- data.table::fread(here("enscomb-data", "pwscores", paste0("ens_comb_pwscores", loctarg,".csv")))
  arrow::write_parquet(tmp, here("enscomb-data", "pwscores", paste0("ens_comb_pwscores", loctarg,".parquet")))

  #tmp <- data.table::fread(here("enscomb-data", "mean-pwscores", paste0("ens_comb_pwscores", loctarg, "_k", k, ".csv")))
  #arrow::write_parquet(tmp, here("enscomb-data", "mean-pwscores", paste0("ens_comb_pwscores", loctarg, "_k", k, ".parquet")))

  #tmp <- data.table::fread(here("enscomb-data", "hubreplica-pwscores", paste0("ens_comb_pwscores", loctarg, "_k", k, ".csv")))
  #arrow::write_parquet(tmp, here("enscomb-data", "hubreplica-pwscores", paste0("ens_comb_pwscores", loctarg, "_k", k, ".parquet")))

}
