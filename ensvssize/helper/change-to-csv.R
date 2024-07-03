library(here)
library(arrow)
library(data.table)

source(here("ensvssize", "specs.R"))
loctargets <- enscomb_specs$loctargets
ks <- 2:14

for(k in ks){
  for(loctarg in loctargets){

    tmp <- arrow::read_parquet(here("enscomb-data", paste0("predictions_enscomb", loctarg, "_k", k, ".parquet")))
    data.table::fwrite(tmp, here("enscomb-data", paste0("predictions_enscomb", loctarg, "_k", k, ".csv")))
  }
}
