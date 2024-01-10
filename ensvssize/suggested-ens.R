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


####EDIT
fcdat <- fread(here("data", "depldat.csv"))

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


  sapply(loctargets,
         function(loctarg) data.table::fwrite(combdat[[loctarg]], here("enscomb-data", paste0("enscomb_", loctarg, "_k", k, ".csv"))))


}

