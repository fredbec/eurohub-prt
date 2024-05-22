library(data.table)
library(dplyr)
library(here)
DT <- `[`


source(here("ensvssize", "specs.R"))
loctargets <- enscomb_specs$loctargets
locs <- unique(substr(loctargets, 0, 2))
targs <- unique(substr(loctargets, 3, 100))

set.seed(36)
rdseeds <- expand.grid(location = locs,
                       target_type = targs,
                       kval = enscomb_specs$ks)
rdseeds$seed <- sample.int(10000, size = nrow(rdseeds))
data.table::fwrite(rdseeds, here("enscomb-data", "rdseeds.csv"))

source(here("R", "utils-enscomb.R"))
source(here("ensvssize", "suggested-ens.R"))
source(here("ensvssize", "filter-ens.R"))
source(here("ensvssize", "make-ens.R"))
