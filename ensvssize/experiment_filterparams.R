#IMPORTANT: for this to work, lines reading in specs such as model_avail
#directly from specs.R file were commented out

library(here)
library(data.table)
source(here("ensvssize", "specs.R"))
ks <- enscomb_specs$ks
loctargets <- enscomb_specs$loctargets
#loctargets <- "DEDeaths"

maxens <- 1000000

qms <- c(0.3, 0.5)
qes <- c(1, 0.75)
qt <- c(0.5, 0.3)
param_combs <- expand.grid(qms, qes, qt)
names(param_combs) <- c("qm", "qe", "qt")


for(pcomb in 1:nrow(param_combs)){
  datacombs <- expand.grid(ks, loctargets)
  names(datacombs) <- c("k", "loctarg")
  datacombs$nens <- NA

  pcombs <- param_combs[pcomb,]

  model_avail <- pcombs$qm
  availpropmods <- pcombs$qe
  availproptime <- pcombs$qt

  if(availpropmods==1){
    availpropmodsk3 <- 1
  } else {
    availpropmodsk3 <- 0.66
  }

  source(here("ensvssize", "suggested-ens.R"))
  source(here("ensvssize", "filter-ens.R"))

  for(nk in 1:nrow(datacombs)){

    file <- datacombs[nk,]

    enscombs <- data.table::fread(here("enscomb-data", paste0("enscomb_", file$loctarg, "_k", file$k, ".csv")))

    datacombs[nk, "nens"] <- length(unique(enscombs$ensid))
  }

  data.table::fwrite(datacombs, here("enscomb-data", "exp_params",
                          paste0("modelavail", gsub("\\.", "", model_avail), "_availpropmods", gsub("\\.", "", availpropmods), "_availproptime", gsub("\\.", "", availproptime), ".csv")))
}


