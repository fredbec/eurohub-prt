#this file holds the main specifications used throughout the analysis of ensemble
#vs size

enscomb_specs <- list(
  #threshold for individual model availability -> models below this threshold are filtered out
  "indmodel_avail" = 0.5,
  #threshold for model availability within ensemble at a given data, for ensemble to be considered 'available'
  "availpropmods" = 0,
  #same as above, just being more lenient with k = 3 (otherwise, all models would have to be available)
  "availpropmodsk3" = 0,
  #proportion of time an ensemble needs to be available to be eligible for pairwise comparison
  "availproptime" = 0.5,
  #forecast date to start analysis
  "start_date" = "2021-03-11",
  #forecast date to end analysis
  "end_date" = "2022-03-11",
  #values of k to consider
  "ks" = seq(2,14),
  #combinations of locations and targets to consider
  #"loctargets" = c("DECases", "DEDeaths", "PLCases", "PLDeaths"),
  "loctargets" = c("DECases", "DEDeaths", "PLCases", "PLDeaths", "GBCases", "GBDeaths", "CZCases", "CZDeaths", "FRCases", "FRDeaths"),
  #maximum number of ensembles to consider for each location-target-combination
  "maxens" = 1000000000000,

  "horizons" = c(1,2),

  "with_anomalies" = FALSE
)
