# eurohub-prt

This repo runs analyses on ensemble composition, using data from the European Covid-19 Forecast Hub (henceforth "Hub"), see https://github.com/european-modelling-hubs/covid19-forecast-hub-europe.


## Contents of the repository
This repo is organized as follows: 

- `R`: Contains scripts needed to load and clean data from the Hub to prep for analyses. Also some utils-type R files containing functions that are loaded in executing scripts (either for data-cleaning scripts or for scripts in e.g. `ensvssize`, see below)

- `ensvssize`: Contains scripts that run an ensemble recombination experiment from available host of Hub models:
  - `master-enscomb.R`: loads the following scripts (all in this folder unless indicated otherwise):
    - `specs.R`: contains all settings used for this analysis (locations, horizon values, etc.). All documented in this file, with commentary.
    - `R/util-enscomb.R`: contains all needed functions for recombination, filtering etc. 
    - *The following scripts are run consecutively, and all use the output of the script before*
    - `suggested-ens.R`: recombines all models into ensembles of size k, subsequently saves into folder `enscomb-data` (local only due to large files), under name `"enscomb_suggested_<indicator:location_target_k>.csv"`
    - `filter-ens.R`: filters suggested ensembles from previous step, according to availability constraints in `specs.R`, subsequently saves filtered ensemble combinations into `enscomb-data` under name `"enscomb_<indicator:location_target_k>.csv"` and saves availability data for ensembles (by data) under name `"ens_unavail_bydate_<indicator:location_target_k>.csv"`
    - `make-ens.R`: actually constructs ensemble predictions according to ensemble combinations from previous step.
  - `save-ensemble.R`: scores ensemble predictions from previous step, via pairwise comparison (this is not called from `master-enscomb.R` as it takes a long time to run and is thus run practically on a remote server.) 
  - `stablek3ensemble_basesets.R` identifies the three models with the highest availability for each location - target combination and saves them into folder `specs` under name `stablek3ensemble_basesets.csv`
  - `select-stablek5ensemblePLDE.R` (older file): identified stable five-model ensembles for Germany and Poland only 
  - `experiment-filterparams.R` and `analyze_experiment_filterparams.R` analyzed the number of available recombined ensembles of size k, according to the filtering parameters for individual model and ensemble availability 

- `specs`: Contains some globally relevant files:
  - `stablek3ensemble_basesets.csv`: csv file (long format) containing the models that make up the stable three model ensembles for each location-target combination 
  

