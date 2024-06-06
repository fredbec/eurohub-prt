# eurohub-prt

This repo runs analyses on ensemble composition, using data from the European Covid-19 Forecast Hub (henceforth "Hub"), see https://github.com/european-modelling-hubs/covid19-forecast-hub-europe.


## Contents of the repository
This repo is organized as follows: 

- `R`: Contains scripts needed to load and clean data from the Hub to prep for analyses. Also some utils-type R files containing functions that are loaded in executing scripts (either for data-cleaning scripts or for scripts in e.g. `ensvssize`, see below)

- `ensvssize`: Contains scripts that run an ensemble recombination experiment from available host of Hub models:
  - `master-enscomb.R`: loads the following scripts (all in this folder unless indicated otherwise):
    - `specs.R`: contains all settings used for this analysis (locations, horizon values, etc.). All documented in this file, with commentary.
    - `R/util-enscomb.R`: contains all needed functions for recombination, filtering etc. 
    - `suggested-ens.R`: recombines all models into ensembles of size k, subsequently saves into `here("enscomb-data", "enscomb-suggested_<indicator:location_target_k>.csv")`. Folder `enscomb-data` is local only due to large files.
