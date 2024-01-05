# Load packages
library(data.table)
library(here)

# Load already processed forecasts and get target date range
target_forecasts <- fread(here("data", "forecasts.csv")) |>
  DT(, .(target_end_date, location)) |>
  unique()

# Download European Hub anomalies list as present on the 1st September 2022
anomalies <- fread(
  "https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/f6922c3e4bdcb055abcbba8e73472afacac4cf40/data-truth/anomalies/anomalies.csv" # nolint
)

# Restrict to just incident case targets
anomalies <- anomalies |>
  setnames("target_variable", "target_type") |>
  DT(, target_type := fcase(target_type == "inc case", "Cases",
                            target_type == "inc death", "Deaths",
                            target_type == "inc hosp", "Hosp")) |>
  DT(target_type %in% c("Cases", "Deaths"))

# Restrict to just anomalies in scope for our target forecasts
in_scope_anomalies <- anomalies[
  target_forecasts, on = c("location", "target_end_date"),
  nomatch = NULL
]

# Reorder anomalies for iterpretatio
setkey(in_scope_anomalies, location_name, target_end_date)

# Save anomalies
fwrite(in_scope_anomalies, here("data", "anomalies.csv"))
