# Load packages
library(data.table)
library(here)

DT <- `[`

# Download JHU data from the hub - as available on the 1st of September 2022
jhu_cases <- fread(
  "https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/0b7c857/data-truth/JHU/truth_JHU-Incident%20Cases.csv" # nolint
) |>
  DT(, target_type := "Cases")

# Download JHU data from the hub - as available on the 1st of September 2022
jhu_deaths <- fread(
  "https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/0b7c857/data-truth/JHU/truth_JHU-Incident%20Deaths.csv" # nolint
) |>
  DT(, target_type := "Deaths")

jhu <- rbind(jhu_cases, jhu_deaths)

# Format date
jhu[, date := as.Date(date)]

# Order data by date and location
setkey(jhu, location_name, target_type, date)

# Summarise to weekly cases starting on Saturday to Sync with the forecast hubs
truth <- copy(jhu)[,
                   true_value := frollsum(value, n = 7), by = c("location_name", "target_type")
]

# Filter from the 15th of January 2022 to keep only observations with forecasts
truth <- truth[date >= as.Date("2021-03-11")]
truth <- truth[weekdays(date) %in% "Saturday"]

# Drop unnecessary columns
set(truth, j = c("value"), value = NULL)

# Save data
fwrite(truth, here("data", "truth.csv"))
