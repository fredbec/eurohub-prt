library(here)

source(here("scripts", "load_dat.R"))

dates <- combdat |>
  select(forecast_date, target_end_date, horizon) |>
  distinct() |>
  mutate(weekd = weekdays(forecast_date)) |>
  arrange(forecast_date) |>
  mutate(checkround = target_end_date > forecast_date)

testthat::expect_equal(unique(dates$weekd), "Monday")

testthat::expect_true(all(dates$checkround))

fcdates <- dates |> select(forecast_date) |> pull() |> unique()

testthat::expect_equal(length(fcdates) * 4, nrow(dates))

exp_dates <- seq.Date(min(fcdates), max(fcdates), by = 7)

testthat::expect_equal(fcdates, exp_dates)
