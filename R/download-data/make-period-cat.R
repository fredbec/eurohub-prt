fc_dates <- seq.Date(as.Date("2021-03-15"),
                     as.Date("2023-03-06"),
                     by = 7)
fc_dates <- c(fc_dates, fc_dates[length(fc_dates)] + 7)
splitter <- c(1,rep(10,10),3) |> cumsum()

#list for plots (overlapping dates to avoid gaps)
period_cat_plots <- lapply(1:11, function(i)
  c(fc_dates[splitter[i]],
    fc_dates[(splitter[i+1])]))

#list with endpoints as correct
period_cat <- lapply(period_cat_plots,
                     function(dat) c(dat[1], dat[2]-7))

#data.table for joining with hub_date
period_cat_dt <- lapply(seq_along(period_cat),
                        function(k) data.table::data.table(forecast_date =
                                                 seq.Date(period_cat[[k]][1],
                                                          period_cat[[k]][2], by = 7),
                                               period_cat = k)) |>
  data.table::rbindlist() |>
  dplyr::mutate(period_cat = factor(period_cat),
         forecast_date = as.Date(forecast_date))


data.table::fwrite(period_cat_dt, here::here("data", "period_cats.csv"))
