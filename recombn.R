library(data.table)
library(dplyr)
library(here)
library(ggplot2)
DT <- `[`

nfcsts <- 50
plength <- 10
model_avail <- 0.7



fcdat <- fread(here("data", "forecasts.csv"))

comdat <- fcdat |>
  filter(location %in% c("PL", "DE")) |>
  mutate(forecast_date2 =
           lubridate::ceiling_date(forecast_date,
                                   change_on_boundary = FALSE,
                                 unit = "week",
                                 week_start = getOption("lubridate.week.start", 1)) #round down to Saturday
  ) |>
  filter(!model == "EuroCOVIDhub-ensemble") |>
  filter(forecast_date >= as.Date("2021-03-11")) |> #before: 2021-03-20
  filter(forecast_date <= as.Date("2023-03-11"))  |>
  mutate(forecast_date = as.IDate(forecast_date)) #|>
  left_join(period_cat_dt, by = "forecast_date")


comb1 <- comdat |>
  select(model, forecast_date, target_type, location) |>
  distinct() |>
  DT(,  n := .N, by = c("forecast_date", "target_type", "location")) |>
  select(forecast_date, target_type, location, n) |>
  distinct() |>
  mutate("k=4" = choose(n, 4) > nfcsts,
         "k=5" = choose(n, 5) > nfcsts,
         "k=6" = choose(n, 6) > nfcsts,
         "k=7" = choose(n, 7) > nfcsts,
         "k=8" = choose(n, 8) > nfcsts) |>
  melt(id.vars = c("forecast_date", "target_type", "location", "n"))


comb2 <- comdat |>
  #filter(period_cat == pc) |>
  select(model, forecast_date, period_cat, location, target_type) |>
  distinct()|>
  DT(,  n := .N, by = c("model", "period_cat", "location", "target_type")) |>
  filter(n >= model_avail * plength) |>
  select(period_cat, model, location, target_type) |>
  distinct() |>
  DT(,  n := .N, by = c("period_cat", "location", "target_type")) |>
  select(period_cat, target_type, location, n) |>
  distinct() |>
  mutate("k=4" = choose(n, 4),
         "k=5" = choose(n, 5),
         "k=6" = choose(n, 6),
         "k=7" = choose(n, 7),
         "k=8" = choose(n, 8)) |>
  melt(id.vars = c("period_cat", "target_type", "location", "n"),
       variable.name = "k", value.name = "cvg") |>
  mutate(cvg = cut(cvg, breaks = c(-0.1,30,50,50000),
                   labels = c("<30", ">30", ">50")))


ggplot(aes(x = period_cat, y = k, fill = cvg), data = comb2) +
  geom_tile() +
  scale_fill_brewer() +
  #scale_fill_viridis_d(option = "inferno") +
  facet_grid(location~target_type) +
  labs(fill = "n > 50") +
  ylab("k (number of models)") +
  xlab("period") +
  theme_masterthesis()


