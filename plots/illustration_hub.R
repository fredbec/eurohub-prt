library(scoringutils)
library(data.table)
library(here)
library(dplyr)
library(purrr)
library(RColorBrewer)
library(patchwork)
DT <- `[`
source(here("ensvssize", "specs.R"))


start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date

czdat <- fread(here("data", "depldat.csv")) |>
  filter(forecast_date >= as.Date(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(end_date)) |>
  filter(location == "CZ") |>
  filter(target_type == "Cases")

ensdat <- fread(here("data", "median_hubreplica_ensemble.csv")) |>
  filter(forecast_date >= as.Date(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(end_date)) |>
  filter(location == "CZ") |>
  filter(target_type == "Cases") |>
  filter(forecast_date == "2021-10-11")

ensdat50 <- ensdat |>
  filter(quantile %in% c(0.5))|>
  mutate(target_end_date = (as.numeric(target_end_date) %% 18881)/7 + 1)

ensdatshade <- ensdat|>
  filter(quantile %in% c(0.25, 0.75)) |>
  select(model, target_end_date, horizon, quantile, prediction) |>
  setDT() |>
  DT(, quantile := paste0("q", 100*quantile)) |>
  dcast(model + target_end_date + horizon ~ quantile) |>
  mutate(target_end_date = (as.numeric(target_end_date) %% 18881)/7 + 1)

ensdatshade2 <- ensdat|>
  filter(quantile %in% c(0.05, 0.95)) |>
  select(model, target_end_date, horizon, quantile, prediction) |>
  setDT() |>
  DT(, quantile := paste0("q", 100*quantile)) |>
  dcast(model + target_end_date + horizon ~ quantile) |>
  mutate(target_end_date = (as.numeric(target_end_date) %% 18881)/7 + 1)


fcdat <- czdat |>
  filter(forecast_date == "2021-10-11")

fcdat50 <- fcdat |>
  filter(quantile %in% c(0.5))|>
  mutate(target_end_date = (as.numeric(target_end_date) %% 18881)/7 + 1)

fcdatshade <- fcdat |>
  filter(quantile %in% c(0.25, 0.75)) |>
  select(model, target_end_date, horizon, quantile, prediction) |>
  setDT() |>
  DT(, quantile := paste0("q", 100*quantile)) |>
  dcast(model + target_end_date + horizon ~ quantile) |>
  mutate(target_end_date = (as.numeric(target_end_date) %% 18881)/7 + 1)


realdat <- czdat |>
  filter(forecast_date < "2021-09-20" & forecast_date > "2021-09-05") |>
  select(target_end_date, true_value) |>
  distinct() |>
  mutate(target_end_date = (as.numeric(target_end_date) %% 18881)/7 + 1)

textsize_y = 14

plot1 <- ggplot() +
  geom_line(aes(x = target_end_date, y = true_value), data = realdat) +
  geom_point(aes(x = target_end_date, y = true_value), data = realdat, size = 2.5) +
  geom_ribbon(aes(x = target_end_date, ymin = q25, ymax = q75, fill = model), alpha = 0.2, data = fcdatshade) +
  geom_line(aes(x=target_end_date, y = prediction, group = model, color = model),
            data = fcdat50) +
  geom_point(aes(x=target_end_date, y = prediction, group = model, color = model),
             pch = 18,
             size = 3.5,
             data = fcdat50) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  #scale_x_continuous(breaks = unique(realdat$target_end_date)) +
  ylab("Incident Cases") +
  xlab("")+
  scale_y_continuous(breaks = seq(0, 30000, by = 5000), limits = c(0, 32000)) +
  scale_x_continuous(breaks = 1:9,,
                     labels = as.character(seq(18881, 18881 + 8*7, by = 7) |> as.Date() |> format("%b. %d"))) +
  theme_masterthesis()  %+replace%
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = textsize_y,
                                   angle = 45, vjust = 1, hjust=1),

        axis.text.y = element_text(size = textsize_y),
        axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
        strip.text = element_text(size=textsize_y),
        legend.text=element_text(size=textsize_y-2),
        plot.margin = margin(t=20,b=5,r=20,l=20, unit = "pt"),
        plot.title = element_text(hjust = 0.5,
                                  size = textsize_y + 3,
                                  vjust = 5),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = textsize_y-2,
                                     vjust = 6)) +
  ggtitle(label = "Component Forecasts", subtitle = "Czech Rep. Cases, October 2021")


plot2 <- ggplot() +
  geom_line(aes(x = target_end_date, y = prediction, group = model), color = "grey80", data = fcdat50) +
  geom_line(aes(x = target_end_date, y = true_value), data = realdat) +
  geom_point(aes(x = target_end_date, y = true_value), data = realdat, size = 2.5) +
  geom_ribbon(aes(x = target_end_date, ymin = q25, ymax = q75), fill = "deepskyblue4", alpha = 0.2, data = ensdatshade) +
  geom_ribbon(aes(x = target_end_date, ymin = q5, ymax = q95), fill = "deepskyblue4", alpha = 0.1, data = ensdatshade2) +
  geom_line(aes(x=target_end_date, y = prediction, group = model),
            color = "deepskyblue4",
            data = ensdat50) +
  geom_point(aes(x=target_end_date, y = prediction, group = model),
             color = "deepskyblue4",
             pch = 18,
             size = 3.5,
             data = ensdat50) +
  #scale_x_continuous(breaks = unique(realdat$target_end_date)) +
  ylab("Incident Cases") +
  xlab("")+
  scale_y_continuous(breaks = seq(0, 30000, by = 5000), limits = c(0, 32000)) +
  scale_x_continuous(breaks = 1:9,,
                     labels = as.character(seq(18881, 18881 + 8*7, by = 7) |> as.Date() |> format("%b. %d"))) +
  theme_masterthesis()  %+replace%
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = textsize_y,
                                   angle = 45, vjust = 1, hjust=1),

        axis.text.y = element_text(size = textsize_y),
        axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
        strip.text = element_text(size=textsize_y),
        legend.text=element_text(size=textsize_y-2),
        plot.margin = margin(t=20,b=5,r=20,l=20, unit = "pt"),
        plot.title = element_text(hjust = 0.5,
                                  size = textsize_y + 3,
                                  vjust = 5),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = textsize_y-2,
                                     vjust = 6)) +
  ggtitle(label = "Median Ensemble Forecast" , subtitle ="Czech Rep. Cases, October 2021")

pdf("illustration.pdf", width = 10.5, height = 5.5)
plot1 + plot2 +
  plot_layout(guides = "collect")  &
  theme(legend.position = "bottom")
dev.off()
