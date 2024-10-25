library(here)
library(ggplot2)
library(scoringutils)
library(patchwork)
library(dplyr)
library(data.table)

DT <- `[`

source(here("ensvssize", "specs.R"))
source(here("R", "utils-ext.R"))

start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date
loctargets <- enscomb_specs$loctargets

fcdat <- data.table::fread(here("data", "depldat.csv")) |>
  filter(forecast_date >= data.table::as.IDate(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= data.table::as.IDate(end_date))

fcdat_avail <- fcdat |>
  select(model, forecast_date, location, target_type) |>
  distinct() |>
  setDT() |>
  DT(,  availability := .N/52, by = c("model", "location", "target_type"))

avails <- fcdat_avail |>
  select(model, availability, location, target_type) |>
  distinct()


num_mods <- fcdat |>
  select(model, location, target_type, forecast_date) |>
  distinct() |>
  filter(forecast_date >= as.Date(start_date) + 28,
         !grepl("EuroCOVID", model)) |>
  group_by(location, target_type, forecast_date) |>
  summarise(num_mods = n())

bpp <- fcdat |>
  select(model, forecast_date, location, target_type) |>
  distinct() |>
  setDT() |>
  DT(,  num_mods := .N, by = c("forecast_date", "location", "target_type")) |>
  select(forecast_date, location, target_type, num_mods) |>
  distinct() |>
  group_by(location, target_type) |>
  summarise(mean_mods = mean(num_mods)) |>
  mutate(nmods = ifelse(location %in% c("PL", "DE"), 10, 5)) |>
  mutate(avgselec = mean_mods/nmods)

bpset <- fread(here("bestperformers-data", "weights", "best_performers_incl_mods.csv")) |>
  mutate(incl = ifelse(location %in% c("PL", "DE") & nmod == 10 | location %in% c("CZ", "FR", "GB") & nmod == 5, 1, 0)) |>
  filter(incl == 1) |>
  left_join(num_mods, by = c("location", "target_type", "forecast_date")) |>
  filter(nmod < num_mods) |> #exclude instances where no "choosing" happened
  mutate(fct = num_mods/nmod) |>
  ###new
  left_join(bpp, by = c("location", "target_type")) |>
  ##setDT() |>
  #DT(, meanfct := mean(fct), by = c("location", "target_type")) |>
  mutate(fct = fct/mean(fct)) |>
  group_by(model, location, target_type) |>
  summarise(count = sum(fct)) |>
  right_join(avails, by = c("model", "location", "target_type")) |>
  filter(availability > 0.15) |>
  mutate(count = count / (availability)) |>
  filter(model != "EuroCOVIDhub-baseline") |> #not included in selection
  mutate(count = ifelse(is.na(count), 0, count))


textsize_y <- 18

ggplot(bpset, aes(x = count)) +
  geom_histogram(aes(y = after_stat(density)), color="#1f4d3e", fill="#66C2A5", breaks = seq(0, 55, by = 6)) +
  theme_masterthesis() +
  theme(#axis.text.x = element_text(size = textsize_y, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        #strip.text = element_text(size = 8),
        axis.text.y = element_text(size = textsize_y),
        axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
        axis.text.x = element_text(size = textsize_y),
        strip.text = element_text(size=textsize_y),
        legend.text=element_text(size=textsize_y),
        legend.title=element_blank(),
        plot.margin = margin(b=0, l = 5, r = 5, unit = "pt")) +
  ylab("frequency") +
  xlab("") +
  facet_wrap(~target_type
             #labeller = as_labeller(specs$plot_target_label)
             ) #+
  #ylim(0, 0.85)

ggsave(here("plot_results", "bestperformers_nselection.pdf"), width = 10, height = 3)
