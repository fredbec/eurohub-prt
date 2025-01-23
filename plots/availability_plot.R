library(data.table)
library(dplyr)
library(here)
library(ggplot2)
library(MetBrewer)
DT <- `[`

source(here("ensvssize", "specs.R"))
source(here("R", "utils-enscomb.R"))
source(here("R", "utils-ext.R"))

model_avail <- 0 #enscomb_specs$indmodel_avail
start_date <- enscomb_specs$start_date
end_date <- enscomb_specs$end_date
ks <- enscomb_specs$ks
loctargets <- enscomb_specs$loctargets
cscale <- "Veronese"

####EDIT
fcdat <- fread(here("data", "depldat.csv"))

num_mods <- function(
    fcdat,
    start_date,
    end_date,
    model_avail,
    sampleens = NULL,
    mode = "long"){

  num_weeks <- fcdat |>
    filter(forecast_date >= as.IDate(start_date)) |> #before: 2021-03-20
    filter(forecast_date <= as.IDate(end_date))  |>
    select(forecast_date) |>
    pull() |>
    unique() |>
    length()

  combdat <- fcdat |>
    filter(!model == "EuroCOVIDhub-ensemble") |>
    filter(forecast_date >= as.IDate(start_date)) |>
    filter(forecast_date <= as.IDate(end_date)) |>
    select(model, forecast_date, location, target_type) |>
    distinct() |>
    setDT() |>
    DT(,  n := .N, by = c("model", "location", "target_type")) |>
    filter(n >= model_avail*num_weeks) #|>
    #select(model, location, target_type) |>
    #distinct()

  return(combdat)
}

availdat <- fcdat |>
    setDT() |>
    DT(, loctarg := paste0(location, target_type)) |>
    split(by = c("loctarg")) |>
    lapply(function(dat) num_mods(dat,
                                  start_date,
                                  end_date,
                                  model_avail)) |>
  rbindlist() |>
  DT(, n := NULL) |>
  DT(,  n := .N, by = c("forecast_date", "location", "target_type")) |>
  DT(, location := factor(location,
                         levels = c("DE", "PL", "CZ", "FR", "GB"),
                         labels = c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")))


colors_manual <- met.brewer(cscale, 5)
names(colors_manual) <- c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")


avail_plot <- ggplot(aes(x = forecast_date, y = n, group = location, color = location), data = availdat) +
  geom_line(lwd = 0.85, position = position_dodge(width = 12)) +
  scale_color_manual(values = colors_manual) +
  theme_masterthesis() %+replace%
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = textsize_y),
        axis.title.x = element_text(size = textsize_y, vjust = -2),
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
  facet_wrap(~target_type)+
  ggplot2::scale_x_date(date_breaks = "1 month",
                        date_labels = "%b %y",
                        expand = c(0,0)) +
  xlab("Forecast Date") +
  ylab("Number of Component Models")

ggsave(here("plots", "availabilityofmodels.pdf"), height = 4, width = 8.5)
