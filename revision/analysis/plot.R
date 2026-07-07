library(ggplot2)
library(data.table)
library(dplyr)
library(patchwork)
library(knitr)
library(stringr)
library(tidyr)
library(kableExtra)
library(colorspace)
library(MetBrewer)
library(arrow)
library(here)

source(here("R", "utils-ext.R"))
source(here("specs", "specs.R"))

DT <- `[`
textsize_y = 14

###############################################################################
###########################Illustration plots###################################
###############################################################################
#####hub data illustration (component and ensemble models)
availdat <- arrow::read_parquet(here("revision", "data", "fcdat_allcountries_filtered.parquet")) |>
  DT(, n_models := uniqueN(model), by = .(location, target_type, forecast_date)) |>
  DT(, n_models := n_models - 1) #subtract baseline from count


avail_plot <- ggplot(aes(x = forecast_date, y = n_models, group = location, color = location), data = availdat) +
  geom_line(lwd = 0.85, position = position_dodge(width = 12)) +
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
print(avail_plot)
ggsave(here("revision", "output", "additional", "availability_plot.pdf"), height = 8, width = 12)
