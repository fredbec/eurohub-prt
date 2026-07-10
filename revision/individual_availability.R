library(ggplot2)
library(tidyr)
library(here)
library(dplyr)

fcdat <- arrow::read_parquet(here("revision", "data", "fcdat_allcountries_filtered.parquet"))

# Create complete grid and mark availability
plot_data <- fcdat |>
  mutate(available = TRUE) |>
  select("forecast_date", "model", "location", "target_type") |>
  distinct() |>
  mutate(available = TRUE)
  #complete(model, forecast_date, location, target_type, fill = list(available = FALSE))|>
  #mutate(forecast_date = as.Date(forecast_date))

availability_plot <- ggplot(plot_data, aes(x = forecast_date, y = model, fill = available)) +
  geom_tile(color = "white", linewidth = 0.3) +
  #scale_fill_manual(
 #   values = c("TRUE" = "#2C7BB6", "FALSE" = "#F0F0F0"),
 ##   labels = c("TRUE" = "Available", "FALSE" = "Missing")
  #) +
  #scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(
    x = "Forecast date",
    y = NULL,
    fill = NULL
  ) +
  facet_grid(location ~ target_type) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )
availability_plot
ggsave(here("individual_availability.pdf"), height = 30, width = 8)
