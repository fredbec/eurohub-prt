
library(data.table)
library(here)
library(arrow)
library(scoringutils)
library(ggplot2)

DT <- `[`

fcdat <- read_parquet(here("revision", "data", "fcdat_allcountries_filtered.parquet"))

#scoredat <- fcdat |>
#  DT(, .SD, .SDcols = c("model", "location", "target_type", "forecast_date",
#                        "horizon", "quantile", "prediction", "true_value")) |>
#  as_forecast_quantile(observed = "true_value", predicted = "prediction",
#                       quantile_level = "quantile") |>
#  score()

scoredat <- fread(here("revision", "data", "component-model-scores.csv"))

percdat <- scoredat |>
  DT(, q_wis := (frank(wis, ties.method = "average") - 1) / (.N - 1),
     by = .(location, target_type, forecast_date, horizon)) |>
  DT(horizon == 2) |>
  #DT(, avgwis := mean(wis), by = .(location, target_type, forecast_date, model)) |>
  DT(, .SD, .SDcols = c("model", "location", "target_type", "forecast_date", "q_wis")) |>
  DT(, forecast_date := forecast_date + 14) |>
  setnames("q_wis", "relperform_prior")

percdat <- scoredat |>
  DT(, q_wis := (frank(wis, ties.method = "average") - 1) / (.N - 1),
     by = .(location, target_type, forecast_date, horizon)) |>
  DT(horizon == 1) |>
  DT(, .SD, .SDcols = c("model", "location", "target_type", "forecast_date", "q_wis")) |>
  DT(, forecast_date := forecast_date + 7) |>
  setnames("q_wis", "relperform_prior")

# Get unique submission instances (model x location x target x date)
fcpresent <- fcdat |>
  DT(, .SD, .SDcols = c("model", "location", "target_type", "forecast_date")) |>
  unique() |>
  DT(, model_present := 1)

# For each model x location x target combination, expand to all forecast dates
# to create a complete grid of possible submission opportunities
instances <- fcdat |>
  DT(, .SD, .SDcols = c("model", "location", "target_type"))
fcdates <- unique(fcpresent$forecast_date)

fcfull <- instances[,
                    .(forecast_date = fcdates),
                    by = .(model, location, target_type)
]

# Join back actual submissions; missing entries become model_present = 0
fcfull <- fcpresent[fcfull, on = c("model", "location", "target_type", "forecast_date")] |>
  DT(is.na(model_present), model_present := 0)

# Match each submission opportunity with the model's prior relative performance
# (i.e., what the team could have known at that forecast date)
# Observations without a prior score (e.g. first submission) are excluded
fcpres_prevscore <- percdat[fcfull, on = c("model", "location", "target_type", "forecast_date")] |>
  DT(!is.na(relperform_prior))

missing_plot <- ggplot(data = fcpres_prevscore,
       aes(x = as.factor(model_present), y = relperform_prior)) +
           #fill = as.factor(model_present))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("0" = "Did not submit", "1" = "Submitted")) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::label_percent(),
                     name = "Prior relative performance (WIS rank)\n0% = best, 100% = worst") +
  #scale_fill_manual(values = c("0" = "#E07070", "1" = "#5BB8B0")) +
  labs(
    #title = "Prior performance by submission status",
    #subtitle = "Comparison of models that submitted vs. did not submit in a given week,\nconditional of their most recent 1-week forecast",
    x = NULL
  ) +
  facet_wrap(~target_type) +
  theme_minimal(base_size = 13) +
  guides(fill = "none")

missing_plot
ggsave(here("revision", "plot_results", "missingness.pdf"), height = 6, width = 7)

submitted    <- fcpres_prevscore[model_present == 1, relperform_prior]
not_submitted <- fcpres_prevscore[model_present == 0, relperform_prior]

wtest <- wilcox.test(relperform_prior ~ model_present, data = fcpres_prevscore)

cat(sprintf(
  "Submitted:     n = %d, median = %.3f (IQR: %.3f - %.3f)\n",
  length(submitted), median(submitted), quantile(submitted, 0.25), quantile(submitted, 0.75)
))
cat(sprintf(
  "Did not submit: n = %d, median = %.3f (IQR: %.3f - %.3f)\n",
  length(not_submitted), median(not_submitted), quantile(not_submitted, 0.25), quantile(not_submitted, 0.75)
))
cat(sprintf(
  "Wilcoxon rank-sum test: W = %.0f, p = %.3f\n",
  wtest$statistic, wtest$p.value
))

