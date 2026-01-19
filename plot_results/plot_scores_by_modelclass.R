library(here)
library(arrow)
library(purrr)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)

plot_cols <- c("Heterogeneous" = "grey30",
               "Homogeneous" = "#165e2d")

source(here("R", "utils-ext.R"))

min_num_ensembles <- 10

ensemble_scores <- arrow::read_parquet(here("model-diversity", "enscomb_scores_with_classification.parquet"))
model_class <- arrow::read_parquet(here("model-diversity", "component_model_classification.parquet"))
models <- arrow::read_parquet(here("model-diversity", "enscomb_with_classification.parquet"))

k_max <- filter(ensemble_scores, homog == "Homogeneous") |>
  group_by(target, horizon) |>
  summarise(k = max(k))

# Count number of ensembles by homogeneous/heterogeneous type
num_ensembles <- ensemble_scores |>
  select(target, homog, ensid, k) |>
  distinct() |>
  group_by(target, homog, k) |>
  summarise(N = n()) |>
  mutate(location = substr(target, 1, 2)) |>
  mutate(target_type = substr(target, 3, 8)) |>
  mutate(target_type = gsub("_", "", target_type)) |>
  filter(k <= 5) |>
  group_by(target_type, location, k) |>
  mutate(Nmin = min(N)) |>
  filter(Nmin >= min_num_ensembles) |>
  select(target_type, location, N, k, homog) |>
  pivot_wider(values_from = N, names_from = homog, names_prefix = "N_")

#only keep instances in ensemble_scores with enough models (as given by num_ensembles)
ensemble_scores <- ensemble_scores |>
  mutate(location = substr(target, 1, 2)) |>
  mutate(target_type = substr(target, 3, 8)) |>
  mutate(target_type = gsub("_", "", target_type)) |>
  inner_join(num_ensembles, by = c("target_type", "location", "k"))




make_boxplot_data <- function(ens_scores_data,
                             num_ens_data,
                             tgttype){

  summary_scores_data_homog <- ens_scores_data |>
    filter(target_type == tgttype) |>
    group_by(location, k, horizon, homog) |>
    summarise(
      n = n(),
      medianrelskill = median(scaled_rel_skill),
      maxrelskill = max(scaled_rel_skill),
      minrelskill = min(scaled_rel_skill),
      q05relskill = quantile(scaled_rel_skill, 0.05),
      q95relskill = quantile(scaled_rel_skill, 0.95)) |>
    mutate(location := factor(location,
                            levels = c("DE", "PL", "CZ", "FR", "GB"),
                            labels = c("Germany", "Poland", "Czech Rep.", "France", "Great Br.")))

  labeldat = num_ens_data |>
    mutate(location := factor(location,
                              levels = c("DE", "PL", "CZ", "FR", "GB"),
                              labels = c("Germany", "Poland", "Czech Rep.", "France", "Great Br."))) |>
    filter(target_type == tgttype) |>
    pivot_longer(cols = c("N_Heterogeneous", "N_Homogeneous"), names_to = "homog", values_to = "N") |>
    mutate(homog = gsub("N_", "", homog)) |>
    mutate(label = paste0("N=",N)) |>
    mutate(vpos = ifelse(location == "DE", 1.95, 2.4)) |>
    mutate(k = ifelse(homog == "Homogeneous", k+0.22, k-0.21)) #only relevant for positioning in plot!

  return(list(sum_scores = summary_scores_data_homog |>
                mutate(target_type = tgttype),
              labeldat = labeldat |>
           mutate(target_type = tgttype)))

}

dat_deaths <- make_boxplot_data(ensemble_scores, num_ensembles, "Deaths")
dat_cases <- make_boxplot_data(ensemble_scores, num_ensembles, "Cases")

alldat_scores <- rbind(dat_deaths$sum_scores, dat_cases$sum_scores)
alldat_labels <- rbind(dat_deaths$labeldat, dat_cases$labeldat)



retplot <- alldat_scores |>
  ggplot(aes(x = k, col = homog)) +
  # geoms
  geom_point(aes(y = medianrelskill),
             alpha = 0.8,
             position = position_dodge(width=0.8)) +
  geom_linerange(aes(ymin = q05relskill,
                     ymax = q95relskill),
                 alpha = 0.6,
                 linewidth = 2,
                 position = position_dodge(width=0.8)) +
  # formatting
  scale_x_continuous(breaks = 2:5, labels = paste0("k = ", 2:5)) +
  geom_hline(aes(yintercept = 1),
             linetype = 2, alpha = 0.5) +
  scale_color_manual(values = plot_cols) +
  labs(#subtitle = "European ensemble forecasts, deaths",
    col = "Ensemble composition",
    x = NULL, y = "Scaled relative skill") +
  facet_grid(rows = vars(target_type, horizon),
             cols = vars(location),
             scales = "free_y") +
  theme_masterthesis() +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = NA, colour = NA)) +
geom_text(aes(x = k, y = vpos, label = label), data = alldat_labels, size = 2.2)


pdf(here("plot_results", "ens_type_vs_scores.pdf"), width = 7.5, height = 6)
retplot
dev.off()

ensemble_scores |>
  group_by(homog, k, location) |>
  summarise(
    n = n(),
    medianrelskill = mean(scaled_rel_skill),
    maxrelskill = max(scaled_rel_skill),
    minrelskill = min(scaled_rel_skill),
    q05relskill = quantile(scaled_rel_skill, 0.05),
    q95relskill = quantile(scaled_rel_skill, 0.95))
