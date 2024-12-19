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
  filter(k <= 5) |>
  group_by(target_type, location, k) |>
  mutate(Nmin = min(N)) |>
  filter(Nmin >= min_num_ensembles) |>
  select(target_type, location, N, k, homog) |>
  pivot_wider(values_from = N, names_from = homog, names_prefix = "N_")


ensemble_scores <- ensemble_scores |>
  mutate(target_type = "Deaths") |>
  inner_join(num_ensembles, by = c("target_type", "location", "k"))

summary_scores_k_homog <- ensemble_scores |>
  group_by(location, k, horizon, homog) |>
  summarise(
    n = n(),
    medianrelskill = median(scaled_rel_skill),
    maxrelskill = max(scaled_rel_skill),
    minrelskill = min(scaled_rel_skill),
    q05relskill = quantile(scaled_rel_skill, 0.05),
    q95relskill = quantile(scaled_rel_skill, 0.95))

labeldat = num_ensembles |>
  pivot_longer(cols = c("N_Heterogeneous", "N_Homogeneous"), names_to = "homog", values_to = "N") |>
  mutate(homog = gsub("N_", "", homog)) |>
  mutate(label = paste0("N=",N)) |>
  mutate(vpos = ifelse(location == "DE", 1.95, 2.4)) |>
  mutate(k = ifelse(homog == "Homogeneous", k+0.22, k-0.21)) #only relevant for positioning in plot!


  data.frame(k = c(2.5),
                      vpos = 2,
                      location = "DE",
                      target_type = "Deaths",
                      homog = "Homogenerous",
                      label = "N=50")

# Plot side-by-side ---------------------------------------------------
boxplots_summary <- summary_scores_k_homog |>
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
  facet_grid(rows = vars(location),
             cols = vars(horizon),
             scales = "free_y") +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = NA, colour = NA)) +
  geom_text(aes(x = k, y = vpos, label = label), data = labeldat, size = 2.2)

pdf(here("plot_results", "ens_type_vs_scores.pdf"), width = 6, height = 4)
boxplots_summary
dev.off()
