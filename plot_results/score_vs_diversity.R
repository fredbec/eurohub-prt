library(ggplot2)
library(data.table)
library(here)
library(purrr)

source(here("ensvssize", "specs.R"))

DT <- `[`

loctargets <- enscomb_specs$loctargets
ks <- enscomb_specs$ks
cmpa <- "median-hubreplica"

scores_distances <- map(loctargets, \(loctarg) {
  cat(loctarg, "\n")
  scores <- arrow::read_parquet(
    here(
      "enscomb-data", "pwscores",
      paste0("ens_comb_pwscores", loctarg, ".parquet")
    )
  )
  comparison <- map(ks, \(k) {
    cat(k, "\n")
    distances <- arrow::read_parquet(
      here(
        "distance-data", paste0("distances", loctarg, "_k", k, ".parquet")
      )
    ) |>
      scoringutils:::as_scores(metrics = "mean_distance") |>
      mutate(model = sub("mean_ensemble", "median_ensemble", model)) |>
      mutate(model = paste0(model, "_k", k))
    if (nrow(distances) > 0) {
      pw_distances <- scoringutils::get_pairwise_comparisons(
        distances, metric = "mean_distance", by = "horizon"
      ) |>
        select(model, horizon, mean_distance_relative_skill) |>
        unique()
      k_scores <- scores |>
        filter(grepl(paste0("_k", k, "$"), model))
      merge.data.table(k_scores, pw_distances, by = c("model", "horizon")) |>
        mutate(k = k)
    } else {
      NULL
    }
  }) |>
    rbindlist(fill = TRUE)
})

scores_distances <- rbindlist(scores_distances) |>
  DT(, location := substr(loctarg, 0, 2)) |>
  DT(, target_type := substr(loctarg, 3, 8))

scores_distances <- scores_distances |>
  DT(, horizon := ifelse(horizon == 1, "1-week horizon", "2-week horizon")) |>
  DT(, location := factor(location,
    levels = c("DE", "PL", "CZ", "FR", "GB"),
    labels = c("Germany", "Poland", "Czech Rep.", "France", "Great Br."))
  )

p <- ggplot(
  scores_distances[horizon == "2-week horizon"],
  aes(x = mean_distance, y = relative_skill)
) +
  geom_jitter() +
  theme_bw() +
  facet_wrap(location ~ target_type, scales = "free") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  xlab("Mean Cramér distance") + ylab("Relative skill")

ggsave(here("plot_results", "distance_vs_skill.pdf"))

scores_distances[,
  list(pearson = cor(mean_distance, relative_skill)),
  by = c("location", "target_type")
]
