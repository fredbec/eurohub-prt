library(ggplot2)
library(data.table)
library(here)
library(purrr)

source(here("ensvssize", "specs.R"))

DT <- `[`

loctargets <- enscomb_specs$loctargets
ks <- enscomb_specs$ks
cmpa <- "EuroCOVIDhub-ensemble"

loopdf <- expand.grid(loctarg = loctargets, k = ks, stringsAsFactors = FALSE)


scores_distances <- pmap(loopdf, \(loctarg, k) {

  scores <- arrow::read_parquet(here("enscomb-data", "mean-pwscores", paste0("ens_comb_pwscores", loctarg, "_k", k, ".parquet")))
  distances <- arrow::read_parquet(here("distance-data", paste0("distances", loctarg, "_k", k, ".parquet")))
  distances <- scoringutils:::as_scores(distances, metrics = "mean_distance")
  pw_distances <- scoringutils::get_pairwise_comparisons(distances, metric = "mean_distance")

  cat(loctarg, k, nrow(scores), nrow(distances), "\n")

  if(nrow(scores)>0){
    scores <- scores |>
    DT(, loctarg := loctarg) |>
    DT(, k := k) |>
    DT(, meanrelskill := mean(scaled_rel_skill), by = "horizon")|>
    DT(, medrelskill := median(scaled_rel_skill), by = "horizon")|>
    DT(, minrelskill := min(scaled_rel_skill), by = "horizon")|>
    DT(, maxrelskill := max(scaled_rel_skill), by = "horizon") |>
    DT(, q05relskill := quantile(scaled_rel_skill, 0.05), by = "horizon") |>
    DT(, q95relskill := quantile(scaled_rel_skill, 0.95), by = "horizon") |>
    DT(grepl("^mean_ensemble", model), ensid := sub("[^0-9]+", "", model)) |>
    DT(, c("model", "ensid", "horizon", "relative_skill", "scaled_rel_skill", "k", "loctarg",
           "meanrelskill", "medrelskill", "minrelskill", "maxrelskill", "q05relskill", "q95relskill")) |>
    unique()
    scores <- merge.data.table(scores, distances, by = c("model", "horizon"))
  }

  return(scores)
})

scores_distances <- rbindlist(scores_distances) |>
  DT(, location := substr(loctarg, 0, 2)) |>
  DT(, target_type := substr(loctarg, 3, 8))

scores_distances <- scores_distances |>
  DT(,horizon := ifelse(horizon == 1, "1-week horizon", "2-week horizon")) |>
  DT(, location := factor(location,
                          levels = c("DE", "PL", "CZ", "FR", "GB"),
                          labels = c("Germany", "Poland", "Czech Rep.", "France", "Great Br.")))

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
