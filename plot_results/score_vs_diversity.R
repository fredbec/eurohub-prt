library(ggplot2)
library(data.table)
library(here)
library(purrr)
library(MetBrewer)

source(here("ensvssize", "specs.R"))
source(here("R", "utils-ext.R"))

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
      DT(, model := sub("mean_ensemble", "median_ensemble", model)) |>
      DT(, model := paste0(model, "_k", k))
    if (nrow(distances) > 0) {
      pw_distances <- scoringutils::get_pairwise_comparisons(
        distances, metric = "mean_distance", by = "horizon"
      ) |>
        DT(, list(model, horizon, mean_distance_relative_skill)) |>
        unique()
      k_scores <- scores |>
        DT(grepl(paste0("_k", k, "$"), model))
      merge.data.table(k_scores, pw_distances, by = c("model", "horizon")) |>
        DT(, k := k) |>
        DT(, loctarg := loctarg)
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
  #DT(, horizon := ifelse(horizon == 1, "1-week horizon", "2-week horizon")) |>
  DT(, location := factor(location,
                          levels = c("DE", "PL", "CZ", "FR", "GB"),
                          labels = c("Germany", "Poland", "Czech Rep.", "France", "United Kingd."))
  )


##############Plotting
p <- function(score_dist_data,
              plot_horizon){
  textsize_y <- 12
  colors_manual <- met.brewer("Veronese", 5)
  names(colors_manual) <- c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")
  size_manual <- c(0.35, 0.5, rep(0.75, 3))
  names(size_manual) <- c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")
  alpha_manual <- c(0.25, 0.3, 0.7, 0.7, 0.7)
  names(size_manual) <- c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")

  plot_horizon <- paste0(plot_horizon, "-week horizon")
  scp <- ggplot(
    score_dist_data[horizon == plot_horizon],
    aes(x = mean_distance_relative_skill, y = relative_skill)
  ) +
    geom_jitter(aes(color = location, size = location, alpha = location)) +
    scale_color_manual(values = colors_manual) +
    scale_size_manual(values = size_manual) +
    scale_alpha_manual(values = alpha_manual) +
    theme_masterthesis() %+replace%
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = textsize_y),
          axis.title.x = element_text(size = textsize_y, vjust = -1),
          axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
          strip.text = element_text(size=textsize_y),
          legend.text=element_text(size=textsize_y-2),
          plot.title = element_text(hjust = 0.5,
                                    size = textsize_y + 3,
                                    vjust = 5),
          plot.subtitle = element_text(hjust = 0.5,
                                       size = textsize_y-2,
                                       vjust = 6)) +
    facet_grid(target_type + horizon ~ location, scales = "free") +
    xlab("Relative mean Cramér distance") +
    ylab("Relative skill") +
    guides(color = "none", size = "none", alpha = "none")
  return(scp)
}
p(scores_distances, 1)
ggsave(here("plot_results", "distance_vs_skill_1week.pdf"), width = 13, height = 4.25)
p(scores_distances, 2)
ggsave(here("plot_results", "distance_vs_skill_2week.pdf"), width = 13, height = 4.25)

scores_distances[,
                 list(pearson = cor(mean_distance_relative_skill, relative_skill)),
                 by = c("location", "target_type")
][, mean(pearson)]
## [1] 0.004306724

saveRDS(scores_distances, here("plot_results", "scores_distances.rds"))
