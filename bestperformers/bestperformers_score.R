library(ggplot2)
library(dplyr)
library(patchwork)
library(knitr)
library(tidyr)
library(kableExtra)
library(MetBrewer)

library(here)
#source(here("specs", "specs.R"))

source(here("bestperformers", "bestperformers_function.R"))

horizons <- c(1,2)

median_ens <-
  data.table::fread(here("bestperformers-data", "median_ensemble.csv")) |>
  filter(horizon %in% horizons)


excl_from_bp <-
  data.table::fread(here("bestperformers-data", "excl_from_bp.csv"))

su_cols <- c("model", "forecast_date", "quantile", "horizon",
             "target_type", "location", "target_end_date",
             "prediction", "true_value")
plot_location_label <- c(`PL` = "Poland", `DE` = "Germany",
                         `CZ` = "Czech Rep.", `GB` = "United Kingd.",
                         `FR` = "France")


all_evals <- NULL
for(nm in c(3,5,8,10)){
  bestperforms_mean <- data.table::fread(here("bestperformers-data",
                                              paste0("best_performers_ensemble_mean_nmod", nm, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)

  bestperforms_median <- data.table::fread(here("bestperformers-data",
                                                paste0("best_performers_ensemble_median_nmod", nm, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)

  bestperforms_invscore_mean <- data.table::fread(here("bestperformers-data",
                                                       paste0("best_performers_ensemble_invscore_mean_nmod", nm, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)

  bestperforms_invscore_median <- data.table::fread(here("bestperformers-data",
                                                         paste0("best_performers_ensemble_invscore_median_nmod", nm, ".csv"))) |>
    mutate(nmod = nm) |>
    anti_join(excl_from_bp, by = c("location", "target_type", "nmod")) |>
    select(-nmod)



  eval_mean <- fast_eval(bestperforms_mean, median_ens,
                         su_cols = su_cols,
                         strat_by = c("model", "location", "target_type", "forecast_date"),
                         return_eval = TRUE,
                         comp_avg_by = c("forecast_date", "target_type")) |>
    mutate(location = ifelse(is.na(location), "Average", location)) |>
    comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
    mutate(nmod = nm)


  eval_median <- fast_eval(bestperforms_median, median_ens,
                           su_cols = su_cols,
                           strat_by = c("model", "location", "target_type", "forecast_date"),
                           return_eval = TRUE,
                           comp_avg_by = c("forecast_date", "target_type")) |>
    mutate(location = ifelse(is.na(location), "Average", location)) |>
    comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
    mutate(nmod = nm)


  eval_invscore_mean <- fast_eval(bestperforms_invscore_mean, median_ens,
                                  su_cols = su_cols,
                                  strat_by = c("model", "location", "target_type", "forecast_date"),
                                  return_eval = TRUE,
                                  comp_avg_by = c("forecast_date", "target_type")) |>
    mutate(location = ifelse(is.na(location), "Average", location)) |>
    comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
    mutate(nmod = nm)


  eval_invscore_median <- fast_eval(bestperforms_invscore_median, median_ens,
                                    su_cols = su_cols,
                                    strat_by = c("model", "location", "target_type", "forecast_date"),
                                    return_eval = TRUE,
                                    comp_avg_by = c("forecast_date", "target_type")) |>
    mutate(location = ifelse(is.na(location), "Average", location)) |>
    comp_avg_by_extra(comp_avg_by = c("target_type", "location")) |>
    mutate(nmod = nm)


  all_evals <- all_evals |>
    rbind(eval_mean) |>
    rbind(eval_median) |>
    rbind(eval_invscore_mean) |>
    rbind(eval_invscore_median)
}


all_evals_na <- all_evals |>
  filter(!is.na(forecast_date), #this is only to exclude average values(in next dataframe)
         rel_score < 3,
         model == "median_ensemble")
#get average values
all_evals_avg <- all_evals |>
  filter(!is.na(average),
         rel_score < 3,
         is.na(forecast_date),
         model == "median_ensemble")
plot1 <- best_performers_boxplot(all_evals_na,
                                 all_evals_avg,
                                 labeller_locs = c(plot_location_label,
                                                   Average = "Average")) +
  theme(#axis.text.x = element_text(size = textsize_y, angle = 90, hjust = .5, vjust = .5, face = "plain"),
    #strip.text = element_text(size = 8),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    strip.text = element_text(size=14),
    legend.text=element_text(size=14),

    legend.title=element_blank(),
    plot.margin = margin(t=5,b=5,r=5,l=5, unit = "pt"))

all_evals_na <- all_evals |>
  filter(!is.na(forecast_date), #this is only to exclude average values (in next dataframe)
         rel_score < 3,
         model == "mean_ensemble")
#get average values
all_evals_avg <- all_evals |>
  filter(!is.na(average),
         rel_score < 3,
         is.na(forecast_date),
         model == "mean_ensemble")
plot2 <- best_performers_boxplot(all_evals_na,
                                 all_evals_avg,
                                 labeller_locs = c(plot_location_label,
                                                   Average = "Average")) +
  theme(#axis.text.x = element_text(size = textsize_y, angle = 90, hjust = .5, vjust = .5, face = "plain"),
    #strip.text = element_text(size = 8),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    strip.text = element_text(size=14),
    legend.text=element_text(size=14),

    legend.title=element_blank(),
    plot.margin = margin(t=5,b=5,r=5,l=5, unit = "pt"))

all_evals_na <- all_evals |>
  filter(!is.na(forecast_date), #this is only to exclude average values(in next dataframe)
         rel_score < 3,
         model == "weighted.median_ensemble")
#get average values
all_evals_avg <- all_evals |>
  filter(!is.na(average),
         rel_score < 3,
         is.na(forecast_date),
         model == "weighted.median_ensemble")
plot3 <- best_performers_boxplot(all_evals_na,
                                 all_evals_avg,
                                 labeller_locs = c(plot_location_label,
                                                   Average = "Average")) +
  theme(#axis.text.x = element_text(size = textsize_y, angle = 90, hjust = .5, vjust = .5, face = "plain"),
    #strip.text = element_text(size = 8),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    strip.text = element_text(size=14),
    legend.text=element_text(size=14),

    legend.title=element_blank(),
    plot.margin = margin(t=5,b=5,r=5,l=5, unit = "pt"))

all_evals_na <- all_evals |>
  filter(!is.na(forecast_date), #this is only to exclude average values (in next dataframe)
         rel_score < 3,
         model == "weighted.mean_ensemble")
#get average values
all_evals_avg <- all_evals |>
  filter(!is.na(average),
         rel_score < 3,
         is.na(forecast_date),
         model == "weighted.mean_ensemble")
plot4 <- best_performers_boxplot(all_evals_na,
                                 all_evals_avg,
                                 labeller_locs = c(plot_location_label,
                                                   Average = "Average")) +
  theme(#axis.text.x = element_text(size = textsize_y, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        #strip.text = element_text(size = 8),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size=14),
        legend.text=element_text(size=14),

        legend.title=element_blank(),
        plot.margin = margin(t=5,b=5,r=5,l=5, unit = "pt"))

overall_plot <-
  (plot1) /
  (plot2) /
  (plot3) /
  (plot4)+
  plot_layout(guides = "collect",
              heights = c(1, 1,1,1)) &
  plot_annotation(tag_levels = 'I')

pdf(here("plots", "best_performers_boxplot_cscale.pdf"),
    height = 15, width = 12)
overall_plot
dev.off()


mytab <- all_evals |>
  group_by(model, location, nmod, target_type) |>
  summarise(avgtg = mean(target_val),
            avgcr = mean(current_val)) |>
  mutate(relval = avgtg/avgcr) |>
  filter(nmod %in% c(5,10)) |>
  select(model, location, target_type, nmod, relval)


mytab_wide <- mytab |>
  dplyr::mutate(relval = round(relval,2)) |>
  mutate(relval = as.character(relval)) |>
  mutate(relval = ifelse(is.na(relval), "--", relval)) |>
  mutate(model = factor(model, levels = c("median_ensemble",
                                          "weighted.median_ensemble",
                                          "mean_ensemble",
                                          "weighted.mean_ensemble"),
                        labels = c("median - unw.",
                                   "median - weighted",
                                   "mean - unw.",
                                   "mean - weighted"),
                        ordered = TRUE)) |>
  pivot_wider(names_from = location, values_from = relval) |>
  arrange(target_type, nmod, model) |>
  select(target_type, nmod, model, everything())


kable(mytab_wide, format = "latex", booktabs = TRUE,
      caption = "Performance Values by Location, Target, and Model")
