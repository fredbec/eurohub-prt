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

###############################################################################
###########################Selection ensemble###################################
###############################################################################

source(here("R", "functions-selection-ensemble.R"))

#plot_location_label <- c(`PL` = "Poland", `DE` = "Germany",
#                         `CZ` = "Czech Rep.", `GB` = "United Kingd.",
#                         `FR` = "France")


plot_location_label <- plot_location_label <- c(
  `AT` = "Austria",
  `BE` = "Belgium",
  `BG` = "Bulgaria",
  `CY` = "Cyprus",
  `EE` = "Estonia",
  `ES` = "Spain",
  `FI` = "Finland",
  `GR` = "Greece",
  `HR` = "Croatia",
  `HU` = "Hungary",
  `IT` = "Italy",
  `LT` = "Lithuania",
  `LU` = "Luxembourg",
  `LV` = "Latvia",
  `MT` = "Malta",
  `NO` = "Norway",
  `PT` = "Portugal",
  `RO` = "Romania",
  `SI` = "Slovenia",
  `SK` = "Slovakia"
)

scores_individual <- data.table::fread(here("revision", "output", "selection-ensemble", "scores", "scores-selection-ens.csv"))
scores_average <- data.table::fread(here("revision", "output", "selection-ensemble", "scores", "avg-scores-selection-ens.csv"))

country_levels <- c("Average", "AT", "BE", "BG", "CY", "EE", "ES",
                    "FI", "GR", "HR", "HU", "IT", "LT", "LU",
                    "LV", "MT", "NO", "PT", "RO", "SI", "SK")
##Selection ensemble
all_evals_na <- scores_individual |>
  filter(rel_score < 3, #this is only for the boxplots, and accounted for by showing average values (any mentioned in figure caption)
         model == "median_ensemble") |>
  mutate(location = factor(location, levels = country_levels))
#get average values
all_evals_avg <- scores_average |>
  filter(!is.na(average),
         rel_score < 3,
         model == "median_ensemble") |>
  mutate(location = factor(location, levels = country_levels))

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
    strip.text = element_text(size=12),
    legend.text=element_text(size=14),
    strip.text.x = element_text(angle = 45, hjust = 0, vjust = 0, size = 9),
    legend.title=element_blank(),
    plot.margin = margin(t=5,b=5,r=5,l=5, unit = "pt"))

all_evals_na <- scores_individual |>
  filter(rel_score < 3,
         model == "mean_ensemble")|>
  mutate(location = factor(location, levels = country_levels))
#get average values
all_evals_avg <- scores_average |>
  filter(!is.na(average),
         rel_score < 3,
         model == "mean_ensemble")|>
  mutate(location = factor(location, levels = country_levels))
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
    strip.text = element_text(size=12),
    legend.text=element_text(size=14),
    strip.text.x = element_text(angle = 45, hjust = 0, vjust = 0, size = 9),

    legend.title=element_blank(),
    plot.margin = margin(t=5,b=5,r=5,l=5, unit = "pt"))

all_evals_na <- scores_individual |>
  filter(rel_score < 3,
         model == "weighted.median_ensemble")|>
  mutate(location = factor(location, levels = country_levels))
#get average values
all_evals_avg <- scores_average |>
  filter(!is.na(average),
         rel_score < 3,
         model == "weighted.median_ensemble")|>
  mutate(location = factor(location, levels = country_levels))
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
    strip.text = element_text(size=12),
    legend.text=element_text(size=14),
    strip.text.x = element_text(angle = 45, hjust = 0, vjust = 0, size = 9),
    legend.title=element_blank(),
    plot.margin = margin(t=5,b=5,r=5,l=5, unit = "pt"))

all_evals_na <- scores_individual |>
  filter(rel_score < 3,
         model == "weighted.mean_ensemble")|>
  mutate(location = factor(location, levels = country_levels))
#get average values
all_evals_avg <- scores_average |>
  filter(!is.na(average),
         rel_score < 3,
         model == "weighted.mean_ensemble")|>
  mutate(location = factor(location, levels = country_levels))
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
    strip.text = element_text(size=12),
    legend.text=element_text(size=14),
    strip.text.x = element_text(angle = 45, hjust = 0, vjust = 0, size = 9),

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

pdf(here("revision", "plot_results", "selection-ensemble_boxplot.pdf"),
    height = 15, width = 12)
print(overall_plot)
dev.off()



tileplot <- function(horizons = c(1,2)){

  if(length(horizons) == 4){
    suffix <- "_allhor"
    range_tileplot <- c(0.831,2.05)
  } else {
    suffix <- ""
    range_tileplot <- c(0.862,1.55)
  }

  scores_individual <- data.table::fread(here("revision", "output", "selection-ensemble", "scores", paste0("scores-selection-ens", suffix, ".csv")))
  scores_average <- data.table::fread(here("revision", "output", "selection-ensemble", "scores", paste0("avg-scores-selection-ens", suffix, ".csv")))

  score_table <- scores_individual |>
    group_by(model, location, nmod, target_type) |>
    summarise(avgtg = mean(target_val),
              avgcr = mean(current_val)) |>
    mutate(relval = avgtg/avgcr) |>
    filter(nmod %in% c(3)) |>
    select(model, location, target_type, nmod, relval)


  score_table_wide <- score_table |>
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
  #kable(mytab_wide, format = "latex", booktabs = TRUE,
  #      caption = "Performance Values by Location, Target, and Model")
  print(range_tileplot)
  #new code for plot
  score_table_weighted <- score_table |>
    filter(grepl("weighted*", model)) |>
    mutate(model = gsub("weighted.", "", model)) |>
    mutate(model = ifelse(model == "median_ensemble", "median ens.", "mean ens.")) |>
    mutate(nmod = paste0("k = ", nmod)) |>
    mutate(fac2 = paste0(nmod,",\n", model)) |>
    mutate(location = factor(location,
                           levels = c("Average", "AT", "BE", "BG", "CY", "EE", "ES", "FI", "GR", "HR",
                                      "HU", "IT", "LT", "LU", "LV", "MT", "NO", "PT", "RO", "SI", "SK"),
                           labels = c("Average", "Austria", "Belgium", "Bulgaria", "Cyprus", "Estonia",
                                      "Spain", "Finland", "Greece", "Croatia", "Hungary", "Italy",
                                      "Lithuania", "Luxembourg", "Latvia", "Malta", "Norway", "Portugal",
                                      "Romania", "Slovenia", "Slovakia")))

  textsize_y <- 14

  plot1 <- ggplot(aes(x = location, y = fac2), data = score_table_weighted) +
    geom_tile(aes(fill = relval)) +
    scale_fill_continuous_divergingx("BrBG", mid = 1, limits = range_tileplot, rev = TRUE) + #0.6,1.78 for appendix
    theme_masterthesis()  %+replace%
    theme(legend.title = element_blank(),
          panel.spacing = unit(1, "cm"),
          axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = textsize_y, angle = 45, vjust = -2),
          axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
          strip.text = element_text(size=textsize_y),
          legend.text=element_text(size=textsize_y-2),
          plot.margin = margin(t=0,b=0,r=0,l=0, unit = "pt"),
          plot.title = element_text(hjust = 0.5,
                                    size = textsize_y + 3,
                                    vjust = 2),
          plot.subtitle = element_text(hjust = 0.5,
                                       size = textsize_y-2,
                                       vjust = 6)) +
    geom_text(aes(label = round(relval, 2)), size = 4, color = "black") +
    facet_wrap(~target_type) +
    ylab("") +
    xlab("") +
    scale_x_discrete(drop = TRUE)  +
    ggtitle("Weighted ensembles")+
    guides(
      fill = guide_colorbar(
        barwidth = 20,  # Width of the color bar (in 'npc' units, normalized plot coordinates)
        barheight = 1.5 # Height of the color bar
      ))

  score_table_unweighted <- score_table |>
    filter(!grepl("weighted*", model)) |>
    mutate(model = ifelse(model == "median_ensemble", "median ens.", "mean ens.")) |>
    mutate(nmod = paste0("k = ", nmod)) |>
    mutate(fac2 = paste0(nmod,",\n", model)) |>
    mutate(location = factor(location,
                              levels = c("Average", "AT", "BE", "BG", "CY", "EE", "ES", "FI", "GR", "HR",
                                         "HU", "IT", "LT", "LU", "LV", "MT", "NO", "PT", "RO", "SI", "SK"),
                              labels = c("Average", "Austria", "Belgium", "Bulgaria", "Cyprus", "Estonia",
                                         "Spain", "Finland", "Greece", "Croatia", "Hungary", "Italy",
                                         "Lithuania", "Luxembourg", "Latvia", "Malta", "Norway", "Portugal",
                                         "Romania", "Slovenia", "Slovakia")))

  plot2 <- ggplot(aes(x = location, y = fac2), data = score_table_unweighted) +
    geom_tile(aes(fill = relval)) +
    scale_fill_continuous_divergingx("BrBG", mid = 1, limits = range_tileplot, rev = TRUE) +  #0.6,1.78 for appendix #0.74,1.31 for main
    theme_masterthesis()  %+replace%
    theme(legend.title = element_blank(),
          panel.spacing = unit(1, "cm"),
          axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = textsize_y, angle = 45, vjust = -2),
          axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
          strip.text = element_text(size=textsize_y),
          legend.text=element_text(size=textsize_y-2),
          plot.margin = margin(t=0,b=0,r=0,l=0, unit = "pt"),
          plot.title = element_text(hjust = 0.5,
                                    size = textsize_y + 3,
                                    vjust = 2)) +
    geom_text(aes(label = round(relval, 2)), size = 4, color = "black") +
    facet_wrap(~target_type) +
    ylab("") +
    scale_x_discrete(drop = TRUE)  +# this is actually the default
    xlab("") +
    ggtitle("Equally weighted ensembles") +
    guides(
      fill = guide_colorbar(
        barwidth = 20,  # Width of the color bar (in 'npc' units, normalized plot coordinates)
        barheight = 1.5 # Height of the color bar
      ))

  ovr_plot <- plot1 /
    plot2 +
    plot_layout(guides = "collect")  &
    theme(legend.position = "bottom")

  ovr_plot
}

pdf(here("revision","plot_results", "selection-ensemble_tileplot_allcountries.pdf"), width = 18, height = 6)
print(tileplot())
dev.off()

pdf(here("revision","plot_results", "selection-ensemble_tileplot_allcountries_allhor.pdf"), width = 18, height = 6)
print(tileplot(horizons = 1:4))
dev.off()
