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

###############################################################################
###########################Illustration plots###################################
###############################################################################
#####hub data illustration (component and ensemble models)
start_date <- specs$start_date
end_date <- specs$end_date

czdat <- read_parquet(here("data", "processed", "fcdat.parquet")) |>
  filter(forecast_date >= as.Date(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(end_date)) |>
  filter(location == "CZ") |>
  filter(target_type == "Cases")

ensdat <- fread(here("data", "processed", "hubreplica-ensemble.csv")) |>
  filter(forecast_date >= as.Date(start_date)) |> #before: 2021-03-20
  filter(forecast_date <= as.Date(end_date)) |>
  filter(location == "CZ") |>
  filter(target_type == "Cases") |>
  filter(forecast_date == "2021-10-11")

ensdat50 <- ensdat |>
  filter(quantile %in% c(0.5))|>
  mutate(target_end_date = (as.numeric(target_end_date) %% 18881)/7 + 1)

ensdatshade <- ensdat|>
  filter(quantile %in% c(0.25, 0.75)) |>
  select(model, target_end_date, horizon, quantile, prediction) |>
  setDT() |>
  DT(, quantile := paste0("q", 100*quantile)) |>
  dcast(model + target_end_date + horizon ~ quantile) |>
  mutate(target_end_date = (as.numeric(target_end_date) %% 18881)/7 + 1)

ensdatshade2 <- ensdat|>
  filter(quantile %in% c(0.05, 0.95)) |>
  select(model, target_end_date, horizon, quantile, prediction) |>
  setDT() |>
  DT(, quantile := paste0("q", 100*quantile)) |>
  dcast(model + target_end_date + horizon ~ quantile) |>
  mutate(target_end_date = (as.numeric(target_end_date) %% 18881)/7 + 1)


fcdat <- czdat |>
  filter(forecast_date == "2021-10-11")

fcdat50 <- fcdat |>
  filter(quantile %in% c(0.5))|>
  mutate(target_end_date = (as.numeric(target_end_date) %% 18881)/7 + 1)

fcdatshade <- fcdat |>
  filter(quantile %in% c(0.25, 0.75)) |>
  select(model, target_end_date, horizon, quantile, prediction) |>
  setDT() |>
  DT(, quantile := paste0("q", 100*quantile)) |>
  dcast(model + target_end_date + horizon ~ quantile) |>
  mutate(target_end_date = (as.numeric(target_end_date) %% 18881)/7 + 1)


realdat <- czdat |>
  filter(forecast_date < "2021-09-20" & forecast_date > "2021-09-05") |>
  select(target_end_date, true_value) |>
  distinct() |>
  mutate(target_end_date = (as.numeric(target_end_date) %% 18881)/7 + 1)

textsize_y = 14

plot1 <- ggplot() +
  geom_line(aes(x = target_end_date, y = true_value), data = realdat) +
  geom_point(aes(x = target_end_date, y = true_value), data = realdat, size = 2.5) +
  geom_ribbon(aes(x = target_end_date, ymin = q25, ymax = q75, fill = model), alpha = 0.2, data = fcdatshade) +
  geom_line(aes(x=target_end_date, y = prediction, group = model, color = model),
            data = fcdat50) +
  geom_point(aes(x=target_end_date, y = prediction, group = model, color = model),
             pch = 18,
             size = 3.5,
             data = fcdat50) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  #scale_x_continuous(breaks = unique(realdat$target_end_date)) +
  ylab("Incident Cases") +
  xlab("")+
  scale_y_continuous(breaks = seq(0, 30000, by = 5000), limits = c(0, 32000)) +
  scale_x_continuous(breaks = 1:9,,
                     labels = as.character(seq(18881, 18881 + 8*7, by = 7) |> as.Date() |> format("%b. %d"))) +
  theme_masterthesis()  %+replace%
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = textsize_y,
                                   angle = 45, vjust = 1, hjust=1),

        axis.text.y = element_text(size = textsize_y),
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
  ggtitle(label = "Component Forecasts", subtitle = "Czech Rep. Cases, October 2021")


plot2 <- ggplot() +
  geom_line(aes(x = target_end_date, y = prediction, group = model), color = "grey80", data = fcdat50) +
  geom_line(aes(x = target_end_date, y = true_value), data = realdat) +
  geom_point(aes(x = target_end_date, y = true_value), data = realdat, size = 2.5) +
  geom_ribbon(aes(x = target_end_date, ymin = q25, ymax = q75), fill = "deepskyblue4", alpha = 0.2, data = ensdatshade) +
  geom_ribbon(aes(x = target_end_date, ymin = q5, ymax = q95), fill = "deepskyblue4", alpha = 0.1, data = ensdatshade2) +
  geom_line(aes(x=target_end_date, y = prediction, group = model),
            color = "deepskyblue4",
            data = ensdat50) +
  geom_point(aes(x=target_end_date, y = prediction, group = model),
             color = "deepskyblue4",
             pch = 18,
             size = 3.5,
             data = ensdat50) +
  #scale_x_continuous(breaks = unique(realdat$target_end_date)) +
  ylab("Incident Cases") +
  xlab("")+
  scale_y_continuous(breaks = seq(0, 30000, by = 5000), limits = c(0, 32000)) +
  scale_x_continuous(breaks = 1:9,,
                     labels = as.character(seq(18881, 18881 + 8*7, by = 7) |> as.Date() |> format("%b. %d"))) +
  theme_masterthesis()  %+replace%
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = textsize_y,
                                   angle = 45, vjust = 1, hjust=1),

        axis.text.y = element_text(size = textsize_y),
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
  ggtitle(label = "Median Ensemble Forecast" , subtitle ="Czech Rep. Cases, October 2021")

dir.create(here("plot_results"), recursive = TRUE, showWarnings = FALSE)
pdf(here("plot_results", "hubdata-illustration.pdf"), width = 10.5, height = 5.5)
illustration_plot <- plot1 + plot2 +
  plot_layout(guides = "collect")  &
  theme(legend.position = "bottom")
print(illustration_plot)
dev.off()


##number of available models

cscale <- "Veronese"

####EDIT
fcdat <- read_parquet(here("data", "processed", "fcdat.parquet"))

num_mods <- function(
    fcdat,
    start_date,
    end_date,
    model_avail = 0,
    sampleens = NULL,
    mode = "long"){

  num_weeks <- fcdat |>
    filter(forecast_date >= as.IDate(start_date)) |> #before: 2021-03-20
    filter(forecast_date <= as.IDate(end_date))  |>
    select(forecast_date) |>
    pull() |>
    unique() |>
    length()

  combdat <- fcdat |>
    filter(!model %in% c("EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline")) |>
    filter(forecast_date >= as.IDate(start_date)) |>
    filter(forecast_date <= as.IDate(end_date)) |>
    select(model, forecast_date, location, target_type) |>
    distinct() |>
    setDT() |>
    DT(,  n := .N, by = c("model", "location", "target_type")) #|>
  #filter(n >= model_avail*num_weeks) #|>
  #select(model, location, target_type) |>
  #distinct()

  return(combdat)
}

availdat <- fcdat |>
  setDT() |>
  DT(, loctarg := paste0(location, target_type)) |>
  split(by = c("loctarg")) |>
  lapply(function(dat) num_mods(dat,
                                start_date,
                                end_date)) |>
  rbindlist() |>
  DT(, n := NULL) |>
  DT(,  n := .N, by = c("forecast_date", "location", "target_type")) |>
  DT(, location := factor(location,
                          levels = c("DE", "PL", "CZ", "FR", "GB"),
                          labels = c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")))


colors_manual <- met.brewer(cscale, 5)
names(colors_manual) <- c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")


avail_plot <- ggplot(aes(x = forecast_date, y = n, group = location, color = location), data = availdat) +
  geom_line(lwd = 0.85, position = position_dodge(width = 12)) +
  scale_color_manual(values = colors_manual) +
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
ggsave(here("plot_results", "model-availability.pdf"), height = 4, width = 8.5)



###############################################################################
###########################Selection ensemble###################################
###############################################################################

source(here("R", "functions-selection-ensemble.R"))

plot_location_label <- c(`PL` = "Poland", `DE` = "Germany",
                         `CZ` = "Czech Rep.", `GB` = "United Kingd.",
                         `FR` = "France")


scores_individual <- data.table::fread(here("output", "selection-ensemble", "scores", "scores-selection-ens.csv"))
scores_average <- data.table::fread(here("output", "selection-ensemble", "scores", "avg-scores-selection-ens.csv"))

##Selection ensemble
all_evals_na <- scores_individual |>
  filter(rel_score < 3, #this is only for the boxplots, and accounted for by showing average values (any mentioned in figure caption)
         model == "median_ensemble")
#get average values
all_evals_avg <- scores_average |>
  filter(!is.na(average),
         rel_score < 3,
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

all_evals_na <- scores_individual |>
  filter(rel_score < 3,
         model == "mean_ensemble")
#get average values
all_evals_avg <- scores_average |>
  filter(!is.na(average),
         rel_score < 3,
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

all_evals_na <- scores_individual |>
  filter(rel_score < 3,
         model == "weighted.median_ensemble")
#get average values
all_evals_avg <- scores_average |>
  filter(!is.na(average),
         rel_score < 3,
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

all_evals_na <- scores_individual |>
  filter(rel_score < 3,
         model == "weighted.mean_ensemble")
#get average values
all_evals_avg <- scores_average |>
  filter(!is.na(average),
         rel_score < 3,
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

pdf(here("plot_results", "selection-ensemble_boxplot.pdf"),
    height = 15, width = 12)
print(overall_plot)
dev.off()



tileplot <- function(horizons = c(1,2)){

  if(length(horizons) == 4){
    suffix <- "_allhor"
    range_tileplot <- c(0.6,1.78)
  } else {
    suffix <- ""
    range_tileplot <- c(0.739,1.31)
  }

  scores_individual <- data.table::fread(here("output", "selection-ensemble", "scores", paste0("scores-selection-ens", suffix, ".csv")))
  scores_average <- data.table::fread(here("output", "selection-ensemble", "scores", paste0("avg-scores-selection-ens", suffix, ".csv")))

  score_table <- scores_individual |>
    group_by(model, location, nmod, target_type) |>
    summarise(avgtg = mean(target_val),
              avgcr = mean(current_val)) |>
    mutate(relval = avgtg/avgcr) |>
    filter(nmod %in% c(5,10)) |>
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

  #new code for plot
  score_table_weighted <- score_table |>
    filter(grepl("weighted*", model)) |>
    mutate(model = gsub("weighted.", "", model)) |>
    mutate(model = ifelse(model == "median_ensemble", "median ens.", "mean ens.")) |>
    mutate(nmod = paste0("k = ", nmod)) |>
    mutate(fac2 = paste0(nmod,",\n", model)) |>
    mutate(location = factor(location,
                             levels = c("Average", "DE", "PL", "CZ", "FR", "GB"),
                             labels = c("Average", "Germany", "Poland", "Czech Rep.", "France", "United Kingd.")))


  textsize_y <- 14

  plot1 <- ggplot(aes(x = location, y = fac2), data = score_table_weighted) +
    geom_tile(aes(fill = relval)) +
    scale_fill_continuous_divergingx("BrBG", mid = 1, limits = range_tileplot, rev = TRUE) + #0.6,1.78 for appendix
    theme_masterthesis()  %+replace%
    theme(legend.title = element_blank(),
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
                             levels = c("Average", "DE", "PL", "CZ", "FR", "GB"),
                             labels = c("Average", "Germany", "Poland", "Czech Rep.", "France", "United Kingd.")))

  plot2 <- ggplot(aes(x = location, y = fac2), data = score_table_unweighted) +
    geom_tile(aes(fill = relval)) +
    scale_fill_continuous_divergingx("BrBG", mid = 1, limits = range_tileplot, rev = TRUE) +  #0.6,1.78 for appendix #0.74,1.31 for main
    theme_masterthesis()  %+replace%
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust=1),
          axis.text.y = element_blank(),
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
    xlab("") +
    ggtitle("Equally weighted ensembles") +
    guides(
      fill = guide_colorbar(
        barwidth = 20,  # Width of the color bar (in 'npc' units, normalized plot coordinates)
        barheight = 1.5 # Height of the color bar
      ))

  ovr_plot <- plot1 + plot2 +
    plot_layout(guides = "collect")  &
    theme(legend.position = "bottom")

  ovr_plot
}

pdf(here("plot_results", "selection-ensemble_tileplot.pdf"), width = 13, height = 4.25)
print(tileplot())
dev.off()

pdf(here("plot_results", "selection-ensemble_tileplot_allhor.pdf"), width = 13, height = 4.25)
print(tileplot(horizons = 1:4))
dev.off()


###############################################################################
#############################ensemble size#####################################
###############################################################################

loctargets <- specs$loctargets #exclude PL and DE for now, since not done yet
ks <- specs$ks
enstypes <- c("median_ensemble", "mean_ensemble")

#model to compare to
cmpa <- "median-hubreplica"

plot_horizon_label <- function(length.out = 4){

  phl <- c(`1` = "1-week horizon",
           `2`= "2-week horizon",
           `PL`= "Poland",
           `DE`= "Germany",
           `FR`= "France",
           `CZ`= "Czech Rep.",
           `GB`= "United Kingd.")

  return(phl[1:length.out])
}

substrRight <- function(x, n=1){
  substr(x, nchar(x)-n+1, nchar(x))
}

make_recomb_plot <- function(enstype, annotation = FALSE){

  #######################read in data and wrangle###############################
  all_pwscores <- NULL
  all_pwscores_med <- vector(mode = "list", length = length(ks))
  bsmod <- NULL
  for(loctarg in loctargets){
    all_pwscores_med <- vector(mode = "list", length = length(ks))

    all_pwscores_med <- read_parquet(here("output", "ensemble-size", paste0("pwscores-", enstype), paste0("ens_comb_pwscores", loctarg, ".parquet")))

    loc <- substr(loctarg, 0, 2)
    targ <- substr(loctarg, 3, 100)

    bsmodel <- all_pwscores_med |>
      copy() |>
      DT(model == "EuroCOVIDhub-baseline_k0") |>
      DT(compare_against == cmpa) |>
      DT(, location := loc) |>
      DT(, target_type := targ)

    bsmod <- rbind(bsmod, bsmodel)

    all_pwscores_med <- all_pwscores_med |>
      DT(,model := ifelse(model == "median-hubreplica_k0", "median-hubreplica", model)) |>
      DT(,compare_against := ifelse(compare_against == "median-hubreplica_k0", "median-hubreplica", compare_against)) |>
      DT(compare_against == cmpa) |>
      DT(model != cmpa) |>
      DT(model != "EuroCOVIDhub-baseline_k0") |>
      DT(, k := as.numeric(str_extract(model, "(?<=_k)\\d+"))) |>
      DT(, model := gsub("_k[0-9]*", "", model)) |>
      DT(, meanrelskill := mean(scaled_rel_skill), by = c("horizon", "k"))|>
      DT(, medrelskill := median(scaled_rel_skill),  by = c("horizon", "k"))|>
      DT(, minrelskill := min(scaled_rel_skill),  by = c("horizon", "k"))|>
      DT(, maxrelskill := max(scaled_rel_skill),  by = c("horizon", "k")) |>
      DT(, q05relskill := quantile(scaled_rel_skill, 0.05),  by = c("horizon", "k")) |>
      DT(, q95relskill := quantile(scaled_rel_skill, 0.95),  by = c("horizon", "k")) |>
      DT(, c("model", "horizon", "mean_scores_ratio", "relative_skill", "scaled_rel_skill", "k",
             "meanrelskill", "medrelskill", "minrelskill", "maxrelskill", "q05relskill", "q95relskill")) |>
      unique() |>
      DT(, location := substr(loctarg, 0, 2)) |>
      DT(, target_type := substr(loctarg, 3, 8))

    all_pwscores <- rbind(all_pwscores, all_pwscores_med)
  }
  #baseline model
  bsmod <- bsmod |>
    DT(,horizon := ifelse(horizon == 1, "1-week horizon", "2-week horizon")) |>
    DT(, location := factor(location,
                            levels = c("DE", "PL", "CZ", "FR", "GB"),
                            labels = c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")))

  all_pwscores <- all_pwscores |>
    DT(,horizon := ifelse(horizon == 1, "1-week horizon", "2-week horizon")) |>
    DT(, location := factor(location,
                            levels = c("DE", "PL", "CZ", "FR", "GB"),
                            labels = c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")))

  colors = met.brewer(name="Hokusai3", n=3)

  textsize_y <- 14
  col_hlines <- "black" #"grey50" for appendix plot
  plot_name <- "pwscores_pivot" #"pwscores_pivot_withrelchanges"  for appendix plot

  ltypes <- c("recombined ensembles\nmedian scaled rel. skill"="solid","Hub ensemble scaled\nrel. skill (=1 by definition)"="dashed", "Baseline scaled\nrelative skill" = "dotted")
  colfills <- c("min-max range: recombined\nensembles scaled rel. skill" = 0.28, "5%-95% quantile: recombined\nensembles scaled rel. skill" = 0.45)
  recomb_plot <- ggplot(data = all_pwscores) +
    #ggplot(data = all_pwscores) +
    geom_line(aes(x = k, y = medrelskill, linetype = "recombined ensembles\nmedian scaled rel. skill", color = location), lwd = 1, show.legend = F) +
    geom_ribbon(aes(x = k, ymin = minrelskill, ymax = maxrelskill, alpha = "min-max range: recombined\nensembles scaled rel. skill", fill = location)) +
    geom_ribbon(aes(x = k, ymin = q05relskill, ymax = q95relskill, alpha = "5%-95% quantile: recombined\nensembles scaled rel. skill", fill = location)) +
    geom_hline(aes(yintercept = 1, linetype = "Hub ensemble scaled\nrel. skill (=1 by definition)"), show.legend = F, col = col_hlines) +
    geom_hline(aes(yintercept = scaled_rel_skill, linetype = "Baseline scaled\nrelative skill"), data = bsmod, col = col_hlines) +
    scale_x_continuous(breaks = ks) + # Adjust the x-axis limits
    ylab("Scaled relative skill") +
    xlab("k - number of models in recombined ensemble") +
    scale_fill_met_d("Veronese") +
    scale_color_met_d("Veronese") +
    scale_linetype_manual(name="",values=ltypes) +
    scale_alpha_manual(name="",values=colfills) +
    theme_masterthesis()  %+replace%
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = textsize_y),
          axis.title.x = element_text(size = textsize_y, vjust = -2),
          axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
          strip.text = element_text(size=textsize_y,
                                    margin = unit(rep(8, 4), "pt")),
          legend.text=element_text(size=textsize_y-2),
          plot.margin = margin(t=20,b=5,r=20,l=20, unit = "pt"),
          plot.title = element_text(hjust = 0.5,
                                    size = textsize_y + 3,
                                    vjust = 5),
          plot.subtitle = element_text(hjust = 0.5,
                                       size = textsize_y-2,
                                       vjust = 6)) +
    guides(fill = "none", color = "none") +
    guides(linetype = guide_legend(override.aes = list(size = 1))) +
    #ggtitle(loctarg) +
    facet_grid(target_type + horizon ~ location, scales = "free")

  if(annotation){
    #plot showing relative changes (for appendix)
    relmedscores <- all_pwscores |>
      DT(,c("horizon", "k", "medrelskill", "location", "target_type")) |>
      unique() |>
      DT(, groups := paste0(horizon, location, target_type)) |>
      DT(order(location, target_type, horizon, k)) |>
      DT(, lag.medrelskill:=c(NA, medrelskill[-.N]), by=groups) |>
      DT(, groups := NULL) |>
      DT(, relchange := (medrelskill - lag.medrelskill)/lag.medrelskill) |>
      DT(, multploty := ifelse(k%%2 == 0, -1, 1)) |>
      DT(, distploty := ifelse(k>4, 0.1, 0.2)) |>
      DT(, ploty := (medrelskill + lag.medrelskill)/2 + multploty*distploty) |>
      DT(, relchange := 100*round(relchange, 3)) |>
      DT(, c("horizon", "k", "relchange", "location", "target_type", "ploty")) |>
      DT(, k := k - 0.5) |> #for plotting purposes
      DT(!is.na(relchange)) |>
      DT(, relchange := paste0(relchange))

    recomb_plot <- recomb_plot +
     geom_text(aes(x = k, y = ploty, label = relchange),
               data = relmedscores, size = 3,
               label.padding = unit(0.05, "lines"), fontface = "bold")
  }

  return(recomb_plot)

}
pdf(here("plot_results", paste0("ensemble-size_", "median_ensemble",".pdf")), width = 12, height = 8)
median_plot <- make_recomb_plot("median_ensemble")
print(median_plot)
dev.off()

pdf(here("plot_results", paste0("ensemble-size_", "mean_ensemble",".pdf")), width = 12, height = 8)
mean_plot <- make_recomb_plot("mean_ensemble")
print(mean_plot)
dev.off()


pdf(here("plot_results", paste0("ensemble-size_", "median_ensemble_annotated",".pdf")), width = 12, height = 8)
median_plot_ann <- make_recomb_plot("median_ensemble", annotation = TRUE)
print(median_plot_ann)
dev.off()




###############################################################################
#############################ensemble diversity#####################################
###############################################################################
plot_cols <- c("Heterogeneous" = "grey30",
               "Homogeneous" = "#165e2d")

source(here("R", "utils-ext.R"))

min_num_ensembles <- specs$min_num_ensembles

ensemble_scores <- arrow::read_parquet(here("output", "ensemble-diversity", "enscomb_scores_with_classification.parquet"))
model_class <- arrow::read_parquet(here("output", "ensemble-diversity", "component_model_classification.parquet"))
models <- arrow::read_parquet(here("output", "ensemble-diversity", "enscomb_with_classification.parquet"))

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


pdf(here("plot_results", "ensemble-diversity.pdf"), width = 7.5, height = 6)
print(retplot)
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



##############Plotting
scores_distances <- arrow::read_parquet(here("output", "ensemble-diversity", "scores-distances.parquet")) |>
  DT(, horizon := paste0(horizon, "-week horizon"))

p <- function(score_dist_data,
              plot_horizon){
  textsize_y <- 12
  colors_manual <- met.brewer("Veronese", 5)
  names(colors_manual) <- c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")
  size_manual <- c(0.35, 0.5, rep(0.75, 3))
  names(size_manual) <- c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")
  alpha_manual <- c(0.25, 0.3, 0.7, 0.7, 0.7)
  names(alpha_manual) <- c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")

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
    facet_grid(target_type ~ location, scales = "free") +
    xlab("Relative mean Cramér distance") +
    ylab("Relative skill") +
    guides(color = "none", size = "none", alpha = "none")
  return(scp)
}
p(scores_distances, 1)
ggsave(here("plot_results", "distance_vs_skill_hor1.pdf"), width = 13, height = 4.25)
p(scores_distances, 2)
ggsave(here("plot_results", "distance_vs_skill_hor2.pdf"), width = 13, height = 4.25)

scores_distances[,
                 list(pearson = cor(mean_distance_relative_skill, relative_skill)),
                 by = c("location", "target_type")
][, mean(pearson)]
## [1] 0.004306724
