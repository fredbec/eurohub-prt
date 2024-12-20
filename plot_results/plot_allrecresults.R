library(ggplot2)
library(data.table)
library(here)
library(MetBrewer)
library(stringr)

source(here("R", "utils-ext.R"))
source(here("ensvssize", "specs.R"))

DT <- `[`

loctargets <- enscomb_specs$loctargets #exclude PL and DE for now, since not done yet
ks <- enscomb_specs$ks
targetplot <- "Cases"
enstype <- "median_ensemble"

if(enstype == "against_stablek3"){
  cmpa <- "stablek3_median_ensemble"
} else{
  cmpa <- "median-hubreplica"
}

plot_horizon_label <- function(length.out = 4){

  phl <- c(`1` = "1-week horizon",
           `2`= "2-week horizon",
           `PL`= "Poland",
           `DE`= "Germany",
           `FR`= "France",
           `CZ`= "Czech Rep.",
           `GB`= "Great Br.")

  return(phl[1:length.out])
}

substrRight <- function(x, n=1){
  substr(x, nchar(x)-n+1, nchar(x))
}


all_pwscores <- NULL
all_pwscores_med <- vector(mode = "list", length = length(ks))
bsmod <- NULL
#loctargets <- "GBDeaths"
for(loctarg in loctargets){
  all_pwscores_med <- vector(mode = "list", length = length(ks))

    all_pwscores_med <- data.table::fread(here("enscomb-data", "pwscores", paste0("ens_comb_pwscores", loctarg, ".csv")))

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

bsmod <- bsmod |>
  DT(,horizon := ifelse(horizon == 1, "1-week horizon", "2-week horizon")) |>
  DT(, location := factor(location,
                          levels = c("DE", "PL", "CZ", "FR", "GB"),
                          labels = c("Germany", "Poland", "Czech Rep.", "France", "Great Br.")))

all_pwscores <- all_pwscores |>
  DT(,horizon := ifelse(horizon == 1, "1-week horizon", "2-week horizon")) |>
  DT(, location := factor(location,
                          levels = c("DE", "PL", "CZ", "FR", "GB"),
                          labels = c("Germany", "Poland", "Czech Rep.", "France", "Great Br.")))

colors = met.brewer(name="Hokusai3", n=3)

textsize_y <- 16

ltypes <- c("recombined\nensembles\nmedian rel. skill"="solid","Hub ensemble\nrel. skill\n(=1 by definition)"="dashed", "Baseline\nrel. skill" = "dotted")
colfills <- c("min-max range\nensembles rel. skill" = 0.28, "q05-q95 range\nensembles rel. skill" = 0.45)
pdf(here("plot_results", paste0("ens_comb_","pwscores",".pdf")), width = 12, height = 10)
ggplot(data = all_pwscores) +
#ggplot(data = all_pwscores) +
  geom_line(aes(x = k, y = medrelskill, linetype = "recombined\nensembles\nmedian rel. skill", color = location), lwd = 1) +
  geom_ribbon(aes(x = k, ymin = minrelskill, ymax = maxrelskill, alpha = "min-max range\nensembles rel. skill", fill = location)) +
  geom_ribbon(aes(x = k, ymin = q05relskill, ymax = q95relskill, alpha = "q05-q95 range\nensembles rel. skill", fill = location)) +
  geom_hline(aes(yintercept = 1, linetype = "Hub ensemble\nrel. skill\n(=1 by definition)")) +
  geom_hline(aes(yintercept = scaled_rel_skill, linetype = "Baseline\nrel. skill"), data = bsmod) +
  scale_x_continuous(breaks = ks) + # Adjust the x-axis limits
  ylab("Scaled relative skill") +
  xlab("k - number of models in recombined ensemble") +
  scale_fill_met_d("Degas") +
  scale_color_met_d("Degas") +
  scale_linetype_manual(name="",values=ltypes) +
  scale_alpha_manual(name="",values=colfills) +
  theme_masterthesis()  %+replace%
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 13),
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
  guides(fill = "none", color = "none") +
  #ggtitle(loctarg) +
  facet_grid(location ~ target_type + horizon, scales = "free")
dev.off()

