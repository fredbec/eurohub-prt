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
#loctargets <- "GBDeaths"
for(loctarg in loctargets){
  all_pwscores_med <- vector(mode = "list", length = length(ks))

    all_pwscores_med <- data.table::fread(here("enscomb-data", "allk-pwscores", enstype, paste0("ens_comb_pwscores", loctarg, "allk.csv")))

    all_pwscores_med <- all_pwscores_med |>
      DT(,model := ifelse(model == "median-hubreplica_k0", "median-hubreplica", model)) |>
      DT(,compare_against := ifelse(compare_against == "median-hubreplica_k0", "median-hubreplica", compare_against)) |>
      DT(compare_against == cmpa) |>
      DT(model != cmpa) |>
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

all_pwscores <- all_pwscores |>
  DT(,horizon := ifelse(horizon == 1, "1-week horizon", "2-week horizon")) |>
  DT(, location := factor(location,
                          levels = c("DE", "PL", "CZ", "FR", "GB"),
                          labels = c("Germany", "Poland", "Czech Rep.", "France", "Great Br.")))

colors = met.brewer(name="Hokusai3", n=3)


ltypes <- c("Ensembles mean rel. skill"="solid","Hub Ensemble rel. skill (=1 by definition)"="dashed")
colfills <- c("min-max range ensembles rel. skill" = 0.25, "q05-q95 range ensembles rel. skill" = 0.5)
pdf(here("plot_results", paste0("ens_comb_","allk_fixedcode",".pdf")), width = 9.5, height = 12)
ggplot(data = all_pwscores) +
#ggplot(data = all_pwscores) +
  geom_line(aes(x = k, y = meanrelskill, linetype = "Ensembles mean rel. skill", color = location), lwd = 1) +
  geom_ribbon(aes(x = k, ymin = minrelskill, ymax = maxrelskill, alpha = "min-max range ensembles rel. skill", fill = location)) +
  geom_ribbon(aes(x = k, ymin = q05relskill, ymax = q95relskill, alpha = "q05-q95 range ensembles rel. skill", fill = location)) +
  geom_hline(aes(yintercept = 1, linetype = "Hub Ensemble rel. skill (=1 by definition)")) +
  scale_x_continuous(breaks = ks) + # Adjust the x-axis limits
  ylab("Scaled relative skill") +
  xlab("k - number of models in recombined ensemble") +
  scale_fill_met_d("Hokusai3") +
  scale_color_met_d("Hokusai3") +
  scale_linetype_manual(name="",values=ltypes) +
  scale_alpha_manual(name="",values=colfills) +
  theme_masterthesis() %+replace%
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none", color = "none") +
  #ggtitle(loctarg) +
  facet_grid(location ~ target_type + horizon, scales = "fixed")
dev.off()

