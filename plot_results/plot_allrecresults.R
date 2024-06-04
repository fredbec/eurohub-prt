library(ggplot2)
library(data.table)
library(here)
library(MetBrewer)

source(here("R", "utils-ext.R"))
source(here("ensvssize", "specs.R"))

DT <- `[`

loctargets <- c(enscomb_specs$loctargets, "DECases", "PLCases", "DEDeaths", "PLDeaths")
ks <- enscomb_specs$ks
targetplot <- "Cases"
enstype <- "mean_ensemble"


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

all_pwscores <- NULL
all_pwscores_med <- vector(mode = "list", length = length(ks))
#loctargets <- "GBDeaths"
for(loctarg in loctargets){
  all_pwscores_med <- vector(mode = "list", length = length(ks))
for (k in ks){

  all_pwscores_med[[k-1]] <- data.table::fread(here("ensvssize", "serverrun", enstype, paste0("ens_comb_pwscores", loctarg, "_k", k, ".csv")))

  print(k)

  if(nrow(all_pwscores_med[[k-1]])>0){
    all_pwscores_med[[k-1]] <-  all_pwscores_med[[k-1]] |>
    DT(, k := k) |>
    DT(compare_against == "EuroCOVIDhub-ensemble") |>
    DT(, meanrelskill := mean(scaled_rel_skill), by = "horizon")|>
    DT(, medrelskill := median(scaled_rel_skill), by = "horizon")|>
    DT(, minrelskill := min(scaled_rel_skill), by = "horizon")|>
    DT(, maxrelskill := max(scaled_rel_skill), by = "horizon") |>
    DT(, q05relskill := quantile(scaled_rel_skill, 0.05), by = "horizon") |>
    DT(, q95relskill := quantile(scaled_rel_skill, 0.95), by = "horizon") |>
    DT(, c("model", "horizon", "mean_scores_ratio", "relative_skill", "scaled_rel_skill", "k",
           "meanrelskill", "medrelskill", "minrelskill", "maxrelskill", "q05relskill", "q95relskill")) |>
    unique()

    #if(length(grep("mean*", unique(all_pwscores_med[[k-1]]$model))) > 0){
    #  all_pwscores_med[[k-1]] <- NULL
    #}
  }
}

all_pwscores_med <- rbindlist(all_pwscores_med) |>
  DT(, location := substr(loctarg, 0, 2)) |>
  DT(, target_type := substr(loctarg, 3, 8))

all_pwscores <- rbind(all_pwscores, all_pwscores_med)
}

all_pwscores <- all_pwscores |>
  DT(,horizon := ifelse(horizon == 1, "1-week horizon", "2-week horizon"))

colors = met.brewer(name="Hokusai3", n=3)


ltypes <- c("Ensembles mean rel. skill"="solid","Hub Ensemble rel. skill (=1 by definition)"="dashed")
colfills <- c("min-max range ensembles rel. skill" = 0.25, "q05-q95 range ensembles rel. skill" = 0.5)
pdf(here("plot_results", paste0("ens_comb",targetplot, enstype,".pdf")), width = 8, height = 10)
ggplot(data = all_pwscores |> DT(target_type == targetplot)) +
  geom_line(aes(x = k, y = meanrelskill, linetype = "Ensembles mean rel. skill", color = location), lwd = 1) +
  geom_ribbon(aes(x = k, ymin = minrelskill, ymax = maxrelskill, alpha = "min-max range ensembles rel. skill", fill = location)) +
  geom_ribbon(aes(x = k, ymin = q05relskill, ymax = q95relskill, alpha = "q05-q95 range ensembles rel. skill", fill = location)) +
  geom_hline(aes(yintercept = 1, linetype = "Hub Ensemble rel. skill (=1 by definition)")) +
  scale_x_continuous(breaks = ks) + # Adjust the x-axis limits
  ylab("Scaled relative skill") +
  xlab("k - number of models in recombined ensemble") +
  scale_fill_met_d("Hokusai1") +
  scale_color_met_d("Hokusai1") +
  scale_linetype_manual(name="",values=ltypes) +
  scale_alpha_manual(name="",values=colfills) +
  theme_masterthesis() %+replace%
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none", color = "none") +
  #ggtitle(loctarg) +
  facet_grid(location ~ horizon)
dev.off()
