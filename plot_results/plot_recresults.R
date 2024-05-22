library(ggplot2)
library(data.table)
library(here)
library(MetBrewer)

source(here("R", "utils-ext.R"))
source(here("ensvssize", "specs.R"))

DT <- `[`

loctarg <- "DEDeaths"
ks <- enscomb_specs$ks

all_pwscores <- vector(mode = "list", length = length(ks))

for (k in ks){

  all_pwscores[[k-1]] <- data.table::fread(here("ensvssize", "serverrun", "mean_ensemble", paste0("ens_comb_pwscores", loctarg, "_k", k, ".csv"))) |>
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

  if(length(grep("median*", unique(all_pwscores[[k-1]]$model))) > 0){
    all_pwscores[[k-1]] <- NULL
  }
}

all_pwscores <- rbindlist(all_pwscores)

colors = met.brewer(name="Hokusai3", n=3)


ltypes <- c("Ensembles mean rel. skill"="solid","Hub Ensemble rel. skill (=1 by definition)"="dashed")
colfills <- c("min-max range ensembles rel. skill" = colors[2], "q05-q95 range ensembles rel. skill" = colors[2])
pdf(here("ensvssize", "serverrun", paste0("ens_comb", loctarg,".pdf")), width = 11, height = 6.5)
ggplot(data = all_pwscores) +
  geom_line(aes(x = k, y = meanrelskill, linetype = "Ensembles mean rel. skill"), color = colors[2], lwd = 1) +
  geom_ribbon(aes(x = k, ymin = minrelskill, ymax = maxrelskill, fill = "min-max range ensembles rel. skill" ), alpha = 0.25) +
  geom_ribbon(aes(x = k, ymin = q05relskill, ymax = q95relskill, fill = "q05-q95 range ensembles rel. skill" ), alpha = 0.5) +
  geom_hline(aes(yintercept = 1, linetype = "Hub Ensemble rel. skill (=1 by definition)")) +
  scale_x_continuous(breaks = ks) + # Adjust the x-axis limits
  ylab("Scaled relative skill") +
  xlab("k - number of models in recombined ensemble") +
  scale_linetype_manual(name="",values=ltypes) +
  scale_fill_manual(name="",values=colfills) +
  theme_masterthesis() %+replace%
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(loctarg) +
  facet_wrap(~horizon)
dev.off()
