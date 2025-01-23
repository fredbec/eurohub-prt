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
           `GB`= "United Kingd.")

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
                          labels = c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")))

all_pwscores <- all_pwscores |>
  DT(,horizon := ifelse(horizon == 1, "1-week horizon", "2-week horizon")) |>
  DT(, location := factor(location,
                          levels = c("DE", "PL", "CZ", "FR", "GB"),
                          labels = c("Germany", "Poland", "Czech Rep.", "France", "United Kingd.")))

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

colors = met.brewer(name="Hokusai3", n=3)

textsize_y <- 14
col_hlines <- "black" #"grey50" for appendix plot
plot_name <- "pwscores_pivot" #"pwscores_pivot_withrelchanges"  for appendix plot

ltypes <- c("recombined ensembles\nmedian scaled rel. skill"="solid","Hub ensemble scaled\nrel. skill (=1 by definition)"="dashed", "Baseline scaled\nrelative skill" = "dotted")
colfills <- c("min-max range: recombined\nensembles scaled rel. skill" = 0.28, "5%-95% quantile: recombined\nensembles scaled rel. skill" = 0.45)
pdf(here("plot_results", paste0("ens_comb_", plot_name,".pdf")), width = 12, height = 8)
ggplot(data = all_pwscores) +
  #ggplot(data = all_pwscores) +
  geom_line(aes(x = k, y = medrelskill, linetype = "recombined ensembles\nmedian scaled rel. skill", color = location), lwd = 1, show.legend = F) +
  geom_ribbon(aes(x = k, ymin = minrelskill, ymax = maxrelskill, alpha = "min-max range: recombined\nensembles scaled rel. skill", fill = location)) +
  geom_ribbon(aes(x = k, ymin = q05relskill, ymax = q95relskill, alpha = "5%-95% quantile: recombined\nensembles scaled rel. skill", fill = location)) +
  geom_hline(aes(yintercept = 1, linetype = "Hub ensemble scaled\nrel. skill (=1 by definition)"), show.legend = F, col = col_hlines) +
  geom_hline(aes(yintercept = scaled_rel_skill, linetype = "Baseline scaled\nrelative skill"), data = bsmod, col = col_hlines) +
  #uncomment following lines for appendix version of plot
  #geom_text(aes(x = k, y = ploty, label = relchange),
  #          data = relmedscores, size = 3,
  #          label.padding = unit(0.05, "lines"), fontface = "bold") +
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
dev.off()
