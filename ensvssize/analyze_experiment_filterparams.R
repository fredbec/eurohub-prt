library(here)
library(data.table)
library(ggplot2)
library(MetBrewer)
source(here("ensvssize", "specs.R"))

.d <- `[`

ks <- enscomb_specs$ks
loctargets <- enscomb_specs$loctargets

maxens <- 1000000

qms <- c(0.5)
qes <- c(1, 0)
qt <- c(0.5)
param_combs <- expand.grid(qms, qes, qt)
names(param_combs) <- c("qm", "qe", "qt")

res <- vector(mode = "list", length = nrow(param_combs))

for(pcomb in 1:nrow(param_combs)){

  pcombs <- param_combs[pcomb,]

  model_avail <- pcombs$qm
  availpropmods <- pcombs$qe
  availproptime <- pcombs$qt

  res[[pcomb]] <- data.table::fread(here("enscomb-data", "exp_params",
                                     paste0("modelavail", gsub("\\.", "", model_avail), "_availpropmods", gsub("\\.", "", availpropmods), "_availproptime", gsub("\\.", "", availproptime), ".csv"))) |>
    .d(, model_avail := model_avail) |>
    .d(, availpropmods := availpropmods) |>
    .d(, availproptime := availproptime)
}

res1 <- rbindlist(res) |>
  .d(, availpropmods := as.character(availpropmods))


plot1 <- ggplot(res1, aes(x = k, y = nens, group = interaction(model_avail, availpropmods, availproptime))) +
  geom_line(aes(color = interaction(model_avail, availproptime),linetype = availpropmods), lwd = 1.01) +
  scale_x_continuous(breaks = 2:14) +
  scale_linetype_manual(values = c("0" = "dashed", "1" = "solid")) +
  facet_wrap(~loctarg, scales = "fixed") +
  scale_color_met_d("Hokusai1") +#+
  theme_minimal() %+replace%
  theme(legend.position = "bottom") +
  guides(color=guide_legend(title="qm, qt"), linetype=guide_legend(title="qe"))
  #scale_y_continuous(trans = "log")


res2 <- rbindlist(res) |>
  .d(, nens := ifelse(nens > 7550 & loctarg == "DECases", 7550, nens)) |>
  .d(, nens := ifelse(nens > 2500 & loctarg != "DECases", 2500, nens)) |>
  .d(, availpropmods := as.character(availpropmods))


plot2 <- ggplot(res2, aes(x = k, y = nens, group = interaction(model_avail, availpropmods, availproptime))) +
  geom_line(aes(color = interaction(model_avail, availproptime),linetype = availpropmods), lwd = 1.01) +
  scale_x_continuous(breaks = 2:14) +
  scale_linetype_manual(values = c("0" = "dashed", "1" = "solid")) +
  facet_wrap(~loctarg, scales = "free") +
  scale_color_met_d("Hokusai1") +#+
  theme_minimal() %+replace%
  theme(legend.position = "bottom") +
  guides(color=guide_legend(title="qm, qt"), linetype=guide_legend(title="qe"))
#scale_y_continuous(trans = "log")
