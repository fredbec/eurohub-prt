best_performers_ensemble <- function(data,
                                     su_cols,
                                     score = "interval_score",
                                     nmods = 3,
                                     window = 4,
                                     may_miss = 1,
                                     #excl = c("EuroCOVIDhub-baseline",
                                     #          "EuroCOVIDhub-ensemble"),
                                     excl = c("EuroCOVIDhub-ensemble"),
                                     return_model_data = FALSE){


  #filter out models that are in excl
  data <- data |>
    dplyr::filter(!model %in% excl)

  dates <- data$forecast_date |>
    unique() |>
    as.Date() |>
    sort()

  numhors <- length(unique(data$horizon))

  locs <- unique(data$location)
  targets <- unique(data$target_type)

  ########################### CHECK THIS##################
  ###could just be length(dates)################
  stop_ind <- length(dates) - window #not window-1 since we still need one obs for forecasting

  #result containers
  bestperf_ens <- NULL


  #fill a list with a matrix for each combination of location and target_type
  # column of each matrix are the models, rows are the forecast dates
  list_names <- c("all", paste(rep(locs, each = length(targets)),
                               targets, sep = "."))

  model_matrices <- vector(mode = "list",
                           length = length(list_names))
  names(model_matrices) <- list_names

  #each list element is the same initial matrix
  model_matrices <- lapply(model_matrices, function(x)
    matrix(ncol = length(unique(data$model)),
           nrow = stop_ind,
           dimnames = list(as.character(dates[window+1:stop_ind]),
                           unique(data$model)
           )))


  for(i in 1:stop_ind){

    #dates that are in current sliding window
    sub_dates <- seq.Date(from = dates[i],
                          to = dates[i] + (window - 1) * 7,
                          by = 7)
    print(sub_dates)
    #get models that are available at given forecast date
    fc_date <- dates[i] + window * 7
    print(fc_date)
    avail_models <- data |>
      dplyr::filter(forecast_date == as.Date(fc_date)) |>
      dplyr::select(model, target_type, location) |>
      dplyr::distinct() |>
      dplyr::mutate(is_present = 1)


    #find best performers in subset
    best_models <- data |>
      dplyr::filter(forecast_date %in% sub_dates) |>
      merge(avail_models, #kick out models which don't predict at given forecast_date
            by = c("model", "location", "target_type")) |>
      dplyr::group_by(model, target_type, location) |>
      dplyr::mutate(n = n() / (23*numhors))  |> #forecast present => 92 rows, divide to get unique forecast presence
      dplyr::filter(target_end_date < fc_date) |>
      dplyr::filter(n >= window - may_miss) |> #threshold to include model
      dplyr::ungroup() |>
      dplyr::select(all_of(su_cols)) |>
      scoringutils::score() |>
      scoringutils::pairwise_comparison(
        by = c("model", "location", "target_type"),
        baseline = "EuroCOVIDhub-baseline") |>
      dplyr::filter(compare_against == "EuroCOVIDhub-baseline",
                    model != "EuroCOVIDhub-baseline") |>
      dplyr::group_by(location, target_type) |>
      dplyr::slice_min(order_by = scaled_rel_skill, #only keep best performers
                       n = nmods) |>
      dplyr::select(model, location, target_type)


    #just a small function that matches unique models in best_models
    #to column names in model matrices, to fill a row of the model matrix
    match_models <- function(data, model_matrix){
      mod_names <- unique(data$model)
      matches <- match(colnames(model_matrix), mod_names)
      return(as.numeric(!is.na(matches)))
    }



    #fill list of model matrices with above function
    for(comb in names(model_matrices)){
      if(comb == "all"){
        model_matrices[["all"]][i,] <- match_models(best_models,
                                                    model_matrices[["all"]])
      } else {
        #split up combination of location and target
        filters <- strsplit(comb, split = "[.]")[[1]]

        #only keep those best models that match location and target
        comb_models <- best_models |>
          dplyr::filter(location == filters[1],
                        target_type == filters[2])

        model_matrices[[comb]][i,] <- match_models(comb_models,
                                                   model_matrices[[comb]])


      }
    }


    #test if filtering worked correctly
    testlength <- data |>
      dplyr::select(all_of(c("location", "target_type", "model"))) |>
      merge(best_models, by = c("model", "location",
                                "target_type")) |>
      dplyr::distinct()

    if(nrow(testlength)>5*2*nmod){
      print(testlength)
      message("too many models, something might be wrong here")
    }


    #keep only those models which are in best_models and make
    #next week's prediction out of these models
    ens_preds_mean <- data |>
      dplyr::filter(forecast_date == (sub_dates[window] + 7)) |>
      merge(best_models, by = c("model", "location",
                                "target_type")) |>
      make_ensemble(summary_function = mean, old_call = TRUE)

    ens_preds_median <- data |>
      dplyr::filter(forecast_date == (sub_dates[window] + 7)) |>
      merge(best_models, by = c("model", "location",
                                "target_type"))|>
      make_ensemble(summary_function = median, old_call = TRUE)

    ens_preds <- rbind(ens_preds_mean, ens_preds_median)

    bestperf_ens <- rbind(bestperf_ens, ens_preds)


  }

  if(!return_model_data){
    return(bestperf_ens)
  }

  #for each comb only keep columns of models that actually forecast that comb
  for(comb in names(model_matrices)){
    filters <- strsplit(comb, split = "[.]")[[1]]

    present_models <- data |>
      dplyr::filter(location == filters[1],
                    target_type == filters[2]) |>
      select(model) |>
      distinct() |>
      pull()
    model_matrices[[comb]] <-
      model_matrices[[comb]][, present_models]
  }

  ##score ensemble
  score_best_ens <- bestperf_ens |>
    scoringutils::score() |>
    scoringutils::summarise_scores(
      by = c("model", "target_type",
             "location", "horizon")) |>
    dplyr::mutate(best_perf = 1)

  if(return_model_data){
    return(list(bestperf_ens, model_matrices))
  }

  normal_ens1 <- hub_data |>
    make_ensemble(mean, old_call = TRUE)

  normal_ens <- hub_data |>
    make_ensemble(median, old_call = TRUE) |>
    rbind(normal_ens1) |>
    scoringutils::score() |>
    scoringutils::summarise_scores(
      by = c("model", "target_type",
             "location", "horizon")) |>
    dplyr::mutate(best_perf = 0)

  score_best_ens <- score_best_ens |>
    rbind(normal_ens)

  if(return_model_data){
    return(list(bestperf_ens, model_matrices, score_best_ens))
  } else {
    return(bestperf_ens)
  }


}

inverse_score_weights <- function(data,
                                  score_data = NULL,
                                  su_cols = NULL,
                                  fc_date,
                                  at_level = "model",
                                  window = 4,
                                  exp_smooth = NULL,
                                  by_target_end_date = TRUE,
                                  score_fun = "interval_score",
                                  may_miss = 1){

  #####IMPUTE SCORES#######
  horizons <- unique(data$horizon)

  #warning if baseline or ensemble in data
  if(score_fun != "interval_score"){
    stop("can only impute scores for the WIS")
  }


  if(is.null(score_data) & is.null(su_cols)){
    stop("if not supplying scores, must supply su_cols")
  } else if (is.null(score_data)){
    score_data <- data |>
      select(su_cols) |>
      score() |>
      summarise_scores(by = c("model", "location", "target_type",
                              "forecast_date", "horizon"))
  }

  if(!setequal(unique(data$model), unique(score_data$model))){
    stop("data and score_data do not contain the same models")
  }

  #map of forecast_date and horizon to target_end_date
  tg_end_map <- data |>
    select(forecast_date, horizon, target_end_date) |>
    distinct()

  #get window of dates
  curr_dates <- fc_dates_window(fc_date, window, incl = FALSE)


  #check if current dates in data and score data
  num_missing_data <- setdiff(curr_dates, unique(data$forecast_date)) |> length()
  num_missing_scores <- setdiff(curr_dates, unique(score_data$forecast_date)) |> length()
  if(num_missing_data > 0){
    message(paste(setdiff(curr_dates, unique(data$forecast_date)),
                  collapse = ", "))
    stop("dates needed to compute weights missing from data")
  }
  if(num_missing_scores> 0){
    message(paste(setdiff(curr_dates, unique(score_data$forecast_date)),
                  collapse = ", "))
    stop("dates needed to compute weights missing from score_data")
  }



  #compute score smoother values i
  if(!is.null(exp_smooth)){
    smoothing_vals <- tg_end_map |>
      #only keep dates of relevant forecasts
      filter(forecast_date <= fc_date,
             target_end_date < fc_date) |>
      #determine how many dates behind the forecast is
      mutate(lag_behind = ((as.IDate(fc_date) -
                              target_end_date) + 5) / 7) |>
      mutate(smoother = exp_smooth *
               (1 - exp_smooth)^(lag_behind - 1))
  } else {
    #equal weights for all past observations
    smoothing_vals <- tg_end_map |>
      #only keep dates of relevant forecasts
      filter(forecast_date <= fc_date,
             target_end_date < fc_date) |>
      mutate(smoother = 1)
  }

  #append weights to data
  #(actually, now comes at a later point)
  #score_data <- score_data |>
  #  left_join(smoothing_vals,
  #            by = c("forecast_date", "target_end_date", "horizon"))



  #make full sets (where all models predict for all targets in window)
  #to determine number of missings and impute missing scores
  full_sets <- score_data |>
    filter(forecast_date %in% curr_dates) |>
    split(by = c("target_type", "location")) |>
    lapply(function(dat)
      tidyr::crossing(placeholder = unique(dat$model), #rename afterwards because character
                      location = unique(dat$location),
                      target_type = unique(dat$target_type),
                      forecast_date = unique(dat$forecast_date),
                      horizon = horizons)) |>
    rbindlist() |>
    left_join(tg_end_map, by = c("forecast_date", "horizon"))

  names(full_sets)[1] <- at_level

  #print(horizons)

  #compute ivnerse score weights
  inv_score_weights <- score_data |>
    #get data from window
    #keep in unresolved horizon forecasts for counting (remove after)
    filter(forecast_date %in% curr_dates) |>
    mutate(present = 1) |> #this is to determine missings
    full_join(full_sets,
              by = c(at_level, "location", "target_type",
                     "forecast_date", "horizon", "target_end_date"))  |>
    mutate(present = ifelse(is.na(present), 0, 1)) |>
    #count number of unique forecasts for each model
    group_by(get(at_level), location, target_type) |>
    mutate(count = sum(present)/length(horizons)) |> #divide by 4 because of horizon
    ungroup() |>
    filter(count >= (window - may_miss),
           target_end_date < fc_date) |> #remove as yet unresolved horizons
    #impute missing scores (stratify by location, target_type, horizon)
    group_by(across(all_of(c("location", "target_type", "horizon")))) |>
    mutate(maxscore = max(get(score_fun), na.rm = TRUE)) |>
    filter(maxscore >= 0) |>
    ungroup() |>
    mutate(interval_score = ifelse(is.na(interval_score),  #actual imputation
                                   maxscore, interval_score)) |>
    select(-c(present, maxscore)) |>
    #join with exponential smoothing values
    left_join(smoothing_vals,
              by = c("forecast_date", "target_end_date", "horizon")) |>
    #calculate inverse scores
    group_by(across(all_of(c(at_level, "location", "target_type")))) |>
    summarise(interval_score = weighted.mean(get(score_fun),
                                             w = smoother),
              .groups = "drop") |>
    select(all_of(c(at_level, "target_type", "location", score_fun))) |>
    mutate(inv_score = 1/get(score_fun)) |>
    #normalize so sum of weights is 1
    group_by(target_type, location) |>
    mutate(weights = inv_score / sum(inv_score)) |>
    select(all_of(c(at_level, "location", "target_type", "weights"))) |>
    #add back forecast date
    mutate(forecast_date = fc_date)

  print(inv_score_weights)
  return(inv_score_weights)

}

fc_dates_window <- function(curr_date, window, incl = FALSE){

  curr_date <- as.Date(curr_date)

  if(incl){
    end_date <- curr_date
    start_date <- end_date - window * 7
  } else {
    end_date <- curr_date - 7
    start_date <- end_date - (window-1) * 7
  }


  all_dates <- seq.Date(from = start_date,
                        to = end_date,
                        by = 7)

  return(all_dates)
}



#this is a stupid function because I'm stupid
add_tt_and_dates <- function(add_model_dat,
                             ens_dat){

  #find breaks in date (switch from idx 50 to 1)
  matcher <- add_model_dat |>
    mutate(lagg = idx - lag(idx),
           lagg = ifelse(is.na(lagg), 0, lagg),
           identi = as.numeric(lagg < 0),
           groupp = cumsum(identi)) |>
    group_by(groupp) |>
    summarise(count = n())

  #how many observations does each target_type have at each forecast_date
  case_deaths_counts <- ens_dat |>
    select(target_type, forecast_date) |>
    distinct()

  #two dataframes that have info about combination of fc_date and number of observations
  #for each target_type
  nrow_cases <- nrow(case_deaths_counts[case_deaths_counts$target_type == "Cases",])
  nrow_deaths <- nrow(case_deaths_counts[case_deaths_counts$target_type == "Deaths",])
  matcher_cases <- matcher[1:nrow_cases,]
  matcher_deaths <- matcher[(nrow_cases+1):nrow(matcher),]


  #lapply function that reps the respective forecast_date as many times as it is
  #available in ensemble data
  fcdates <- case_deaths_counts |>
    split(by = "target_type")

  fcdates_cases <- lapply(seq_along(fcdates[[1]]$forecast_date), function(k)
    rep(fcdates[[1]]$forecast_date[k], times = matcher_cases$count[k])) |>
    unlist() |>
    as.Date(origin = "1970-01-01") |>
    data.table() |>
    mutate(target_type = "Cases") |>
    rename(forecast_date = V1)
  fcdates_deaths <- lapply(seq_along(fcdates[[2]]$forecast_date), function(k)
    rep(fcdates[[2]]$forecast_date[k], times = matcher_deaths$count[k])) |>
    unlist() |>
    as.Date(origin = "1970-01-01") |>
    data.table() |>
    mutate(target_type = "Deaths") |>
    rename(forecast_date = V1)


  if(FALSE){
    #can be used alternatively if one target_type has same number of observations
    #across the whole dataset
    fcdates_deaths <- rep(fcdates[[2]]$forecast_date, each = 3700) |>
      as.Date(origin = "1970-01-01") |>
      data.table() |>
      mutate(target_type = "Deaths") |>
      rename(forecast_date = V1)
  }

  extra_cols <- rbind(fcdates_cases, fcdates_deaths)

  return(cbind(add_model_dat, extra_cols))
}


fast_eval <- function(target, current, return_eval = FALSE,
                      su_cols = su_cols,
                      strat_by = c("model", "target_type","location",
                                   "horizon", "forecast_date"),
                      comp_avg_by = NULL,
                      period_cat_dt = NULL){


  common_base <- target |>
    select(target_type, location, forecast_date) |>
    mutate(forecast_date = as.Date(forecast_date))

  current <- current |>
    mutate(forecast_date = as.Date(forecast_date)) |>
    semi_join(common_base,
              by = c("target_type", "location", "forecast_date"))

  eval <- make_eval(target, current, su_cols,
                    strat_by = strat_by)

  if(!is.null(period_cat_dt)){
    eval <- eval |>
      left_join(period_cat_dt, by = "forecast_date")
  }

  #Compute averages across dimensions as specified in comp_avg_by
  if(!is.null(comp_avg_by)){
    comp_avg <- eval |>
      group_by(across(all_of(comp_avg_by))) |>
      summarise(target_val = mean(target_val),
                current_val = mean(current_val)) |>
      mutate(rel_score = target_val / current_val,
             model = unique(target$model),
             average = "average")

    eval <- rbind(eval, comp_avg, fill = TRUE)
  }


  myval <- mean(eval$target_va) / mean(eval$current_val)
  print(myval)

  if(return_eval){
    return(eval)
  }
}


comp_avg_by_extra <- function(fast_eval_result,
                              comp_avg_by){
  comp_avg <- fast_eval_result |>
    group_by(across(all_of(comp_avg_by))) |>
    summarise(target_val = mean(target_val),
              current_val = mean(current_val)) |>
    mutate(rel_score = target_val / current_val,
           model = unique(fast_eval_result$model),
           average = "average")

  eval <- rbind(fast_eval_result, comp_avg, fill = TRUE)

  return(eval)

}

make_eval <- function(target,
                      current,
                      su_cols,
                      scoring_fun = "interval_score",
                      strat_by = c("model", "target_type","location",
                                   "horizon", "forecast_date"),
                      return_anti_join = FALSE){

  #check if there are multiple models in any of the datasets
  if(length(unique(target$model))!=1){
    mods <- paste0(unique(target$model))
    message(paste0("There is more than one model in target: ", paste(mods, collapse = ", ")))
    stop("Can only pass one model per dataframe.")
  }
  if(length(unique(current$model))!=1){
    mods <- paste0(unique(current$model))
    message(paste0("There is more than one model in current: ", paste(mods, collapse = ", ")))
    stop("Can only pass one model per dataframe.")
  }


  #####check if both models predict for all the same instances#######
  all_inst_target <- target |>
    dplyr::select(all_of(strat_by[-1])) |> #remove model from strat_by
    dplyr::distinct()
  all_inst_current <- current |>
    dplyr::select(all_of(strat_by[-1])) |> #remove model from strat_by
    dplyr::distinct()

  #perform anti join to find instances that are not in intersection
  not_in_current <- dplyr::anti_join(all_inst_target |> mutate(forecast_date = as.Date(forecast_date)), all_inst_current|> mutate(forecast_date = as.Date(forecast_date)), by = strat_by[-1])
  not_in_target <- dplyr::anti_join(all_inst_current|> mutate(forecast_date = as.Date(forecast_date)), all_inst_target|> mutate(forecast_date = as.Date(forecast_date)), by = strat_by[-1])

  if(!nrow(not_in_current) == 0){
    message("There are instances in target that are not in current.")
    if(return_anti_join){
      message("Returning these instances.")
      return(not_in_current)
    }
    stop("Relative WIS cannot be reliably computed")
  }
  if(!nrow(not_in_target) == 0){
    message("There are instances in current that are not in target.")
    if(return_anti_join){
      message("Returning these instances.")
      return(not_in_target)
    }
    stop("Relative WIS cannot be reliably computed")
  }

  #additionally check for nrow (could e.g. be missing quantiles,
  #although I have absolutely no idea how that would have happened)
  if(!identical(nrow(target), nrow(current))){
    message(paste0("Target has ", nrow(target), " rows, current has ", nrow(current)))
    stop("Relative WIS cannot be reliably computed")
  }



  ########Scoring#########
  #score target
  target_score <- target |>
    dplyr::select(all_of(su_cols)) |> #remove redundant cols before scoring
    scoringutils::score(metrics = "interval_score") |>
    scoringutils::summarise_scores(by = strat_by) |>
    dplyr::select(all_of(c(strat_by, scoring_fun))) |>
    dplyr::rename(target_val = scoring_fun)

  #score current
  current_score <- current |>
    dplyr::select(all_of(su_cols)) |> #remove redundant cols before scoring
    scoringutils::score(metrics = "interval_score") |>
    scoringutils::summarise_scores(by = strat_by) |>
    dplyr::select(all_of(c(strat_by, scoring_fun))) |>
    dplyr::rename(current_val = scoring_fun) |>
    dplyr::select(-model) #remove before joining


  #join scores and compute relative score
  joined_scores <- target_score |>
    mutate(forecast_date = as.Date(forecast_date)) |>
    dplyr::left_join(current_score, by = strat_by[-1]) |> #remove model from strat_by
    dplyr::mutate(rel_score = target_val/current_val)

  return(joined_scores)

}


best_performers_boxplot <- function(best_performers_data,
                                    avg_points_data,
                                    labeller_targets =plot_target_label <- c(`Cases` = "Target: Cases", `Deaths` = "Target: Deaths"),
                                    labeller_locs = specs$plot_location_label){

  #best_performers_data <- best_performers_data |>
  #  DT(, location := factor(location,
  #                          levels = c("DE", "PL", "CZ", "FR", "GB", "Average"),
  #                          labels = c("Germany", "Poland", "Czech Rep.", "France", "Great Br.", "Average")))

  #avg_points_data <- avg_points_data |>
  #  DT(, location := factor(location,
  #                          levels = c("DE", "PL", "CZ", "FR", "GB", "Average"),
  #                          labels = c("Germany", "Poland", "Czech Rep.", "France", "Great Br.", "Average")))

  locnames <- best_performers_data$location
  locnames <- locnames[locnames != "Average"]

  colors_manual <- met.brewer("Veronese", 5)
  #names(colors_manual) <- unique(locnames)
  names(colors_manual) <- c("DE", "PL", "CZ", "FR", "GB")

  colors_manual[6] <- met.brewer("Veronese", 6)[6]
  names(colors_manual)[6] <- "Average"


  plot <- ggplot(best_performers_data, aes(x = factor(nmod),
                                           y = rel_score,
                                           color = factor(location))) +
    geom_boxplot(outlier.shape = 20,
                 outlier.color = "darkgray",
                 lwd = 1.05,
                 fatten = 0.8) +
    scale_color_manual(values = colors_manual) +
    guides(color = "none") +
    #scale_color_brewer(palette = "Set2",
    #                   guide = "none") +
    geom_hline(yintercept = 1) +
    facet_grid(target_type ~ location,
               labeller = as_labeller(c(labeller_targets,
                                        labeller_locs))) +
    theme_masterthesis() +
    ylab("WIS relative to Median Ensemble") +
    xlab("Number of best performers")


  if(!is.null(avg_points_data)){
    plot <- plot +
      geom_point(data = all_evals_avg, aes(x = factor(nmod),
                                           y = rel_score),
                 shape = 18, color = "black", size = 2)
  }

  return(plot)
}


#model cvg is output from comp_quantile_cvg (utils)
one_sided_cvg_plot <- function(model_cvg,
                               ylims = c(-0.225, 0.225)){
  plot <- ggplot(model_cvg, aes(x = quantile,
                                y = cvg_deviation,
                                color = model)) +
    geom_point() +
    geom_line() +
    scale_color_discrete(name="") +
    #scale_color_brewer(palette = "Set1") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~target_type,
               labeller = as_labeller(specs$plot_target_label))+
    ylab("Empirical Minus Nominal Quantile Coverage") +
    xlab("Nominal Quantile Level") +
    theme_masterthesis() +
    ylim(ylims)

  return(plot)
}

theme_masterthesis <- function() {
  theme_minimal() %+replace%
    theme(axis.line = element_line(colour = "grey80"),
          axis.ticks = element_line(colour = "grey80"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom")
}
