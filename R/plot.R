plot_model_availability <- function(data,
                                    saveplot = FALSE,
                                    path = (here("plots",
                                                 "availability.pdf")),
                                    height = 12,
                                    width = 8,
                                    indiv = TRUE,
                                    main = NULL,
                                    xlab = NULL,
                                    ylab = NULL,
                                    palette = "Set2"){


  plot_model_avail_total <- data |>
    dplyr::group_by(forecast_date, location, target_type) |>
    dplyr::summarise(nmods = length(unique(model))-2) |> #subtract 2 bc of baseline, ensemble
    dplyr::ungroup() |>
    ggplot2::ggplot(
      aes(x = forecast_date, y = nmods,
          group = location,
          color = location)) +
    ggplot2::geom_line(lwd = 1.1) +
    #ggplot2::scale_color_discrete(name ="",
    #                        breaks = c("PL", "GB", "FR", "DE", "CZ"),
    #                        labels=c("Poland","United Kingdom","France",
    #                                 "Germany", "Czech R.")) +
    ggplot2::labs(title = main) +
    ggplot2::facet_grid(~ target_type) +
    ggplot2::scale_color_brewer(palette = palette,
                                name = "",
                                breaks = c("PL", "GB", "FR", "DE", "CZ"),
                                labels=c("Poland","United Kingdom","France",
                                         "Germany", "Czech Republic")) +
    ggplot2::scale_x_date(date_breaks = "1 month",
                          date_labels = "%b %y",
                          expand = c(0,0)) +
    ggplot2::ylim(2, 17)+
    theme_masterthesis() %+replace%
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  print(plot_model_avail_total)
}
