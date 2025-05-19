#this function was copied and slightly adapted (just changing from "file" (to be read in) to "dat" parameter (already loaded))
#from (related project):
#https://github.com/epiforecasts/eval-by-method/blob/f8d0ffc0ff59e079e6815c0c49466a7e1e5f735d/R/prep-data.R#L7

classify_models <- function(dat) {
  methods <- dat |>
    pivot_longer(
      -model,
      names_to = "classifier", values_to = "classification"
    ) |>
    filter(!(is.na(classification) | classification == "#N/A")) |>
    group_by(model) |>
    summarise(
      agreement = (n_distinct(classification) == 1),
      classification = names(
        sort(table(classification), decreasing = TRUE)[1]
      ),
      .groups = "drop"
    ) |>
    mutate(classification = factor(
      classification,
      levels = c(
        "Agent-based", "Mechanistic",
        "Semi-mechanistic", "Statistical",
        "Machine learning", "Qualitative"
      )
    ))
  return(methods)
}
