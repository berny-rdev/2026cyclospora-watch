## ---- EXTERNAL REFERENCE BASELINES (USDA ERS) ----
##
## Shared by index.Rmd and cyclospora_outbreak_analysis.R, so a local run shows
## the same reference column the published page does.

## External reference baselines from USDA ERS, for DISPLAY ONLY. These are
## never fed into signal_ratio: ERS publishes pounds of per-capita
## availability per year, while baseline_commonness is a 0-100 "percent of
## people who ate this in a two-week period" scale. The two are not
## convertible, so showing both side by side is honest and substituting one
## for the other would not be. Reconciling the units is future work.
EXTERNAL_BASELINES_PATH <- "baselines-external.json"

external_baselines <- if (file.exists(EXTERNAL_BASELINES_PATH)) {
  eb <- tryCatch(jsonlite::fromJSON(EXTERNAL_BASELINES_PATH, simplifyVector = FALSE),
                 error = function(e) NULL)
  if (is.null(eb$baselines)) list() else eb$baselines
} else list()

ers_reference <- function(cat) {
  e <- external_baselines[[cat]]
  if (is.null(e) || is.null(e$value_lb_per_year)) return(NA_character_)
  sprintf("%.1f lb/yr (%s)", e$value_lb_per_year, e$source_year)
}
