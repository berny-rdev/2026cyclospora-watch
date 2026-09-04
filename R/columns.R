## ---- FORM COLUMN MATCHING -------------------------------------------------
##
## Shared by index.Rmd and cyclospora_outbreak_analysis.R.
##
## Google Forms puts the entire question text - instructions, examples and all -
## into the Sheet header, and can wrap or reformat it between edits. So columns
## are located by a short distinctive keyword rather than an exact string.
##
## The failure mode that motivates everything below: an unmatched signature used
## to be dropped silently, and every downstream use of a column is written as
## `if ("x" %in% names(df))`. So a reworded question does not raise an error -
## it removes a column, skips the block that reads it, and publishes a page that
## looks normal and is quietly missing data.
##
## The consent column is the sharp edge. Its guard reads:
##
##     if ("consent" %in% names(df)) { ...keep only responses starting "yes"... }
##
## If that header stops matching, the guard is FALSE, the filter never runs, and
## responses from people who did not consent are silently included in a
## published analysis. That is not a degraded page, it is the wrong page, so a
## missing required column is a hard error rather than a warning.

## Locates each signature in the sheet's actual headers.
##
##   required    - names that MUST match. Missing any of them stops the run.
##   require_any - list of name-groups where at least one member must match.
##                 Used for the two produce columns: either the checklist or
##                 the free text is enough, but with neither there is no
##                 exposure data at all and the whole analysis is vacuous.
##
## Signatures that are merely optional and unmatched produce a warning naming
## them, so a rename shows up in the Action log instead of vanishing.
match_columns <- function(actual_names, signatures,
                          required = character(0), require_any = list()) {
  matched <- vapply(signatures, function(sig) {
    hits <- actual_names[str_detect(str_to_lower(actual_names), str_to_lower(sig))]
    if (length(hits) == 0) NA_character_ else hits[1]
  }, character(1))

  missing <- names(signatures)[is.na(matched)]

  ## Printing the real headers is the whole point of the message: the fix is
  ## always "edit the regex to match what the form now says", and that is
  ## guesswork without seeing the current text.
  headers_note <- function() {
    paste0("\n  Actual columns in the sheet:\n",
           paste0("    - ", actual_names, collapse = "\n"))
  }

  req_missing <- intersect(required, missing)
  if (length(req_missing)) {
    stop("Required form column(s) not found: ", paste(req_missing, collapse = ", "), "\n",
         "  Their signatures matched no header, so every block that reads them\n",
         "  would be skipped and the run would publish a page silently missing\n",
         "  that data. Refusing to continue.",
         if ("consent" %in% req_missing)
           paste0("\n  'consent' in particular gates the filter that drops non-consenting\n",
                  "  responses - without it they would be included in the analysis.")
         else "",
         "\n  Update col_signatures to match the question's current wording.",
         headers_note(), call. = FALSE)
  }

  for (grp in require_any) {
    if (all(grp %in% missing)) {
      stop("None of these form columns were found: ", paste(grp, collapse = ", "), "\n",
           "  At least one is needed; with none of them there is no exposure data\n",
           "  to analyse and the page would render empty tables as though the\n",
           "  outbreak had no reported foods.",
           headers_note(), call. = FALSE)
    }
  }

  optional_missing <- setdiff(missing, unlist(require_any))
  if (length(optional_missing)) {
    warning("Optional form column(s) not found: ", paste(optional_missing, collapse = ", "),
            ". The sections that use them will be skipped.", call. = FALSE)
  }

  matched[!is.na(matched)]
}
