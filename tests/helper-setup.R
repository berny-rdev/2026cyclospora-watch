## Loaded automatically by testthat::test_dir() before any test file.
##
## test_dir() sets the working directory to tests/, but R/checklist.R reads
## checklist-mapping.json relative to the working directory when it is sourced.
## So find the repo root by walking up, and stay there for the whole run - a
## silent failure here loads no functions at all and every test errors with
## "could not find function".

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr); library(tibble)
})

repo_root <- local({
  d <- normalizePath(getwd(), mustWork = TRUE)
  for (i in 1:5) {
    if (dir.exists(file.path(d, "R")) && file.exists(file.path(d, "checklist-mapping.json"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("Could not locate the repo root from ", getwd(), call. = FALSE)
})
setwd(repo_root)

sourced <- list.files("R", pattern = "[.]R$", full.names = TRUE)
if (!length(sourced)) stop("No modules found in R/ - nothing would be under test.", call. = FALSE)
for (f in sourced) source(f)

## Every fixture below is a phrase that actually broke the pipeline. Keeping
## them named makes the tests read as a list of defects rather than a list of
## strings, and makes it obvious what a regression would mean.
FIXTURES <- list(
  ## One answer naming two businesses, each with a comma inside parentheses.
  ## A plain comma split turned this into three junk categories, including the
  ## fragments "ma); campfire grille (bridgton" and "me)".
  two_businesses_parens = "Agawam Diner (Rowley, MA); Campfire Grille (Bridgton, ME)",

  ## Seven businesses on seven lines collapsed into a single category because
  ## newlines were not treated as a delimiter.
  newline_list = "Atomic Wings\nChez Oskar\nBurger Bus\nTaco Bell",

  ## Curly apostrophe. 13 of 36 poisoned categories came from this: a phrase
  ## sent as a JSON key with U+2019 came back with U+0027, the exact-name
  ## lookup missed, and the item fell through to the regex fallback.
  curly_apostrophe = "Trader Joe’s",

  ## Declining to answer is not a food. Before the negation filter these
  ## reached the classifier and became categories.
  negations = c("No", "none", "N/A", "nothing", "idk", "unsure",
                "I don't", "not really", "-", "  "),

  ## Real foods that merely start with the same letters as a negation word.
  ## The \\b guards in NEGATION_RE exist for these.
  negation_lookalikes = c("nopales", "nectarines", "napa cabbage")
)
