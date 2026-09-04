#!/usr/bin/env Rscript
## Run the suite:  Rscript tests/run-tests.R   (from the repo root)
##
## Must run from the repo root: R/checklist.R reads checklist-mapping.json
## relative to the working directory when it is sourced.

if (!file.exists("R") || !file.exists("checklist-mapping.json")) {
  stop("Run this from the repo root: Rscript tests/run-tests.R", call. = FALSE)
}

## The suite asserts that normalisation actually works, which it cannot under a
## C locale - the same trap assert_utf8_locale() exists for.
if (!grepl("UTF-8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)) {
  invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
}

res <- testthat::test_dir("tests", stop_on_failure = TRUE, reporter = "summary")
invisible(res)
