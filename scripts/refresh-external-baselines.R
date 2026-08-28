#!/usr/bin/env Rscript
## =============================================================
## refresh-external-baselines.R
## =============================================================
##
## Fetches per-capita food availability figures from USDA ERS and writes
## them to baselines-external.json.
##
## THIS SCRIPT DOES NOT TOUCH category_vocabulary.json, and nothing in the
## rendering pipeline reads its output yet. It exists so that the
## baseline_commonness numbers - currently hand-typed guesses sitting in
## the denominator of every signal ratio - can eventually be replaced by
## sourced values with real provenance. Wiring that up is a separate,
## deliberate step.
##
## IMPORTANT - UNITS DO NOT MATCH YET. baseline_commonness is documented as
## "% of a general population who eats this in any given 2-week period".
## ERS publishes POUNDS PER CAPITA PER YEAR (and cups/day in the
## loss-adjusted series). These are not convertible: 19 lb/yr of tomatoes
## could be everyone eating a little or a fifth of people eating a lot.
## Dropping these values into baseline_commonness as-is would silently
## change what signal_ratio means. That reconciliation is the integration
## step's problem; this script only gathers and records the source data.
##
## Usage:  Rscript scripts/refresh-external-baselines.R
##         Rscript scripts/refresh-external-baselines.R --output path.json
##
## Exit codes: 0 = success (file written or already current), 1 = failure.
## =============================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

## ---- CONFIG ---------------------------------------------------------

FADS_PAGE  <- "https://www.ers.usda.gov/data-products/food-availability-per-capita-data-system"
OUTPUT_DEFAULT <- "baselines-external.json"

## How far a value may move between runs before we treat it as a likely
## upstream unit change rather than a real revision. A genuine year-over-year
## revision is a few percent; a factor of three means something structural
## changed and a human should look before it lands.
MAX_FOLD_CHANGE <- 3

## The four columns every ERS Food Availability CSV is expected to have.
## Checked exactly - if ERS restructures these files we want to stop, not
## guess which column is which.
EXPECTED_HEADER <- c("Commodity", "Year", "Attribute", "Value")

## Source files, resolved by filename off the FADS landing page rather than
## hardcoded, because the /media/NNNN/ id and the ?v= cache-buster both
## change whenever ERS republishes.
SOURCE_FILES <- list(
  veg_fresh   = list(file = "vegetables-fresh.csv",
                     block = "Fresh vegetables (farm weight): Per capita availability"),
  fruit_fresh = list(file = "fruit-fresh.csv",
                     block = "Fresh fruit (farm weight): Per capita availability"),
  lafa_veg    = list(file = "vegetables.csv", block = NA_character_)
)

LAFA_CUPS_ATTR <- "Food pattern equivalents available daily-Cups"

## ---- THE MAPPING ----------------------------------------------------
## Reviewed and approved before this script was written. Decisions:
##
##   Q2 - ERS splits lettuce by botanical TYPE, never by packaging, which is
##        the opposite axis from our taxonomy. So only the type-level totals
##        are sourced (iceberg_head, romaine_head). The packaging-level
##        categories and spring mix stay author estimates.
##   Q3 - green_onion is NOT mapped to ERS "Onions": that row is all onion
##        types combined and overwhelmingly dry bulb onions, so it would
##        misrepresent scallions by an order of magnitude.

ERS_MAPPING <- list(
  iceberg_head = list(src = "veg_fresh",   attr = "Lettuce head-Pounds",
                      lafa = "Fresh head lettuce"),
  romaine_head = list(src = "veg_fresh",   attr = "Romaine and leaf-Pounds",
                      lafa = "Fresh romaine and leaf lettuce"),
  spinach      = list(src = "veg_fresh",   attr = "Spinach-Pounds",
                      lafa = "Fresh spinach"),
  cucumber     = list(src = "veg_fresh",   attr = "Cucumbers-Pounds",
                      lafa = "Fresh cucumbers"),
  tomato       = list(src = "veg_fresh",   attr = "Tomatoes-Pounds",
                      lafa = "Fresh tomatoes"),
  cabbage      = list(src = "veg_fresh",   attr = "Cabbage-Pounds",
                      lafa = "Fresh cabbage"),
  carrot       = list(src = "veg_fresh",   attr = "Carrots-Pounds",
                      lafa = "Fresh carrots"),
  broccoli     = list(src = "veg_fresh",   attr = "Broccoli-Pounds",
                      lafa = "Fresh broccoli"),
  bell_pepper  = list(src = "veg_fresh",   attr = "Bell peppers-Pounds",
                      lafa = "Fresh bell peppers"),
  celery       = list(src = "veg_fresh",   attr = "Celery-Pounds",
                      lafa = "Fresh celery"),
  cauliflower  = list(src = "veg_fresh",   attr = "Cauliflower-Pounds",
                      lafa = "Fresh cauliflower"),
  radish       = list(src = "veg_fresh",   attr = "Radishes-Pounds",
                      lafa = "Fresh radishes"),
  strawberries = list(src = "fruit_fresh", attr = "Noncitrus-Strawberries-Pounds",
                      lafa = NA_character_),
  raspberries  = list(src = "fruit_fresh", attr = "Noncitrus-Raspberries-Pounds",
                      lafa = NA_character_),
  avocado      = list(src = "fruit_fresh", attr = "Noncitrus-Avocados-Pounds",
                      lafa = NA_character_),
  melon        = list(src = "fruit_fresh", attr = "Noncitrus-Melons-Pounds",
                      lafa = NA_character_,
                      note = paste("ERS reports melons only in aggregate - cantaloupe,",
                                   "honeydew and watermelon are not broken out. Our",
                                   "regex matches all three, so the aggregate is the",
                                   "right granularity here."))
)

## Categories ERS cannot support. Values carried over from
## baseline_commonness_seed in index.Rmd. Recorded here so the output file
## covers the whole taxonomy and a consumer can tell a sourced number from
## an estimate at a glance, rather than the two being indistinguishable.
AUTHOR_ESTIMATES <- list(
  iceberg_prepackaged = list(value = 12, note = "ERS splits lettuce by botanical type, not packaging - no prepackaged/bagged figure exists."),
  romaine_prepackaged = list(value = 20, note = "ERS splits lettuce by botanical type, not packaging - no prepackaged/bagged figure exists."),
  mesclun_spring_mix  = list(value = 8,  note = "Not a commodity ERS tracks; spring mix is a product form blended from leaf lettuces already counted in 'Romaine and leaf'."),
  green_onion         = list(value = 20, note = "Deliberately NOT mapped to ERS 'Onions': that row combines all onion types and is overwhelmingly dry bulb onions, which would misrepresent green onions/scallions by roughly an order of magnitude."),
  cilantro            = list(value = 20, note = "ERS Food Availability tracks no fresh herbs."),
  basil               = list(value = 10, note = "ERS Food Availability tracks no fresh herbs."),
  parsley             = list(value = 12, note = "ERS Food Availability tracks no fresh herbs."),
  dill                = list(value = 8,  note = "ERS Food Availability tracks no fresh herbs."),
  mint                = list(value = 5,  note = "ERS Food Availability tracks no fresh herbs."),
  blackberries        = list(value = 8,  note = "ERS fruit series covers strawberries, raspberries, blueberries and cranberries - blackberries are absent."),
  snap_peas           = list(value = 8,  note = "ERS has 'Green peas' (processing peas) and 'Snap beans'; neither represents sugar snap peas."),
  salad_bagged        = list(value = 25, note = "A product form, not a commodity - ERS has no equivalent."),
  salad_restaurant    = list(value = 20, note = "A consumption venue, not a commodity - ERS has no equivalent.")
)

## ---- FAIL-LOUD HELPERS ----------------------------------------------

die <- function(...) {
  message("\nFAILED: ", ...)
  quit(status = 1)
}

info <- function(...) cat(..., "\n", sep = "")

## Resolves one download URL off the FADS landing page. Requires EXACTLY one
## match: zero means ERS renamed or removed the file, more than one means the
## page structure changed enough that we can't tell which link is right.
## Either way we stop rather than guess.
resolve_url <- function(page_html, filename) {
  pattern <- paste0('href="([^"]*/', gsub("\\.", "\\\\.", filename), '[^"]*)"')
  m <- gregexpr(pattern, page_html, perl = TRUE)
  hits <- regmatches(page_html, m)[[1]]
  urls <- unique(sub(pattern, "\\1", hits, perl = TRUE))
  urls <- urls[!grepl("\\.xlsx|\\.xls", urls)]

  if (length(urls) == 0) {
    die("could not find a download link for '", filename, "' on ", FADS_PAGE, "\n",
        "  ERS has renamed, moved or removed this file. The mapping in this script\n",
        "  needs to be updated by hand - refusing to write a partial baselines file.")
  }
  if (length(urls) > 1) {
    die("found ", length(urls), " candidate links for '", filename, "' on ", FADS_PAGE, ":\n",
        paste0("    ", urls, collapse = "\n"), "\n",
        "  Expected exactly one. The page structure has changed - refusing to guess.")
  }

  url <- urls[1]
  if (!grepl("^https?://", url)) url <- paste0("https://www.ers.usda.gov", url)
  url
}

## Downloads and parses one ERS CSV, checking the header is exactly what we
## expect. A restructured file must stop the run, not be silently misread.
fetch_ers_csv <- function(url, filename) {
  tmp <- tempfile(fileext = ".csv")
  ok <- tryCatch({
    req_perform(req_timeout(request(url), 120), path = tmp)
    TRUE
  }, error = function(e) {
    die("could not download ", filename, " from ", url, "\n  ", conditionMessage(e))
  })

  df <- tryCatch(
    read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) die("could not parse ", filename, " as CSV: ", conditionMessage(e))
  )
  unlink(tmp)

  if (!identical(names(df), EXPECTED_HEADER)) {
    die("unexpected column layout in ", filename, "\n",
        "    expected: ", paste(EXPECTED_HEADER, collapse = ", "), "\n",
        "    found   : ", paste(names(df), collapse = ", "), "\n",
        "  ERS has restructured this file - the parsing logic in this script needs\n",
        "  to be reviewed before the values can be trusted.")
  }
  if (nrow(df) == 0) die(filename, " parsed to zero rows.")
  df
}

## Most recent year with a usable value for one commodity/attribute pair.
latest_value <- function(df, block, attribute, filename, category) {
  sub <- if (is.na(block)) df else df[df$Commodity == block, , drop = FALSE]
  sub <- sub[sub$Attribute == attribute, , drop = FALSE]

  if (nrow(sub) == 0) {
    die("row '", attribute, "' not found in ", filename, " (needed for category '", category, "')\n",
        "  ERS has renamed or dropped this commodity. Update ERS_MAPPING in this\n",
        "  script after checking what it became - refusing to write a file that\n",
        "  silently omits a category.")
  }

  vals  <- suppressWarnings(as.numeric(sub$Value))
  years <- suppressWarnings(as.integer(sub$Year))
  keep  <- !is.na(vals) & !is.na(years)
  if (!any(keep)) {
    die("row '", attribute, "' in ", filename, " has no numeric values at all",
        " (needed for category '", category, "').")
  }

  vals <- vals[keep]; years <- years[keep]
  i <- which.max(years)
  value <- vals[i]; year <- years[i]

  if (value < 0) {
    die("row '", attribute, "' in ", filename, " has a negative value (", value,
        ") for ", year, " - per-capita availability cannot be negative.")
  }
  list(year = year, value = value)
}

## ---- ARGS -----------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
output_path <- OUTPUT_DEFAULT
if (length(args) >= 2 && args[1] == "--output") output_path <- args[2]

## ---- FETCH ----------------------------------------------------------

info("Resolving ERS download URLs from ", FADS_PAGE)

page <- tryCatch(
  resp_body_string(req_perform(req_timeout(request(FADS_PAGE), 60))),
  error = function(e) die("could not load the ERS landing page: ", conditionMessage(e))
)

resolved <- list(); data <- list()
for (key in names(SOURCE_FILES)) {
  spec <- SOURCE_FILES[[key]]
  url  <- resolve_url(page, spec$file)
  info("  ", spec$file, "  ->  ", url)
  data[[key]] <- fetch_ers_csv(url, spec$file)
  resolved[[key]] <- list(name = spec$file, url = url)
}

## ---- BUILD ----------------------------------------------------------

## Loss-adjusted cups/day, where the category has a LAFA counterpart.
lafa_cups <- function(lafa_commodity, category) {
  if (is.na(lafa_commodity)) return(NULL)
  block <- paste0(lafa_commodity, ": Per capita availability adjusted for loss")
  df <- data$lafa_veg
  sub <- df[df$Commodity == block & df$Attribute == LAFA_CUPS_ATTR, , drop = FALSE]
  if (nrow(sub) == 0) {
    die("loss-adjusted block '", block, "' not found in vegetables.csv",
        " (needed for category '", category, "').")
  }
  vals  <- suppressWarnings(as.numeric(sub$Value))
  years <- suppressWarnings(as.integer(sub$Year))
  keep  <- !is.na(vals) & !is.na(years)
  if (!any(keep)) return(NULL)
  vals <- vals[keep]; years <- years[keep]
  i <- which.max(years)
  list(year = years[i], value = vals[i])
}

baselines <- list()

for (category in sort(names(ERS_MAPPING))) {
  m    <- ERS_MAPPING[[category]]
  spec <- SOURCE_FILES[[m$src]]
  pc   <- latest_value(data[[m$src]], spec$block, m$attr, spec$file, category)
  cups <- lafa_cups(m$lafa, category)

  entry <- list(
    value_lb_per_year = round(pc$value, 4),
    ers_row           = sub("-Pounds$", "", m$attr),
    source            = "USDA ERS Food Availability",
    source_file       = spec$file,
    source_year       = pc$year,
    confidence        = "ers_exact_match"
  )
  if (!is.null(cups)) {
    entry$value_cups_per_day <- round(cups$value, 6)
    entry$cups_source        = "USDA ERS Loss-Adjusted Food Availability"
    entry$cups_source_year   <- cups$year
  }
  if (!is.null(m$note)) entry$note <- m$note

  baselines[[category]] <- entry
}

for (category in sort(names(AUTHOR_ESTIMATES))) {
  a <- AUTHOR_ESTIMATES[[category]]
  baselines[[category]] <- list(
    value_baseline_commonness = a$value,
    source                    = "author estimate (unsourced)",
    confidence                = "author_estimate",
    note                      = a$note
  )
}

## Deterministic key order so the diff reflects value changes only.
baselines <- baselines[sort(names(baselines))]

## ---- VALIDATE AGAINST THE PREVIOUS RUN ------------------------------

previous <- NULL
if (file.exists(output_path)) {
  previous <- tryCatch(
    fromJSON(output_path, simplifyVector = FALSE),
    error = function(e) die("existing ", output_path, " could not be parsed: ",
                            conditionMessage(e),
                            "\n  Repair or delete it before re-running.")
  )
}

if (!is.null(previous) && !is.null(previous$baselines)) {
  for (category in names(baselines)) {
    new_v <- baselines[[category]]$value_lb_per_year
    old_v <- previous$baselines[[category]]$value_lb_per_year
    if (is.null(new_v) || is.null(old_v) || old_v == 0) next
    fold <- max(new_v / old_v, old_v / new_v)
    if (fold > MAX_FOLD_CHANGE) {
      die("'", category, "' moved from ", old_v, " to ", new_v,
          " (", round(fold, 1), "x) between runs.\n",
          "  A genuine ERS revision is a few percent. A change this large means the\n",
          "  units or the underlying series probably changed upstream. Refusing to\n",
          "  write - check the ERS file by hand and update this script if the change\n",
          "  is real.")
    }
  }
}

## ---- WRITE (only if something actually changed) ---------------------

## Writing only on change is what makes this idempotent: a second run with no
## upstream movement leaves the file byte-identical, so the monthly workflow
## sees an empty diff and opens no PR. fetched_at therefore means "when these
## values were last retrieved AND found to differ", not "last time we looked".
## Compare canonical JSON text, not the R objects. identical() is too strict
## here: a JSON round-trip turns integer years into doubles, so comparing the
## parsed structures reports a difference on every run even when the numbers
## are the same, and the file would churn forever.
canonical <- function(x) as.character(toJSON(x, auto_unbox = TRUE, pretty = FALSE, digits = NA))

if (!is.null(previous) && identical(canonical(previous$baselines), canonical(baselines))) {
  info("\nNo change: ", output_path, " already matches the current ERS data.")
  info("  ", length(ERS_MAPPING), " ERS-sourced and ",
       length(AUTHOR_ESTIMATES), " author-estimated categories. File left untouched.")
  quit(status = 0)
}

payload <- list(
  fetched_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
  generated_by = "scripts/refresh-external-baselines.R",
  units_warning = paste(
    "value_lb_per_year is pounds of per-capita availability per year, NOT the",
    "0-100 'percent of population eating this in a 2-week period' scale that",
    "baseline_commonness uses in category_vocabulary.json. The two are not",
    "interchangeable. Nothing in the rendering pipeline reads this file yet."),
  source_page = FADS_PAGE,
  source_files = unname(resolved),
  baselines = baselines
)

tmp <- tempfile(pattern = ".baselines-external", tmpdir = dirname(output_path), fileext = ".json")
write_json(payload, tmp, auto_unbox = TRUE, pretty = TRUE, digits = NA)
if (!file.rename(tmp, output_path)) {
  unlink(tmp)
  die("could not write ", output_path)
}

info("\nWrote ", output_path)
info("  ", length(ERS_MAPPING), " ERS-sourced categories (latest ERS years present in file)")
info("  ", length(AUTHOR_ESTIMATES), " author estimates carried over, flagged as unsourced")
