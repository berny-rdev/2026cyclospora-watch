## =============================================================
## Crowdsourced Cyclospora Outbreak Investigation
## Google Form -> R analysis pipeline  (CASE-ONLY VERSION)
## =============================================================
##
## No control group = no odds ratios. What this script does instead
## is the same first move real outbreak investigators make before a
## formal case-control study exists: rank exposures by how often SICK
## people report them, and flag the ones that are uncommonly common -
## i.e. reported way more often than you'd expect for an everyday food.
## That's a hypothesis-generating signal, not proof.
##
## INPUT: two free-text, comma-separated fields -
##   1) raw produce eaten in the 2 weeks before symptoms
##   2) stores/restaurants shopped/eaten at in the 2 weeks before symptoms
## Respondents type things inconsistently ("bagged lettuce", "romaine",
## "lettuce from Trader Joe's") so Section 2 normalizes free text into
## clean categories using editable keyword dictionaries.
## =============================================================

## ---- 0. PACKAGES ----------------------------------------------------
required_pkgs <- c("googlesheets4", "dplyr", "tidyr", "stringr", "lubridate",
                    "ggplot2", "janitor", "purrr", "knitr", "httr2", "jsonlite")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs)

library(googlesheets4); library(dplyr); library(tidyr); library(stringr)
library(lubridate); library(ggplot2); library(janitor); library(purrr); library(knitr)

## Shared pipeline core, sourced by BOTH this script and index.Rmd. These
## functions used to be maintained by hand in both files and had already
## drifted apart: this script's classify_and_grow() never read the item maps,
## so a local run re-classified every cached phrase from scratch and could
## write answers that contradicted the ones the report had locked in. Sourcing
## the one copy removes that whole class of divergence.
source("R/columns.R")               # match_columns
source("R/text-normalize.R")       # normalize_punct, split_delims, is_negation, split_freetext
source("R/vocabulary-io.R")        # read/load/save vocabulary, stamp_category_provenance
source("R/checklist.R")            # checklist_map + classify_checklist
source("R/classify.R")             # regex + LLM classification, classify_and_grow
source("R/stats.R")                 # wilson_ci, add_wilson_ci
source("R/run-manifest.R")          # build/write run-manifest.json
source("R/vocabulary-integrity.R") # assert_utf8_locale, check_vocabulary_integrity

## Matters most HERE: `Rscript` on macOS defaults to LC_CTYPE=C, which silently
## turns normalize_punct() into a no-op, so a local run would fork a duplicate
## category off every curly apostrophe.
assert_utf8_locale()

## ---- 1. CONFIG -- EDIT FOR YOUR FORM ----------------------------------

SHEET_URL <- "https://docs.google.com/spreadsheets/d/1n1VJ99Ko7mvFQmKRX_QrFLFFziMAGKpkUNmK40QxLpU/edit?usp=sharing"
gs4_deauth()   # sheet must be "anyone with link can view"; comment out + use gs4_auth() if private

# Google Forms stuffs the ENTIRE question text (including instructions and
# examples) into the Sheet header, and can wrap/format it slightly
# differently than expected - so instead of matching the full exact
# question, we match on a short, distinctive KEYWORD/PHRASE that should
# appear ONLY in that one column's header. Much more robust than exact
# string matching. Edit the regex on the right if a column isn't matching -
# just needs to be a phrase unique to that question.
col_signatures <- c(
  timestamp        = "^timestamp$",
  consent          = "consent",
  state            = "what state",
  why_believe      = "why do you believe",
  produce_checklist = "did you eat any of the following",
  produce_other    = "anything else you remember eating",
  shop_raw         = "shop.?dine",
  duration         = "how long did symptoms last",
  onset_date       = "when did symptoms start",
  high_confidence_meal = "fairly confident caused this"
)

## Without `consent` the filter that drops non-consenting responses is skipped
## silently, so it is required rather than optional. Either produce column will
## do, but with neither there is no exposure data to analyse.
REQUIRED_COLUMNS    <- c("consent")
REQUIRE_ANY_COLUMNS <- list(c("produce_checklist", "produce_other"))

## ---- 2. SEED DICTIONARIES (starting point only, not a hard ceiling) ------
## These seed the category vocabulary on the very first run and double as
## the OFFLINE FALLBACK if the LLM is unavailable. But the real, growing
## vocabulary lives in category_vocabulary.json (created automatically) -
## once a category like "cilantro" exists there, every future run reuses
## it; genuinely new foods (e.g. "purslane") get their own new category
## instead of being dumped into a permanent "other" bucket.

## KEEP IN SYNC WITH index.Rmd. Both files seed the same shared
## category_vocabulary.json, so a category that exists here but not there
## gets unioned into the live vocabulary on any local run - which is how
## the obsolete flat "lettuce" category kept coming back.

produce_dict_seed <- list(
  # Lettuce is split into whole-head vs prepackaged/bagged - this
  # distinction matters a lot for traceback (prepackaged/bagged salad is
  # the recurring outbreak vehicle, whole heads much less so), so we keep
  # them as separate categories rather than merging into one "lettuce".
  romaine_head        = "romaine.*head|head.*romaine|whole romaine",
  romaine_prepackaged = "romaine.*(prepackag|bagged)|bagged romaine",
  iceberg_head        = "iceberg.*head|head.*iceberg|whole iceberg",
  iceberg_prepackaged = "iceberg.*(prepackag|bagged)|bagged iceberg",
  mesclun_spring_mix  = "mesclun|spring mix",
  spinach      = "spinach",
  cilantro     = "cilantro|coriander",
  basil        = "\\bbasil\\b",
  parsley      = "parsley",
  raspberries  = "raspberr",
  strawberries = "strawberr",
  blackberries = "blackberr",
  cucumber     = "cucumber",
  tomato       = "tomato",
  snap_peas    = "snap pea|sugar snap",
  green_onion  = "green onion|scallion|spring onion",
  cabbage      = "cabbage|slaw",
  carrot       = "carrot",
  broccoli     = "broccoli",
  melon        = "cantaloupe|honeydew|melon",
  bell_pepper  = "bell pepper|sweet pepper",
  avocado      = "avocado|guacamole",
  celery       = "celery",
  cauliflower  = "cauliflower",
  dill         = "\\bdill\\b",
  radish       = "radish",
  mint         = "\\bmint\\b",
  # Generic bagged/premade salad where the specific lettuce type isn't
  # named - catches free-text synonyms like "premade salad", "pre-made
  # side salad", "grab and go salad" so they land here instead of
  # spawning a near-duplicate category.
  salad_bagged = "bagged salad|salad kit|salad mix|premade salad|pre-made salad|pre made salad|ready.to.eat salad|grab.and.go salad",
  salad_restaurant = "restaurant salad|salad bar"
)

store_dict_seed <- list(
  kroger        = "kroger",
  trader_joes   = "trader joe",
  whole_foods   = "whole foods",
  walmart       = "wal[- ]?mart",
  target        = "\\btarget\\b",
  publix        = "publix",
  aldi          = "aldi",
  costco        = "costco",
  safeway       = "safeway",
  wegmans       = "wegmans",
  taco_bell     = "taco bell",
  chipotle      = "chipotle",
  subway        = "subway",
  mcdonalds     = "mcdonald",
  local_farmers_market = "farmers?[- ]?market",
  local_restaurant = "^restaurant$|local restaurant"
)

## Everyday-ness reference: rough population baseline for how commonly
## these items appear in a typical American diet, so a food isn't flagged
## just for being popular. Coarse editable guess, not a citation - tune it.
## Scale: 0-100 = roughly what % of a general population eats this in any
## given 2-week period. NEW categories the LLM creates won't have an entry
## here automatically (there's no way to guess a sensible number for a food
## nobody's told us about yet) - the script will flag these each run so you
## can add a number to category_vocabulary.json's baseline_commonness object.
baseline_commonness_seed <- c(
  # Old single "lettuce" baseline (55) split across the new granular
  # categories - rough guesses, tune as you like.
  romaine_head = 15, romaine_prepackaged = 20, iceberg_head = 10,
  iceberg_prepackaged = 12, mesclun_spring_mix = 8,
  spinach = 25, cilantro = 20, basil = 10, parsley = 12,
  raspberries = 10, strawberries = 30, blackberries = 8, cucumber = 30,
  tomato = 45, snap_peas = 8, green_onion = 20, cabbage = 15, carrot = 40,
  broccoli = 35, melon = 20, bell_pepper = 30, avocado = 35, celery = 15,
  cauliflower = 15, dill = 8,
  radish = 6, mint = 5, salad_bagged = 25, salad_restaurant = 20
)

## ---- 2b. PERSISTENT CATEGORY VOCABULARY -----------------------------------
## This JSON file is the source of truth for "what categories exist so
## far" and grows across runs. If you're running this locally, commit and
## push the updated file so the live page's next run sees your additions
## too (or vice versa - pull before you run locally).

VOCAB_PATH <- "category_vocabulary.json"

vocab <- load_vocabulary(VOCAB_PATH, names(produce_dict_seed), names(store_dict_seed), baseline_commonness_seed)


## ---- 3. PULL + CLEAN DATA -------------------------------------------------

raw <- read_sheet(SHEET_URL)
present_map <- match_columns(names(raw), col_signatures,
                              required = REQUIRED_COLUMNS,
                              require_any = REQUIRE_ANY_COLUMNS)
# (The "these columns could not be matched" note that used to sit here is now
# inside match_columns(), so both entry points report it the same way.)

df <- raw %>%
  select(all_of(unname(present_map))) %>%
  rename(!!!present_map) %>%
  clean_names() %>%
  mutate(response_id = row_number())

n_before_consent <- nrow(df)

if ("consent" %in% names(df)) {
  df <- df %>%
    filter(str_detect(str_to_lower(str_trim(as.character(consent))), "^yes"))
}

n_total <- nrow(df)
cat(sprintf("\nLoaded %d responses, %d after keeping only consented responses (all treated as cases - no control group in this form)\n",
            n_before_consent, n_total))

## Duration summary - closest thing this form has to a "does this look like
## cyclospora and not just a stomach bug" filter. Cyclospora illness tends to
## run longer (often weeks, sometimes relapsing) than most foodborne bugs.
if ("duration" %in% names(df)) {
  duration_summary <- df %>%
    count(duration, sort = TRUE, name = "n_responses") %>%
    mutate(pct = round(100 * n_responses / n_total, 1))
  cat("\n===== Reported symptom duration =====\n")
  print(kable(duration_summary))
  write.csv(duration_summary, "duration_summary.csv", row.names = FALSE)
}

## ---- 4. CLASSIFY FREE-TEXT EXPOSURES (grows the vocabulary as it goes) ----
## LLM does the primary classification - it handles typos, hedged phrasing
## ("I think it was romaine?"), and mints new categories for genuinely new
## foods instead of dumping them in "other". Requires your own Anthropic
## API key (separate account from claude.ai) - get one at
## console.anthropic.com, then set it before running:
##   Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-...")
## Only DISTINCT raw phrases are sent (not one call per response). If the
## API call fails for any reason, automatically falls back to the regex
## seed dictionaries so the pipeline never crashes.

CLASSIFICATION_METHOD <- "llm"   # "llm" (recommended) or "regex" (free, no API key needed)

## Checkbox answers come through as one comma-separated cell, same shape as
## the free-text answers - so we just concatenate the two into a single
## string per person before splitting into items. Everything downstream
## (classification, vocabulary growth) works unchanged; checklist items
## will classify essentially perfectly since they're already exact
## category-shaped text (e.g. "Fresh basil", "Snow peas").
## The two produce columns are NO LONGER concatenated. They are different
## kinds of data and get different treatment: checklist answers are preset
## strings resolved by direct lookup, free text goes to the classifier.
produce_checklist_raw <- if ("produce_checklist" %in% names(df)) as.character(df$produce_checklist) else NULL
produce_freetext_raw  <- if ("produce_other" %in% names(df)) as.character(df$produce_other) else NULL

## CHECKLIST PATH - direct lookup, no LLM, no regex, no cache writes.
produce_checklist_long <- if (!is.null(produce_checklist_raw)) {
  classify_checklist(produce_checklist_raw) %>%
    mutate(response_id = row_id, source_type = "checklist_direct", .keep = "unused")
} else tibble()

## FREE-TEXT PATH - unchanged: classify, grow the vocabulary, cache.
produce_freetext_long <- if (!is.null(produce_freetext_raw)) {
  r <- classify_and_grow(produce_freetext_raw, vocab$produce_categories, vocab$produce_item_map,
                         produce_dict_seed, domain = "produce", method = CLASSIFICATION_METHOD)
  vocab$produce_categories <- r$vocab
  vocab$produce_item_map   <- r$item_map
  r$long %>% mutate(response_id = row_id, source_type = "freetext_classified", .keep = "unused")
} else tibble()

produce_long <- bind_rows(produce_checklist_long, produce_freetext_long)
vocab$produce_categories <- union(vocab$produce_categories,
                                  unique(unlist(unname(checklist_map))))

## Label any category that arrived without provenance - newly minted by the
## classifier, pulled in by the checklist union, or added by hand. Runs after
## every route that can add one, and never overwrites an existing label.
vocab <- stamp_category_provenance(vocab, unique(unlist(unname(checklist_map))))

if ("shop_raw" %in% names(df)) {
  store_result <- classify_and_grow(df$shop_raw, vocab$store_categories, vocab$store_item_map,
                                    store_dict_seed, domain = "store", method = CLASSIFICATION_METHOD)
  store_long <- store_result$long %>% mutate(response_id = row_id, .keep = "unused")
  vocab$store_categories <- store_result$vocab
  vocab$store_item_map   <- store_result$item_map
} else {
  store_long <- tibble()
}

## Categories that exist but don't have a baseline_commonness number yet -
## these won't show up in the signal ratio table until you add a rough
## "how often does a normal person eat this" guess to
## category_vocabulary.json's baseline_commonness object.
categories_missing_baseline <- setdiff(unique(produce_long$category), names(vocab$baseline_commonness))
if (length(categories_missing_baseline)) {
  cat("\nNEW produce categories with no baseline_commonness yet (won't appear in signal ratio until you add one to category_vocabulary.json):\n  - ",
      paste(categories_missing_baseline, collapse = "\n  - "), "\n")
}

## ---- 5. FREQUENCY ANALYSIS (case-only "proportional reporting" ranking) --
## Includes a 95% Wilson score confidence interval on each proportion - a
## food reported by 100% of cases means very different things at n=2 vs
## n=50, and the CI makes that visible instead of hiding it behind a
## single misleadingly precise-looking percentage. Wilson (not the naive
## normal-approximation interval) because it stays sane even for small n
## and proportions near 0% or 100%, which is exactly the regime a young
## crowdsourced dataset lives in. Base R only, no extra package needed.


produce_freq <- produce_long %>%
  distinct(response_id, category) %>%          # count each person once per food even if mentioned twice
  count(category, sort = TRUE, name = "n_cases") %>%
  mutate(pct_of_cases = round(100 * n_cases / n_total, 1)) %>%
  add_wilson_ci(n_total)

store_freq <- store_long %>%
  distinct(response_id, category) %>%
  count(category, sort = TRUE, name = "n_cases") %>%
  mutate(pct_of_cases = round(100 * n_cases / n_total, 1)) %>%
  add_wilson_ci(n_total)

cat("\n===== PRODUCE reported by cases, most common first =====\n")
cat("(ci_low_pct/ci_high_pct = 95% confidence interval on the true % - wide intervals mean small sample, don't over-read them)\n")
print(kable(produce_freq, caption = "Produce frequency among cases"))

cat("\n===== STORES/RESTAURANTS reported by cases, most common first =====\n")
print(kable(store_freq, caption = "Store/restaurant frequency among cases"))

## ---- 6. "UNUSUAL SIGNAL" FLAGGING -----------------------------------------
## Uses vocab$baseline_commonness (seeded from baseline_commonness_seed
## above, persisted/growable in category_vocabulary.json). Categories
## without a baseline number just don't get a signal ratio - see the
## "categories_missing_baseline" message printed in Section 4.
## signal_ratio_low/high propagate the same Wilson CI through the ratio
## (dividing the CI bounds on pct_of_cases by the fixed baseline) so you
## can see the plausible RANGE of the signal, not just a point estimate
## that looks falsely precise at low n.

produce_signal <- produce_freq %>%
  mutate(baseline_pct = unlist(vocab$baseline_commonness)[category]) %>%
  filter(!is.na(baseline_pct)) %>%
  mutate(
    signal_ratio = round(pct_of_cases / baseline_pct, 2),
    signal_ratio_low = round(ci_low_pct / baseline_pct, 2),
    signal_ratio_high = round(ci_high_pct / baseline_pct, 2)
  ) %>%
  # Sort by the CONSERVATIVE (lower-bound CI) ratio, not the raw point
  # estimate - this is the "likely causality" ranking a real epi
  # investigator would use. A food at 5.0x based on 1 person is a much
  # weaker lead than a food at 3.75x based on 4 people; ranking by the
  # low end of the interval automatically discounts small-n flukes and
  # rewards foods that stay elevated even under the pessimistic estimate.
  arrange(desc(signal_ratio_low), desc(signal_ratio))

cat("\n===== SIGNAL RATIO: reported-by-cases % vs. typical population baseline % =====\n")
cat("(ratio well above 1 = shows up in cases way more than you'd expect from normal eating habits - that's your lead list)\n")
cat("(signal_ratio_low/high = 95% CI range on that ratio - if this range still sits above 1 even at its low end, that's a much stronger lead than a point estimate alone)\n")
print(kable(produce_signal, caption = "Signal ratio (case % / baseline %)"))

signal_plot <- produce_signal %>%
  filter(n_cases >= 3) %>%   # ignore items only 1-2 people mentioned, too noisy
  ggplot(aes(x = reorder(category, signal_ratio_low), y = signal_ratio)) +
  geom_col(fill = "#c0392b") +
  geom_errorbar(aes(ymin = signal_ratio_low, ymax = signal_ratio_high), width = 0.3, color = "gray30") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  coord_flip() +
  labs(title = "Produce Signal Ratio (cases vs. everyday baseline)",
       subtitle = "Bars = point estimate, whiskers = 95% CI. Above dashed line = more than expected for a normal diet.",
       x = NULL, y = "Signal ratio") +
  theme_minimal(base_size = 13)

print(signal_plot)
ggsave("produce_signal.png", signal_plot, width = 9, height = 6, dpi = 150)

## ---- 7. EPI CURVE (real onset dates now collected) ------------------------

if ("onset_date" %in% names(df)) {
  # Try a handful of common formats in one pass rather than guessing one at
  # a time - covers most of what Sheets/Forms might store a date as.
  epi_curve_data <- df %>%
    mutate(onset_date = suppressWarnings(
      lubridate::parse_date_time(onset_date,
        orders = c("mdy", "ymd", "dmy", "mdY HMS", "ymd HMS"))
    )) %>%
    mutate(onset_date = as_date(onset_date)) %>%
    filter(!is.na(onset_date)) %>%
    count(onset_date, name = "cases")

  if (nrow(epi_curve_data) > 0) {
    epi_plot <- ggplot(epi_curve_data, aes(onset_date, cases)) +
      geom_col(fill = "#c0392b") +
      scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
      labs(title = "Epi Curve: Self-Reported Symptom Onset Dates",
           x = "Onset date", y = "Cases") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    print(epi_plot)
    ggsave("epi_curve.png", epi_plot, width = 10, height = 5, dpi = 150)
    write.csv(epi_curve_data, "epi_curve_data.csv", row.names = FALSE)
  } else if (n_total > 0) {
    message("onset_date column found but no dates parsed. Run `print(df$onset_date)` ",
            "to see the raw values and add that format to the `orders` vector above.")
  }
}

## ---- 7b. HIGH-CONFIDENCE INDIVIDUAL REPORTS (anecdotal, NOT aggregated) ---
## A small number of people may have strong, specific recall about what
## caused their illness. These are valuable as human-readable leads for
## whoever's investigating, but must NEVER be folded into the aggregate
## signal-ratio math above - one confident person's guess shouldn't move
## a population-level statistic. Kept as a raw, unclassified list instead.

if ("high_confidence_meal" %in% names(df)) {
  high_confidence_reports <- df %>%
    filter(!is.na(high_confidence_meal), str_trim(as.character(high_confidence_meal)) != "") %>%
    select(any_of(c("state", "onset_date", "high_confidence_meal")))

  if (nrow(high_confidence_reports) > 0) {
    cat("\n===== HIGH-CONFIDENCE INDIVIDUAL REPORTS (anecdotal - not part of aggregate stats) =====\n")
    print(kable(high_confidence_reports, caption = "Individual high-confidence suspected meals"))
    write.csv(high_confidence_reports, "high_confidence_reports.csv", row.names = FALSE)
  }
}

## ---- 8. SAVE OUTPUTS -----------------------------------------------------

write.csv(produce_freq, "produce_frequency.csv", row.names = FALSE)
write.csv(store_freq, "store_frequency.csv", row.names = FALSE)
write.csv(produce_signal, "produce_signal_ratio.csv", row.names = FALSE)
## Checked here rather than next to stamp_category_provenance(), because that
## runs before the store classification and cannot see categories minted by it.
vocab <- check_vocabulary_integrity(vocab)
save_vocabulary(vocab, VOCAB_PATH)

cat("\nDone. Key file: produce_signal_ratio.csv - sort by signal_ratio descending for your current top leads.\n")
cat("Vocabulary saved to", VOCAB_PATH, "- if you're running this locally, commit + push it so the live page sees your new categories too.\n")

## ---- INTERPRETATION NOTES -------------------------------------------------
## - This is case-only proportional reporting, not a case-control study.
##   Real confirmation needs a comparison group and eventually lab/traceback
##   work by state health departments or FDA/CDC. Treat every result here
##   as "worth asking louder questions about," not "confirmed cause."
## - Recall bias is real: people who got sick rack their brains harder to
##   remember what they ate than people filling out a control survey would.
##   That alone can inflate reporting of "memorable" foods.
## - Small samples make the signal_ratio noisy - the n_cases >= 3 filter
##   on the plot is there to keep single weird responses from dominating.
## - New categories the LLM creates won't have a signal ratio until you add
##   a baseline_commonness number for them in category_vocabulary.json -
##   check the "NEW produce categories with no baseline_commonness" message
##   each run.
