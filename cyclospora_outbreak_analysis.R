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

match_columns <- function(actual_names, signatures) {
  matched <- vapply(signatures, function(sig) {
    hits <- actual_names[str_detect(str_to_lower(actual_names), str_to_lower(sig))]
    if (length(hits) == 0) NA_character_ else hits[1]
  }, character(1))
  matched[!is.na(matched)]
}

## ---- 2. SEED DICTIONARIES (starting point only, not a hard ceiling) ------
## These seed the category vocabulary on the very first run and double as
## the OFFLINE FALLBACK if the LLM is unavailable. But the real, growing
## vocabulary lives in category_vocabulary.json (created automatically) -
## once a category like "cilantro" exists there, every future run reuses
## it; genuinely new foods (e.g. "purslane") get their own new category
## instead of being dumped into a permanent "other" bucket.

produce_dict_seed <- list(
  lettuce      = "lettuce|romaine|iceberg|spring mix|spinach mix|leafy green",
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
  salad_bagged = "bagged salad|salad kit|salad mix",
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
  lettuce = 55, spinach = 25, cilantro = 20, basil = 10, parsley = 12,
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

load_vocabulary <- function(path, seed_produce, seed_store, seed_baseline) {
  if (file.exists(path)) {
    v <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
  } else {
    v <- NULL
  }
  list(
    produce_categories = union(if (!is.null(v)) v$produce_categories else character(0), seed_produce),
    store_categories   = union(if (!is.null(v)) v$store_categories else character(0), seed_store),
    baseline_commonness = modifyList(
      as.list(seed_baseline),
      if (!is.null(v) && !is.null(v$baseline_commonness)) as.list(v$baseline_commonness) else list()
    )
  )
}

save_vocabulary <- function(vocab, path) {
  jsonlite::write_json(vocab, path, auto_unbox = TRUE, pretty = TRUE)
}

vocab <- load_vocabulary(VOCAB_PATH, names(produce_dict_seed), names(store_dict_seed), baseline_commonness_seed)

split_freetext <- function(raw_text_vec) {
  # Splits comma-separated free text into individual trimmed/lowercased items.
  # No classification here - just tokenizing.
  tibble(row_id = seq_along(raw_text_vec), raw = raw_text_vec) %>%
    filter(!is.na(raw), str_trim(raw) != "") %>%
    separate_rows(raw, sep = ",") %>%
    mutate(item = str_trim(str_to_lower(raw))) %>%
    filter(item != "")
}

regex_classify <- function(item, dict) {
  hit <- names(dict)[map_lgl(dict, ~ str_detect(item, .x))]
  if (length(hit) == 0) return(str_to_title(item))
  hit[1]
}

## Classifies DISTINCT raw items against a GROWING vocabulary: if an item
## matches a category that already exists, it's reused; if it's genuinely
## new, the LLM mints a new lowercase_snake_case category name instead of
## defaulting to "other". Falls back to the regex seed dictionary (which
## can't invent new categories, only title-case unmatched text) if the API
## call fails for any reason - the pipeline never crashes either way.
## Returns list(mapping = named vector item->category, vocab = updated
## character vector of all known categories after this batch).
classify_items_dynamic <- function(items, known_categories, dict, domain = c("produce", "store"), method = c("llm", "regex")) {
  domain <- match.arg(domain)
  method <- match.arg(method)
  if (length(items) == 0) return(list(mapping = character(0), vocab = known_categories))

  if (method == "regex") {
    mapping <- setNames(map_chr(items, ~ regex_classify(.x, dict)), items)
    return(list(mapping = mapping, vocab = union(known_categories, unique(mapping))))
  }

  result <- call_claude_classify_dynamic(items, known_categories, domain = domain)
  if (is.null(result) || length(result) == 0) {
    warning("LLM classification failed - falling back to regex dictionary for this batch.")
    mapping <- setNames(map_chr(items, ~ regex_classify(.x, dict)), items)
    return(list(mapping = mapping, vocab = union(known_categories, unique(mapping))))
  }

  mapping <- unlist(result)[items]
  names(mapping) <- items
  # normalize any category the LLM returns to lowercase_snake_case so
  # "Purslane" and "purslane" don't become two different categories
  mapping <- str_replace_all(str_to_lower(str_trim(mapping)), "[^a-z0-9]+", "_")
  mapping <- str_replace_all(mapping, "^_|_$", "")
  names(mapping) <- items
  missing <- is.na(mapping) | mapping == ""
  if (any(missing)) mapping[missing] <- map_chr(items[missing], ~ regex_classify(.x, dict))

  list(mapping = mapping, vocab = union(known_categories, unique(mapping)))
}

call_claude_classify_dynamic <- function(items, known_categories, domain = c("produce", "store"),
                                          api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                                          model = "claude-haiku-4-5-20251001") {
  domain <- match.arg(domain)
  if (identical(api_key, "")) {
    warning("ANTHROPIC_API_KEY not set - falling back to regex classification.")
    return(NULL)
  }
  if (length(items) == 0) return(NULL)

  ## PRODUCE and STORE need opposite classification instincts:
  ## - Produce: GENERALIZE. "romaine" and "bagged lettuce" should both
  ##   become "lettuce" - that's the whole point of the category system.
  ## - Store/restaurant: NEVER generalize into a business-type bucket like
  ##   "regional_grocery" or "fast_casual" - that destroys the one thing
  ##   that matters for a traceback (which SPECIFIC place was it). Only
  ##   merge spelling/phrasing variants of the SAME named place.
  domain_instructions <- if (domain == "produce") {
    paste0(
      "1. If it clearly matches an EXISTING category, use that EXACT category name - do not ",
      "create a near-duplicate or synonym (e.g. don't make \"fresh_cilantro\" if \"cilantro\" ",
      "already exists).\n",
      "2. If it genuinely doesn't fit any existing category, invent ONE new concise category ",
      "name in lowercase_snake_case (1-3 words, e.g. \"purslane\" or \"bean_sprouts\") that ",
      "could sensibly apply to future similar items too. Do NOT use \"other\" - always pick or ",
      "create a real, specific category.\n",
      "3. If multiple items in this batch describe the same new food, give them the SAME new ",
      "category name.\n"
    )
  } else {
    paste0(
      "1. Your ONLY job is to normalize SPELLING/PHRASING variants of the SAME specific named ",
      "place into one category (e.g. \"Krogers\", \"the kroger on main\", \"kroger grocery\" all ",
      "become \"kroger\"). If an EXISTING category is clearly the same specific place, use that ",
      "EXACT category name.\n",
      "2. NEVER invent a generic business-type category like \"regional_grocery\", ",
      "\"casual_dining\", \"fast_casual\", \"grocery_store\", or \"local_restaurant\" - these hide ",
      "the actual place and are USELESS for a foodborne illness traceback, which requires ",
      "knowing exactly which specific establishment was involved.\n",
      "3. If the item names any identifiable specific business, chain, or restaurant (even a ",
      "small independent one you don't otherwise know), use a lowercase_snake_case version of ",
      "THAT EXACT NAME as the category (e.g. \"harris_teeter\", \"joes_pizza_downtown\"). Do not ",
      "abstract it into a category of business.\n",
      "4. ONLY if the respondent's answer truly contains NO identifiable name at all (e.g. they ",
      "wrote just \"a restaurant\" or \"the grocery store\" with zero distinguishing details) use ",
      "exactly \"unspecified_restaurant\" or \"unspecified_grocery_store\" - do not invent any ",
      "other generic bucket beyond these two exact fallback labels.\n"
    )
  }

  prompt <- paste0(
    "You are maintaining a GROWING category taxonomy for a citizen-science foodborne ",
    "illness investigation. Respondents write vague, hedged, or typo'd free text (e.g. ",
    "\"idk maybe romaine?\", \"bagged salad mix i think\").\n\n",
    "Categories that ALREADY EXIST: ", paste(known_categories, collapse = ", "), "\n\n",
    "For each item below:\n",
    domain_instructions, "\n",
    "Respond with ONLY a raw JSON object mapping each exact input item (as written, as the ",
    "key) to its category (as the value). No prose, no markdown code fences.\n\nItems:\n",
    paste0("- ", items, collapse = "\n")
  )

  resp <- tryCatch({
    httr2::request("https://api.anthropic.com/v1/messages") %>%
      httr2::req_headers(
        "x-api-key" = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type" = "application/json"
      ) %>%
      httr2::req_body_json(list(
        model = model,
        max_tokens = 4096,
        messages = list(list(role = "user", content = prompt))
      )) %>%
      httr2::req_perform()
  }, error = function(e) { warning("Claude API request failed: ", conditionMessage(e)); NULL })

  if (is.null(resp)) return(NULL)

  text_out <- httr2::resp_body_json(resp)$content[[1]]$text
  text_out <- str_remove_all(text_out, "```json|```")
  tryCatch(jsonlite::fromJSON(text_out), error = function(e) {
    warning("Couldn't parse JSON back from Claude - falling back to regex classification.")
    NULL
  })
}

## Classifies free text AND grows the vocabulary in one step. Returns
## list(long = long-format dataframe with categories, vocab = updated
## character vector of categories to feed back into the vocabulary object).
classify_and_grow <- function(raw_text_vec, known_categories, dict, domain = "produce", method = "llm") {
  long <- split_freetext(raw_text_vec)
  if (nrow(long) == 0) return(list(long = long %>% mutate(category = character(0)), vocab = known_categories))
  distinct_items <- unique(long$item)
  result <- classify_items_dynamic(distinct_items, known_categories, dict, domain = domain, method = method)
  list(long = long %>% mutate(category = unname(result$mapping[item])), vocab = result$vocab)
}

## ---- 3. PULL + CLEAN DATA -------------------------------------------------

raw <- read_sheet(SHEET_URL)
present_map <- match_columns(names(raw), col_signatures)
missing_cols <- setdiff(names(col_signatures), names(present_map))
if (length(missing_cols)) {
  message("NOTE: these expected columns could not be matched by keyword and will be skipped:\n  - ",
          paste(missing_cols, collapse = "\n  - "),
          "\n  Actual column names in your sheet are:\n  - ",
          paste(names(raw), collapse = "\n  - "))
}

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
produce_combined <- if (all(c("produce_checklist", "produce_other") %in% names(df))) {
  paste(coalesce(as.character(df$produce_checklist), ""), coalesce(as.character(df$produce_other), ""), sep = ", ")
} else if ("produce_checklist" %in% names(df)) {
  df$produce_checklist
} else if ("produce_other" %in% names(df)) {
  df$produce_other
} else {
  NULL
}

if (!is.null(produce_combined)) {
  produce_result <- classify_and_grow(produce_combined, vocab$produce_categories, produce_dict_seed, domain = "produce", method = CLASSIFICATION_METHOD)
  produce_long <- produce_result$long %>% mutate(response_id = row_id, .keep = "unused")
  vocab$produce_categories <- produce_result$vocab
} else {
  produce_long <- tibble()
}

if ("shop_raw" %in% names(df)) {
  store_result <- classify_and_grow(df$shop_raw, vocab$store_categories, store_dict_seed, domain = "store", method = CLASSIFICATION_METHOD)
  store_long <- store_result$long %>% mutate(response_id = row_id, .keep = "unused")
  vocab$store_categories <- store_result$vocab
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

wilson_ci <- function(x, n, conf_level = 0.95) {
  if (n == 0) return(c(lower = NA_real_, upper = NA_real_))
  p_hat <- x / n
  z <- qnorm(1 - (1 - conf_level) / 2)
  denom <- 1 + z^2 / n
  center <- p_hat + z^2 / (2 * n)
  adj <- z * sqrt((p_hat * (1 - p_hat) + z^2 / (4 * n)) / n)
  c(lower = max(0, (center - adj) / denom), upper = min(1, (center + adj) / denom))
}

add_wilson_ci <- function(freq_df, n_total) {
  ci <- purrr::map2_dfr(freq_df$n_cases, n_total, ~ as.list(wilson_ci(.x, .y)))
  freq_df %>%
    mutate(ci_low_pct = round(ci$lower * 100, 1), ci_high_pct = round(ci$upper * 100, 1))
}

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
