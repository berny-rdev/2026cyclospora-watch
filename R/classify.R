## ---- FREE-TEXT CLASSIFICATION ----
##
## Shared by index.Rmd and cyclospora_outbreak_analysis.R. Extracted verbatim
## from index.Rmd, which was the newer of the two copies.

regex_classify <- function(item, dict) {
  hit <- names(dict)[map_lgl(dict, ~ str_detect(item, .x))]
  # No seed pattern matched. Return NA rather than str_to_title(item):
  # echoing the respondent's raw text back as a category name is how
  # entries like "No. I Made Sure To Not :(" and "Bibb Lettuce Head"
  # became permanent categories. NA means "undecided this run" - the
  # item is left uncategorized and retried next run, when the LLM may
  # well be reachable again.
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# Categories only ever enter the vocabulary from a real decision.
# NA means undecided, so it must never be unioned in.
merge_categories <- function(known_categories, mapping) {
  union(known_categories, unique(mapping[!is.na(mapping)]))
}

# LLM does the primary classification and grows the category vocabulary -
# reuses existing categories where they fit, mints new lowercase_snake_case
# ones for genuinely new foods instead of dumping them in "other". Falls
# back to regex automatically if the API call fails.

call_claude_classify_dynamic <- function(items, known_categories, domain = c("produce", "store"),
                                          api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                                          model = "claude-haiku-4-5-20251001") {
  domain <- match.arg(domain)
  ## Both conditions returned NULL silently before. A missing key is worth
  ## saying out loud - it silently downgrades the whole run to regex, which is
  ## a real change in output quality and previously left no trace in the
  ## Action log. An empty batch is not a problem and stays quiet.
  if (identical(api_key, "")) {
    warning("ANTHROPIC_API_KEY not set - falling back to regex classification.")
    return(NULL)
  }
  if (length(items) == 0) return(NULL)

  domain_instructions <- if (domain == "produce") {
    paste0(
      "1. If it clearly matches an EXISTING category, use that EXACT category name - do not ",
      "create a near-duplicate or synonym (e.g. don't make \"fresh_cilantro\" if \"cilantro\" ",
      "already exists, or \"pre_made_salad\" if \"salad_bagged\" already exists - these describe ",
      "the same thing).\n",
      "2. For lettuce/salad specifically, PRESERVE the distinction between whole-head produce ",
      "(e.g. \"romaine_head\", \"iceberg_head\") and prepackaged/bagged forms (e.g. ",
      "\"romaine_prepackaged\", \"iceberg_prepackaged\", \"salad_bagged\") when the respondent's ",
      "answer indicates which form it was - this distinction matters for outbreak traceback. If ",
      "the form isn't specified and it's a generic mixed salad, use the existing generic ",
      "\"salad_bagged\" category rather than inventing a new one.\n",
      "3. If it genuinely doesn't fit any existing category, invent ONE new concise category ",
      "name in lowercase_snake_case (1-3 words, e.g. \"purslane\" or \"bean_sprouts\") that ",
      "could sensibly apply to future similar items too. Do NOT use \"other\" - always pick or ",
      "create a real, specific category.\n",
      "4. If multiple items in this batch describe the same new food, give them the SAME new ",
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
    "illness investigation. Respondents write vague, hedged, or typo'd free text.\n\n",
    "Categories that ALREADY EXIST: ", paste(known_categories, collapse = ", "), "\n\n",
    "For each item below:\n",
    domain_instructions, "\n",
    "Respond with ONLY a raw JSON object mapping each exact input item (as written, as the ",
    "key) to its category (as the value). No prose, no markdown code fences.\n\nItems:\n",
    # Items are already punctuation-normalised by split_freetext(), but
    # normalise again here so this function is safe to call directly: these
    # strings become JSON keys that Claude must echo back verbatim, and a
    # curly apostrophe on either side breaks the exact-name lookup.
    paste0("- ", normalize_punct(items), collapse = "\n")
  )
  resp <- tryCatch({
    httr2::request("https://api.anthropic.com/v1/messages") %>%
      httr2::req_headers("x-api-key" = api_key, "anthropic-version" = "2023-06-01",
                          "content-type" = "application/json") %>%
      httr2::req_body_json(list(model = model, max_tokens = 4096,
                                 messages = list(list(role = "user", content = prompt)))) %>%
      httr2::req_perform()
  }, error = function(e) NULL)
  if (is.null(resp)) return(NULL)
  text_out <- str_remove_all(httr2::resp_body_json(resp)$content[[1]]$text, "```json|```")
  tryCatch(jsonlite::fromJSON(text_out), error = function(e) NULL)
}

classify_items_dynamic <- function(items, known_categories, dict, domain = c("produce", "store"), method = c("llm", "regex")) {
  domain <- match.arg(domain)
  method <- match.arg(method)
  if (length(items) == 0) return(list(mapping = character(0), vocab = known_categories))
  if (method == "regex") {
    mapping <- setNames(map_chr(items, ~ regex_classify(.x, dict)), items)
    return(list(mapping = mapping, vocab = merge_categories(known_categories, mapping)))
  }
  result <- call_claude_classify_dynamic(items, known_categories, domain = domain)
  if (is.null(result) || length(result) == 0) {
    warning("LLM classification failed - falling back to regex dictionary for this batch.")
    mapping <- setNames(map_chr(items, ~ regex_classify(.x, dict)), items)
    return(list(mapping = mapping, vocab = merge_categories(known_categories, mapping)))
  }
  mapping <- unlist(result)[items]
  mapping <- str_replace_all(str_to_lower(str_trim(mapping)), "[^a-z0-9]+", "_")
  mapping <- str_replace_all(mapping, "^_|_$", "")
  names(mapping) <- items
  missing <- is.na(mapping) | mapping == ""
  if (any(missing)) mapping[missing] <- map_chr(items[missing], ~ regex_classify(.x, dict))
  list(mapping = mapping, vocab = merge_categories(known_categories, mapping))
}

## Classifies free text AND grows the vocabulary in one step - CACHE-FIRST:
## any raw phrase already decided in a previous run gets its locked-in
## answer from item_map, no LLM call, no chance of drifting to a
## different answer. Only phrases genuinely never seen before go to the
## LLM, and that one-time decision gets saved into item_map permanently.
classify_and_grow <- function(raw_text_vec, known_categories, item_map, dict, domain = "produce", method = "llm") {
  long <- split_freetext(raw_text_vec)
  if (nrow(long) == 0) return(list(long = long %>% mutate(category = character(0)), vocab = known_categories, item_map = item_map))

  distinct_items <- unique(long$item)

  cached_category <- vapply(distinct_items, function(it) {
    val <- item_map[[it]]
    if (is.null(val)) NA_character_ else as.character(val)
  }, character(1))
  names(cached_category) <- distinct_items

  uncached_items <- distinct_items[is.na(cached_category)]

  if (length(uncached_items) > 0) {
    result <- classify_items_dynamic(uncached_items, known_categories, dict, domain = domain, method = method)
    known_categories <- result$vocab
    cached_category[uncached_items] <- result$mapping[uncached_items]
    # Only cache real decisions. An NA means we couldn't classify the
    # phrase this run (usually a failed/unparseable API call), and
    # writing that to item_map would lock the failure in permanently -
    # the cache is checked before the LLM, so it would never be retried.
    for (it in uncached_items) {
      decided <- unname(cached_category[[it]])
      if (!is.na(decided)) item_map[[it]] <- decided
    }
  }

  list(
    # Undecided items drop out of this run's counts entirely rather than
    # forming an NA category row in the frequency tables. The response
    # still counts toward n_total; it just contributes no exposure.
    long = long %>%
      mutate(category = unname(cached_category[item])) %>%
      filter(!is.na(category)),
    vocab = known_categories,
    item_map = item_map
  )
}

