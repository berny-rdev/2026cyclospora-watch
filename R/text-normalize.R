## ---- TEXT NORMALISATION AND SPLITTING ----
##
## Shared by index.Rmd and cyclospora_outbreak_analysis.R. Extracted verbatim
## from index.Rmd, which was the newer of the two copies.

## Canonicalise typographic punctuation to ASCII before anything else reads
## the string. Curly apostrophes were the single largest source of poisoned
## categories (13 of 36) and the most likely trigger for LLM key-drift: a
## phrase sent as a JSON key containing U+2019 can come back with U+0027,
## the exact-name lookup misses, and the item falls to the regex fallback.
## Applied at ingestion so tokenisation, caching and the API call all see
## the same canonical form.
normalize_punct <- function(x) {
  x <- gsub("[‘’ʼ′´]", "'", x, perl = TRUE)
  x <- gsub("[“”″]", '"', x, perl = TRUE)
  x <- gsub("[‐‑‒–—]", "-", x, perl = TRUE)
  x <- gsub(" ", " ", x, perl = TRUE)
  x
}

## Split on , ; and newline - but NOT inside parentheses. Respondents write
## "Agawam Diner (Rowley, MA); Campfire Grille (Bridgton, ME)", and a plain
## comma split turned that one answer into three junk categories including
## the fragments "Ma); Campfire Grille (Bridgton" and "Me)". Newlines matter
## too: one response listed seven businesses on seven lines and produced a
## single category containing all of them.
split_delims <- function(s) {
  if (is.na(s) || !nzchar(s)) return(character(0))
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  depth <- 0L; cur <- character(0); out <- character(0)
  for (c in ch) {
    if (c == "(") depth <- depth + 1L
    else if (c == ")") depth <- max(0L, depth - 1L)
    if (depth == 0L && (c == "," || c == ";" || c == "\n")) {
      out <- c(out, paste0(cur, collapse = "")); cur <- character(0)
    } else cur <- c(cur, c)
  }
  c(out, paste0(cur, collapse = ""))
}

## Declining to answer is not a food. Nine responses answer the produce
## free-text with a negation; before this filter they reached the classifier
## and could become categories like "No. I Made Sure To Not :(". Dropped
## here means never classified and never cached. The \\b guards keep real
## foods that merely start with these letters - "nopales", "nectarines".
NEGATION_RE <- paste0(
  "^(no|nope|none|nothing|n/?a|na|nil|never|not really|not that i|",
  "i don'?t|i do not|unsure|unknown|idk|[-.–]+)\\b|^[-.–[:space:]]*$")
is_negation <- function(item) {
  it <- str_squish(str_to_lower(item))
  !nzchar(it) | str_detect(it, NEGATION_RE)
}

split_freetext <- function(raw_text_vec) {
  tibble(row_id = seq_along(raw_text_vec), raw = raw_text_vec) %>%
    filter(!is.na(raw), str_trim(raw) != "") %>%
    mutate(raw = normalize_punct(raw)) %>%
    mutate(parts = map(raw, split_delims)) %>%
    select(row_id, parts) %>%
    unnest(parts) %>%
    mutate(item = str_squish(str_to_lower(parts))) %>%
    filter(item != "", !is_negation(item)) %>%
    select(row_id, item)
}

