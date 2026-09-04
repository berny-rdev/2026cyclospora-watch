## ---- VOCABULARY INTEGRITY -------------------------------------------------
##
## Sourced by BOTH index.Rmd and cyclospora_outbreak_analysis.R, so these rules
## exist once rather than drifting between two copies. Every check below is
## here because the defect it catches actually reached the committed
## vocabulary at least once.
##
## The division of labour is deliberate and worth stating, because the two
## halves of the vocabulary are keyed differently and a check that ignores
## that does real damage:
##
##   CATEGORY-KEYED maps  (baseline_commonness, baseline_source,
##                         baseline_confidence, category_source_type,
##                         checklist_label, active_since_form_version,
##                         category_notes)
##       Every key must name a real category. A key that doesn't is residue.
##
##   PHRASE-KEYED maps    (produce_item_map, store_item_map)
##       Keys are the raw text respondents typed. They are SUPPOSED to contain
##       apostrophes, parentheses, accents and punctuation - that text is the
##       record of what someone actually reported. Never "clean" these; the
##       only invalid key here is one current ingestion could no longer
##       produce.

CATEGORY_NAME_PATTERN <- "^[a-z0-9_]+$"

CATEGORY_KEYED_MAPS <- c("baseline_commonness", "baseline_source",
                         "baseline_confidence", "category_source_type",
                         "checklist_label", "active_since_form_version",
                         "category_notes")

PHRASE_KEYED_MAPS <- c("produce_item_map", "store_item_map")


## Guarantees text normalisation actually works before any text is read.
##
## normalize_punct() folds curly quotes, en/em dashes and non-breaking spaces
## to ASCII. Under a non-UTF-8 LC_CTYPE it is SILENTLY A NO-OP: R's regex
## engine will not match a multibyte character class in a C locale, so
## gsub("[‘’ʼ′´]", "'", x) returns x unchanged with no error and no warning.
##
## The consequence is not cosmetic. Every curly apostrophe then survives
## ingestion, becomes its own cache key and its own category, and forks
## "wendy_s" off from "wendys" - the exact poisoning this pipeline already had
## to be cleaned up for once. macOS `Rscript` defaults to LC_CTYPE=C, and the
## README tells people to run the analysis locally, so this is a live path.
##
## Setting the locale in-process is the only fix that works. Declaring the
## pattern with \u escapes, enc2utf8() and Encoding(x) <- "UTF-8" were all
## tried and all still fail: the problem is the regex engine's locale, not the
## encoding of the pattern or the string.
assert_utf8_locale <- function() {
  ctype <- Sys.getlocale("LC_CTYPE")
  if (grepl("UTF-8", ctype, ignore.case = TRUE)) return(invisible(ctype))

  for (loc in c("en_US.UTF-8", "C.UTF-8")) {
    applied <- suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
    if (nzchar(applied)) {
      message("LC_CTYPE was '", ctype, "', which silently disables punctuation ",
              "normalisation. Set to '", loc, "' for this session.")
      return(invisible(applied))
    }
  }

  stop("LC_CTYPE is '", ctype, "' and no UTF-8 locale could be set.\n",
       "  Refusing to continue: normalize_punct() would silently do nothing,\n",
       "  and every curly apostrophe in the responses would fork a duplicate\n",
       "  category. Re-run with a UTF-8 locale, e.g.\n",
       "      LC_ALL=en_US.UTF-8 Rscript ...\n",
       "  or set LANG=en_US.UTF-8 in the environment.",
       call. = FALSE)
}


## Which phrase-keyed cache keys could current ingestion no longer produce?
##
## A cached key is unreachable when normalize_punct() would have rewritten it
## (so today the same phrase is stored under a different key) or when it
## contains a newline (split_delims() now splits on newlines, so the whole
## string can never arrive as one item). These are fossils from before those
## two fixes landed - inert rather than harmful, but they are the same class
## of residue as an orphaned baseline and worth sweeping on the same pass.
unreachable_cache_keys <- function(keys) {
  if (!length(keys)) return(character(0))
  if (!exists("normalize_punct", mode = "function")) {
    stop("unreachable_cache_keys() needs normalize_punct(); source this file ",
         "after the functions chunk that defines it.", call. = FALSE)
  }
  keys[normalize_punct(keys) != keys | grepl("\n", keys, fixed = TRUE)]
}


## The single entry point. Call immediately before save_vocabulary(), which is
## the only moment every route that can add a category has finished running.
##
## Two severities, chosen by whether the problem is recoverable:
##
##   stop()  - a malformed category name. Not self-healing, and it is the
##             signature of classifier poisoning, so publishing over it is
##             worse than a stale page. This is the "fails the run" part.
##
##   prune   - orphaned metadata and unreachable cache keys. Both are inert
##             and removing them loses nothing, so repairing in place beats
##             failing. Same self-healing idiom as stamp_category_provenance().
##
## A cache key is pruned ONLY when its reachable twin resolves to the same
## category, so no classification decision is ever silently discarded. A key
## whose twin disagrees, or has no twin, is reported and left for a human.
check_vocabulary_integrity <- function(vocab, repair = TRUE, verbose = TRUE) {
  cats <- unique(c(vocab$produce_categories, vocab$store_categories))
  say  <- function(...) if (verbose) cat(..., sep = "")

  ## ---- 1. Category names (hard) -------------------------------------------
  malformed <- cats[!grepl(CATEGORY_NAME_PATTERN, cats)]
  if (length(malformed)) {
    stop("Malformed category name(s) in the vocabulary:\n",
         paste0("    ", encodeString(malformed, quote = '"'), collapse = "\n"), "\n",
         "  Categories must match ", CATEGORY_NAME_PATTERN, " (lowercase_snake_case).\n",
         "  A name carrying capitals, spaces or punctuation is raw respondent text\n",
         "  that reached the category list instead of being classified - the\n",
         "  poisoning signature. Refusing to publish over it.",
         call. = FALSE)
  }
  say("  category names      ", length(cats), " ok\n")

  ## ---- 2. Orphaned metadata (repaired) ------------------------------------
  for (m in CATEGORY_KEYED_MAPS) {
    if (is.null(vocab[[m]])) next
    orphans <- setdiff(names(vocab[[m]]), cats)
    if (!length(orphans)) next

    say("  ", m, ": ", length(orphans), " orphaned key(s)",
        if (repair) " - removing" else "", "\n")
    for (k in orphans) say("      ", encodeString(k, quote = '"'), "\n")
    if (repair) vocab[[m]] <- vocab[[m]][setdiff(names(vocab[[m]]), orphans)]
  }

  ## ---- 3. Unreachable cache keys (repaired, conservatively) ---------------
  for (m in PHRASE_KEYED_MAPS) {
    if (is.null(vocab[[m]])) next
    keys <- names(vocab[[m]])
    stale <- unreachable_cache_keys(keys)
    if (!length(stale)) next

    drop <- character(0)
    for (k in stale) {
      twin <- normalize_punct(k)
      if (twin != k && twin %in% keys && identical(vocab[[m]][[twin]], vocab[[m]][[k]])) {
        drop <- c(drop, k)
      } else {
        say("  ", m, ": unreachable key kept, needs a human -> ",
            encodeString(k, quote = '"'), "\n",
            "      ", if (twin == k || !(twin %in% keys))
                         "no reachable twin; removing it would lose this decision"
                       else "twin resolves to a different category", "\n")
      }
    }
    if (length(drop)) {
      say("  ", m, ": ", length(drop), " unreachable key(s)",
          if (repair) " - removing (twin agrees)" else "", "\n")
      if (repair) vocab[[m]] <- vocab[[m]][setdiff(keys, drop)]
    }
  }

  say("  vocabulary integrity ok\n")
  vocab
}
