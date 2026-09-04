## ---- VOCABULARY PERSISTENCE ----
##
## Shared by index.Rmd and cyclospora_outbreak_analysis.R. Extracted verbatim
## from index.Rmd, which was the newer of the two copies.

# Reads the vocabulary file, or returns NULL if it genuinely doesn't exist
# yet (first run). A file that EXISTS but won't parse is a hard error: the
# old behaviour returned NULL there, which silently rebuilt the vocabulary
# from seed values and then overwrote every accumulated category, baseline
# and cached classification with that skeleton.
read_vocabulary_file <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = TRUE),
    error = function(e) {
      stop(
        path, " exists but could not be parsed: ", conditionMessage(e), "\n",
        "  Refusing to continue. Proceeding would rebuild the vocabulary from seed\n",
        "  values and overwrite every accumulated category, baseline and cached\n",
        "  classification. Restore it with `git checkout -- ", path, "` or repair\n",
        "  the file by hand, then re-run.",
        call. = FALSE
      )
    }
  )
}

# as.list() on a possibly-absent JSON field, without inventing a value.
as_list_or_empty <- function(x) if (is.null(x)) list() else as.list(x)

load_vocabulary <- function(path, seed_produce, seed_store, seed_baseline) {
  v <- read_vocabulary_file(path)
  list(
    produce_categories = union(if (!is.null(v)) v$produce_categories else character(0), seed_produce),
    store_categories   = union(if (!is.null(v)) v$store_categories else character(0), seed_store),
    baseline_commonness = modifyList(
      as.list(seed_baseline),
      if (!is.null(v) && !is.null(v$baseline_commonness)) as.list(v$baseline_commonness) else list()
    ),
    # PERSISTENT CLASSIFICATION CACHE - locks in each specific raw phrase's
    # decision forever, so already-classified responses can't silently
    # drift to a different category on a later run.
    produce_item_map = if (!is.null(v) && !is.null(v$produce_item_map)) as.list(v$produce_item_map) else list(),
    store_item_map    = if (!is.null(v) && !is.null(v$store_item_map)) as.list(v$store_item_map) else list(),
    # PROVENANCE for each baseline. Parallel maps rather than nesting inside
    # baseline_commonness, because that field is read as a flat named vector
    # by the signal-ratio code and changing its shape would break the metric.
    # baseline_source: "USDA ERS Food Availability" | "author_estimate"
    # baseline_confidence: "exact_match" | "unsourced"
    baseline_source     = if (!is.null(v) && !is.null(v$baseline_source)) as.list(v$baseline_source) else list(),
    baseline_confidence = if (!is.null(v) && !is.null(v$baseline_confidence)) as.list(v$baseline_confidence) else list(),
    # CATEGORY provenance (distinct from the BASELINE provenance above).
    # Must be loaded, not just carried over on save: stamp_category_provenance()
    # below adds to these, and if they arrived empty the save-time merge would
    # treat the partial in-memory copy as authoritative and drop the rest.
    category_source_type      = if (!is.null(v) && !is.null(v$category_source_type)) as.list(v$category_source_type) else list(),
    active_since_form_version = if (!is.null(v) && !is.null(v$active_since_form_version)) as.list(v$active_since_form_version) else list(),
    checklist_label           = if (!is.null(v) && !is.null(v$checklist_label)) as.list(v$checklist_label) else list(),
    category_notes            = if (!is.null(v) && !is.null(v$category_notes)) as.list(v$category_notes) else list()
  )
}

## Stamps provenance on any category that lacks it.
##
## Deliberately here rather than inside classify_and_grow(): categories can
## enter the vocabulary by several routes - the LLM minting a new one, the
## checklist union, a hand edit to the file - and only a sweep catches all of
## them. Existing labels are never overwritten, so this is purely additive and
## self-healing. Without it the provenance fields go stale the first time
## anyone reports a new food or store, which is exactly what happened to the
## 16 categories added after the initial backfill.
stamp_category_provenance <- function(vocab, checklist_categories = character(0)) {
  allc <- c(vocab$produce_categories, vocab$store_categories)
  for (cat in setdiff(allc, names(vocab$category_source_type))) {
    vocab$category_source_type[[cat]] <-
      if (cat %in% checklist_categories) "checklist_direct" else "freetext_classified"
  }
  for (cat in setdiff(allc, names(vocab$active_since_form_version))) {
    vocab$active_since_form_version[[cat]] <- "v1"
  }
  vocab
}

# MERGE-ON-WRITE, not clobber-on-write.
#
# `vocab` is a snapshot taken when this run started. Another writer - the
# hourly Action, or someone running the analysis script locally - may have
# added entries since. Serializing the snapshot over the whole file would
# silently discard them, with no conflict, because a full-file overwrite
# always wins a rebase.
#
# So: re-read the file we are about to replace and merge into it. A side
# effect worth stating, because it is the main safety property here: a
# caller that loaded only PART of the file (e.g. a script whose
# load_vocabulary() never reads the item maps) can no longer destroy the
# keys it never looked at.
save_vocabulary <- function(vocab, path) {
  on_disk <- read_vocabulary_file(path)
  merged <- vocab

  if (!is.null(on_disk)) {
    # Carry over any top-level key this function doesn't explicitly manage.
    # Without this, a field added to the file (by hand, by a tool, or by a
    # newer version of this script) is silently dropped the next time any
    # run saves - which is exactly how the item maps used to get wiped.
    for (k in setdiff(names(on_disk), names(merged))) merged[[k]] <- on_disk[[k]]

    # Categories are additive - keep both sides.
    merged$produce_categories <- union(vocab$produce_categories, on_disk$produce_categories)
    merged$store_categories   <- union(vocab$store_categories,   on_disk$store_categories)

    # For per-key maps, the on-disk value wins. modifyList(a, b) gives `b`
    # precedence, so passing on_disk second means an entry already written
    # by someone else survives, while genuinely new keys from this run are
    # still added. On-disk decisions are the earlier, possibly already
    # published ones, and neither script ever edits these in memory.
    merged$baseline_commonness <- modifyList(
      as_list_or_empty(vocab$baseline_commonness), as_list_or_empty(on_disk$baseline_commonness))
    merged$produce_item_map <- modifyList(
      as_list_or_empty(vocab$produce_item_map), as_list_or_empty(on_disk$produce_item_map))
    merged$store_item_map <- modifyList(
      as_list_or_empty(vocab$store_item_map), as_list_or_empty(on_disk$store_item_map))
    merged$baseline_source <- modifyList(
      as_list_or_empty(vocab$baseline_source), as_list_or_empty(on_disk$baseline_source))
    merged$baseline_confidence <- modifyList(
      as_list_or_empty(vocab$baseline_confidence), as_list_or_empty(on_disk$baseline_confidence))
    # Same direction as the maps above: on-disk wins for keys both sides have,
    # while categories stamped for the first time in this run are kept.
    for (k in c("category_source_type", "active_since_form_version",
                "checklist_label", "category_notes")) {
      merged[[k]] <- modifyList(as_list_or_empty(vocab[[k]]), as_list_or_empty(on_disk[[k]]))
    }
  }

  # ATOMIC WRITE. write_json() straight onto `path` leaves a truncated file
  # if the process dies mid-write, and a truncated file is exactly what
  # read_vocabulary_file() now refuses to load. Write to a tempfile in the
  # SAME directory (so rename stays on one filesystem and is atomic), then
  # rename over the target. A killed process can leave a stray temp file,
  # but never a partial category_vocabulary.json.
  tmp <- tempfile(pattern = ".category_vocabulary", tmpdir = dirname(path), fileext = ".json")
  jsonlite::write_json(merged, tmp, auto_unbox = TRUE, pretty = TRUE)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Could not atomically replace ", path, call. = FALSE)
  }
  invisible(merged)
}

