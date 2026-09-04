#!/usr/bin/env Rscript
## ---- PRE-COMMIT RENDER GUARD ----------------------------------------------
##
## Run by the publishing workflow AFTER index.Rmd renders and BEFORE anything is
## committed. Exits non-zero to stop the commit.
##
## The workflow used to render and commit unconditionally, and every way this
## pipeline degrades is quiet:
##
##   - a partial Google Sheets read renders a page with fewer responses
##   - an unavailable or unauthorised API drops classification to the regex seed
##     dictionaries, which takes the store vocabulary from ~127 categories to
##     the 16 the seeds can match
##   - a truncated render writes a small but syntactically fine HTML file
##
## None of these raise an error, so all three would replace a good page with a
## worse one. This compares the new run-manifest.json against the one already
## committed and refuses to publish a run that went backwards.
##
## A FIRST RUN, or any run with no previous manifest, passes: there is nothing
## to compare against and blocking would be worse than publishing.

suppressWarnings(suppressMessages(library(jsonlite)))

MANIFEST   <- "run-manifest.json"
PREV_REF   <- "HEAD"

## How far each measure may fall before the run is refused. Responses can drop
## slightly and legitimately - a respondent withdrawing, or a consent answer
## being edited - so this is a tolerance, not equality. The category measures
## are deliberately tighter: those only fall when classification degraded.
TOLERANCE <- list(
  n_responses                 = 0.10,
  produce_categories_reported = 0.25,
  store_categories_reported   = 0.25,
  index_html_bytes            = 0.30
)

fail <- function(...) { cat("\n[check-render] BLOCKED:", ..., "\n"); quit(status = 1) }
ok   <- function(...) cat("[check-render]", ..., "\n")

if (!file.exists(MANIFEST)) {
  fail(MANIFEST, "was not written. The render did not reach the end of",
       "index.Rmd, so the page is incomplete even if index.html exists.")
}
new <- fromJSON(MANIFEST, simplifyVector = TRUE)

## index.Rmd writes the manifest in its last chunk, before pandoc has produced
## index.html, so the size it records is always 0. Fill in the real figure here
## and write it back, so the manifest that gets committed is accurate and the
## next run has something honest to compare against.
if (!file.exists("index.html")) {
  fail("index.html was not produced, even though the manifest was written.")
}
new$index_html_bytes <- as.integer(file.info("index.html")$size)
write_json(new, MANIFEST, auto_unbox = TRUE, pretty = TRUE)

## The previous manifest comes from git rather than the working tree, because
## the working copy has already been overwritten by this run.
prev_raw <- suppressWarnings(system2("git", c("show", paste0(PREV_REF, ":", MANIFEST)),
                                     stdout = TRUE, stderr = FALSE))
## system2() attaches a non-zero "status" attribute when git show fails, which
## is the normal case on the very first run - the file is not in HEAD yet.
git_failed <- !is.null(attr(prev_raw, "status"))
has_prev   <- !git_failed && length(prev_raw) > 0

## ---- Absolute checks (no history needed) ----------------------------------

if (isTRUE(new$classification_method == "llm") && isFALSE(new$api_key_present)) {
  fail("classification_method is \"llm\" but ANTHROPIC_API_KEY is not set.",
       "\n  Every free-text answer fell back to the regex seed dictionaries.",
       "\n  Publishing this would silently replace the LLM-built vocabulary",
       "\n  with the ~16 categories the seed patterns can match.")
}

if (isTRUE(new$fallback_no_api_key > 0)) {
  fail("the classifier fell back", new$fallback_no_api_key,
       "time(s) because no API key was available.")
}

if (isTRUE(new$n_responses < 1)) {
  fail("the render saw", new$n_responses, "responses.",
       "\n  Almost certainly a failed or empty sheet read.")
}

ok("responses:", new$n_responses,
   "| produce reported:", new$produce_categories_reported,
   "| stores reported:", new$store_categories_reported,
   "| html:", new$index_html_bytes, "bytes")

if (!has_prev) {
  ok("no previous manifest in", PREV_REF, "- nothing to compare, allowing.")
  quit(status = 0)
}
old <- fromJSON(paste(prev_raw, collapse = "\n"), simplifyVector = TRUE)

## ---- Comparative checks ---------------------------------------------------

for (field in names(TOLERANCE)) {
  before <- old[[field]]
  after  <- new[[field]]
  if (is.null(before) || is.null(after) || before <= 0) next

  drop <- (before - after) / before
  if (drop > TOLERANCE[[field]]) {
    fail(sprintf("%s fell from %s to %s (-%.0f%%, tolerance %.0f%%).",
                 field, before, after, 100 * drop, 100 * TOLERANCE[[field]]),
         "\n  A drop this size is a degraded run, not new data.")
  }
}

## The vocabulary is append-only by construction: categories are added, and the
## integrity check prunes only metadata. A real shrink means something
## overwrote the file - the clobber that merge-on-write exists to prevent.
for (field in c("produce_categories_known", "store_categories_known")) {
  if (!is.null(old[[field]]) && !is.null(new[[field]]) && new[[field]] < old[[field]]) {
    fail(sprintf("%s shrank from %s to %s.", field, old[[field]], new[[field]]),
         "\n  The vocabulary only grows; a shrink means it was overwritten.")
  }
}

if (isTRUE(new$fallback_batch_failed > 0) && isTRUE(old$fallback_batch_failed == 0)) {
  fail("this run had", new$fallback_batch_failed, "failed LLM batch(es)",
       "and the previous run had none.\n  Classification degraded mid-run.")
}

ok("all checks passed against", PREV_REF, "- safe to publish.")
