## ---- RUN MANIFEST ---------------------------------------------------------
##
## A small machine-readable record of what each render actually produced,
## written next to index.html and committed alongside it.
##
## It exists because the pipeline's failure modes are quiet ones. Nothing here
## crashes: a partial sheet read renders a page with fewer responses, and an
## unavailable API silently drops classification to the regex seed dictionaries,
## which collapses the store vocabulary from ~127 categories to the 16 the seeds
## can match. Both publish a page that looks entirely normal.
##
## Comparing this run's manifest against the previous committed one is what
## turns those into something a workflow can refuse to publish. The comparison
## lives in scripts/check-render.R; this file only records the facts.

MANIFEST_PATH <- "run-manifest.json"

## `events` is classification_events(); passing it in rather than reading the
## global keeps this function testable.
build_run_manifest <- function(n_responses,
                               produce_categories_reported,
                               store_categories_reported,
                               vocab,
                               classification_method,
                               events = list(),
                               html_path = "index.html") {
  int <- function(x) as.integer(x)
  list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),

    ## Outcome measures - what the page actually says.
    n_responses                 = int(n_responses),
    produce_categories_reported = int(produce_categories_reported),
    store_categories_reported   = int(store_categories_reported),

    ## Vocabulary size, which should only ever grow.
    produce_categories_known = int(length(vocab$produce_categories)),
    store_categories_known   = int(length(vocab$store_categories)),
    baselines_set            = int(length(vocab$baseline_commonness)),

    ## How the run was classified, and whether it degraded on the way.
    classification_method = classification_method,
    api_key_present       = nzchar(Sys.getenv("ANTHROPIC_API_KEY")),
    fallback_no_api_key   = int(if (is.null(events$no_api_key)) 0 else events$no_api_key),
    fallback_batch_failed = int(if (is.null(events$batch_failed)) 0 else events$batch_failed),

    ## Rendered output size. Zero when the manifest is written before the HTML
    ## exists, which is the case during a knit.
    index_html_bytes = int(if (file.exists(html_path)) file.info(html_path)$size else 0)
  )
}

write_run_manifest <- function(manifest, path = MANIFEST_PATH) {
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(manifest)
}
