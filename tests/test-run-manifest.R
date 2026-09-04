vocab_small <- list(
  produce_categories  = c("cilantro", "romaine_head"),
  store_categories    = c("trader_joes"),
  baseline_commonness = list(cilantro = 20)
)

test_that("build_run_manifest records the outcome measures the guard compares", {
  m <- build_run_manifest(164, 61, 127, vocab_small, "llm",
                          events = list(), html_path = "no-such-file.html")
  expect_equal(m$n_responses, 164L)
  expect_equal(m$produce_categories_reported, 61L)
  expect_equal(m$store_categories_reported, 127L)
  expect_equal(m$produce_categories_known, 2L)
  expect_equal(m$store_categories_known, 1L)
  expect_equal(m$baselines_set, 1L)
  expect_equal(m$classification_method, "llm")
})

test_that("counts are integers, so the JSON round-trips without decimals", {
  m <- build_run_manifest(164, 61, 127, vocab_small, "llm", html_path = "nope.html")
  for (f in c("n_responses", "produce_categories_reported", "store_categories_reported",
              "produce_categories_known", "store_categories_known", "baselines_set",
              "fallback_no_api_key", "fallback_batch_failed", "index_html_bytes")) {
    expect_type(m[[f]], "integer")
  }
})

test_that("missing fallback counters default to zero rather than NULL", {
  m <- build_run_manifest(1, 1, 1, vocab_small, "regex", events = list(), html_path = "nope.html")
  expect_equal(m$fallback_no_api_key, 0L)
  expect_equal(m$fallback_batch_failed, 0L)
})

test_that("fallback counters are carried through when they fired", {
  m <- build_run_manifest(1, 1, 1, vocab_small, "llm",
                          events = list(no_api_key = 2L, batch_failed = 1L),
                          html_path = "nope.html")
  expect_equal(m$fallback_no_api_key, 2L)
  expect_equal(m$fallback_batch_failed, 1L)
})

test_that("index_html_bytes is 0 when the HTML does not exist yet", {
  # index.Rmd writes the manifest before pandoc runs; check-render.R fills this in.
  m <- build_run_manifest(1, 1, 1, vocab_small, "llm", html_path = "definitely-not-here.html")
  expect_equal(m$index_html_bytes, 0L)
})

test_that("the manifest survives a JSON round-trip unchanged", {
  m <- build_run_manifest(164, 61, 127, vocab_small, "llm", html_path = "nope.html")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_run_manifest(m, tmp)
  back <- jsonlite::fromJSON(tmp, simplifyVector = TRUE)
  expect_equal(back$n_responses, m$n_responses)
  expect_equal(back$store_categories_reported, m$store_categories_reported)
  expect_equal(back$classification_method, m$classification_method)
})

test_that("classification events are counted and resettable", {
  reset_classification_events()
  expect_equal(classification_events()$no_api_key, 0L)
  record_classification_event("no_api_key")
  record_classification_event("no_api_key")
  record_classification_event("batch_failed")
  expect_equal(classification_events()$no_api_key, 2L)
  expect_equal(classification_events()$batch_failed, 1L)
  reset_classification_events()
  expect_equal(classification_events()$no_api_key, 0L)
})

test_that("a missing-key fallback is both warned about and counted", {
  reset_classification_events()
  expect_warning(
    call_claude_classify_dynamic("romaine", c("romaine_head"), domain = "produce", api_key = ""),
    "ANTHROPIC_API_KEY not set")
  expect_equal(classification_events()$no_api_key, 1L)
  reset_classification_events()
})
