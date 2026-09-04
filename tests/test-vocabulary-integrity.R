## A minimal vocabulary shaped like the real one, small enough to reason about.
vocab_fixture <- function() {
  list(
    produce_categories = c("romaine_head", "cilantro"),
    store_categories   = c("trader_joes", "mcdonalds"),
    baseline_commonness = list(romaine_head = 15, cilantro = 20),
    baseline_source     = list(romaine_head = "USDA ERS Food Availability",
                               cilantro     = "author_estimate"),
    baseline_confidence = list(romaine_head = "exact_match", cilantro = "unsourced"),
    category_source_type = list(romaine_head = "checklist_direct",
                                cilantro     = "checklist_direct",
                                trader_joes  = "freetext_classified",
                                mcdonalds    = "freetext_classified"),
    produce_item_map = list("romaine lettuce" = "romaine_head"),
    store_item_map   = list("trader joe's" = "trader_joes",
                            "mcdonald's"   = "mcdonalds")
  )
}

test_that("a clean vocabulary passes and is returned unchanged", {
  v <- vocab_fixture()
  expect_equal(check_vocabulary_integrity(v, verbose = FALSE), v)
})

## The poisoning signature: raw respondent text reaching the category list.
test_that("a malformed category name stops the run", {
  for (bad in c("Red Onions", "No. I Made Sure To Not :(", "Trader Joe's Tabouli",
                "Pre-Chopped/Packaged", "wendy s")) {
    v <- vocab_fixture()
    v$produce_categories <- c(v$produce_categories, bad)
    expect_error(check_vocabulary_integrity(v, verbose = FALSE),
                 "Malformed category name", fixed = TRUE)
  }
})

test_that("legitimate snake_case names are accepted", {
  v <- vocab_fixture()
  v$produce_categories <- c(v$produce_categories, "fruit_salad_prepackaged", "tabouli", "acai99")
  v$category_source_type[c("fruit_salad_prepackaged", "tabouli", "acai99")] <- "freetext_classified"
  expect_silent(check_vocabulary_integrity(v, verbose = FALSE))
})

## The residue actually found in the committed vocabulary: 8 keys in each of
## baseline_source and baseline_confidence naming no real category.
test_that("orphaned metadata keys are pruned, and only those", {
  v <- vocab_fixture()
  v$baseline_source[["Bibb Lettuce Head"]]  <- "author_estimate"
  v$baseline_confidence[["Red Onions"]]     <- "unsourced"

  out <- check_vocabulary_integrity(v, verbose = FALSE)

  expect_false("Bibb Lettuce Head" %in% names(out$baseline_source))
  expect_false("Red Onions"        %in% names(out$baseline_confidence))
  expect_setequal(names(out$baseline_source),     c("romaine_head", "cilantro"))
  expect_setequal(names(out$baseline_confidence), c("romaine_head", "cilantro"))
})

## Item maps are keyed by the raw text a respondent typed, NOT by category.
## Applying the orphan rule to them would delete almost every entry.
test_that("phrase-keyed item maps are never treated as orphans", {
  v <- vocab_fixture()
  out <- check_vocabulary_integrity(v, verbose = FALSE)
  expect_equal(out$produce_item_map, v$produce_item_map)
  expect_equal(out$store_item_map,   v$store_item_map)
})

test_that("unreachable_cache_keys finds keys current ingestion cannot produce", {
  keys <- c("trader joe's", "trader joe’s", "burger bus\ndetroit mi", "plain text")
  expect_setequal(unreachable_cache_keys(keys),
                  c("trader joe’s", "burger bus\ndetroit mi"))
  expect_equal(unreachable_cache_keys(character(0)), character(0))
})

test_that("an unreachable cache key is pruned only when its twin agrees", {
  v <- vocab_fixture()
  v$store_item_map[["trader joe’s"]] <- "trader_joes"     # twin exists, same answer
  out <- check_vocabulary_integrity(v, verbose = FALSE)
  expect_false("trader joe’s" %in% names(out$store_item_map))
  expect_true("trader joe's"  %in% names(out$store_item_map))
})

test_that("an unreachable key is KEPT when its twin disagrees", {
  v <- vocab_fixture()
  v$store_item_map[["mcdonald’s"]] <- "somewhere_else"    # twin says mcdonalds
  out <- check_vocabulary_integrity(v, verbose = FALSE)
  expect_true("mcdonald’s" %in% names(out$store_item_map))
})

test_that("an unreachable key is KEPT when it has no twin at all", {
  v <- vocab_fixture()
  v$store_item_map[["some bodega’s"]] <- "trader_joes"    # no straight-quote twin
  out <- check_vocabulary_integrity(v, verbose = FALSE)
  expect_true("some bodega’s" %in% names(out$store_item_map))
})

test_that("assert_utf8_locale leaves a UTF-8 session alone", {
  before <- Sys.getlocale("LC_CTYPE")
  expect_silent(assert_utf8_locale())
  expect_equal(Sys.getlocale("LC_CTYPE"), before)
})
