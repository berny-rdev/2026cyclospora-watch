test_that("merge_categories never unions an NA decision into the vocabulary", {
  # NA means "undecided". Before this guard an undecided item could enter the
  # category list as a literal NA and then be written to the vocabulary file.
  expect_equal(merge_categories(c("cilantro"), c(a = NA_character_)), "cilantro")
  expect_setequal(merge_categories(c("cilantro"), c(a = "romaine_head", b = NA)),
                  c("cilantro", "romaine_head"))
})

test_that("merge_categories is a set union, not an append", {
  expect_equal(merge_categories(c("cilantro"), c(a = "cilantro")), "cilantro")
})

test_that("regex_classify returns NA rather than inventing a category", {
  # Returning str_to_title(item) here is what produced categories like
  # "Shredded Lettuce On Food From Culver's".
  dict <- list(cilantro = "cilantro|coriander", romaine_head = "romaine.*head")
  expect_equal(regex_classify("fresh cilantro", dict), "cilantro")
  expect_true(is.na(regex_classify("something nobody seeded", dict)))
})

test_that("classify_and_grow answers from cache without calling the LLM", {
  # The cache-first path: a phrase already decided must return its locked-in
  # answer. method="llm" with no API key would otherwise fall back to regex,
  # so a cache hit is the only way this can return trader_joes.
  item_map <- list("trader joe's" = "trader_joes")
  r <- classify_and_grow("Trader Joe's", c("trader_joes"), item_map,
                         dict = list(), domain = "store", method = "llm")
  expect_equal(r$long$category, "trader_joes")
  expect_equal(r$item_map[["trader joe's"]], "trader_joes")
})

test_that("classify_and_grow caches a decision it had to make", {
  dict <- list(cilantro = "cilantro")
  r <- classify_and_grow("cilantro", c("cilantro"), list(),
                         dict = dict, domain = "produce", method = "regex")
  expect_equal(r$long$category, "cilantro")
  expect_equal(r$item_map[["cilantro"]], "cilantro")   # now locked in
})

test_that("classify_and_grow drops undecided items from the counts", {
  # An unclassifiable item must not become an NA category row in the
  # frequency tables; the response still counts toward n_total.
  r <- classify_and_grow("something nobody seeded", character(0), list(),
                         dict = list(), domain = "produce", method = "regex")
  expect_equal(nrow(r$long), 0)
})

test_that("classify_and_grow normalises punctuation before the cache lookup", {
  # Cache holds the straight-apostrophe form; input arrives curly.
  item_map <- list("trader joe's" = "trader_joes")
  r <- classify_and_grow("Trader Joe’s", c("trader_joes"), item_map,
                         dict = list(), domain = "store", method = "llm")
  expect_equal(r$long$category, "trader_joes")
  expect_length(r$item_map, 1)   # no second key minted for the curly form
})
