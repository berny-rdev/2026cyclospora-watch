test_that("normalize_punct folds typographic punctuation to ASCII", {
  expect_equal(normalize_punct("Trader Joe’s"), "Trader Joe's")
  expect_equal(normalize_punct("Wendy‘s"),      "Wendy's")
  expect_equal(normalize_punct("“bagged”"),     '"bagged"')
  expect_equal(normalize_punct("pre–chopped"),  "pre-chopped")
  expect_equal(normalize_punct("pre—chopped"),  "pre-chopped")
  expect_equal(normalize_punct("a b"),     "a b")   # non-breaking space
})

test_that("normalize_punct leaves already-ASCII text untouched", {
  s <- "trader joe's bagged salad (rowley, ma)"
  expect_equal(normalize_punct(s), s)
})

test_that("normalize_punct is vectorised", {
  expect_equal(normalize_punct(c("Joe’s", "Wendy’s")), c("Joe's", "Wendy's"))
})

## This is the guard for the locale bug: under LC_CTYPE=C the regex engine will
## not match a multibyte character class, normalize_punct() silently returns its
## input unchanged, and every curly apostrophe forks a duplicate category.
## If this test ever fails, the suite is running in a locale where the whole
## normalisation layer is inert.
test_that("the session locale actually supports multibyte matching", {
  expect_match(Sys.getlocale("LC_CTYPE"), "UTF-8", ignore.case = TRUE)
  expect_false(identical(normalize_punct("Joe’s"), "Joe’s"))
})


test_that("split_delims splits on comma, semicolon and newline", {
  expect_equal(split_delims("a,b"),    c("a", "b"))
  expect_equal(split_delims("a;b"),    c("a", "b"))
  expect_equal(split_delims("a\nb"),   c("a", "b"))
})

test_that("split_delims does NOT split on delimiters inside parentheses", {
  # The Agawam Diner case: two businesses, each with a comma inside parens.
  parts <- split_delims(FIXTURES$two_businesses_parens)
  expect_length(parts, 2)
  expect_match(parts[1], "Agawam Diner")
  expect_match(parts[2], "Campfire Grille")
  # The specific fragments the old comma split produced must not reappear.
  expect_false(any(grepl("^\\s*MA\\)", parts)))
  expect_false(any(grepl("^\\s*ME\\)$", parts)))
})

test_that("split_delims handles unbalanced and nested parentheses", {
  expect_equal(split_delims("a (b, c"), "a (b, c")        # never closed: no split
  expect_equal(split_delims("a (b (c, d)), e"), c("a (b (c, d))", " e"))
  expect_equal(split_delims("a), b"), c("a)", " b"))      # stray close, still splits
})

test_that("split_delims returns empty for blank or NA input", {
  expect_equal(split_delims(NA_character_), character(0))
  expect_equal(split_delims(""),            character(0))
})


test_that("is_negation catches every way people decline to answer", {
  expect_true(all(is_negation(FIXTURES$negations)))
})

test_that("is_negation does not swallow real foods that look like negations", {
  expect_false(any(is_negation(FIXTURES$negation_lookalikes)))
})


test_that("split_freetext splits the seven-businesses-on-seven-lines response", {
  out <- split_freetext(FIXTURES$newline_list)
  expect_equal(nrow(out), 4)
  expect_setequal(out$item, c("atomic wings", "chez oskar", "burger bus", "taco bell"))
  expect_true(all(out$row_id == 1))
})

test_that("split_freetext normalises punctuation before splitting", {
  out <- split_freetext(FIXTURES$curly_apostrophe)
  expect_equal(out$item, "trader joe's")   # straight apostrophe, lowercased
})

test_that("split_freetext drops negations and blanks but keeps row_id alignment", {
  out <- split_freetext(c("romaine, spinach", "No", NA, "", "cilantro"))
  expect_setequal(out$item, c("romaine", "spinach", "cilantro"))
  expect_equal(sort(unique(out$row_id)), c(1, 5))   # rows 2-4 contributed nothing
})
