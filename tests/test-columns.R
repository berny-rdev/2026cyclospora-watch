SIGS <- c(
  consent           = "consent",
  state             = "what state",
  produce_checklist = "did you eat any of the following",
  produce_other     = "anything else you remember eating",
  duration          = "how long did symptoms last"
)
REQ <- c("consent")
ANY <- list(c("produce_checklist", "produce_other"))

## Shaped like the real sheet: Google Forms puts the whole question text in the
## header, so these are long and only a keyword is distinctive.
HEADERS <- c(
  "Timestamp",
  "Do you consent to your self report being used to run statistical analysis?",
  "What state/province are you in?",
  "Did you eat any of the following in the 14 days before symptoms started?",
  "Is there anything else you remember eating that was not listed above?",
  "How long did symptoms last?"
)

test_that("all signatures match the real header shapes", {
  m <- match_columns(HEADERS, SIGS, REQ, ANY)
  expect_length(m, 5)
  expect_setequal(names(m), names(SIGS))
  expect_equal(unname(m[["consent"]]), HEADERS[2])
})

## The reason this item existed: losing `consent` silently skips the filter
## that drops non-consenting responses, publishing data that should not be in
## the analysis at all.
test_that("a missing required column is fatal, and says why", {
  reworded <- sub("Do you consent.*", "Do you agree to take part?", HEADERS)
  expect_error(match_columns(reworded, SIGS, REQ, ANY), "Required form column")
  expect_error(match_columns(reworded, SIGS, REQ, ANY), "consent")
  # The message must show the actual headers - the fix is always to edit the regex.
  expect_error(match_columns(reworded, SIGS, REQ, ANY), "Actual columns in the sheet")
})

test_that("losing BOTH produce columns is fatal", {
  none <- HEADERS[!grepl("Did you eat|anything else", HEADERS, ignore.case = TRUE)]
  expect_error(suppressWarnings(match_columns(none, SIGS, REQ, ANY)),
               "None of these form columns")
})

test_that("losing ONE produce column is allowed", {
  one <- HEADERS[!grepl("anything else", HEADERS, ignore.case = TRUE)]
  m <- match_columns(one, SIGS, REQ, ANY)
  expect_true("produce_checklist" %in% names(m))
  expect_false("produce_other"    %in% names(m))
})

test_that("an unmatched optional column warns instead of vanishing", {
  no_duration <- HEADERS[!grepl("How long", HEADERS)]
  expect_warning(match_columns(no_duration, SIGS, REQ, ANY),
                 "Optional form column", fixed = TRUE)
  expect_warning(match_columns(no_duration, SIGS, REQ, ANY), "duration")
})

test_that("matching is case-insensitive and takes the first hit", {
  m <- match_columns(toupper(HEADERS), SIGS, REQ, ANY)
  expect_length(m, 5)
})
