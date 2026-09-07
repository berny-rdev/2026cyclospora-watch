test_that("the two preset options resolve exactly", {
  # These cover ~88% of responses and must never be caught by a looser rule.
  expect_equal(as.character(classify_case_definition("Lab confirmed test")), "Lab confirmed")
  expect_equal(as.character(classify_case_definition("I think I have it")), "Self-suspected")
})

test_that("a pending result is never counted as confirmed", {
  # "I think I have it waiting on lab" and "Lab confirmed test" both contain
  # "lab"; "Inconclusive 1st test" contains "test". Ordering is what separates them.
  for (s in c("I think I have it waiting on lab",
              "Waiting for lab tests since July 21",
              "i was tested for it waiting result",
              "Most likely- still waiting on lab results",
              "Inconclusive 1st test too hard to collect (gross) finished Bactrim today")) {
    expect_equal(as.character(classify_case_definition(s)),
                 "Awaiting or inconclusive lab result", info = s)
  }
})

test_that("a clinician treating without a test is its own tier", {
  for (s in c("Doctor is treating me for it without a lab test",
              "ER doctor is treating me for it without confirmed test",
              "Urgent Care doctor believed I had it, and prescribed me Bactrim",
              "Dr ruled out all other causes with stool samples")) {
    expect_equal(as.character(classify_case_definition(s)),
                 "Treated by a clinician, not lab confirmed", info = s)
  }
})

test_that("'without confirmed test' is not read as confirmation", {
  # The confirmed rule runs after the clinical one precisely so this phrase,
  # which contains "confirmed", does not promote a case to Lab confirmed.
  expect_false(as.character(classify_case_definition(
    "ER doctor is treating me for it without confirmed test")) == "Lab confirmed")
})

test_that("symptom-based self-assessment stays self-suspected", {
  for (s in c("Have the symptoms", "had it i think.", "I had it before the outbreak")) {
    expect_equal(as.character(classify_case_definition(s)), "Self-suspected", info = s)
  }
})

test_that("blank and unrecognised answers are Unclassified, not dropped", {
  expect_equal(as.character(classify_case_definition(NA_character_)), "Unclassified")
  expect_equal(as.character(classify_case_definition("")), "Unclassified")
  expect_equal(as.character(classify_case_definition("   ")), "Unclassified")
})

test_that("classification is vectorised and total", {
  x <- c("Lab confirmed test", "I think I have it", NA, "Have the symptoms")
  out <- classify_case_definition(x)
  expect_length(out, 4)
  expect_false(any(is.na(out)))   # every input lands in some tier
})

test_that("the summary keeps every tier and every response", {
  x <- c(rep("I think I have it", 119), rep("Lab confirmed test", 26),
         "Doctor is treating me for it", "Waiting for lab tests")
  s <- case_definition_summary(x, length(x))
  expect_equal(nrow(s), length(CASE_DEFINITION_LEVELS))       # zero-count tiers kept
  expect_equal(sum(s$n_cases), length(x))                     # nothing lost
  expect_equal(as.character(s$case_definition), CASE_DEFINITION_LEVELS)  # certainty order
  expect_equal(s$n_cases[s$case_definition == "Lab confirmed"], 26L)
})

test_that("percentages are computed against the response total", {
  s <- case_definition_summary(rep("Lab confirmed test", 26), 164)
  expect_equal(s$pct_of_cases[s$case_definition == "Lab confirmed"], 15.9)
})
