## ---- CASE DEFINITION ------------------------------------------------------
##
## Shared by index.Rmd and cyclospora_outbreak_analysis.R.
##
## The form asks "Why do you believe you have cyclospora?". That field was
## collected from the first response and read by nothing, which left the most
## important qualifier on the whole page invisible: of 164 responses, 26 are
## lab-confirmed and 119 are "I think I have it". Every count, percentage and
## signal ratio published here is computed over a population that is roughly
## three-quarters self-diagnosed, and a reader had no way to know that.
##
## This does NOT filter anything out. Self-suspected cases are legitimate input
## to a hypothesis-generating tool - excluding them would throw away most of the
## data and bias what remains toward whoever could get a stool test. The point
## is to make the mix visible, not to prune it.
##
## Answers are mostly two preset options plus a small tail of write-ins, so
## resolution is exact-match first, patterns second. Unlike the checklist
## resolver, an unrecognised answer is NOT a hard error - this is a free-text
## field and new phrasings will keep arriving. They land in "unclassified",
## which is reported rather than silently folded into a tier.

## Ordered most to least certain. The order matters: it is the display order and
## the order rules are tried in.
CASE_DEFINITION_LEVELS <- c(
  "Lab confirmed",
  "Treated by a clinician, not lab confirmed",
  "Awaiting or inconclusive lab result",
  "Self-suspected",
  "Unclassified"
)

## Exact preset options, matched before any pattern. These two cover ~88% of
## responses and must never be caught by a looser rule - "Lab confirmed test"
## and "I think I have it waiting on lab" both contain "lab".
CASE_DEFINITION_EXACT <- list(
  "lab confirmed test" = "Lab confirmed",
  "i think i have it"  = "Self-suspected"
)

classify_case_definition <- function(x) {
  raw <- str_squish(str_to_lower(as.character(x)))

  out <- rep(NA_character_, length(raw))
  out[is.na(x) | !nzchar(raw)] <- "Unclassified"

  ## 1. Exact preset options.
  for (k in names(CASE_DEFINITION_EXACT)) {
    out[is.na(out) & raw == k] <- CASE_DEFINITION_EXACT[[k]]
  }

  ## 2. Write-ins, most specific first. "Awaiting" is tested BEFORE any
  ## confirmation rule, because "waiting on lab" and "inconclusive 1st test"
  ## both mention testing without having a result.
  awaiting <- "wait|pending|inconclusive|still out|not back|hasn'?t come"
  out[is.na(out) & str_detect(raw, awaiting)] <- "Awaiting or inconclusive lab result"

  ## A clinician acting on it without a confirmed test. "Ruled out all other
  ## causes" belongs here: it is a clinical judgement, not a positive result.
  clinical <- "doctor|dr\\b|physician|clinician|urgent care|\\ber\\b|hospital|prescrib|treating me|ruled out|bactrim"
  out[is.na(out) & str_detect(raw, clinical)] <- "Treated by a clinician, not lab confirmed"

  ## Only a positive result counts as confirmed. Requiring an explicit positive
  ## word keeps "tested for it" and "unable to perform testing" out.
  confirmed <- "(confirm|positive)"
  out[is.na(out) & str_detect(raw, confirmed)] <- "Lab confirmed"

  ## Symptom-based self-assessment.
  suspected <- "symptom|think|likely|probab|certain|had it|have it|believe"
  out[is.na(out) & str_detect(raw, suspected)] <- "Self-suspected"

  out[is.na(out)] <- "Unclassified"
  factor(out, levels = CASE_DEFINITION_LEVELS)
}

## Counts by tier, in certainty order, including tiers with zero responses so
## the table shape is stable between runs.
case_definition_summary <- function(x, n_total = length(x)) {
  tibble(case_definition = factor(CASE_DEFINITION_LEVELS, levels = CASE_DEFINITION_LEVELS)) %>%
    left_join(
      tibble(case_definition = classify_case_definition(x)) %>% count(case_definition, name = "n_cases"),
      by = "case_definition"
    ) %>%
    mutate(
      n_cases = ifelse(is.na(n_cases), 0L, n_cases),
      pct_of_cases = round(100 * n_cases / n_total, 1)
    )
}
