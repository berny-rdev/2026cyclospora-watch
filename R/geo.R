## ---- GEOGRAPHY ----
##
## Shared by index.Rmd and cyclospora_outbreak_analysis.R. Respondents type
## states every way there is - "CA", "california", "Washington DC" - so counts
## are wrong unless they are folded to one canonical name first. The script
## previously counted the raw strings, which split one state across several rows.

# --- US state counts ---
normalize_state <- function(x) {
  x <- str_trim(x)
  full_match <- state.name[match(str_to_title(x), state.name)]
  abbr_match <- state.name[match(str_to_upper(x), state.abb)]
  dc_match <- ifelse(
    str_to_lower(x) %in% c("dc", "washington dc", "washington, dc", "district of columbia"),
    "District of Columbia", NA_character_
  )
  coalesce(full_match, abbr_match, dc_match)
}
