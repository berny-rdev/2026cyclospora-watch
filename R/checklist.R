## ---- CHECKLIST ROUTING ----
##
## Shared by index.Rmd and cyclospora_outbreak_analysis.R. Extracted verbatim
## from index.Rmd, which was the newer of the two copies.

## ---- CHECKLIST ROUTING ---------------------------------------------------
## Checklist answers are preset option strings - already category-shaped and
## unambiguous - so they resolve by direct lookup and never reach the LLM or
## the regex fallback. That removes the whole class of drift for ~90% of the
## reported volume, and leaves the classifier to do the one job it's needed
## for: messy free text.
CHECKLIST_MAP_PATH <- "checklist-mapping.json"

checklist_map <- local({
  if (!file.exists(CHECKLIST_MAP_PATH)) {
    stop(CHECKLIST_MAP_PATH, " is missing - checklist answers cannot be routed. ",
         "Refusing to fall back to the classifier, which would silently re-introduce ",
         "drift on the highest-volume field.", call. = FALSE)
  }
  m <- jsonlite::fromJSON(CHECKLIST_MAP_PATH, simplifyVector = FALSE)$mapping
  if (is.null(m) || length(m) == 0) stop(CHECKLIST_MAP_PATH, " has no 'mapping' object.", call. = FALSE)
  m
})

## Resolves checklist text to categories. An option present in the form but
## absent from the map is a hard error, not a guess - a new checklist option
## must be mapped by hand or the counts silently lose it.
classify_checklist <- function(raw_text_vec) {
  long <- split_freetext(raw_text_vec)
  if (nrow(long) == 0) return(long %>% mutate(category = character(0)))
  unknown <- setdiff(unique(long$item), names(checklist_map))
  if (length(unknown)) {
    stop("checklist option(s) not present in ", CHECKLIST_MAP_PATH, ":\n  - ",
         paste(unknown, collapse = "\n  - "),
         "\n  Add them to the mapping (with the category slug they belong to) and re-run.",
         call. = FALSE)
  }
  long %>% mutate(category = vapply(item, function(i) as.character(checklist_map[[i]]), character(1)))
}

