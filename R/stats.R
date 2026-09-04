## ---- PROPORTION STATISTICS ----
##
## Shared by index.Rmd and cyclospora_outbreak_analysis.R. Extracted verbatim
## from index.Rmd, which was the newer of the two copies.

# 95% Wilson score CI on each proportion - stays sane at small n and near
# 0%/100%, unlike the naive normal approximation. Base R only.
wilson_ci <- function(x, n, conf_level = 0.95) {
  if (n == 0) return(c(lower = NA_real_, upper = NA_real_))
  p_hat <- x / n
  z <- qnorm(1 - (1 - conf_level) / 2)
  denom <- 1 + z^2 / n
  center <- p_hat + z^2 / (2 * n)
  adj <- z * sqrt((p_hat * (1 - p_hat) + z^2 / (4 * n)) / n)
  c(lower = max(0, (center - adj) / denom), upper = min(1, (center + adj) / denom))
}

add_wilson_ci <- function(freq_df, n_total) {
  ci <- purrr::map2_dfr(freq_df$n_cases, n_total, ~ as.list(wilson_ci(.x, .y)))
  freq_df %>%
    mutate(ci_low_pct = round(ci$lower * 100, 1), ci_high_pct = round(ci$upper * 100, 1))
}

