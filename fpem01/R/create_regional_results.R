#' Summarize Regional Samples Function
#'
#' Summarize regional samples into points estimates (median), and
#' uncertainty intervals (lower and upper given by the 2.5th and 97.5th percentiles
#' of the sample, respectively)
#'
#' @param samples tibble or data frame with iso-year-samples-region-population counts
#' (iso refers to country iso, variable names for samples include "samp")
#'
#' @return tibble with region-year-median-lower-upper
#' @export
#'
#' @examples
#' create_regional_results(tribble(
#'   ~iso, ~ year, ~ sample1, ~ sample2, ~region, ~population_ct,
#'      4, 1990, 0.3, 0.4, "Southern Asia", 2025079,
#'      4, 1991, 0.5, 0.6, "Southern Asia", 2069776,
#'      858, 2020, 0.7, 0.9, "South America", 440817))
#'
create_regional_results <- function(samples){
  samples %>%
    gather(key ="sample", value ="value", contains("sample")) %>%
    group_by (region, year, sample) %>%
    summarize ("weighted_mean" = weighted.mean(value, population_ct))%>%
    summarize(median = quantile(weighted_mean, 0.5, na.rm=TRUE),
              lower = quantile(weighted_mean, 0.025, na.rm=TRUE),
              upper = quantile(weighted_mean, 0.975, na.rm=TRUE))
}
