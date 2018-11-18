
#'Summarize samples per area-year
#'
#'Summarize samples per area-year into median, lower and upper, where lower and
#'upper are given by the 2.5th and 97.5th percentiles of the sample,
#'respectively
#'
#'@param samples tibble or data frame with iso-year-samples (iso refers to country iso or region name,
#' variable names for samples include "samp")
#'
#'@return tibble with iso-year-median-lower-upper
#'@export
#'
#' @examples
#'  summarize_fp(tribble(
#'  ~iso, ~ year, ~ sample1, ~ sample2,
#'  4, 1990, 0.5, 0.7,
#'  4, 1990, 0.4, 0.8,
#'  5, 1991, 0.3, 0.2))

summarize_fp <- function(samples){
  samples %>%
    gather(key = "sample", value = "value", contains("samp")) %>%
    group_by(iso, year) %>%
    summarize(median = quantile(value, 0.5), lower = quantile(value, 0.025),
              upper = quantile(value, 0.975))
  #less ideal: needs repetition of quantile
}

# summarize_fp(tribble(
#     ~iso, ~ year, ~ sample1, ~ sample2,
#     4, 1990, 0.5, 0.7,
#     4, 1990, 0.4, 0.8,
#     5, 1991, 0.3, 0.2))
