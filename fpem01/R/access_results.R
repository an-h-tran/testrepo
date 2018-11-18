

#' Access FP results
#'
#' @param fp_results tibble with iso (to filter on)
#' @param regions subset of fp_results$iso (error is returned when region is not included)
#'
#' @return fp_results for the selected regions
#'
#' @examples
access_results <- function(fp_results, regions) {
  check_names(regions, fp_results$iso)
  fp_results %>%
    filter(iso %in% regions)
}

#' Check if regions are included in a larger list
#'
#' @param regions vector of strings
#' @param all_regions vector of strings
#'
#' @return error if there is at least one element of regions not included in all_regions
#' @export
#'
#' @examples
#' check_names(letters[1:10], letters[1:20]) # no error
#' check_names(letters[1:21], letters[1:20]) # error produced
check_names <- function(regions, all_regions) {
  if (!all(regions %in% all_regions)) {
    stop(
      paste0(
        # here paste0 is used to add values of the arguments to the error message
        "All elements in 'regions' must be in 'all_regions'. \n",
        "These regions are NOT in all regions: ",
        paste0(regions[!regions %in% all_regions], collapse = ", ")
      ),
      call. = FALSE
    )
  }
}
