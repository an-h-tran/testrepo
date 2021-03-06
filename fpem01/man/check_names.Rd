% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access_results.R
\name{check_names}
\alias{check_names}
\title{Check if regions are included in a larger list}
\usage{
check_names(regions, all_regions)
}
\arguments{
\item{regions}{vector of strings}

\item{all_regions}{vector of strings}
}
\value{
error if there is at least one element of regions not included in all_regions
}
\description{
Check if regions are included in a larger list
}
\examples{
check_names(letters[1:10], letters[1:20]) # no error
check_names(letters[1:21], letters[1:20]) # error produced
}
