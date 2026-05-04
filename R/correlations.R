
# correlations.R

#' Correlation between IQ and placement
#' @param data Cleaned placement data
#' @return Numeric correlation
#' @export
correlation_iq_placement <- function(data) {
  stats::cor(data$IQ, data$placement_binary)
}

#' Correlation between CGPA and placement
#' @param data Cleaned placement data
#' @return Numeric correlation
#' @export
correlation_cgpa_placement <- function(data) {
  stats::cor(data$CGPA, data$placement_binary)
}

#' Correlation between internship and placement
#' @param data Cleaned placement data
#' @return Numeric correlation
#' @export
correlation_internship_placement <- function(data) {
  stats::cor(data$internship_binary, data$placement_binary)
}

#' Correlation matrix for key variables
#' @param data Cleaned placement data
#' @return Correlation matrix
#' @export
correlation_matrix <- function(data) {
  vars <- data[, c("IQ", "CGPA", "internship_binary", "placement_binary")]
  stats::cor(vars)
}

