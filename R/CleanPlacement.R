#' Clean placement data
#' @param data A data frame like placement
#' @return A cleaned data frame
#' @export
clean_placement <- function(data) {

  # Fix empty internship values
  data$internship_completion[data$internship_completion == ""] <- "No"

  # Standardize Yes/No
  data$internship_completion <- ifelse(data$internship_completion == "Yes", "Yes", "No")

  # Binary versions
  data$internship_binary <- ifelse(data$internship_completion == "Yes", 1, 0)
  data$placement_binary  <- ifelse(data$placement == "Placed", 1, 0)

  # Numeric conversions
  data$IQ   <- suppressWarnings(as.numeric(data$IQ))
  data$CGPA <- suppressWarnings(as.numeric(data$CGPA))

  # Keep only needed columns and drop NAs
  data <- stats::na.omit(data[, c("IQ", "CGPA",
                                  "internship_completion", "internship_binary",
                                  "placement", "placement_binary",
                                  "college_name")])

  return(data)
}
