#' Summarize Student Job Placement Data
#'
#' @param data A cleaned placement dataset
#'
#' @return An object of class placement_summary
#' @export
summarize_placement <- function(data) {

  total <- nrow(data)
  placed <- sum(data$Placement == "Placed", na.rm = TRUE)
  not_placed <- sum(data$Placement == "Not Placed", na.rm = TRUE)
  placement_rate <- placed / total

  structure(
    list(
      total = total,
      placed = placed,
      not_placed = not_placed,
      placement_rate = placement_rate
    ),
    class = "placement_summary"
  )
}
