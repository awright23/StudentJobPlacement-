#' Print method for Student job placement_summary objects
#'
#' @param x A placement_summary object
#' @param ... Additional arguments
#'
#' @export
print.placement_summary <- function(x, ...) {
  cat("Placement Summary\n")
  cat("-----------------\n")
  cat("Total Students:", x$total, "\n")
  cat("Placed:", x$placed, "\n")
  cat("Not Placed:", x$not_placed, "\n")
  cat("Placement Rate:", round(x$placement_rate, 3), "\n")
  invisible(x)
}
