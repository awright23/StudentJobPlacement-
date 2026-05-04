#' ggplot2: Placement rate by internship
#' @export
plot_internship_placement_rate_ggplot <- function(data) {
  agg <- aggregate(placement_binary ~ internship_completion, data, mean)
  ggplot2::ggplot(agg, ggplot2::aes(x = internship_completion, y = placement_binary)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(title = "Placement Rate by Internship",
                  x = "Internship Completion",
                  y = "Placement Rate")
}

#' ggplot2: Placement rate by college
#' @export
plot_college_placement_rate_ggplot <- function(data) {
  agg <- aggregate(placement_binary ~ college_name, data, mean)
  ggplot2::ggplot(agg, ggplot2::aes(x = college_name, y = placement_binary)) +
    ggplot2::geom_col(fill = "darkorange") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(title = "Placement Rate by College",
                  x = "College",
                  y = "Placement Rate") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' ggplot2: Internship completion counts
#' @export
plot_internship_counts_ggplot <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = internship_completion)) +
    ggplot2::geom_bar(fill = "forestgreen") +
    ggplot2::labs(title = "Internship Completion Counts",
                  x = "Internship",
                  y = "Count")
}

#' ggplot2: Placement rate by IQ group
#' @export
plot_iq_group_placement_rate_ggplot <- function(data) {
  data$IQ_group <- cut(data$IQ,
                       breaks = quantile(data$IQ, probs = c(0, .33, .66, 1)),
                       include.lowest = TRUE,
                       labels = c("Low IQ", "Medium IQ", "High IQ"))
  agg <- aggregate(placement_binary ~ IQ_group, data, mean)
  ggplot2::ggplot(agg, ggplot2::aes(x = IQ_group, y = placement_binary)) +
    ggplot2::geom_col(fill = "purple") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(title = "Placement Rate by IQ Group",
                  x = "IQ Group",
                  y = "Placement Rate")
}

#' ggplot2: Grouped boxplot (IQ × Internship × Placement)
#' @export
plot_iq_internship_placement_ggplot <- function(data) {
  ggplot2::ggplot(data,
                  ggplot2::aes(x = placement,
                               y = IQ,
                               fill = internship_completion)) +
    ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 0.8)) +
    ggplot2::labs(title = "IQ by Placement and Internship",
                  x = "Placement",
                  y = "IQ",
                  fill = "Internship") +
    ggplot2::theme_minimal()
}
