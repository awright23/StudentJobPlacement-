#' Base R: Placement rate by internship
#' @export
plot_internship_placement_rate_base <- function(data) {
  rates <- tapply(data$placement_binary, data$internship_completion, mean)
  barplot(rates,
          ylim = c(0, 1),
          main = "Placement Rate by Internship",
          xlab = "Internship Completion",
          ylab = "Placement Rate",
          col = "lightblue")
}

#' Base R: Placement rate by college
#' @export
plot_college_placement_rate_base <- function(data) {
  rates <- tapply(data$placement_binary, data$college_name, mean)
  barplot(rates,
          ylim = c(0, 1),
          las = 2,
          main = "Placement Rate by College",
          xlab = "College",
          ylab = "Placement Rate",
          col = "darkgrey")
}

#' Base R: Internship completion counts
#' @export
plot_internship_counts_base <- function(data) {
  barplot(table(data$internship_completion),
          main = "Internship Completion Counts",
          xlab = "Internship",
          ylab = "Count",
          col = "lightgreen")
}

#' Base R: Placement rate by IQ group
#' @export
plot_iq_group_placement_rate_base <- function(data) {
  data$IQ_group <- cut(data$IQ,
                       breaks = quantile(data$IQ, probs = c(0, .33, .66, 1)),
                       include.lowest = TRUE,
                       labels = c("Low IQ", "Medium IQ", "High IQ"))
  rates <- tapply(data$placement_binary, data$IQ_group, mean)
  barplot(rates,
          ylim = c(0, 1),
          main = "Placement Rate by IQ Group",
          xlab = "IQ Group",
          ylab = "Placement Rate",
          col = "pink")
}

#' Base R: Grouped boxplot (IQ × Internship × Placement)
#' @export
plot_iq_internship_placement_base <- function(data) {
  group <- interaction(data$placement, data$internship_completion, sep = " / ")
  boxplot(data$IQ ~ group,
          main = "IQ by Placement and Internship",
          xlab = "Placement / Internship",
          ylab = "IQ",
          las = 2,
          col = "lightgray")
}
