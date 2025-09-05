#' Print and Plot Methods for 'grouped' Objects
#'
#' @description
#' S3 methods for objects of class `grouped` returned by the `meaning()` function.
#' \itemize{
#'   \item `print.grouped` provides a concise summary view of the results.
#'   \item `plot.grouped` creates a faceted bar chart to visualize the composition of each subgroup.
#' }
#'
#' @param x An object of class `grouped`.
#' @param ... Additional arguments (currently unused).
#'
#' @return
#' The `print` method is called for its side-effect of printing a summary
#' table to the console.
#'
#' The `plot` method is called for its side-effect of generating a faceted
#' bar chart in the current graphics device.
#'
#' Both methods invisibly return the original object `x`.
#'
#' @importFrom dplyr group_by summarise first
#' @importFrom ggplot2 ggplot aes geom_col geom_text facet_wrap labs theme_minimal theme element_text
#' @importFrom rlang .data
#'
#' @author Ahmed Abdelmageed \email{ahmedelsaeedmassad@@gmail.com}
#'
#' @seealso \code{\link{meaning}}
#'
#' @name grouped
#'
#' @export

print.grouped <- function(x, ...) {
  print(x[[3]], ...)
}

#' @rdname grouped
#' @export

plot.grouped <- function(x, ...) {
  df <- x[[1]]

  k_values <- df %>%
    group_by(.data$group) %>%
    summarise(K = first(.data$K), .groups = 'drop')

  ggplot(data = df, aes(x = .data$group, y = .data$percentage)) +
    geom_col(aes(fill = .data$value)) +
    geom_text(
      data = k_values,
      aes(x = .data$group, y = 95, label = .data$K),
      size = 3.5,
      color = "black") +
    facet_wrap(~ .data$variable) +
    labs(
      x = "Group",
      y = "Percentage (%)",
      fill = "Category") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"))
}
