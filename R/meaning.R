#' Explore Composition of Homogeneous Study Subgroups
#'
#' @description
#' After grouping studies using a meta-grouping function (e.g., `mgbin`), this
#' function analyzes the composition of each subgroup based on specified
#' categorical variables. It helps to give "meaning" to the statistically derived groups.
#'
#' @param data A data frame or the list object returned by a meta-grouping function.
#' If a list, the function will automatically use the `data` element.
#' @param subgroup A string specifying the name of the subgroup column. Defaults to `"subgroup"`.
#' @param variables A character vector of column names (categorical variables) to analyze.
#' @param min An integer specifying the minimum number of studies a group must have
#'   to be included in the final summarized output. Defaults to 3.
#'
#' @details
#' For each subgroup and each specified variable, the function calculates the
#' percentage distribution of its categories. It identifies the most frequent
#' (dominant) category and performs a chi-squared test to assess if the
#' distribution of categories within that group is significantly different from random.
#'
#' @return
#' An S3 object of class `grouped`, which is a list containing three data frames:
#' \itemize{
#'   \item `detailed`: Contains the percentage distribution for all categories of all variables within every group.
#'   \item `up.hand`: A subset of `detailed`, showing only the dominant category for each variable in each group.
#'   \item `final`: A summarized version of `up.hand`, showing the single most dominant characteristic for each group that meets the `min` size threshold. This is the default print output.
#' }
#'
#' @importFrom dplyr select group_by mutate ungroup distinct left_join relocate slice_max filter n
#' @importFrom stats chisq.test
#' @importFrom rlang .data
#'
#' @author Ahmed Abdelmageed \email{ahmedelsaeedmassad@@gmail.com}
#'
#' @seealso \code{\link{mgbin}}, \code{\link{mgcont}}, \code{\link{mgcor}}
#'
#' @examples
#' # Create a sample dataset of studies
#' study_data <- data.frame(
#'   author = letters[1:10],
#'   country = sample(c("USA", "China", "Egypt"), 10, replace = TRUE),
#'   setting = sample(c("Hospital", "Community"), 10, replace = TRUE),
#'   subgroup = c("group 1", "group 2", "group 1", "group 1", "group 2",
#'                "group 2", "group 1", "group 3", "group 2", "group 2")
#' )
#'
#' # Analyze the composition of the subgroups
#' meaning_result <- meaning(study_data, variables = c("country", "setting"))
#'
#' # The default print shows the 'final' summary table
#' print(meaning_result)
#'
#' # Access detailed results
#' # meaning_result$detailed
#'
#' @export

meaning <- function(data,
                    subgroup = "subgroup",
                    variables,
                    min = 3) {
  if (is.list(data) && !is.data.frame(data)) {
    data = data$data
  }else {
    data = data
  }
  K <- data %>% select(subgroup) %>% group_by(subgroup) %>% mutate(K = n()) %>% ungroup() %>% distinct()

  results <- list()

  for (variable in variables) {
    unique_groups <- unique(data[[subgroup]])

    for (group_level in unique_groups) {
      sub_data <- data[data[[subgroup]] == group_level, ]

      tbl <- table(sub_data[[variable]])
      percentages <- round(prop.table(tbl) * 100, 1)

      res <- data.frame(
        group = group_level,
        variable = variable,
        value = names(tbl),
        percentage = as.numeric(percentages)
      )

      res$upperhand <- ifelse(res$percentage == max(res$percentage), "*", "-")

      if (length(tbl) < 2) {
        p_value <- "N/A"
      } else {
        chi_test <- suppressWarnings(chisq.test(tbl))
        p_value <- ifelse(chi_test$p.value < 0.05,
                          paste0(round(chi_test$p.value, 4), "*"),
                          round(chi_test$p.value, 4))}

      res$p_value <- p_value
      results[[length(results) + 1]] <- res
    }
  }

  detailed <- do.call(rbind, results) %>% left_join(K, by = c("group"= subgroup)) %>% relocate(K, .after = .data$group)

  up.hand <- detailed %>%
    filter(.data$upperhand == "*") %>%
    select(-.data$upperhand)

  final <- up.hand %>% group_by(.data$group) %>% slice_max(order_by = .data$percentage, n = 1) %>% ungroup() %>% filter(.data$K >= min)

  returnedlist <- list(detailed = detailed, up.hand = up.hand, final = final)
  class(returnedlist) <- "grouped"
  return(returnedlist)
}
