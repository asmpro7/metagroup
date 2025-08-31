#' Group Studies with Continuous Outcome Data by Homogeneity
#'
#' @description
#' This function iteratively assigns studies to subgroups based on a homogeneity
#' test. The goal is to create statistically homogeneous groups of studies
#' before performing a final meta-analysis with continuous outcome data.
#'
#' @param data A data frame containing the meta-analysis data.
#' @param n.e A vector of sample sizes in the experimental group.
#' @param mean.e A vector of means in the experimental group.
#' @param sd.e A vector of standard deviations in the experimental group.
#' @param n.c A vector of sample sizes in the control group.
#' @param mean.c A vector of means in the control group.
#' @param sd.c A vector of standard deviations in the control group.
#' @param studlab A vector of study labels.
#' @param ... Additional arguments passed on to `meta::metacont`.
#'
#' @details
#' The algorithm starts with a single study in "group 1". It then processes
#' each subsequent study, attempting to place it in an existing group. A study
#' is added to a group only if its inclusion does not result in significant
#' within-group heterogeneity. If no suitable group is found, a new one is created.
#'
#' @return A list containing the final data with subgroup assignments (`data`),
#' the final `metacont` model (`model`), and the number of attempts (`attempts`).
#'
#' @importFrom meta metacont
#'
#' @author Ahmed Abdelmageed \email{ahmedelsaeedmassad@@gmail.com}
#'
#' @seealso \code{\link{meaning}}
#'
#' @export

mgcont <- function(data,
                   n.e,
                   mean.e,
                   sd.e,
                   n.c,
                   mean.c,
                   sd.c,
                   studlab, ...) {
  if (!"subgroup" %in% colnames(data)) {
    data$subgroup <- "last group"
  }

  data$subgroup[1] <- "group 1"
  numberOfAtemps <- 0
  for (i in 2:nrow(data)) {
    assigned <- FALSE
    existing_groups <- sort(unique(data$subgroup[1:(i-1)]))
    existing_groups <- existing_groups[existing_groups != "last group"]

    for (group in existing_groups) {
      numberOfAtemps <- numberOfAtemps + 1
      temp_data <- data
      temp_data$subgroup[1:i] <- as.character(temp_data$subgroup[1:i])
      temp_data$subgroup[i] <- group

      pval <- tryCatch({
        model <- metacont(
          mean.e = mean.e[1:i],
          sd.e = sd.e[1:i],
          n.e = n.e[1:i],
          mean.c = mean.c[1:i],
          sd.c = sd.c[1:i],
          n.c = n.c[1:i],
          studlab = studlab[1:i],
          subgroup = temp_data$subgroup[1:i],
          random = TRUE,
          common = FALSE,
          ...
        )
        idx <- which(model$subgroup.levels == group)
        ifelse(length(idx) > 0, model$pval.Q.w[idx], NA)
      }, error = function(e) NA)

      if (is.na(pval) || pval >= 0.1) {
        data$subgroup[i] <- group
        assigned <- TRUE
        break
      }
    }

    if (!assigned) {
      new_group <- paste("group", length(existing_groups) + 1)
      data$subgroup[i] <- new_group
    }
  }

  final_model <- metacont(
    mean.e = mean.e,
    sd.e = sd.e,
    n.e = n.e,
    mean.c = mean.c,
    sd.c = sd.c,
    n.c = n.c,
    studlab = studlab,
    subgroup = data$subgroup,
    random = TRUE,
    common = FALSE,
    ...
  )

  list(data = data, model = final_model, attempts = numberOfAtemps)
}
