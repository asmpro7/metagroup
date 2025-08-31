#' Group Studies with Single Incidence Rates by Homogeneity
#'
#' @description
#' This function iteratively assigns studies to subgroups based on a homogeneity
#' test. The goal is to create statistically homogeneous groups of studies
#' before performing a final meta-analysis of single incidence rates.
#'
#' @param data A data frame containing the meta-analysis data.
#' @param event A vector of event counts.
#' @param time A vector of person-time at risk.
#' @param studlab A vector of study labels.
#' @param ... Additional arguments passed on to `meta::metarate`.
#'
#' @details
#' The algorithm starts with a single study in "group 1". It then processes
#' each subsequent study, attempting to place it in an existing group. A study
#' is added to a group only if its inclusion does not result in significant
#' within-group heterogeneity. If no suitable group is found, a new one is created.
#'
#' @return A list containing the final data with subgroup assignments (`data`),
#' the final `metarate` model (`model`), and the number of attempts (`attemps`).
#'
#' @importFrom meta metarate
#'
#' @author Ahmed Abdelmageed \email{ahmedelsaeedmassad@@gmail.com}
#'
#' @seealso \code{\link{meaning}}
#'
#' @export

mgrate <- function(data,
                   event,
                   time,
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
        model <- metarate(
          event = event[1:i],
          time = time[1:i],
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

  final_model <- metarate(
    event = event,
    time = time,
    studlab = studlab,
    subgroup = data$subgroup,
    random = TRUE,
    common = FALSE,
    ...
  )

  list(data = data, model = final_model, attemps = numberOfAtemps)
}
