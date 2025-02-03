#' Calculate Borgan II Weights for Case-Cohort Study
#'
#' This function calculates Borgan II weights for either a survival analysis task
#' or a classification task in a case-cohort study design, allowing flexible event labels.
#'
#' @param data A data frame containing the study data.
#' @param subcoh A string specifying the column name in `data` that identifies
#' whether an observation is part of the subcohort (1 for yes, 0 for no).
#' @param event_or_status A string specifying the column name in `data` that indicates:
#' - For `task_type = "surv"`, an event indicator (1 if the event occurred, 0 otherwise).
#' - For `task_type = "classif"`, the status of observation, which must be a factor.
#' @param sampling_fraction A numeric value representing the proportion of the
#' entire cohort that is sampled to form the subcohort. Default is 0.15.
#' @param task_type A character string indicating the type of task. Options are
#' "surv" for survival analysis and "classif" for classification.
#'
#' @return A numeric vector of weights for each individual in the `data` frame.
#'
#' @details
#' This function adjusts weights according to the Borgan II method,
#' ensuring representative estimation for each outcome category.
#'
#' @examples
#' # Survival task example:
#' df_surv <- data.frame(subcoh = c(1, 0, 1, 0, 1), event = c(0, 1, 1, 1, 0))
#' weights_surv <- Borgan2_weights(df_surv, "subcoh", "event", 0.15, "surv")
#' print(weights_surv)
#'
#' # Classification task example:
#' df_classif <- data.frame(subcoh = c(1, 0, 1, 0, 1),
#'                  status = factor(c('censored', 'main', 'competing', 'main', 'censored')))
#' weights_classif <- Borgan2_weights(df_classif, "subcoh", "status", 0.15, "classif")
#' print(weights_classif)
#'
#' @export
Borgan2_weights <- function(data, subcoh, event_or_status, sampling_fraction=0.15, task_type="surv") {
  if (!task_type %in% c("surv", "classif")) {
    stop("Invalid task_type. Choose either 'surv' for survival analysis or 'classif' for classification.")
  }

  weight_factor <- 1 / sampling_fraction  # Weight factor based on the sampling fraction
  wt <- rep(NA, times = nrow(data))

  if (task_type == "surv") {
    # Validate for survival task
    if (!all(data[[event_or_status]] %in% c(0, 1))) {
      stop("For 'surv', event_or_status must be binary (0 or 1).")
    }
    # For survival analysis
    wt[data[[subcoh]] == 1] <- weight_factor
    wt[data[[subcoh]] == 0 & data[[event_or_status]] == 1] <- 1
  } else if (task_type == "classif") {
    # Ensure event_or_status is a factor
    if (!is.factor(data[[event_or_status]])) {
      stop("For 'classif', event_or_status must be a factor.")
    }
    # Assume first level is 'censored', rest are events
    levels <- levels(data[[event_or_status]])
    if (length(levels) < 2) {
      stop("Classification task requires at least two levels in the factor.")
    }
    censored_label <- levels[1]  # First level as 'censored'
    event_labels <- levels[-1]   # Remaining levels as events

    wt[data[[subcoh]] == 1] <- weight_factor
    wt[data[[subcoh]] == 0 & data[[event_or_status]] %in% event_labels] <- 1
  }

  return(wt)
}
