#' Calculate Weights for Case-Cohort Study
#'
#' This function calculates weights for a case-cohort study design using one of three methods:
#' Borgan I, Borgan II, or Self-Prentice. It supports survival analysis with right-censored data
#' or multi-state analysis.
#'
#' @param data A data frame containing the study data.
#' @param subcoh A string specifying the subcohort column (1 = yes, 0 = no).
#' @param event_or_status A string specifying the event/status column:
#' - For `type = "right"`, an event indicator (1 = event, 0 = censored).
#' - For `type = "mstate"`, a factor indicating status (e.g., censored, events).
#' @param sampling_fraction Proportion of cohort sampled (0 < x <= 1). Default is 0.15.
#' Ignored for Self-Prentice method.
#' @param method Weighting method: "Borgan1" (Borgan I), "Borgan2" (Borgan II), or
#' "SelfPrentice" (Self-Prentice). Default is "Borgan1".
#' @param type Task type: "right" for survival analysis with right-censored data,
#' or "mstate" for multi-state analysis. Default is "right".
#' @param censored_level For "mstate", the censored level in event_or_status. Default is first level.
#'
#' @return A numeric vector of weights. Rows with NA weights must be filtered before analysis (e.g., coxph).
#'
#' @details
#' - **Borgan I**: Weights subcohort censored subjects (or non-events) by `1/sampling_fraction`,
#'   all events (subcohort or not) by 1. Non-subcohort censored subjects are NA.
#' - **Borgan II**: Weights all subcohort members (censored or events) by `1/sampling_fraction`,
#'   non-subcohort events by 1. Non-subcohort censored subjects are NA.
#' - **Self-Prentice**: Weights all subcohort members and non-subcohort events by 1.
#'   Non-subcohort censored subjects are NA. Sampling fraction is not used.
#'
#' @examples
#' # Right-censored survival examples:
#' df <- data.frame(subcoh = c(1, 0, 1, 0, 1, 0), 
#'                  event = c(0, 1, 1, 1, 0, 0))
#' w_b1 <- case_cohort_weights(df, "subcoh", "event", 0.15, "Borgan1", "right")
#' w_b2 <- case_cohort_weights(df, "subcoh", "event", 0.15, "Borgan2", "right")
#' w_sp <- case_cohort_weights(df, "subcoh", "event", 0.15, "SelfPrentice", "right")
#' print(w_b1)  # [6.666667, 1, 1, 1, 6.666667, NA]
#' print(w_b2)  # [6.666667, 1, 6.666667, 1, 6.666667, NA]
#' print(w_sp)  # [1, 1, 1, 1, 1, NA]
#'
#' # Multi-state examples:
#' df_ms <- data.frame(subcoh = c(1, 0, 1, 0, 1, 0),
#'     status = factor(c('censored', 'main', 'competing', 'main', 'censored', 'censored')))
#' w_b1_ms <- case_cohort_weights(df_ms, "subcoh", "status", 0.15, "Borgan1", "mstate")
#' w_b2_ms <- case_cohort_weights(df_ms, "subcoh", "status", 0.15, "Borgan2", "mstate")
#' w_sp_ms <- case_cohort_weights(df_ms, "subcoh", "status", 0.15, "SelfPrentice", "mstate")
#' print(w_b1_ms)  # [6.666667, 1, 1, 1, 6.666667, NA]
#' print(w_b2_ms)  # [6.666667, 1, 6.666667, 1, 6.666667, NA]
#' print(w_sp_ms)  # [1, 1, 1, 1, 1, NA]
#'
#' @export
case_cohort_weights <- function(data, subcoh, event_or_status, sampling_fraction = 0.15, 
                                method = "Borgan1", type = "right", censored_level = NULL) {
  # Input validation
  if (!method %in% c("Borgan1", "Borgan2", "SelfPrentice")) {
    stop("Invalid method. Choose 'Borgan1', 'Borgan2', or 'SelfPrentice'.")
  }
  if (!type %in% c("right", "mstate")) {
    stop("Invalid type. Choose 'right' for survival analysis of right-censored data or 'mstate' for multi-state analysis.")
  }
  if (!is.numeric(sampling_fraction) || sampling_fraction <= 0 || sampling_fraction > 1) {
    stop("sampling_fraction must be between 0 and 1.")
  }

  weight_factor <- 1 / sampling_fraction
  wt <- rep(NA, times = nrow(data))

  if (type == "right") {
    if (!all(data[[event_or_status]] %in% c(0, 1, NA))) {
      stop("For 'right', event_or_status must be binary (0 or 1, NA allowed).")
    }
    if (method == "Borgan1") {
      wt[data[[subcoh]] == 1 & data[[event_or_status]] == 0] <- weight_factor
      wt[data[[subcoh]] == 1 & data[[event_or_status]] == 1] <- 1
      wt[data[[subcoh]] == 0 & data[[event_or_status]] == 1] <- 1
    } else if (method == "Borgan2") {
      wt[data[[subcoh]] == 1] <- weight_factor
      wt[data[[subcoh]] == 0 & data[[event_or_status]] == 1] <- 1
    } else if (method == "SelfPrentice") {
      wt[data[[subcoh]] == 1] <- 1
      wt[data[[subcoh]] == 0 & data[[event_or_status]] == 1] <- 1
    }
  } else if (type == "mstate") {
    if (!is.factor(data[[event_or_status]])) {
      stop("For 'mstate', event_or_status must be a factor.")
    }
    levels <- levels(data[[event_or_status]])
    if (length(levels) < 2) {
      stop("For 'mstate', event_or_status must have at least two levels.")
    }
    censored_label <- if (is.null(censored_level)) levels[1] else censored_level
    if (!censored_label %in% levels) {
      stop("censored_level must be a level in event_or_status.")
    }
    event_labels <- levels[levels != censored_label]
    
    if (method == "Borgan1") {
      wt[data[[subcoh]] == 1 & data[[event_or_status]] == censored_label] <- weight_factor
      wt[data[[subcoh]] == 1 & data[[event_or_status]] %in% event_labels] <- 1
      wt[data[[subcoh]] == 0 & data[[event_or_status]] %in% event_labels] <- 1
    } else if (method == "Borgan2") {
      wt[data[[subcoh]] == 1] <- weight_factor
      wt[data[[subcoh]] == 0 & data[[event_or_status]] %in% event_labels] <- 1
    } else if (method == "SelfPrentice") {
      wt[data[[subcoh]] == 1] <- 1
      wt[data[[subcoh]] == 0 & data[[event_or_status]] %in% event_labels] <- 1
    }
  }

  attr(wt, "note") <- "Filter data to exclude NA weights before using in coxph."
  return(wt)
}
