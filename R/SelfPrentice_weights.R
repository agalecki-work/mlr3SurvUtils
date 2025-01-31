#' Calculate Self-Prentice Weights for a Case-Cohort Study
#'
#' This function calculates weights for a case-cohort study using the Self-Prentice method.
#' These weights adjust for the sampling strategy to provide representative estimates from the subcohort.
#'
#' @param data A data frame containing the study data.
#' @param subcoh A string specifying the column name in `data` that identifies whether an observation is part of the subcohort (1 for yes, 0 for no).
#' @param event A string specifying the column name in `data` that indicates the occurrence of the event of interest (1 if the event occurred, 0 otherwise).
#' @param sampling_fraction A numeric value representing the proportion of the entire cohort that is sampled to form the subcohort. Default is 0.15.
#'
#' @return A numeric vector of weights for each individual in the `data` frame.
#'
#' @details
#' The function assigns weights as follows:
#' - All subcohort members, regardless of event status, receive a weight equal to the reciprocal of the sampling fraction.
#' - Non-subcohort members who had the event receive a weight of 1.
#'
#' The weights ensure that the subcohort offers unbiased estimates of the full cohort.
#'
#' @references
#' Self, S. G., & Prentice, R. L. (1988). Asymptotic Distribution Theory and Efficiency Results for Case-Cohort Studies. Annals of Statistics, 16(1), 64-81.
#'
#' @examples
#' # Example usage:
#' df <- data.frame(subcoh = c(1, 0, 1, 0, 1), event = c(0, 1, 1, 1, 0))
#' weights <- SelfPrentice_weights(df, "subcoh", "event", 0.15)
#' print(weights)
#'
#' @export
SelfPrentice_weights <- function(data, subcoh, event, sampling_fraction=0.15) {
  weight_factor <- 1/sampling_fraction  # weight factor based on sampling fraction
  wt <- rep(NA, times = nrow(data))
  wt[data[[subcoh]] == 1] <- weight_factor  # Subcohort members
  wt[data[[subcoh]] == 0 & data[[event]] == 1] <- 1  # Non-subcohort event cases
  return(wt)
}
