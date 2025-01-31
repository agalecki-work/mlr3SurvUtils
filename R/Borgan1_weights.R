#' Calculate Borgan I Weights for a Case-Cohort Study
#'
#' This function calculates weights for a case-cohort study using the Borgan I method.
#' These weights adjust for the sampling strategy to ensure representative estimates from the subcohort.
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
#' - Members of the subcohort who did not have the event receive a weight equal to the reciprocal of the sampling fraction.
#' - Members of the subcohort who had the event receive a weight of 1.
#' - Non-subcohort members who had the event receive a weight of 1.
#'
#' The weights ensure that the sampled subcohort provides unbiased estimates of the full cohort.
#'
#' @references
#' Moger, T. A., Pawitan, Y., & Borgan, O. (2008). Case-cohort methods for survival data on families from routine registers. Statistics in Medicine, 27(7), 1062-1074.
#'
#' @examples
#' # Example usage:
#' df <- data.frame(subcoh = c(1, 0, 1, 0, 1), event = c(0, 1, 1, 1, 0))
#' weights <- Borgan1_weights(df, "subcoh", "event", 0.15)
#' print(weights)
#'
#' @export
Borgan1_weights <- function(data, subcoh, event, sampling_fraction=0.15) {
  weight_factor <- 1/sampling_fraction  # weight factor based on sampling fraction
  wt <- rep(NA, times = nrow(data))
  wt[data[[subcoh]] == 1 & data[[event]] == 0] <- weight_factor
  wt[data[[subcoh]] == 1 & data[[event]] == 1] <- 1
  wt[data[[subcoh]] == 0 & data[[event]] == 1] <- 1
  return(wt)
}
