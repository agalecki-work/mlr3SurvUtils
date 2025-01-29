#' Calculate Borgan II Weights
#'
#' This function calculates Borgan weights for case-cohort data used in survival analysis.
#' The approach is based on the paper by Moger, Pawitan, and Borgan (2008).
#'
#' @param data A data frame that contains the cohort data.
#' @param subcoh A numeric vector within the data indicating if a subject is part of the subcohort (1 if yes, 0 if no).
#' @param event A numeric vector within the data indicating if an event occurred (1 if event occurred, 0 if not).
#'
#' @return A numeric vector of weights corresponding to each subject in the data frame.
#' Each subject receives a weight based on their subcohort and event status.
#'
#' @details
#' The sampling fraction for non-cases is calculated and used to assign weights to non-case subjects
#' in the subcohort, ensuring an unbiased estimation in survival analysis. Subjects with events are
#' given a weight of 1.
#'
#' @references
#' Moger, T.A., Pawitan, Y. and Borgan, O (2008) Case-cohort methods for survival data on families 
#' from routine registers. Statistics in Medicine, 27(7), pp. 1062-1074.
#'
#' @examples
#' # Example data
#' df <- data.frame(subcoh = c(1, 0, 1, 0, 1), event = c(0, 1, 0, 1, 1))
#'
#' # Calculate weights
#' weights <- Borgan2_weights(df, df$subcoh, df$event)
#' print(weights)
#'
#' @seealso \url{https://biostat3.net/download/R/lecture_Biostat3_NCC_casecoh_Johansson_20201118.pdf}
#'
#' @export
Borgan2_weights <- function(data, subcoh, event){
  tbl = table(event= data$event, subcoh= data$subcoh,  useNA="always")
  ttx <- as.matrix(tbl)
  sf_nc  <- ttx[1,2]/ sum(ttx[1,])  #  sampling_fraction_for non_cases       
  wt <- rep(NA, times= nrow(data)) 
  wt[data$subcoh==1 & data$event==0]  <-  1/sf_nc
  wt[data$subcoh==1 & data$event==1]  <-  1
  wt[data$subcoh==0 & data$event==1]  <-  1
  return(wt)
}