#' Apply a Time Cutoff to Survival Data
#'
#' The `apply_time_cutoff` function truncates survival data at a specified time cutoff and returns the modified dataset.
#'
#' @param data A data frame containing the survival data.
#' @param target_info A named vector with elements `id`, `time`, `event`, and `type`:
#'   * `id` : string identifying target
#'   * `time`: Name of the time column in `data`.
#'   * `event`: Name of the event column in `data`.
#'   * `type`: Censoring type, either `"right"` or `"mstate"`.
#' @param id Optional. Column name in the data, specifying the individual identifiers. Default is `NULL`.
#' @param time_cutoff Optional. Numeric value specifying the time cutoff at which to truncate the time variable. If `NULL`, returns the original data.
#' @param traceon Logical. If `TRUE`, enables tracing for debugging purposes. Default is `FALSE`.
#'
#' @return A data.table with survival times truncated at the specified `time_cutoff`. 
#'
#' @importFrom survival survSplit Surv
#' @importFrom data.table as.data.table
#'
#' @examples
#' # Load the required packages
#' library(survival)
#' library(data.table)
#' data(cancer, package="survival")
#' lung$status_01 <- lung$status -1
#' lung$status <- NULL
#' head(lung)
#'
#' # Define the target_info list
#' target_info <- c(time = "time", event = "status_01", type = "right")
#'
#' # Apply the time cutoff
#' lungx <- apply_time_cutoff(lung, target_info, time_cutoff = 365)
#'
#' # View the first few rows of the transformed data
#' head(lungx)
#'
#' @note This function employs `survival::survSplit()` and is useful for preparing survival data for analyses requiring a time cutoff.
#' @importFrom utils head str
#' @importFrom stats na.omit
#' @export
apply_time_cutoff <- function(data, target_info, id = NULL, time_cutoff = NULL, traceon = FALSE) {
  # Nested trace function for debugging
  traceit <- function(msg, variable = NULL) {
    if (isTRUE(traceon)) {
      cat(msg, "\n")
      if (!is.null(variable)) {
        print(variable)
      }
    }
  }
  
  # Return original data if no time cutoff is specified
  if (is.null(time_cutoff)) {
    return(data)
  }
  
  # Unpack target_info 
  time <- target_info["time"]
  event <- target_info["event"]
  
  type <- target_info["type"]
  if (is.null(type)) type = "right"
  if (!type %in% c("right", "mstate")) stop("Censoring type: `", type, "` is not supported")

  # Prepare survival object based on type
  surv_obj = Surv(data[[time]], data[[event]], type=type)
  traceit("--1 surv_obj:", str(surv_obj))

  target_names <- c(time, event)
    traceit("--2 target_names:", target_names)
  traceit("--3 Original data:", head(data[, c(time, event)], n=20))
  
  # Truncate survival times using survSplit
  truncated_data <- survival::survSplit(
    formula = surv_obj ~ ., 
    data = data, 
    cut = time_cutoff, 
    episode = "temp_episode", 
    id = "temp_id"
  )
  
  traceit("--4 Data after survSplit:", head(truncated_data[, c("temp_id", time,event,"surv_obj")], n=20))
 
  # Filter for the relevant episode
 
  truncated_data <- truncated_data[truncated_data$temp_episode == 1, ]

  traceit("--5 Data for episode 1:", head(truncated_data[, c("temp_id", time,event,"surv_obj")], n=20))
 
   # Extract the new survival object values
    surv_object = truncated_data$surv_obj
    time_var <- surv_object[, "stop"]
    traceit("--5.2  time_var",  head(time_var, n=20))
    
    status_var <- if (type == "right") {
       surv_object[, "status"]
    } else if (type == "mstate") {
      status_numeric <-surv_object[, 3]
      traceit("--5.3 :", str(status_numeric))    
      factor_levels <- attr(surv_object, "inputAttributes")$event$levels
      len1 <- length(factor_levels) - 1
      factor(status_numeric, levels = 0:len1, labels = factor_levels)
    }
    traceit("--5.5 status_var:", head(status_var, n=20))
  # Remove columns
  
  columns_to_remove <- c("temp_episode", "temp_id",  "surv_obj")
  dtout  <- truncated_data[, !(names(truncated_data) %in% columns_to_remove), drop = FALSE]
   traceit("--5.7  truncated_data colnames:", head(colnames(dtout), n=20))
   
  # Prepare the final output data table
  dtout[, time]  <- time_var
  dtout[, event] <- status_var
  
  dtout <- as.data.table(dtout)
  traceit("--6 Final data after time cutoff application:", str(dtout))
  
  return(dtout)
}
