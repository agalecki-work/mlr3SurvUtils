#' Calculate Descriptive Statistics
#'
#' This function computes descriptive statistics for specified columns of a task data object.
#' It supports optional grouping by specified columns and returns a summary data frame.
#'
#' @param task An object containing the data, expected to have a \code{row_ids} attribute and a \code{data()} method to extract the data frame.
#' @param cols A character vector of column names to summarize. If \code{NULL}, all numeric columns are summarized by default.
#' @param group_by_cols A character vector of column names to group by before summarizing. If \code{NULL}, no grouping is applied.
#'
#' @return A data frame of descriptive statistics. Includes counts, missing values, means, standard deviations, quartiles, and more for each specified column, optionally grouped by given columns.
#'
#' @importFrom dplyr select where group_by summarise across
#' @importFrom dplyr %>% all_of
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stats quantile
#'
#' @examples
#' \dontrun{
#' # Load necessary packages
#' library(mlr3SurvUtils)
#' library(mlr3proba)
#' 
#' # Load the 'lung' task
#' task = tsk("lung")
#' 
#' # Calculate descriptive statistics for numeric columns
#' Descriptive_stats(task)
#' 
#' # Calculate statistics for specific columns
#' Descriptive_stats(task, cols = c("age", "wt.loss"))
#' 
#' # Group by `sex`
#' Descriptive_stats(task, group_by_cols = "sex")
#' }
#'
#' @export
Descriptive_stats <- function(task, cols = NULL, group_by_cols = NULL) {
  # Extract data from the task
  row_ids = task$row_ids
  
  task_data <- task$data()

  # Select specified columns or default to all numeric columns
  if (is.null(cols)) {
    data_to_summarize <- task_data %>% select(where(is.numeric))
  } else {
    data_to_summarize <- task_data %>% select(all_of(cols))
  }
  
  # If group_by_cols is specified, add those columns to the data_to_summarize
  if (!is.null(group_by_cols)) {
    data_to_summarize <- task_data %>%
      select(all_of(c(group_by_cols, names(data_to_summarize))))
  }

  # Calculate the descriptive statistics with optional grouping
  descriptive_stats <- data_to_summarize %>%
    { if (!is.null(group_by_cols)) group_by(., across(all_of(group_by_cols))) else . } %>%
    summarise(across(everything(), list(
      n = ~ sum(!is.na(.)),
      missing = ~ sum(is.na(.)),
      mean = ~ mean(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE),
      min = ~ min(., na.rm = TRUE),
      q1 = ~ quantile(., 0.25, na.rm = TRUE),
      median = ~ median(., na.rm = TRUE),
      q3 = ~ quantile(., 0.75, na.rm = TRUE),
      max = ~ max(., na.rm = TRUE)
    ), .names = "{.col}_{.fn}"), .groups = "drop") %>% 
    pivot_longer(
      cols = -all_of(group_by_cols), 
      names_to = c("variable", "statistic"), 
      names_pattern = "(.*)_(.*)") %>%
    pivot_wider(names_from = statistic, values_from = value)
  
  return(descriptive_stats)
}
  
#  Descriptive_stats(task)


