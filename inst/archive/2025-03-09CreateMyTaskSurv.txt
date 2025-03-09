#' Create a Survival Task for mlr3
#'
#' Creates a survival task (`TaskSurv`) for use with the `mlr3` framework, supporting both right-censored
#' and multi-state survival analysis. Applies filtering, time cutoffs, and role assignments based on
#' provided configurations.
#'
#' @param data A data frame containing survival data.
#' @param target_info A named character vector with keys:
#'   - `id`: String identifier for the target vars (e.g., "test1").
#'   - `time`: Name of the survival time column in `data`.
#'   - `event`: Name of the event status column in `data`.
#'   - `type`: Survival type, either `"right"` (right-censored) or `"mstate"` (multi-state).
#' @param backend_info A list with backend configuration:
#'   - `id`: String identifier for the backend dataset (e.g., "ovarian_test").
#'   - `primary_key`: Column name for unique numeric identifiers in `data`.
#'   - `feature_cols`: Vector of column names to use as predictor features.
#'   - `filter`: Optional string condition to subset `data` (e.g., "age <= 65").
#'   - `time_cutoff`: Optional numeric value to truncate survival times.
#'   - `add_to_strata_cols`: Optional column name for stratification in `mlr3`.
#'   - `weight_col`: Optional column name for weights in `data`.
#' @param traceon Logical, if `TRUE` enables debug tracing messages. Default is `FALSE`.
#'
#' @return An object of class `mlr3proba::TaskSurv` configured with the specified parameters.
#'
#' @details
#' This function processes survival data by applying filters and time cutoffs (via `apply_time_cutoff`),
#' then constructs an `mlr3` survival task with appropriate roles for features, weights, and strata.
#' The `primary_key` must be numeric. The resulting task label includes dataset, type, filter, and
#' cutoff information for clarity.
#'
#' @examples
#' library(mlr3)
#' library(mlr3proba)
#' library(survival)
#' library(data.table)
#' 
#' # Prepare data
#' ovarian_dt <- as.data.table(ovarian)
#' ovarian_dt[, id := .I]
#' ovarian_dt[, weights := runif(.N)]
#'
#' # Define configurations
#' target_info <- c(id = "test1", time = "futime", event = "fustat", type = "right")
#' backend_info <- list(
#'   id = "ovarian_test",
#'   primary_key = "id",
#'   feature_cols = c("age", "resid.ds", "ecog.ps"),
#'   filter = "age <= 65",
#'   time_cutoff = 500,
#'   add_to_strata_cols = "rx",
#'   weight_col = "weights"
#' )
#'
#' # Create task
#' task <- CreateMyTaskSurv(ovarian_dt, target_info, backend_info)
#' print(task)
#'
#' @importFrom data.table as.data.table
#' @importFrom mlr3 DataBackendDataTable
#' @importFrom mlr3proba TaskSurv
#' @export
CreateMyTaskSurv <- function(data, target_info, backend_info, traceon = FALSE) {
  
  # Embedded tracing function
  traceit <- function(msg, variable = NULL) {
    if (isTRUE(traceon)) {
      cat(msg, "\n")
      if (!is.null(variable)) {
        print(variable)
      }
    }
  }
  
  # Validate input parameters
  traceit("Validating input parameters")
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!is.character(target_info) || !all(c("id", "time", "event", "type") %in% names(target_info))) {
    stop("target_info must be a named character vector with id, time, event, and type")
  }
  if (!is.list(backend_info) || !all(c("id", "primary_key", "feature_cols") %in% names(backend_info))) {
    stop("backend_info must be a list with required fields: id, primary_key, feature_cols")
  }
  
  # Convert to data.table
  traceit("Converting data to data.table")
  data <- data.table::as.data.table(data)
  traceit("Data after conversion", head(data))
  
  # Extract parameters
  traceit("Extracting parameters")
  task_id <- target_info["id"]
  time <- target_info["time"]
  event <- target_info["event"]
  type <- target_info["type"]
  primary_key <- backend_info[["primary_key"]]
  
  # Create informative label with non-redundant info
  label_parts <- c(
    paste("Dataset:", backend_info$id),
    paste("Task type:", type)
  )
  if (!is.null(backend_info$filter)) {
    label_parts <- c(label_parts, paste("Filter:", backend_info$filter))
  }
  if (!is.null(backend_info$time_cutoff)) {
    label_parts <- c(label_parts, paste("Time cutoff:", backend_info$time_cutoff))
  }
  task_label <- paste(label_parts, collapse = "; ")
  
  traceit("Task parameters", list(task_id = task_id, time = time, event = event, 
                                 type = type, primary_key = primary_key, label = task_label))
  
  # Verify primary_key exists and is numeric
  traceit("Verifying primary key")
  if (!primary_key %in% names(data)) {
    stop(sprintf("Primary key '%s' not found in data", primary_key))
  }
  if (!is.numeric(data[[primary_key]])) {
    stop(sprintf("Primary key '%s' must be numeric", primary_key))
  }
  
  # S1: Apply filter if provided
  traceit("Applying filter")
  subset_df <- data
  if (!is.null(backend_info$filter)) {
    subset_df <- subset(data, eval(parse(text = backend_info$filter)))
    traceit("Data after filtering", head(subset_df))
  }
  
  # S2: Apply time cutoff
  traceit("Applying time cutoff")
  if (!is.null(backend_info$time_cutoff)) {
    subset_df <- apply_time_cutoff(
      data = subset_df,
      target_info = target_info,
      id = primary_key,
      time_cutoff = backend_info$time_cutoff,
      traceon = traceon
    )
    traceit("Data after time cutoff", head(subset_df))
  }
  
  # S3: Handle weights if provided
  traceit("Handling weights")
  if (!is.null(backend_info$weight_col)) {
    weight_col_name <- backend_info$weight_col
    if (weight_col_name %in% names(subset_df)) {
      # Remove rows where weight is NA
      subset_df <- subset_df[!is.na(subset_df[[weight_col_name]]), ]
      traceit("Data after weight NA removal", head(subset_df))
    }
  }
  
  # S4: Configure backend
  traceit("Configuring backend")
  backend <- mlr3::DataBackendDataTable$new(
    data = subset_df,
    primary_key = primary_key
  )
  traceit("Backend created", backend)
  
  # S5: Create the survival task with label
  traceit("Creating survival task")
  task <- mlr3proba::TaskSurv$new(
    id = task_id,
    backend = backend,
    time = time,
    event = event,
    type = type,
    label = task_label
  )
  
  # Set weights role if provided
  traceit("Setting weights role")
  if (!is.null(backend_info$weight_col) && backend_info$weight_col %in% names(subset_df)) {
    task$col_roles$weight <- backend_info$weight_col
    traceit("Weights role set", task$col_roles$weight)
  }
  
  # Add stratification column if provided
  traceit("Adding stratification columns")
  if (!is.null(backend_info$add_to_strata_cols)) {
    task$col_roles$stratum <- backend_info$add_to_strata_cols
    traceit("Stratum columns added", task$col_roles$stratum)
  }
  
  # Explicitly set features to match feature_cols
  traceit("Setting feature columns")
  task$col_roles$feature <- backend_info$feature_cols
  traceit("Feature columns set", task$col_roles$feature)
  
  traceit("Task creation complete", task)
  return(task)
}
