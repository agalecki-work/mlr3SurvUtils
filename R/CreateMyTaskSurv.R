#' Create a Survival Task for mlr3
#'
#' Creates a survival task (`TaskSurv`) for use with the `mlr3` framework, supporting both right-censored
#' and multi-state survival analysis. Applies filtering, time cutoffs, case-cohort study configurations,
#' and role assignments based on provided configurations.
#'
#' @param data A data frame containing survival data.
#' @param target_info A named character vector with keys:
#'   - `id`: String identifier for the target vars (e.g., "tvars").
#'   - `time`: Name of the survival time column in `data`.
#'   - `event`: Name of the event status column in `data`. For `type = "right"`, must be numeric or
#'     logical with values 0 (censored) and 1 (event). For `type = "mstate"`, must be a factor with
#'     levels including 0 (censored) and at least 1 (main event), commonly 2 (competing event) for
#'     competing risks; additional levels may be supported depending on the learner.
#'   - `type`: Survival type, either `"right"` (right-censored) or `"mstate"` (multi-state).
#' @param backend_info A list with backend configuration:
#'   - `id`: String identifier for the backend dataset (e.g., "ovarian_test").
#'   - `primary_key`: Column name for unique numeric identifiers in `data`.
#'   - `feature_cols`: Vector of column names to use as predictor features.
#'   - `filter`: Optional string condition to subset `data` (e.g., "age <= 65").
#'   - `time_cutoff`: Optional numeric value to truncate survival times.
#'   - `add_to_strata_cols`: Optional column name for stratification in `mlr3`.
#'   - `weight_col`: Optional column name for weights in `data`.
#' @param cch Optional list for case-cohort study configuration:
#'   - `subcohort`: String specifying the subcohort column (1 = yes, 0 = no).
#'   - `cch_weight`: Weighting method: "Borgan1", "Borgan2", or "SelfPrentice". Default is "Borgan1".
#'   - `sampling_fraction`: Proportion of cohort sampled (0 < x <= 1). Default is 0.15.
#'   - `subcohort_only`: Logical, if TRUE, restricts to subcohort rows only. Default is FALSE.
#' @param traceon Logical, if `TRUE` enables debug tracing messages. Default is `FALSE`.
#'
#' @return An object of class `mlr3proba::TaskSurv` configured with the specified parameters.
#'
#' @details
#' This function processes survival data by applying filters, time cutoffs, and case-cohort study
#' configurations. For case-cohort studies (`cch` provided):
#' - **Option #1 (all data included)**: Applies weights via `case_cohort_weights` and assigns the event
#'   column a dual role as both event status and a stratum role in the `mlr3` task.
#' - **Option #2 (subcohort data only)**: Restricts to subcohort rows only and uses weights from
#'   `backend_info$weight_col` if provided.
#' The task label reflects these configurations.
#'
#' @examples
#' library(mlr3)
#' library(mlr3proba)
#' library(data.table)
#' 
#' # Example 1: Right-censored data
#' test_dt_right <- data.table(
#'   id = 1:6,
#'   futime = c(300, 600, 400, 700, 200, 800),
#'   fustat = c(0, 1, 1, 1, 0, 0),
#'   subcoh = c(1, 0, 1, 0, 1, 0),
#'   age = c(50, 60, 55, 65, 45, 70)
#' )
#' target_info_right <- c(id = "tvars", time = "futime", event = "fustat", type = "right")
#' backend_info_right <- list(
#'   id = "test_data",
#'   primary_key = "id",
#'   feature_cols = c("age")
#' )
#' cch_config_right <- list(
#'   subcohort = "subcoh",
#'   cch_weight = "Borgan1",
#'   sampling_fraction = 0.15,
#'   subcohort_only = FALSE
#' )
#' task_right <- CreateMyTaskSurv(
#'   test_dt_right, 
#'   target_info_right, 
#'   backend_info_right, 
#'   cch = cch_config_right
#' )
#' print(task_right)
#' 
#' # Example 2: Multi-state data
#' test_dt_mstate <- data.table(
#'   id = 1:6,
#'   survtime = c(300, 600, 400, 700, 200, 800),
#'   status = factor(c(0, 1, 2, 1, 0, 2), levels = c("0", "1", "2")),
#'   subcoh = c(1, 0, 1, 0, 1, 0),
#'   age = c(50, 60, 55, 65, 45, 70)
#' )
#' target_info_mstate <- c(id = "tvars_mstate", time = "survtime", event = "status", type = "mstate")
#' backend_info_mstate <- list(
#'   id = "mstate_data",
#'   primary_key = "id",
#'   feature_cols = c("age")
#' )
#' cch_config_mstate <- list(
#'   subcohort = "subcoh",
#'   cch_weight = "Borgan2",
#'   sampling_fraction = 0.15,
#'   subcohort_only = FALSE
#' )
#' task_mstate <- CreateMyTaskSurv(
#'   test_dt_mstate, 
#'   target_info_mstate, 
#'   backend_info_mstate, 
#'   cch = cch_config_mstate
#' )
#' print(task_mstate)
#'
#' @importFrom data.table as.data.table
#' @importFrom mlr3 DataBackendDataTable
#' @importFrom mlr3proba TaskSurv
#' @export
CreateMyTaskSurv <- function(data, target_info, backend_info, cch = NULL, traceon = FALSE) {
  # [Function body remains unchanged]
  traceit <- function(msg, variable = NULL) {
    if (isTRUE(traceon)) {
      cat(msg, "\n")
      if (!is.null(variable)) {
        print(variable)
      }
    }
  }
  
  traceit("Validating input parameters")
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!is.character(target_info) || !all(c("id", "time", "event", "type") %in% names(target_info))) {
    stop("target_info must be a named character vector with id, time, event, and type")
  }
  if (!is.list(backend_info) || !all(c("id", "primary_key", "feature_cols") %in% names(backend_info))) {
    stop("backend_info must be a list with required fields: id, primary_key, feature_cols")
  }
  if (!is.null(cch)) {
    if (!is.list(cch) || !all(c("subcohort", "cch_weight", "sampling_fraction", "subcohort_only") %in% names(cch))) {
      stop("cch must be a list with subcohort, cch_weight, sampling_fraction, and subcohort_only")
    }
    if (!cch$cch_weight %in% c("Borgan1", "Borgan2", "SelfPrentice")) {
      stop("cch$cch_weight must be 'Borgan1', 'Borgan2', or 'SelfPrentice'")
    }
    if (!is.numeric(cch$sampling_fraction) || cch$sampling_fraction <= 0 || cch$sampling_fraction > 1) {
      stop("cch$sampling_fraction must be between 0 and 1")
    }
  }
  
  traceit("Converting data to data.table")
  data <- data.table::as.data.table(data)
  traceit("Data after conversion", head(data))
  
  traceit("Extracting parameters")
  task_id <- target_info["id"]
  time <- target_info["time"]
  event <- target_info["event"]
  type <- target_info["type"]
  primary_key <- backend_info[["primary_key"]]
  
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
  if (!is.null(cch)) {
    if (isTRUE(cch$subcohort_only)) {
      label_parts <- c(label_parts, "CCH: Subcohort only")
    } else {
      label_parts <- c(label_parts, paste("CCH weights:", cch$cch_weight))
      label_parts <- c(label_parts, paste("Sampling fraction:", cch$sampling_fraction))
    }
  }
  task_label <- paste(label_parts, collapse = "; ")
  
  traceit("Task parameters", list(task_id = task_id, time = time, event = event, 
                                 type = type, primary_key = primary_key, label = task_label))
  
  traceit("Verifying primary key")
  if (!primary_key %in% names(data)) {
    stop(sprintf("Primary key '%s' not found in data", primary_key))
  }
  if (!is.numeric(data[[primary_key]])) {
    stop(sprintf("Primary key '%s' must be numeric", primary_key))
  }
  
  traceit("Applying filter")
  subset_df <- data
  if (!is.null(backend_info$filter)) {
    subset_df <- subset(data, eval(parse(text = backend_info$filter)))
    traceit("Data after filtering", head(subset_df))
  }
  if (!is.null(cch) && isTRUE(cch$subcohort_only)) {
    if (!cch$subcohort %in% names(subset_df)) {
      stop(sprintf("Subcohort column '%s' not found in data", cch$subcohort))
    }
    subset_df <- subset(subset_df, subset_df[[cch$subcohort]] == 1)
    traceit("Data after subcohort_only filter", head(subset_df))
  }
  
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
  
  traceit("Handling weights")
  if (!is.null(cch)) {
    if (isTRUE(cch$subcohort_only)) {
      if (!is.null(backend_info$weight_col) && backend_info$weight_col %in% names(subset_df)) {
        subset_df <- subset_df[!is.na(subset_df[[backend_info$weight_col]]), ]
        traceit("Data after weight NA removal (subcohort_only)", head(subset_df))
      }
    } else {
      if (!cch$subcohort %in% names(subset_df)) {
        stop(sprintf("Subcohort column '%s' not found in data", cch$subcohort))
      }
      weights <- case_cohort_weights(
        data = subset_df,
        subcoh = cch$subcohort,
        event_or_status = event,
        sampling_fraction = cch$sampling_fraction,
        method = cch$cch_weight,
        type = type,
        censored_level = NULL
      )
      subset_df$cch_weight <- weights
      subset_df <- subset_df[!is.na(subset_df$cch_weight), ]
      backend_info$weight_col <- "cch_weight"
      traceit("Data after applying case-cohort weights", head(subset_df))
    }
  } else if (!is.null(backend_info$weight_col)) {
    weight_col_name <- backend_info$weight_col
    if (weight_col_name %in% names(subset_df)) {
      subset_df <- subset_df[!is.na(subset_df[[weight_col_name]]), ]
      traceit("Data after weight NA removal", head(subset_df))
    }
  }
  
  traceit("Configuring backend")
  backend <- mlr3::DataBackendDataTable$new(
    data = subset_df,
    primary_key = primary_key
  )
  traceit("Backend created", backend)
  
  traceit("Creating survival task")
  task <- mlr3proba::TaskSurv$new(
    id = task_id,
    backend = backend,
    time = time,
    event = event,
    type = type,
    label = task_label
  )
  
  traceit("Setting weights role")
  if (!is.null(backend_info$weight_col) && backend_info$weight_col %in% names(subset_df)) {
    task$col_roles$weight <- backend_info$weight_col
    traceit("Weights role set", task$col_roles$weight)
  }
  
  traceit("Adding stratification columns")
  if (!is.null(backend_info$add_to_strata_cols)) {
    task$col_roles$stratum <- backend_info$add_to_strata_cols
    traceit("Stratum columns added", task$col_roles$stratum)
  }
  if (!is.null(cch) && !isTRUE(cch$subcohort_only)) {
    task$col_roles$stratum <- c(task$col_roles$stratum, event)
    traceit("Event added to stratum role (Option #1)", task$col_roles$stratum)
  }
  
  traceit("Setting feature columns")
  task$col_roles$feature <- backend_info$feature_cols
  traceit("Feature columns set", task$col_roles$feature)
  
  traceit("Task creation complete", task)
  return(task)
}
