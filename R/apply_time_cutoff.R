#' Apply a Time Cutoff to Survival Data
#'
#' Truncates survival data at a specified time cutoff, modifying the time and event variables as defined
#' in a `target_info` configuration. Supports right-censored and multi-state survival data.
#'
#' @param data A data frame containing survival data.
#' @param target_info A named character vector with keys:
#'   - `id`: A string identifier for this `target_info` configuration (e.g., "config1").
#'   - `time`: Name of the survival time column in `data`.
#'   - `event`: Name of the event status column in `data`.
#'   - `type`: Survival type, either `"right"` (right-censored survival) or `"mstate"` (multi-state survival).
#' @param id Character. Optional column name for individual identifiers (e.g., patient IDs) in `data`. If `NULL`, no ID is added. Default is `NULL`.
#' @param time_cutoff Numeric. Optional time at which to truncate survival times. If `NULL`, returns `data` unchanged. Must be positive if specified.
#' @param traceon Logical. If `TRUE`, enables debug tracing messages. Default is `FALSE`.
#'
#' @return A `data.table` with the specified time and event variables truncated at `time_cutoff` and updated accordingly.
#'
#' @details
#' This function truncates survival data at `time_cutoff`, adjusting time and event status. The
#' `target_info` vector defines the configuration for truncation, with `id` serving as a unique identifier
#' for the set of `time`, `event`, and `type` values. For `type = "right"`, it handles right-censored data,
#' setting events beyond the cutoff to censored (0). For `type = "mstate"`, it supports multi-state data,
#' preserving the original event factor levels and setting events beyond the cutoff to censored (0).
#'
#' @examples
#' # Right-censored survival example
#' library(survival)
#' library(data.table)
#' lung$status_01 <- lung$status - 1  # Recode event to 0/1
#' lung$status <- NULL
#' target_info <- c(id = "lung_config", time = "time", event = "status_01", type = "right")
#' lungx <- apply_time_cutoff(lung, target_info, time_cutoff = 365)
#' head(lungx)
#'
#' # Multi-state survival example with individual IDs
#' library(survival)
#' library(data.table)
#' mgus2$etime <- with(mgus2, ifelse(pstat == 0, futime, ptime))
#' mgus2$event <- factor(with(mgus2, ifelse(pstat == 0, 2 * death, 1)), 0:2, 
#'                       labels = c("censor", "pcm", "death"))
#' target_info <- c(id = "mgus2_config", time = "etime", event = "event", type = "mstate")
#' mgus2x <- apply_time_cutoff(mgus2, target_info, id = "id", time_cutoff = 500)
#' head(mgus2x)
#'
#' # Use with mlr3
#' library(mlr3)
#' library(mlr3proba)
#' task <- TaskSurv$new(target_info["id"], 
#'               backend = mgus2x, time = "etime", event = "event", type = "mstate")
#'
#' @note Requires `survival` and `data.table` packages. Use `traceon = TRUE` for debugging.
#' Fixed issue with multi-state event handling to preserve original factor levels correctly.
#'
#' @importFrom survival Surv
#' @importFrom data.table as.data.table
#' @importFrom utils head str
#' @importFrom stats na.omit
#' @export
apply_time_cutoff <- function(data, target_info, id = NULL, time_cutoff = NULL, traceon = FALSE) {
  # Validate inputs
  if (!is.data.frame(data)) stop("`data` must be a data frame")
  required_targets <- c("id", "time", "event")
  if (!is.character(target_info) || !all(required_targets %in% names(target_info))) {
    stop("`target_info` must be a named character vector with 'id', 'time', and 'event'")
  }
  if (!is.null(time_cutoff) && (!is.numeric(time_cutoff) || time_cutoff <= 0)) {
    stop("`time_cutoff` must be a positive numeric value")
  }

  # Debug tracing function
  traceit <- function(msg, var = NULL) {
    if (traceon) {
      cat(msg, "\n")
      if (!is.null(var)) print(var)
    }
  }

  # Return original data if no cutoff
  if (is.null(time_cutoff)) {
    traceit("No time cutoff specified; returning original data")
    return(as.data.table(data))
  }

  # Extract target_info components
  config_id <- target_info["id"]
  time_col <- target_info["time"]
  event_col <- target_info["event"]
  surv_type <- target_info["type"] %||% "right"

  traceit("Config ID:", config_id)
  traceit("Time column:", time_col)
  traceit("Event column:", event_col)
  traceit("Survival type:", surv_type)

  # Validate survival type
  if (!surv_type %in% c("right", "mstate")) {
    stop("`type` must be 'right' or 'mstate'; got: ", surv_type)
  }

  # Check column existence
  missing_cols <- setdiff(c(time_col, event_col), names(data))
  if (length(missing_cols) > 0) {
    stop("Target columns not found in `data` for config id '", config_id, "': ", paste(missing_cols, collapse = ", "))
  }
  traceit("Original data columns:", names(data))
  traceit("Original data head:", head(data))

  # Prepare data
  dtout <- as.data.table(data)
  id_col <- id
  if (!is.null(id_col)) {
    if (!id_col %in% names(dtout)) stop("Individual ID column '", id_col, "' not found in `data`")
    traceit("Using individual ID column:", id_col)
  }

  # Create survival object (for validation, not truncation)
  traceit("Creating survival object for config id:", config_id)
  surv_obj <- Surv(dtout[[time_col]], dtout[[event_col]], type = surv_type)
  traceit("Survival object summary:", str(surv_obj))

  # Manually truncate survival times
  traceit("Manually truncating at cutoff:", time_cutoff)
  original_time <- dtout[[time_col]]
  dtout[[time_col]] <- pmin(original_time, time_cutoff)
  if (surv_type == "right") {
    dtout[[event_col]] <- ifelse(original_time > time_cutoff, 0, dtout[[event_col]])
  } else if (surv_type == "mstate") {
    original_levels <- levels(dtout[[event_col]])
    # Convert factor to numeric (0-based), apply truncation, then back to factor
    event_numeric <- as.numeric(dtout[[event_col]]) - 1  # Convert factor levels to 0,1,2
    dtout[[event_col]] <- ifelse(original_time > time_cutoff, 
                                0,  # censoring code
                                event_numeric)
    dtout[[event_col]] <- factor(dtout[[event_col]], 
                                levels = 0:(length(original_levels) - 1), 
                                labels = original_levels)
  }
  traceit("Truncated time variable (first 20):", head(dtout[[time_col]], 20))
  traceit("Truncated event variable (first 20):", head(dtout[[event_col]], 20))

  traceit("Final truncated data structure for config id:", config_id)
  traceit("Structure:", str(dtout))
  return(dtout)
}

