#' Create a Survival or Classification Task
#'
#' This function creates a survival analysis task using the mlr3proba framework, which can
#' be configured for various sampling options and feature specifications.
#' It allows for the inclusion of additional strata or subcohorts according to the provided backend configuration.
#'
#' @param data A `data.table` or `data.frame` containing the dataset.
#' @param target_info A named character vector with keys "id", "time", "event", and "task_type",
#'   giving information about the target variable, the time variable, and the type of the task.
#' @param backend_info A list containing configuration options for the backend, including:
#'   `id`, `option`, `primary_key`, `feature_cols`, `filter`, `time_cutoff`, 
#'   `CCH_subcohort`, `add_to_strata_cols`, and `weight`.
#' @param event_strata A logical value indicating whether the `event` specified in `target_info` should be included in strata (default is `TRUE`).
#' @param traceon A logical value indicating whether to print tracing information for debugging (default is `FALSE`).
#'
#' @details
#' This function supports various sampling options, including simple random sampling (SRS) 
#' and case-cohort (CCH) design. Depending on the specified `option`, different metadata 
#' columns may be included as stratification factors to better support analytical objectives.
#' 
#' - **SRS**: If selected, it will use simple random sampling for the entire cohort.
#' - **CCH**: In this mode, event-related strata will include subcohort indicators to refine the population under consideration.
#' - **CCH1**: Select subcohort subjects only, filter those with subcohort membership confirmed.
#'
#' The `target_info` parameter provides essential metadata for defining the task's survival outcome. 
#' Each element should correspond to:
#' - **id**: A string identifier for the task target. 
#' - **time**: The column name in the dataset representing the time-to-event or censoring.
#' - **event**: The column that indicates if the event of interest has occurred.
#' - **task_type**: The type of task, such as "surv" for survival data, or "classif" for classification problem.
#'
#' The `backend_info` parameter is a configuration list that dictates how the data is processed and includes:
#' - **id**: A string identifier for the backend task.
#' - **option**: Specifies the sampling method to use, such as "SRS" (Simple Random Sampling) or "CCH" (Case-Cohort). Option "CCH1" considers subcohort data only.
#' - **primary_key**: The column name used as the unique identifier for the data rows. Column has to be numeric.
#' - **feature_cols**: A vector of column names to be used as predictor features.
#' - **filter**: A string expressing conditions to subset the data, e.g., "BMI < 35".
#' - **time_cutoff**: A numeric value specifying the time cutoff for survival data.
#' - **CCH_subcohort**: The column name indicating subgroup membership when "CCH" sampling is used.
#' - **add_to_strata_cols**: Additional columns to include in the stratification process.
#' - **weight**: A named vector mapping sampling options to weight column names.
#'
#'
#' @return An object of class `TaskSurv` or `TaskClassif`, depending on the task_type specified in `target_info`.
#' This object contains the configured task suitable for analysis in the mlr3 framework.
#'
#' @examples
#' # Example setup:
#' data(cancer, package="survival")
#' ovarian$id = 1:nrow(ovarian)
#' 
#' target_info <- c(id = "tm1", time = "futime", event = "fustat", task_type = "surv")
#' backend_info <- list(
#'   id = "ovarian",
#'   option = "SRS",
#'   primary_key = "id",
#'   feature_cols = c("age","resid.ds","rx"),
#'   filter = "age < 70",
#'   time_cutoff = 1000,
#'   add_to_strata_cols = "rx"
#' )
#' 
#' # Create task
#' task <- createTaskSurv(data = ovarian, target_info = target_info, backend_info = backend_info)
#' print(task)
#'
#' @export
createTaskSurv <- function(data, target_info, backend_info = NULL, event_strata = TRUE, traceon = FALSE) {
  # Helper for default values
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  traceit <- function(msg, variable = NULL) {
    if (isTRUE(traceon)) {
      cat(msg, "\n")
      if (!is.null(variable)) {
        print(variable)
      }
    }
  }


  # Start tracing
  traceit("--- createTaskSurv STARTS")
  traceit("target_info:", target_info)
  traceit("backend_info:", backend_info)

  # Unpack `backend_info` with defaults
  backend_id <- backend_info$id
  option <- backend_info$option %||% "SRS"
  primary_key <- backend_info$primary_key
  feature_cols <- unique(backend_info$feature_cols)
  filter <- backend_info$filter
  time_cutoff <- backend_info$time_cutoff
  CCH_subcohort <- backend_info$CCH_subcohort
  add_to_strata_cols <- backend_info$add_to_strata_cols
  
  # Determine weight column based on the option
  weight_col <- backend_info$weight[option]
  ## if (is.na(weight_col)) weight_col <- NULL
  
  if (option == "SRS") CCH_subcohort <- NULL

  # Adjust strata columns for "CCH" option
  if (option == "CCH") {
    add_to_strata_cols <- c(add_to_strata_cols, CCH_subcohort)
  }

  traceit("backend settings:", c(id = backend_id, option = option, primary_key = primary_key))

  # Unpack `target_info`
  target_id <- target_info["id"]
  time <- target_info["time"]
  event <- target_info["event"]
  task_type <- target_info["task_type"] # surv or classif
  traceit("target_info:", target_info)
  
  if (!task_type %in% c("surv", "classif"))  stop("Task type: `", task_type, "` is not supported")

  # Apply time cutoff filtering (subset_df initiated)
  subset_df <- apply_time_cutoff(data, target_info, id = NULL, time_cutoff = time_cutoff, traceon = FALSE)
  
  # Rows with valid weight selected 
  if (length(weight_col)> 0){
     wghtx <- !is.na(get(weight_col))
     subset_df <- subset_df[..wghtx, ]  
  }

  # "CCH1" filters for subcohort subjects only
  if (option == "CCH1") {
    subset_df <- subset_df[subset_df[[CCH_subcohort]] == 1, ]
    CCH_subcohort <- NULL
  }

  # Apply filter if present
  if (!is.null(filter)) {
    subset_df <- subset_df[eval(parse(text = filter)), ]
  }

  # Prepare columns to retain in xtra_df
  xtra_cols <- na.omit(c(CCH_subcohort, primary_key))
  keep_cols <- unique(na.omit(c(xtra_cols, event, feature_cols, weight_col, add_to_strata_cols)))
  if (task_type == "surv") keep_cols <- c(keep_cols, time)
  if (task_type == "classif") xtra_cols <- c(xtra_cols, time)
  xtra_df <- subset_df[, ..xtra_cols]
  traceit("-- 1. xtra_df", str(xtra_df))
  eventx <- as.numeric(subset_df[[event]]) - 1
  aux_df = data.frame(event_temp = eventx)
  evnt_nm = paste0(event, "_num") 
  colnames(aux_df) <- evnt_nm
  traceit("-- 2. aux_df", str(aux_df))

  xtra_df <- cbind(xtra_df, aux_df) 
  traceit("-- 3. xtra_df", str(xtra_df))

  # 
  subset_df <- subset_df[, ..keep_cols]
  traceit("Final subset columns:", keep_cols)

  # Configure backend and task
  backend <- mlr3::DataBackendDataTable$new(subset_df, primary_key = primary_key)
  task_id <- paste0(option, ".", backend_id, ":", target_id)

  # Create the task
  task <- switch(
    task_type,
    "surv" = mlr3proba::TaskSurv$new(id = task_id, time = time, event = event, backend = backend, type = "right"),
    "classif" = mlr3::TaskClassif$new(id = task_id, backend = backend, target = event)
  )

  # Define roles for columns
  if (event_strata) add_to_strata_cols <- c(add_to_strata_cols, event)
  target_cols = if (task_type == "surv") target_info[c("time", "event")] else target_info["event"]
  traceit("target cols:", target_cols)

  roles_list <- list(
    target_cols = target_cols,
    weight_col = weight_col,
    feature_cols = feature_cols,
    stratum_cols = add_to_strata_cols
  )
  traceit("Column roles:", roles_list)

  # Assign roles to the task
  column_roles <- list()
  
  if (length(roles_list$target_cols) > 0) {
      for (col in roles_list$target_cols) {
        column_roles[[col]] <- union(column_roles[[col]], "target")
      }
    }


  if (length(roles_list$feature_cols) > 0) {
    for (col in roles_list$feature_cols) {
      column_roles[[col]] <- union(column_roles[[col]], "feature")
    }
  }

  if (length(roles_list$weight_col) > 0) {
    for (col in roles_list$weight_col) {
      column_roles[[col]] <- union(column_roles[[col]], "weight")
    }
  }

  if (length(roles_list$stratum_cols) > 0) {
    for (col in roles_list$stratum_cols) {
      column_roles[[col]] <- union(column_roles[[col]], "stratum")
    }
  }

  traceit("====3")
  traceit("Column roles before assignment:", roles_list)

  # Apply the roles
  if (length(names(column_roles)) > 0) {
    for (col in names(column_roles)) {
      task$set_col_roles(col, role = column_roles[[col]])
    }
  }

  if (length(xtra_cols)> 1 ) task$extra_args <- c(task$extra_args, list(extra_df = xtra_df))

  # Label the task with metadata
  lblx <- c(
    if (!is.null(time_cutoff)) paste0("T_cutoff =", time_cutoff),
    if (!is.null(filter)) paste0("filter=", filter)
  )
  task$label <- paste(lblx, collapse = ", ")

  # Finish
  traceit("--- Task creation finished")
  return(task)
}



