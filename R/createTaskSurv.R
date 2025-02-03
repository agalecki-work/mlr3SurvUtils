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
#' @param CCH_weight_FUN A string indicating what CCH weights functionwill be applied. Possible values: 'Borgan1', 'Borgan2', 'SelfPrentice'. By default 'Borgan2'
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
#' - **filter**: A string expressing conditions to subset the data, e.g., "data$BMI < 35".
#' - **time_cutoff**: A numeric value specifying the time cutoff for survival data.
#' - **CCH_subcohort**: The column name indicating subgroup membership when "CCH" sampling is used.
#' - **add_to_strata_cols**: Additional columns to include in the stratification process.
#' - **weight_col**: A named vector mapping sampling options to weight column names. Run `run_example('04') for illustration. 
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
#'   filter = "ovarian$age < 70",
#'   time_cutoff = 1000,
#'   add_to_strata_cols = "rx"
#' )
#' 
#' # Create task
#' task <- createTaskSurv(data = ovarian, target_info = target_info, backend_info = backend_info)
#' print(task)
#'
#' @export
createTaskSurv <- function(data, target_info, backend_info = NULL, event_strata = TRUE, CCH_weight_FUN = NULL, traceon = FALSE) {
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
  traceit("1. target_info:", target_info)
  traceit("2. backend_info:", backend_info)

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
  CCH_weight_FUN <- CCH_weight_FUN %||% 'Borgan2'
  weight_col <- backend_info$weight_col[option]
  
  traceit("3. weight_col:", weight_col)
  
 
  if (option == "SRS") CCH_subcohort <- NULL

  # Adjust strata columns for "CCH" and "CCH1" option
  
  if (option == "CCH") {
    if (is.null(CCH_subcohort)) stop("CCH_subcohort is NULL. Column name for subcohort required.")
    add_to_strata_cols <- c(add_to_strata_cols, CCH_subcohort)
  }
  
  if (option ==  "CCH1" && is.null(CCH_subcohort)) stop("CCH_subcohort is NULL. Column name for subcohort required.")
  
  traceit("backend settings:", c(id = backend_id, option = option, primary_key = primary_key))

  # Unpack `target_info`
  target_id <- target_info["id"]
  time <- target_info["time"]
  event <- target_info["event"]
  task_type <- target_info["task_type"] # surv or classif
  
  if (!task_type %in% c("surv", "classif"))  stop("Task type: `", task_type, "` is not supported")
  
   # Apply filter if present(`subset_df` initiated)
   traceit("3.1 `filter: ", filter)
   traceit("3.3 `data` before filter: ", str(data))
   subset_df <-  if (!is.null(filter)) data[eval(parse(text = filter)), ] else data
   if (!is.null(filter)) traceit("3.5. `subset_df` filter applied:", str(subset_df))
    

  # Apply time cutoff filtering 
  subset_df <- apply_time_cutoff(subset_df, target_info, id = NULL, time_cutoff = time_cutoff, traceon = FALSE)
  traceit("4. `subset_df` after executing `apply_time_cutoff()`:", str(subset_df))
  
  # Rows with valid weight selected 
   if (option == "CCH") weight_col0 = weight_col
   if (length(weight_col) > 0  && option == "CCH"){ 
      traceit("4.1. weight_col =", weight_col)
      traceit("4.2. names(subset_df) =",  names(subset_df))
      if (!weight_col %in% names(subset_df))  stop ("ERROR: weight column: `", weight_col, "` not found!") 
      subset_df <- subset_df[!is.na(subset_df[[weight_col]]), ] 
      traceit("4.9. weight column after removing NA weights:", head(subset_df[[weight_col]], n=30))
   }
  
 
  if (option == "CCH") { 
    weight_col <- if  (length(weight_col0)> 0) paste0(weight_col0, "_",CCH_weight_FUN) else CCH_weight_FUN
    CCH_wght <- switch(CCH_weight_FUN,
       "Borgan1" = Borgan1_weights(subset_df, subcoh = CCH_subcohort, event_or_status= event,task_type=task_type),
       "Borgan2" = Borgan2_weights(subset_df, subcoh = CCH_subcohort, event_or_status= event,task_type=task_type),
       "SelfPrentice" = SelfPrentice_weights(subset_df, subcoh = CCH_subcohort, event_or_status= event,task_type=task_type)
      )
       traceit("5.1 CCH_wght freq:", table(CCH_wght, useNA="always"))
       traceit("5.2 CCH_wght x event table:", table(CCH_wght, event= subset_df[[event]], useNA="always"))
       subset_df[[weight_col]] <-if  (length(weight_col0)> 0) CCH_wght* subset_df[[weight_col0]] else CCH_wght
       subset_df <- subset_df[!is.na(subset_df[[weight_col]]), ]
 } else NULL

  # "CCH1" filters subcohort subjects only
  if (option == "CCH1") {
    subset_df <- subset_df[subset_df[[CCH_subcohort]] == 1, ]
    CCH_subcohort <- NULL
    traceit("6. subset_df subcohort subjects selected:", str(subset_df))
  }


  # Prepare columns to retain in xtra_df
  xtra_cols <- na.omit(c(CCH_subcohort, primary_key))
  traceit("-- 10. xtra_cols", xtra_cols)
  keep_cols <- unique(na.omit(c(xtra_cols, event, feature_cols, weight_col, add_to_strata_cols)))
  if (task_type == "surv") keep_cols <- c(keep_cols, time)
  if (task_type == "classif") xtra_cols <- c(xtra_cols, time)

  traceit("-- 10.5 xtra_cols", xtra_cols)
  subset_df <- as.data.table(subset_df)
  traceit("-- 10.7 str(subset_df)", str(subset_df))
  xtra_df   <- subset_df[, ..xtra_cols]
  traceit("-- 11. xtra_df", str(xtra_df))
  eventx <- as.numeric(subset_df[[event]]) - 1
  aux_df = data.frame(event_temp = eventx)
  evnt_nm = paste0(event, "_num") 
  colnames(aux_df) <- evnt_nm
  traceit("-- 12. aux_df", str(aux_df))

  if (task_type == "classif") xtra_df <- cbind(xtra_df, aux_df) 
  traceit("-- 13. xtra_df", str(xtra_df))

  subset_df <- subset_df[, ..keep_cols]
  traceit("14. Final subset columns:", keep_cols)
  subset_df <- as.data.table(subset_df)
  traceit("-- 15. subset df:", str(subset_df))

  # Configure backend and task
  backend <- mlr3::DataBackendDataTable$new(subset_df, primary_key = primary_key)
  traceit("-- 16. backend created", class(backend))

  task_id <- paste0(option, ".", backend_id, ":", target_id)

  #------ Create the task
  task <- switch(
    task_type,
    "surv" = mlr3proba::TaskSurv$new(id = task_id, time = time, event = event, backend = backend, type = "right"),
    "classif" = mlr3::TaskClassif$new(id = task_id, backend = backend, target = event)
  )
    traceit("-- 16.1. task created", task)

    if (!is.null(weight_col)){
     # weight_col = unname(weight_col)
     traceit("16.5 weight_col:", weight_col)
     task$set_col_roles(cols= weight_col, roles = "weight")
     } 
  
  traceit("-- 17. task created", task)

  # Define roles for columns
  if (event_strata) add_to_strata_cols <- c(add_to_strata_cols, event)
  target_cols = if (task_type == "surv") target_info[c("time", "event")] else target_info["event"]
  traceit("18. target cols:", target_cols)

  roles_list <- list(
    target_cols = target_cols,
   # weight_col = weight_col,
    feature_cols = feature_cols,
    stratum_cols = add_to_strata_cols
  )
  traceit("18. Column roles:", roles_list)

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

  #if (length(roles_list$weight_col) > 0) {
  #  for (col in roles_list$weight_col) {
  #    column_roles[[col]] <- union(column_roles[[col]], "weight")
  #  }
  #}

  if (length(roles_list$stratum_cols) > 0) {
    for (col in roles_list$stratum_cols) {
      column_roles[[col]] <- union(column_roles[[col]], "stratum")
    }
  }

  
  traceit("20. Column roles before assignment:", roles_list)

  # Apply the roles
  if (length(names(column_roles)) > 0) {
    for (col in names(column_roles)) {
      traceit("--- 20.1 col =", col)
      traceit("20.2 col, role",  c(col,"=> ", column_roles[[col]]))
      task$set_col_roles(col, role = column_roles[[col]])
    }
  }
  traceit("21. task after applying column_roles", task)
  
  traceit("22. xtra_cols :", xtra_cols)
  traceit("23. xtra_df", str(xtra_df))

  if (length(xtra_cols)> 1 ) task$extra_args <- c(task$extra_args, list(extra_df = xtra_df))
  traceit("24. after adding xtra_df")
   

  # Label the task
  lblx <- c(
    if (!is.null(time_cutoff)) paste0("Time_cutoff =", time_cutoff),
    if (!is.null(filter)) paste0("filter = ", filter),
    if (option == "CCH1") "CCH1: subcohort only",
    if (option == "CCH")  "CCH: non-cases outside subcohort excluded"
    if (length(xtra_cols)> 1 ) {
        nms = paste(colnames(xtra_df), collapse = ", ")
        paste0( "extra_df columns: ", nms) 
    }
  )
  task$label <- paste(lblx, collapse = ", ")

  # Finish
  traceit("--- Task creation using `createTaskSurv()` finished")
  return(task)
}



