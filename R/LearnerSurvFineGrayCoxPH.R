#' @title Fine-Gray Competing Risks Model with Cox Proportional Hazards
#'
#' @description
#' A learner for fitting a Fine-Gray competing risks model using Cox proportional hazards.
#' This model estimates the subdistribution hazard for a specified event type (e.g., "death")
#' in the presence of competing events (e.g., "relapse"), incorporating weights if provided.
#'
#' @section Usage:
#' ```
#' learner <- LearnerSurvFineGrayCoxPH$new()
#' ```
#'
#' @section Parameters:
#' - `ties`: Character, method for handling ties in the Cox model. Options are "efron" (default),
#'   "breslow", or "exact".
#' - `iter.max`: Integer, maximum number of iterations for the Cox model fit (default: 100,
#'   range: 1-1000).
#'
#' @section Predict Types:
#' - `crank`: Continuous ranking (linear predictor).
#' - `lp`: Linear predictor.
#' - `distr`: Survival distribution (as a matrix of survival probabilities).
#'
#' @section Properties:
#' - Supports weights via the `wts` column in the task.
#'
#' @section Methods:
#' - `new()`: Initialize a new instance of the learner.
#' - `train(task)`: Train the model on a survival task.
#' - `predict(task)`: Predict on new data from a trained model.
#'
#' @note
#' This learner relies on `distr6` for distribution handling and `purrr` for functional programming utilities,
#' which are used indirectly via `mlr3proba` dependencies.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' data <- data.frame(
#'   time_to_event = c(5, 10, 15, 20, 25, 7, 12, 18, 22, 30,
#'                     8, 14, 19, 23, 28, 6, 11, 16, 21, 26),
#'   outcome = c(1, 2, 0, 1, 0, 1, 1, 2, 0, 1,
#'               2, 0, 1, 2, 0, 1, 0, 2, 1, 0),
#'   x1 = c(1, 2, 3, 4, 5, 1.5, 2.5, 3.5, 4.5, 5.5,
#'          2, 3, 4, 5, 6, 1.2, 2.2, 3.2, 4.2, 5.2),
#'   x2 = c(0.5, 1, 1.5, 2, 2.5, 0.7, 1.2, 1.8, 2.3, 2.8,
#'          0.9, 1.4, 1.9, 2.4, 2.9, 0.6, 1.1, 1.6, 2.1, 2.6),
#'   wts = c(1.0, 1.5, 0.8, 1.2, 1.3, 0.9, 1.1, 1.4, 0.7, 1.6,
#'           1.0, 1.2, 0.9, 1.3, 1.1, 0.8, 1.4, 1.0, 1.2, 0.9)
#' )
#' data$outcome <- factor(data$outcome, levels = c(0, 1, 2),
#'                        labels = c("censored", "death", "relapse"))
#' data$group <- factor(rep(c("A", "B"), length.out = nrow(data)))
#' 
#' task <- TaskSurv$new(
#'   id = "fg_test_standalone_with_weights",
#'   backend = data,
#'   time = "time_to_event",
#'   event = "outcome",
#'   type = "mstate"
#' )
#' task$set_col_roles("wts", "weight")
#' task$set_col_roles("group", "stratum")
#' 
#' part <- partition(task, ratio = 0.7)
#' learner <- LearnerSurvFineGrayCoxPH$new()
#' p <- learner$train(task, part$train)$predict(task, part$test)
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom mlr3proba LearnerSurv TaskSurv
#' @importFrom mlr3 partition msr
#' @importFrom paradox ps p_fct p_int
#' @importFrom survival finegray coxph survfit
#' @export
LearnerSurvFineGrayCoxPH <- R6::R6Class("LearnerSurvFineGrayCoxPH",
  inherit = mlr3proba::LearnerSurv,
  public = list(
    #' @description Initialize a new Fine-Gray Cox PH learner.
    initialize = function() {
      ps <- paradox::ps(
        ties = paradox::p_fct(default = "efron", levels = c("efron", "breslow", "exact"), 
                              tags = "train"),
        iter.max = paradox::p_int(default = 100L, lower = 1L, upper = 1000L, 
                                  tags = "train")
      )
      
      super$initialize(
        id = "surv.finegray_coxph",
        param_set = ps,
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = c("crank", "lp", "distr"),
        properties = "weights",
        packages = c("survival", "distr6", "purrr"),
        label = "Fine-Gray Competing Risks Model with CoxPH"
      )
    }
  ),
  
  private = list(
    basehaz = NULL,
    
    # Train the Fine-Gray model on a survival task (private method).
    # @param task A survival task object with time, event, and feature columns.
    # @return The fitted Cox model.
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      
      row_ids <- task$row_ids
      full_data <- as.data.frame(task$backend$data(rows = 1:20, 
                                                  cols = c(task$target_names, task$feature_names)))
      features <- task$feature_names
      if (length(features) == 0) stop("No features provided!")
      
      time_col <- task$target_names[1]
      event_col <- task$target_names[2]
      event_levels <- task$levels()[[event_col]]
      
      full_data$id <- seq_len(nrow(full_data))
      data <- full_data[row_ids, ]
      
      form <- as.formula(paste("Surv(", time_col, ",", event_col, ") ~", 
                               paste(c(features, "id"), collapse = " + ")))
      if (length(event_levels) < 3) {
        stop("Event status must have at least 3 levels (censored, main event, competing risk)")
      }
      target_event <- event_levels[2]  # "death"
      
      fg_data <- survival::finegray(form, data = data, etype = target_event)
      
      if ("weights" %in% task$properties) {
        weights_data <- task$weights
        if (is.null(weights_data) || !"weight" %in% names(weights_data)) {
          stop("No weights defined in task")
        }
        all_weights <- weights_data$weight
        all_row_ids <- weights_data$row_id
        weight_map <- setNames(all_weights, all_row_ids)
        original_weights <- weight_map[as.character(row_ids)]
        matched_indices <- match(fg_data$id, as.integer(names(weight_map)))
        fg_data$fgwt <- original_weights[matched_indices]
      }
      
      cox_formula <- as.formula(paste("Surv(fgstart, fgstop, fgstatus) ~", 
                                      paste(features, collapse = " + ")))
      model <- do.call(survival::coxph, 
                       args = c(list(formula = cox_formula, 
                                     data = fg_data, 
                                     weights = fg_data$fgwt), 
                                pv))
      
      baseline_data <- as.data.frame(matrix(0, nrow = 1, ncol = length(features)))
      colnames(baseline_data) <- features
      basehaz <- survival::survfit(model, newdata = baseline_data)
      private$basehaz <- list(time = basehaz$time, cumhaz = basehaz$cumhaz)
      
      return(model)
    },
    
    # Predict survival outcomes on new data (private method).
    # @param task A survival task object with new data to predict on.
    # @return A list with crank, lp, and distr predictions.
    .predict = function(task) {
      newdata <- as.data.frame(task$data(rows = task$row_ids, cols = task$feature_names))
      
      lp <- predict(self$model, newdata = newdata, type = "lp")
      
      cif <- matrix(NA, nrow = nrow(newdata), ncol = length(private$basehaz$time))
      for (i in seq_along(lp)) {
        cif[i, ] <- 1 - exp(-private$basehaz$cumhaz * exp(lp[i]))
      }
      
      time_order <- order(private$basehaz$time)
      surv <- 1 - cif[, time_order, drop = FALSE]
      colnames(surv) <- private$basehaz$time[time_order]
      
      list(
        crank = lp,
        lp = lp,
        distr = surv
      )
    }
  )
)