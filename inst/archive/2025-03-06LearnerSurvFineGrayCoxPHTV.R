#' @title Fine-Gray Competing Risks Model with Cox Proportional Hazards and Time-Varying Effects
#'
#' @description
#' A learner for fitting a Fine-Gray competing risks model using Cox proportional hazards,
#' with support for fixed covariates and time-varying covariate effects.
#'
#' @section Usage:
#' ```
#' learner <- LearnerSurvFineGrayCoxPHTV$new()
#' ```
#'
#' @section Parameters:
#' - `ties`: Character, method for handling ties in the Cox model. Options are "efron" (default),
#'   "breslow", or "exact".
#' - `iter.max`: Integer, maximum number of iterations for the Cox model fit (default: 200,
#'   range: 1-1000).
#' - `eps`: Numeric, convergence threshold for the Cox model (default: 1e-9, range: 1e-12 to 1e-4).
#' - `robust`: Logical, whether to compute robust variance estimates (default: FALSE).
#' - `target_event`: Event type (index or name) to model as the target; defaults to the second level if NULL.
#' - `singular.ok`: Logical, whether singular predictors are allowed (default: TRUE).
#' - `cov2_names`: Character vector, names of covariates in the task to use as time-varying covariates (default: NULL).
#' - `tf`: Function, takes a vector of times and returns a matrix of time functions for cov2 (default: identity function).
#'
#' @section Predict Types:
#' - `crank`: Continuous ranking (linear predictor).
#' - `lp`: Linear predictor.
#' - `distr`: Survival distribution (as a matrix of survival probabilities).
#'
#' @section Properties:
#' - Supports weights via the `wts` column in the task.
#'
#' @examples
#' \dontrun{
#' library(mlr3)
#' library(mlr3proba)
#' 
#' # Simulated data
#' set.seed(10)
#' ftime <- rexp(200)  # Failure times
#' fstatus <- factor(sample(0:2, 200, replace = TRUE), 
#'                   levels = 0:2, 
#'                   labels = c("censored", "event", "competing"))  # Factor status
#' cov <- matrix(runif(600), nrow = 200)  # Fixed covariates (cov1)
#' dimnames(cov)[[2]] <- c("x1", "x2", "x3")
#' cov2 <- matrix(runif(400), nrow = 200)  # Time-varying covariates (cov2)
#' dimnames(cov2)[[2]] <- c("z1", "z2")
#' 
#' # Combine into a data frame
#' data <- data.frame(time = ftime, status = fstatus, cov, cov2)
#' 
#' # Create a multi-state survival task
#' task <- TaskSurv$new("simulated", backend = data, time = "time", event = "status", type = "mstate")
#' 
#' # Initialize the learner
#' learner <- LearnerSurvFineGrayCoxPHTV$new()
#' learner$param_set$values <- list(
#'   cov2_names = c("z1", "z2"),          # Specify time-varying covariates
#'   tf = function(t) cbind(log(t + 1), t) # Time functions: log(t+1) for z1, t for z2
#' )
#' 
#' # Train the model
#' learner$train(task)
#' 
#' # Predict
#' pred <- learner$predict(task)
#' print(pred)
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom mlr3proba LearnerSurv TaskSurv
#' @importFrom paradox ps p_fct p_int p_dbl p_lgl p_uty
#' @importFrom survival finegray coxph survfit coxph.control
#' @export
LearnerSurvFineGrayCoxPHTV <- R6::R6Class("LearnerSurvFineGrayCoxPHTV",
  inherit = mlr3proba::LearnerSurv,
  public = list(
    #' @description
    #' Initialize a new Fine-Gray Cox PH learner with time-varying effects.
    initialize = function() {
      ps <- paradox::ps(
        ties = p_fct(default = "efron", levels = c("efron", "breslow", "exact"), tags = "train"),
        iter.max = p_int(default = 200L, lower = 1L, upper = 1000L, tags = "train"),
        eps = p_dbl(default = 1e-9, lower = 1e-12, upper = 1e-4, tags = "train"),
        robust = p_lgl(default = FALSE, tags = "train"),
        target_event = p_uty(default = NULL, tags = "train"),
        singular.ok = p_lgl(default = TRUE, tags = "train"),
        cov2_names = p_uty(default = NULL, tags = "train"),  # Character vector of covariate names
        tf = p_uty(default = function(t) t, tags = "train")  # Default: linear time function
      )
      
      super$initialize(
        id = "surv.finegray_coxph_tv",
        param_set = ps,
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = c("crank", "lp", "distr"),
        properties = "weights",
        packages = c("survival", "distr6", "purrr"),
        label = "Fine-Gray Competing Risks Model with CoxPH and Time-Varying Effects"
      )
    }
  ),
  private = list(
    basehaz = NULL,
    tf = NULL,  # Store the time function for prediction
    
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      
      # Validate iter.max and eps
      iter_max <- as.integer(pv$iter.max %||% 200L)
      if (is.na(iter_max) || iter_max < 1L || iter_max > 1000L) {
        stop("Invalid iter.max value: ", iter_max)
      }
      eps_val <- as.numeric(pv$eps %||% 1e-9)
      if (is.na(eps_val) || eps_val < 1e-12 || eps_val > 1e-4) {
        stop("Invalid eps value: ", eps_val)
      }
      
      # Extract data
      row_ids <- task$row_ids
      full_data <- as.data.frame(task$backend$data(rows = row_ids, 
                                                  cols = c(task$target_names, task$feature_names)))
      features <- task$feature_names
      if (length(features) == 0) stop("No features provided!")
      
      time_col <- task$target_names[1]
      event_col <- task$target_names[2]
      event_levels <- task$levels()[[event_col]]
      
      full_data$id <- seq_len(nrow(full_data))
      data <- full_data
      
      # Handle weights
      weights <- NULL
      if ("weights" %in% task$properties) {
        weights_data <- task$weights
        if (is.null(weights_data) || !"weight" %in% names(weights_data)) {
          stop("No weights defined in task")
        }
        all_weights <- weights_data$weight
        all_row_ids <- weights_data$row_id
        weight_map <- setNames(all_weights, all_row_ids)
        weights <- weight_map[as.character(row_ids)]
      }
      
      # Split features into cov1 (fixed) and cov2 (time-varying)
      cov1_names <- setdiff(features, pv$cov2_names)
      cov2_names <- intersect(features, pv$cov2_names)
      if (length(cov1_names) == 0 && length(cov2_names) == 0) {
        stop("No valid covariates provided!")
      }
      
      # Prepare Fine-Gray data
      form <- as.formula(paste("Surv(", time_col, ",", event_col, ") ~", 
                               paste(c(features, "id"), collapse = " + ")))
      if (length(event_levels) < 3) {
        stop("Event status must have at least 3 levels (censored, main event, competing risk)")
      }
      target_event <- if (is.null(pv$target_event)) event_levels[2] else pv$target_event
      
      fg_data <- survival::finegray(form, data = data, etype = target_event, weights = weights)
      
      if ("weights" %in% task$properties) {
        matched_indices <- match(fg_data$id, as.integer(names(weight_map)))
        fg_data$fgwt <- weights[matched_indices]
      }
      
      # Incorporate time-varying effects via cov2 and tf
      private$tf <- pv$tf
      cox_features <- cov1_names  # Start with fixed covariates
      if (length(cov2_names) > 0) {
        tf_matrix <- private$tf(fg_data$fgstop)  # Evaluate tf at stop times
        if (!is.matrix(tf_matrix) || ncol(tf_matrix) != length(cov2_names)) {
          stop("tf must return a matrix with columns equal to the number of cov2 variables")
        }
        for (j in seq_along(cov2_names)) {
          tf_col_name <- paste0(cov2_names[j], "_tf")
          fg_data[[tf_col_name]] <- fg_data[[cov2_names[j]]] * tf_matrix[, j]
          cox_features <- c(cox_features, tf_col_name)
        }
        # Debug: Verify columns exist
        if (!all(cox_features %in% names(fg_data))) {
          stop("Some features (e.g., z1_tf, z2_tf) not found in fg_data: ", 
               paste(setdiff(cox_features, names(fg_data)), collapse = ", "))
        }
      }
      
      # Fit Cox model with explicit data
      cox_formula <- as.formula(paste("Surv(fgstart, fgstop, fgstatus) ~", 
                                      paste(cox_features, collapse = " + ")))
      model <- survival::coxph(
        formula = cox_formula,
        data = fg_data,
        weights = fg_data$fgwt,
        control = coxph.control(eps = eps_val, iter.max = iter_max),
        robust = pv$robust,
        singular.ok = pv$singular.ok,
        ties = pv$ties
      )
      
      # Compute baseline hazard
      baseline_data <- as.data.frame(lapply(full_data[features], function(x) {
        if (is.factor(x)) return(factor(levels(x)[1], levels = levels(x)))
        else return(0)
      }))
      basehaz <- survival::survfit(model, newdata = baseline_data)
      private$basehaz <- list(time = basehaz$time, cumhaz = basehaz$cumhaz)
      
      return(model)
    },
    
    .predict = function(task) {
      newdata <- as.data.frame(task$data(rows = task$row_ids, cols = task$feature_names))
      
      # Split features into cov1 and cov2
      pv <- self$param_set$get_values(tags = "train")
      cov1_names <- setdiff(task$feature_names, pv$cov2_names)
      cov2_names <- intersect(task$feature_names, pv$cov2_names)
      
      # Compute time-varying terms for prediction
      if (length(cov2_names) > 0) {
        tf_matrix <- private$tf(private$basehaz$time)  # Evaluate tf at baseline times
        if (!is.matrix(tf_matrix) || ncol(tf_matrix) != length(cov2_names)) {
          stop("tf must return a matrix with columns equal to the number of cov2 variables")
        }
        for (j in seq_along(cov2_names)) {
          newdata[[paste0(cov2_names[j], "_tf")]] <- newdata[[cov2_names[j]]] * tf_matrix[, j]
        }
      }
      
      # Predict linear predictor
      lp <- predict(self$model, newdata = newdata, type = "lp")
      
      # Compute survival distribution
      cif <- matrix(NA, nrow = nrow(newdata), ncol = length(private$basehaz$time))
      for (i in seq_along(lp)) {
        cif[i, ] <- 1 - exp(-private$basehaz$cumhaz * exp(lp[i]))
      }
      time_order <- order(private$basehaz$time)
      surv <- 1 - cif[, time_order, drop = FALSE]
      colnames(surv) <- private$basehaz$time[time_order]
      
      list(crank = lp, lp = lp, distr = surv)
    }
  )
)
