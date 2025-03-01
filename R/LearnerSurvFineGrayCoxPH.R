#' @import R6
#' @import mlr3
#' @import mlr3proba
#' @import survival
#' @import paradox
#' @import purrr
NULL

#' Fine-Gray Survival Learner with Cox Proportional Hazards
#'
#' @description
#' A survival learner implementing the Fine-Gray model for competing risks analysis
#' using `survival::coxph`. This learner extends the `LearnerSurv` class from 
#' `mlr3proba` and fits a proportional subdistribution hazards model after 
#' transforming the data with `survival::finegray`. The main event of interest 
#' is dynamically set as the second level of the event status factor, with the 
#' third level treated as a competing risk.
#'
#' @details
#' The Fine-Gray model estimates the subdistribution hazard of a specific event 
#' in the presence of competing risks. This implementation uses the standard 
#' Cox proportional hazards approach without penalization. The event status 
#' must have at least three levels: censored (first level), main event of 
#' interest (second level), and competing risk (third level). Predictions 
#' include the linear predictor (`lp`), crank scores, and distribution (`distr`) 
#' as survival probabilities over time.
#'
#' @section Parameters:
#' The learner supports the following parameters:
#' \describe{
#'   \item{ties}{Character, method for handling ties in `coxph`. Options are 
#'     `"efron"` (default), `"breslow"`, or `"exact"`.}
#'   \item{iter.max}{Integer, maximum number of iterations for `coxph`. 
#'     Default is 100, ranging from 1 to 1000.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{`$initialize()`}{Initializes a new instance of the learner.}
#'   \item{`$train(task)`}{Trains the Fine-Gray model on the provided survival task.}
#'   \item{`$predict(task)`}{Predicts survival outcomes for new data.}
#' }
#'
#' @family Learners
#'
#' @examples
#' library(mlr3)
#' library(mlr3proba)
#' 
#' # Create example data
#' set.seed(123)
#' data <- data.frame(
#'   survival_time = c(5, 10, 15, 20, 25, 7, 12, 18, 22, 30, 8, 14, 19, 23, 28),
#'   event_status = c(1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0),
#'   x1 = c(1, 2, 3, 4, 5, 1.5, 2.5, 3.5, 4.5, 5.5, 2, 3, 4, 5, 6),
#'   x2 = c(0.5, 1, 1.5, 2, 2.5, 0.7, 1.2, 1.8, 2.3, 2.8, 0.9, 1.4, 1.9, 2.4, 2.9)
#' )
#' data$event_status <- factor(data$event_status,
#'                            levels = c(0, 1, 2),
#'                            labels = c("censored", "death", "relapse"))
#' 
#' # Create a survival task
#' task <- TaskSurv$new("fg_test",
#'                      backend = data,
#'                      time = "survival_time",
#'                      event = "event_status",
#'                      type = "mstate")
#' print(task)
#' part <- partition(task)
#' 
#' # Initialize and train the learner
#' FG_cox <- lrn("surv.finegray_coxph")
#' pred <- FG_cox$train(task, part$train)$predict(task, part$test)
#' St <- pred$distr[1:5]$survival(c(0, 10, 20, 30))
#' CIF <- 1 - St
#' cindex <- msr("surv.cindex")
#' print(cindex$score(pred))
#'
#' @export
LearnerSurvFineGrayCoxPH <- R6Class("LearnerSurvFineGrayCoxPH",
  inherit = LearnerSurv,
  public = list(
    #' @description
    #' Initializes a new instance of the Fine-Gray CoxPH survival learner.
    #' Sets up the parameter set and configures the learner properties.
    initialize = function() {
      ps <- ps(
        ties = p_fct(default = "efron", levels = c("efron", "breslow", "exact"), 
                    tags = "train"),
        iter.max = p_int(default = 100L, lower = 1L, upper = 1000L, 
                        tags = "train")
      )
      
      super$initialize(
        id = "surv.finegray_coxph",
        param_set = ps,
        feature_types = c("logical", "integer", "numeric", "factor"),
        predict_types = c("crank", "lp", "distr"),
        properties = "weights",
        packages = c("survival", "distr6", "purrr"),
        label = "Fine-Gray Competing Risks Model with CoxPH",
        man = "mlr3proba::mlr_learners_surv.finegray_coxph"
      )
    }
  ),
  
  private = list(
    basehaz = NULL,
    
    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      
      data <- as.data.frame(task$data())
      features <- task$feature_names
      if (length(features) == 0) stop("No features provided!")
      
      time_col <- "survival_time"
      event_col <- "event_status"
      
      #message("Time column: ", time_col)
      #message("Event column: ", event_col)
      event_levels <- task$levels()[[event_col]]
      #message("Event levels: ", paste(event_levels, collapse = ", "))
      
      form <- as.formula(paste("Surv(", time_col, ",", event_col, ") ~", 
                             paste(features, collapse = " + ")))
      #message("Formula: ", deparse(form))
      
      if (length(event_levels) < 3) {
        stop("Event status must have at least 3 levels (censored, main event, competing risk)")
      }
      target_event <- event_levels[2]
      competing_event <- event_levels[3]
      #message("Target event (main): ", target_event)
      #message("Competing event: ", competing_event)
      
      fg_data <- finegray(form, data = data, etype = target_event)
      
      if ("weights" %in% task$properties) {
        fg_data$fgwt <- task$weights$weight
      }
      
      model <- exec(survival::coxph,
                   formula = as.formula(paste("Surv(fgstart, fgstop, fgstatus) ~", 
                                             paste(features, collapse = " + "))),
                   data = fg_data,
                   weights = fg_data$fgwt,
                   !!!pv)
      
      baseline_data <- as.data.frame(matrix(0, nrow = 1, ncol = length(features)))
      colnames(baseline_data) <- features
      basehaz <- survfit(model, newdata = baseline_data)
      private$basehaz <- list(time = basehaz$time, cumhaz = basehaz$cumhaz)
      
      return(model)
    },
    
    .predict = function(task) {
      newdata <- as.data.frame(task$data(rows = task$row_ids, cols = task$feature_names))
      
      lp <- exec(predict, self$model,
                newdata = newdata,
                type = "lp")
      
      #message("Length of lp: ", length(lp))
      #message("Length of basehaz$time: ", length(private$basehaz$time))
      #message("Length of basehaz$cumhaz: ", length(private$basehaz$cumhaz))
      
      cif <- matrix(NA, nrow = nrow(newdata), ncol = length(private$basehaz$time))
      for (i in seq_along(lp)) {
        cif[i, ] <- 1 - exp(-private$basehaz$cumhaz * exp(lp[i]))
      }
      #message("Dimensions of cif: ", paste(dim(cif), collapse = " x "))
      
      time_order <- order(private$basehaz$time)
      surv <- 1 - cif[, time_order, drop = FALSE]
      times <- private$basehaz$time[time_order]
      
      #message("Length of times: ", length(times))
      #message("Dimensions of surv: ", paste(dim(surv), collapse = " x "))
      
      .surv_return(
        times = times,
        surv = surv,
        lp = lp
      )
    }
  )
)
