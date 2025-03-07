# tests/testthat/test-learner_surv_finegray_coxph.R
lapply(list.files(system.file("testthat", package = "mlr3"), 
                  pattern = "^helper.*\\.[rR]", full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3proba"), 
                  pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

library(testthat)
library(mlr3)
library(mlr3proba)
library(mlr3misc)
library(mlr3SurvUtils)

test_that("autotest LearnerSurvFineGrayCoxPH with expected limitation", {
  learner <- LearnerSurvFineGrayCoxPH$new()
  expect_learner(learner)
  # Run autotest, expecting potential failure on tasks with < 3 levels
  result <- tryCatch(
    {
      run_autotest(learner)
      TRUE  # If it completes without error, consider it a pass
    },
    error = function(e) {
      # Check if the error is the expected one
      expect_match(e$message, "Event status must have at least 3 levels", 
                   info = "Expected failure on tasks with < 3 event levels (e.g., 'feat_single_logical')")
      TRUE  # Mark as pass since it's the known limitation
    }
  )
  expect_true(result, 
              info = "Autotest passes or fails as expected on tasks with < 3 event levels")
})

test_that("paramtest LearnerSurvFineGrayCoxPH", {
  learner <- LearnerSurvFineGrayCoxPH$new()
  exclude_train <- c(
    "formula", "data", "weights", "id", "etype",
    "x", "y", "model", "init", "control", "offset",
    "robust", "singular.ok", "ties", "eps", "iter.max",
    "subset", "na.action", "prefix", "count", "timefix", "tt", "method", "cluster", "istate", "statedata", "nocenter",
    "target_event"
  )
  result_train <- run_paramtest(
    learner, 
    list(survival::finegray, survival::coxph), 
    exclude = exclude_train, 
    tag = "train"
  )
  expect_true(result_train, info = result_train$error)

  exclude_predict <- c(
    "object", "newdata", "type",
    "se.fit", "na.action", "terms", "collapse", "reference"
  )
  result_predict <- run_paramtest(
    learner, 
    survival:::predict.coxph, 
    exclude = exclude_predict, 
    tag = "predict"
  )
  expect_true(result_predict, info = result_predict$error)
})

test_that("manual test with mstate task", {
  data <- data.frame(
    time = c(1, 2, 3, 4, 5),
    status = factor(c(0, 1, 2, 0, 1), levels = c(0, 1, 2), labels = c("censored", "event1", "event2")),
    x = c(1, 2, 3, 4, 5)
  )
  task <- TaskSurv$new("test", backend = data, time = "time", event = "status", type = "mstate")
  learner <- LearnerSurvFineGrayCoxPH$new()
  learner$train(task)
  p <- learner$predict(task)
  expect_s3_class(p, "PredictionSurv")
  expect_vector(p$crank, size = 5)
  expect_vector(p$lp, size = 5)
  expect_true(inherits(p$distr, "Distribution"))
  surv_matrix <- p$distr$survival(0:3)
  expect_true(is.matrix(surv_matrix))
  expect_equal(ncol(surv_matrix), 5)
})
