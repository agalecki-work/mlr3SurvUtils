# tests/test_fine_gray.R
# Test script for Fine-Gray Cox PH learner with synthetic data

test_fine_gray_synthetic_mlr3 <- function() {
  # Load required packages
  library(mlr3)
  library(mlr3proba)
  library(survival)
  library(stats)

  # Set seed for reproducibility
  set.seed(123)

  # Generate synthetic data
  n <- 50
  data <- data.frame(
    time_to_event = rexp(n, rate = 0.1),
    outcome = factor(sample(0:2, n, replace = TRUE, prob = c(0.4, 0.3, 0.3)),
                     levels = c(0, 1, 2),
                     labels = c("censored", "death", "relapse")),
    x1 = rnorm(n, mean = 50, sd = 10),
    x2 = rnorm(n, mean = 2, sd = 1),
    wts = runif(n, min = 0.5, max = 1.5)
  )

  # Create survival task
  task <- mlr3proba::TaskSurv$new(
    id = "synthetic_test",
    backend = data,
    time = "time_to_event",
    event = "outcome",
    type = "mstate"
  )
  task$set_col_roles("wts", "weight")

  # Partition data into train/test sets
  set.seed(123)
  part <- mlr3::partition(task, ratio = 0.7)

  # Initialize and train the learner
  FGcox <- LearnerSurvFineGrayCoxPH$new()
  FGcox$train(task, part$train)
  p <- FGcox$predict(task, part$test)

  # Evaluate survival probabilities at specific times
  eval_times <- c(0, 5, 10, 15, 20)
  St <- p$distr$survival(eval_times)

  # Print results
  cat("Synthetic Data Test Results:\n")
  cat("==========================\n")
  cat("Model Summary:\n")
  print(summary(FGcox$model)$coefficients)
  cat("\nSurvival Probabilities at times", paste(eval_times, collapse = ", "), ":\n")
  print(St[1:3, 1:5])
  cat("\nValidation Checks:\n")
  cat("Model converged:", FGcox$model$iter < FGcox$model$iter.max, "\n")

  # Return results invisibly
  return(invisible(list(learner = FGcox, prediction = p)))
}

# Run the test if in an interactive session
if (interactive()) {
  cat("Running Fine-Gray Cox PH test with mlr3...\n")
  synthetic_result <- test_fine_gray_synthetic_mlr3()
}