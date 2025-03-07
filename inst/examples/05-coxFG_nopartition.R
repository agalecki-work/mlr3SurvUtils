#' ---
#' title: "Fine-Gray Competing Risks Analysis: mlr3 vs coxph"
#' date: "March 04, 2025"
#' output: html_document
#' ---

#' # Setup
#' 
#' Load required libraries:

#+ setup
library(mlr3)
library(mlr3proba)
library(survival)
library(mlr3SurvUtils)

#' # Data Preparation
#' 
#' Treat time to death and plasma cell malignancy as competing risks using the `mgus2` dataset:

#+ data-prep
etime <- with(mgus2, ifelse(pstat == 0, futime, ptime))
event <- with(mgus2, ifelse(pstat == 0, 2 * death, 1))
event <- factor(event, 0:2, labels = c("censor", "pcm", "death"))
mgus2$event <- event
mgus2$etime <- etime

#' Create a survival task in mlr3:

#+ task-creation
task <- TaskSurv$new(
  id = "CRR",
  backend = mgus2,
  time = "etime",
  event = "event",
  type = "mstate"
)
task$select(c("age", "sex"))
task$set_col_roles(cols = "event", add_to = "stratum")
task

#' # Fine-Gray Model Using mlr3
#' 
#' Fit a Fine-Gray model targeting "pcm" (plasma cell malignancy) using the `LearnerSurvFineGrayCoxPH` from the custom package:

#+ mlr3-model
FGcox_pcm <- LearnerSurvFineGrayCoxPH$new()
FGcox_pcm$param_set$values$target_event <- "pcm"  # Target event is "pcm"
p_pcm <- FGcox_pcm$train(task)$predict(task)

#' Print model summary:

#+ mlr3-summary, echo=TRUE
cat("PCM Model Summary:\n")
print(summary(FGcox_pcm$model)$coefficients)

#' Extract survival probabilities at specific evaluation times:

#+ mlr3-surv-probs
eval_times <- c(0, 50, 100, 150, 200)
St_pcm <- p_pcm$distr$survival(eval_times)
dim(St_pcm)

#' Display survival probabilities for the first 6 observations:

#+ mlr3-surv-display
print(St_pcm[, 1:6])  # rows = times, cols = observations

#' # Fine-Gray Model Using coxph Directly
#' 
#' Now, let's replicate this using `coxph` directly without `mlr3`:

#+ coxph-data
pdata <- finegray(Surv(etime, event) ~ ., 
                  data = mgus2[, c("etime", "event", "age", "sex")], 
                  etype = "pcm")
cox_fit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ age + sex, 
                 weight = fgwt, data = pdata)

#' Extract baseline cumulative subdistribution hazard:

#+ coxph-baseline
baseline_data <- data.frame(age = 0, sex = "F")
base_surv <- survfit(cox_fit, newdata = baseline_data)
base_times <- base_surv$time
base_cumhaz <- base_surv$cumhaz  # Cumulative baseline subdistribution hazard

#' Compute linear predictors for the original data:

#+ coxph-lp
lp <- predict(cox_fit, newdata = mgus2[, c("age", "sex")], type = "lp")

#' Calculate survival probabilities:

#+ coxph-surv-probs
cif <- outer(base_cumhaz, exp(lp), "*")  # Matrix: rows = times, cols = observations
St_pcmx <- 1 - (1 - exp(-cif))           # Survival probabilities
rownames(St_pcmx) <- base_times          # Assign time points as row names
dim(St_pcmx)

#' Extract survival probabilities at specific times for comparison:

#+ coxph-surv-display
eval_times <- c(0, 50, 100, 150, 200)  # Adjusted to match mlr3 times
sel <- which(base_times %in% eval_times)
if (length(sel) < length(eval_times)) {
  warning("Some evaluation times not found in base_times; using closest available.")
  sel <- sapply(eval_times, function(t) which.min(abs(base_times - t)))
}
print(St_pcmx[sel, 1:6])  # Display for first 6 observations

#' # Comparison
#' 
#' The `mlr3` approach uses `distr6` to handle survival distributions, providing a convenient `survival()` method. 
#' The `coxph` approach computes probabilities manually, requiring interpolation if exact times don't match. 
#' Differences may arise due to baseline hazard estimation or numerical precision, but results should be similar.