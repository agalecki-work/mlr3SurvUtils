#' ---
#' title: "Create SRS task for right censored data (TaskSurv)"
#' date: Jan. 28, 2025
#' output: html_document
#' ---
#' This document was created by `02-task-SRS-surv.R` script. It illustrates how to:
#'
#' * create mlr3 task for right censored data using `createTaskSurv()` function
#' * extract basic info from the task (for details see [TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.html) documentation)
#'
#' ## Setup
#'
#' Libraries
#| echo =TRUE
library(mlr3SurvUtils)
#?mlr3SurvUtils

#' Load cancer data
data(cancer, package="survival")

#' First few rows of `lung` data
head(lung)

#' Status/event variable redefined
lung$status01 <- lung$status -1
lung$status <- NULL

#' ID (numeric) variable
lung$id <- 1:nrow(lung)

#' First few rows of modified `lung` data
head(lung)

#' ## Create task

#' `target_info` is a named vector
#| echo =TRUE
(target_info <- c(id = "target1", time = "time", event = "status01", task_type="surv"))

#' `backend_info` is a list
backend_info <- list(
   id = "lung",
   option = "SRS",
   primary_key = "id",
   feature_cols = c("sex", "ph.karno", "pat.karno", "meal.cal", "wt.loss"),
   filter = "lung$age < 70",
   time_cutoff = 1000,
   add_to_strata_cols = "sex"
 )

#' Task created
task <- createTaskSurv(lung, target_info, backend_info)

#' Task description
task

#' ## Descriptive statistics

#' Note: For numeric variables only
#'

#' Overall
Descriptive_stats(task)


#' By sex
Descriptive_stats(task, group_by_cols = "sex")


#' ## Explore task
class(task)
mode(task)
names(task)

#' Data column names
colnames(task$data())

#' Column roles
task$col_roles

#' Time variable range
range(task$times())

#' Frequency table for status/event variable
table(task$status())

#' PH assumption
#'

#' Error: Task 'SRS.lung:tm1' has missing values in column(s) 'meal.cal', 'pat.karno', 'ph.karno', 'wt.loss', 
#' but learner 'surv.coxph' does not support this task$prop_haz
#'  task$prop_haz()  # commented out, it works for small number of predictors, graph?

#' Kaplan
#'

 print(task$kaplan(strata = "sex"))

#' Proportion of censored observations across entire dataset
task$cens_prop()

#' Proportion of variables that are significantly associated with the
#'   censoring status via a logistic regression model. 
#'   Zero indicates independent censoring 
task$dep_cens_prop() 

#' Missings
missx = task$missings()
print(missx[missx>0])






