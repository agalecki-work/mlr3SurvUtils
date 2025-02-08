#' ---
#' title: "08. `surv.cv_glmnet` learners"
#' date: Jan. 28, 2025
#' output: html_document
#' ---
#'
#' This document  illustrates how to extract create and examine `surv.cv_glmnet` learners for diffrent
#' values of alpha

#' Load libraries
library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3extralearners)

library(mlr3misc)

#' ### Create `my_learners` list
#' Create a named list to hold learners with specific information
my_learners <- list()

#' Define a list of alpha values
alpha_values = c(0.1, 0.25, 0.5, 0.75, 1)

#' Loop through alpha values and create learners
for (alpha in alpha_values) {
  learner_id = paste0("surv.glmnet.alpha_", alpha)
  learner <- lrn("surv.glmnet", alpha = alpha, mxit =101, id = learner_id)
    # Create a list to hold the specified information
    learner_info <- list(
      label = learner_id,
      task_type = learner$task_type,
      predict_types = learner$predict_types,
      learner = learner
    )
  my_learners[[learner_id]] = learner
}

#' Lasso learner
#'
#' Select last learner from the list ( aplha=1, LASSO)
nms = names(my_learners)
nm_lasso =nms[length(nms)] 
(lasso_lrn = my_learners[[nm_lasso]])
typeof(lasso_lrn)
names(lasso_lrn)
lasso_lrn$id
lasso_lrn$label
lasso_lrn$task_type
paste(lasso_lrn$predict_types, collapse = ", ")
lasso_lrn$param_set


