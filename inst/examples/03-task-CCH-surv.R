#' ---
#' title: "03. Create CCH task for survival data obtained from case-cohort study (TaskSurv)"
#' date: Jan. 28, 2025
#' output: html_document
#' ---
#' This document  illustrates how to:
#'
#' * create CCH tasks for case-cohort study using `createTaskSurv()` function
#'    - First task named `task_CCH` was created using CCH option and  includes all data _except_ non-cases outside of subcohort
#'    - Second task named `task_CCH1` was  created using CCH1 option and  includes subcohort data only
#' * how to fit Cox model and obtain predicted values using `mlr3` syntax and `survival::coxph()` function
#'
#' Based on [link](https://biostat3.net/download/R/lecture_Biostat3_NCC_casecoh_Johansson_20201118.pdf)
#' (slides #43-45)
#'
#' ## Setup
#'
#' Libraries
library(mlr3SurvUtils)
library(survival)

#' Data frame `colon`  loaded
data("colon", package="biostat3")

#' Number of columns
ncol(colon)


#' Column names
colnames(colon)


#' Derive columns
colon$event <- as.numeric((colon$status=="Dead: cancer")) ## creates indicator 0/1
colon$agegrpf <- factor(colon$agegrp)

#' Define subcohort in the `colon` data

set.seed(42) ## make sampling reproducible
colon$u <- runif(nrow(colon), 0, 1) ## assign random number to all obs
colon$subcoh[colon$u <= 0.05] <- 1 ## generate dummy subcohort 5%
colon$subcoh[colon$u>0.05] <- 0

#' Frequency tables
table(colon$stage)
table(colon$event)
table(subcoh = colon$subcoh)
table(subcoh = colon$subcoh, stage= colon$stage)

#' Localised stage only 
lclsed = colon[colon$stage == "Localised", ]
table(subcoh = lclsed$subcoh,  event= lclsed$event)

#' ## Create CCH task

#' `target_info` is a named vector
#| echo =TRUE
(target_info <- c(id = "trgt_id", time = "surv_mm", event = "event", task_type="surv"))

#' `backend_info` is a list
backend_info <- list(
   id = "lcl",
   option = "CCH",
   CCH_subcohort = "subcoh",
   primary_key = "id",
   feature_cols = c("sex", "agegrpf"),
   filter  = 'colon$stage ==  "Localised"'
 )

#' CCH task created
task_CCH <- createTaskSurv(colon, target_info, backend_info)
task_CCH 

#' Task data
task_CCH$data()

#' ### `coxph` using mlr3

#' * Model fit
library(mlr3)
library(mlr3proba)

#' Define learner
cox_CCH = lrn("surv.coxph", ties="breslow")

#'   * Prediction: Train learner and get predictions 
#'
#'  Note: for simplicity all data are used for training and prediction
pred = cox_CCH$train(task_CCH)$predict(task_CCH)

#'  Time points and data rows selected for prediction
rows_selected = 1:5
(time_points <- c(seq(0, 20, by=5), seq(25, 200, by = 25)))
 
#' * prediction type = "distr"
#'
#'  Note: Probability of surviving specified time points for selected rows is returned

pred$distr[rows_selected]$survival(time_points) 

#' * lower `crank` values represent a lower risk of the event taking place and higher values represent higher risk.
pred$crank[rows_selected]


#' ### Cox model fit using `coxph'
#'
#' *  Extract data from `task_CCH`
(task_df <- cbind(task_CCH$weights, task_CCH$data() ))


#' Fit a Cox proportional hazards regression model with Borgan II  weights using the 'coxph' function
cox_borgan2 <- coxph(Surv(surv_mm,event)~sex + factor(agegrpf),
     data= task_df, ties="breslow", weights = weight, robust = TRUE )
     
#' * Obtain survival probabilities using `survfit()`
surv_probs <- survfit(cox_borgan2, newdata = task_df[rows_selected, ])

#' Print survival probabilities at specified time points
surv_probs_summary <- summary(surv_probs, times = time_points)

#' Survival probabilities for selected rows

pred_srv = rep(0, length(rows_selected)* length(time_points))
dim(pred_srv) = c(length(time_points), length(rows_selected))
for (i in 1: length(time_points)) {
  #cat("Subject", i, "\n")
  srvi = surv_probs_summary$surv[i, ]
  print(srvi)
  pred_srv[i,] = srvi
}
rownames(pred_srv) = time_points
pred_srv

#' Prognostic index/linear predictor (corresponds to `crank` values
pred_lp = predict(cox_borgan2, task_df, type="lp")
pred_lp[rows_selected]

#' ## Create CCH1 task

#' We reuse `target_info` defined for CCH task


#' `backend_info` is modified below
backend_info2 <- backend_info
backend_info2$option = "CCH1"

#' CCH task created
task_CCH1 <- createTaskSurv(colon, target_info, backend_info2)
task_CCH1 

#' task data
task_CCH1$data()




