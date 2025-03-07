#' ---
#' title: "04. Create CCH classification task (TaskClassif)"
#' date: Jan. 28, 2025
#' output: html_document
#' ---
#' This document illustrates how to:
#'
#' * create `TaskClassif` task for case-cohort study using `createTaskSurv()` function. 
#' * This task can be used to extract data to fit competing risk model
#' * How to  extract time variable from the task
#' * perform competing risk analysis (F-G model) using `cmprsk::crr()` and `survival::coxph()`
#' * (for details see [TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html) documentation)
#'
#' ## Setup
#'
#' Libraries
library(mlr3SurvUtils)
library(data.table)
library(cmprsk)
library(survival)


#' Convert `mgus2` data to `data.table`
(mgus2 <- as.data.table(mgus2))

#' Define subcohort in the `mgus2` data

set.seed(42) ## make sampling reproducible
mgus2$u <- runif(nrow(mgus2), 0, 1) ## assign random number to all obs
mgus2$subcoh[mgus2$u <= 0.15] <- 1 ## generate dummy subcohort 15%
mgus2$subcoh[mgus2$u>0.15] <- 0


#' Variables needed for competing risks model derived
mgus2$etime <- with(mgus2, ifelse(pstat==0, futime, ptime))
event <- with(mgus2, ifelse(pstat==0, 2*death, 1))
mgus2$event <- factor(event, 0:2, labels=c("censor", "pcm", "death"))
str(mgus2)


#' Frequency table for `event` 
table(mgus2$event)

#' Complete cases
mgus2c <- mgus2[complete.cases(mgus2), ]

#' ## Create TaskClassif task

#' `target_info` is a named vector
(target_info <- c(id = "mytarget", time = "etime", event = "event", task_type="classif"))

#' `backend_info` is a list
backend_info <- list(
   id = "mgus",
   option = "CCH",
   CCH_subcohort ="subcoh",
   primary_key = "id",
   feature_cols = c("sex", "hgb", "creat", "mspike"),
   add_to_strata_cols = "sex"
 )

#' Task created
task_CCH <- createTaskSurv(mgus2c, target_info, backend_info)

#' Task description
task_CCH
task_CCH$missings()

names(task_CCH$extra_args)

#' ## survival::finegray


#' * Data preparation 
(extra_df <- task_CCH$extra_args$extra_df)

#' Bind data columnwise
cbind_data <-  cbind(extra_df, task_CCH$weights, task_CCH$data())

#' Column names
colnames(cbind_data)

#' * `finegray()`  
library(survival)
FG_data <- finegray(Surv(etime, event)~ creat + hgb + mspike + sex, weights = weight, data= cbind_data, etype ="pcm")
colnames(FG_data)


#' * `coxph()`
coxph_fit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ creat + hgb + mspike + sex, data=FG_data, weights = fgwt)
summary(coxph_fit)

#' ##  cmprsk

#' Note: `cmprsk::crr` does not accept 'weights' argument
