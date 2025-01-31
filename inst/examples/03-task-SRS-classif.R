#' ---
#' title: "Create SRS classification task (TaskClassif)"
#' date: Jan. 28, 2025
#' output: html_document
#' ---
#' This document was created by `03-task-SRS-multiclass.R` script. It illustrates how to:
#'
#' * create `TaskClassif` task using `createTaskClassif()` function. 
#' * This task can be used to extract data for competing risk model
#' * how to  extract time variable from the task
#' * perform competing risk analysis (F-G model) using `cmprsk::crr()` and `survival::coxph()'
#' * (for details see [TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html) documentation)
#'
#' ## Setup
#'
#' Libraries
library(mlr3SurvUtils)
library(data.table)
library(cmprsk)

#' Data available in `survival` package
dts <- data(package="survival")
res <- dts$results[, c("Item", "Title")]
head(res)

#' Load `mgus2` data
data(cancer, package="survival")
(mgus2 <- as.data.table(mgus2))

#' Variables derived
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
   option = "SRS",
   primary_key = "id",
   feature_cols = c("sex", "hgb", "creat", "mspike"),
   # time_cutoff = 36,
   add_to_strata_cols = "sex"
 )

#' Task created
task <- createTaskSurv(mgus2c, target_info, backend_info)

#' Task description
task
task$missings()

names(task$extra_args)

#' ##  cmprsk

#' * Data preparation 
(extra_df <- task$extra_args$extra_df)

#' Bind data columnwise
crr_data <-  cbind(extra_df, task$data())

#' Column names
colnames(crr_data)

#' * Model fit using `crr()`

library(cmprsk)

#' * with sex included
covx <- model.matrix(~ creat + hgb + mspike + sex, data= crr_data)
covx1 <- covx[, -1]
crr(crr_data$etime, crr_data$event_num, covx1) 

#' * `sex` omitted
covx <- model.matrix(~ creat + hgb + mspike, data= crr_data)
covx1 <- covx[, -1]
crr(crr_data$etime, crr_data$event_num, covx1) 

#' ## survival

library(survival)
FG_data <- finegray(Surv(etime, event)~ creat + hgb + mspike + sex, data= crr_data, etype ="pcm")
coxph_fit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ creat + hgb + mspike + sex, data=FG_data, weights = fgwt)
summary(coxph_fit)