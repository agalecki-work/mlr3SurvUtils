#' ---
#' title: "91. mlr3 dictionaries"
#' date: Jan. 28, 2025
#' output: html_document
#' ---
#'
#' This document  illustrates how to extract `mlr3` dictionaries for survival tasks, learners, measures, etc 

#' Load libraries
library(mlr3verse)

#' ### `mlr3` packages
ns = sort(loadedNamespaces())
ns[startsWith(ns, "mlr3")]

#' ### `surv` learners

library(mlr3)
library(mlr3proba)
library(mlr3extralearners)

#' <DictionaryLearner>
lrn() # <DictionaryLearner>

#' surv learners
keys = as.data.table(mlr_learners)[, "key"][[1]]
keys = startsWith(keys,"surv.")
(surv_learners <- as.data.table(mlr_learners)[keys, c("key", "label", "predict_types"), with = FALSE])


#' ### `surv` tasks

#' <DictionaryTask>
tsk() # <DictionaryTask> 

# surv tasks
surv_tasks <- as.data.table(mlr_tasks)
colnames(surv_tasks)
surv_tasks[task_type == "surv", c("key", "label", "nrow", "ncol")]


#' ### Measures


mlr_measures
all_meas <- as.data.table(mlr_measures) # <DictionaryMeasure>
colnames(all_meas)
(surv <- all_meas[task_type == "surv",])


#' ### Filters


library(mlr3filters)

flt() # <DictionaryFilter>
available_filters <- as.data.table(mlr_filters)
colnames(available_filters)

#| eval=FALSE
flt("importance")$help()

#' ### Resamplings

mlr_resamplings # <DictionaryResampling>
(available_resamplings  <- as.data.table(mlr_resamplings))

#' ### Terminators

library(mlr3tuning)
mlr_terminators # <DictionaryTerminator>
(available_terminators = as.data.table(mlr_terminators))

#' ### Tuners
mlr_tuners # <DictionaryTuner>
(available_tuners = as.data.table(mlr_tuners))


#' ### PipeOps


library(mlr3pipelines)
print(mlr_pipeops)    #  <DictionaryPipeOp>
class(mlr_pipeops)    #  "DictionaryPipeOp" "Dictionary" "R6" 
available_pipeops   = as.data.table(mlr_pipeops)
colnames(available_pipeops)


#' *  Details for specific types of PipeOps (e.g., mutate)

mutating_pipeops <- available_pipeops[grepl("mutate", key)]
#' * Details for specific types of PipeOps (e.g., impute)
(imputing_pipeops <- available_pipeops[grepl("impute", key)])


