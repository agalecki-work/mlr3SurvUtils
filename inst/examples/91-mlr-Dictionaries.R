#' ---
#' title: "91. mlr3 dictionaries"
#' date: Jan. 28, 2025
#' output: html_document
#' ---
#'
#' This document  illustrates how to extract `mlr3` dictionaries for survival tasks, learners, measures, etc 
#'
#' ### `mlr3` packages
#' Load libraries
library(mlr3verse)
ns = sort(loadedNamespaces())
ns[startsWith(ns, "mlr3")]

#' ### `surv` tasks

#' All tasks <DictionaryTask>
tsk() # <DictionaryTask> 


#' Instantiate  selected task
tsk("gbsg")
#' Equivalent way
#| eval =FALSE
mlr_tasks$get("gbsg")
#| echo = FALSE
cat("... output omitted")


#' surv tasks
surv_tasks <- as.data.table(mlr_tasks)
colnames(surv_tasks)
surv_tasks[task_type == "surv", c("key", "label", "nrow", "ncol")]


#' ### `surv` learners

library(mlr3)
library(mlr3proba)
library(mlr3extralearners)
library(mlr3SurvUtils)

#' all learners <DictionaryLearner>
lrn()     # mlr_learners
surv_learners <- mlr_learners$keys(pattern = "surv")
print(sort(surv_learners))

#' help pages for selected learner
#| eval=FALSE
lrn("surv.ctree")$help()

#'  Instantiate learner
lrn("surv.ctree")

#' Equivalent way
#| eval =FALSE
mlr_learners$get("surv.ctree")
#| echo = FALSE
cat("... output omitted")

#' surv learners
keys = as.data.table(mlr_learners)[, "key"][[1]]
keys = startsWith(keys,"surv.")
(surv_learners <- as.data.table(mlr_learners)[keys, c("key", "label", "predict_types"), with = FALSE])


#' ### Measures

#' All measures <DictionaryMeasure>
msr()   # mlr_measures

#' help pages for selected measure
#| eval=FALSE
msr("surv.cindex")$help()

#' Details on selected measure
msr("surv.cindex")
#' Equivalent way
#| eval =FALSE
mlr_measures$get("surv.cindex")
#| echo = FALSE
cat("... output omitted")

#' surv measures
all_meas <- as.data.table(mlr_measures) # <DictionaryMeasure>
colnames(all_meas)
(surv <- all_meas[task_type == "surv",])


#' ### Filters

library(mlr3filters)

#' All filters   <DictionaryFilter>
flt() # mlr_filters

#' help pages for selected filter
#| eval=FALSE
flt("importance")$help()


#' Instantiate filter
flt("importance")
#' Equivalent way
#| eval =FALSE
mlr_filters$get("importance")
#| echo = FALSE
cat("... output omitted")

#| eval=FALSE
flt("importance")$help()

# Filter attributes 
available_filters <- as.data.table(mlr_filters)
colnames(available_filters)


#' ### Resamplings


#' All resamplings <DictionaryResampling>
rsmp() # mlr_resamplings # 

#' help pages
#| eval=FALSE
rsmp("bootstrap")$help()


#' Instantiate selected resampling (modify default parameter values)

rsmp("bootstrap", repeats = 31, ratio =1)

# Another way
mlr_resamplings$get("bootstrap")



(available_resamplings  <- as.data.table(mlr_resamplings))

#' ### Terminators

#' Teminators <DictionaryTerminator>
library(mlr3tuning)
trm() #  mlr_terminators #
trm("combo")
(available_terminators = as.data.table(mlr_terminators))

#' ### Tuners <DictionaryTuner>

#' mlr_tuners 
tnr()  # mlr_tuners 
tnr("grid_search")

#' help pages
#| eval=FALSE
tnr("grid_search")$help()


tnr("grid_search")
(available_tuners = as.data.table(mlr_tuners))


#' ### PipeOps


library(mlr3pipelines)
pos()                   #  <DictionaryPipeOp>
po("learner", lrn("classif.rpart"), cp = 0.3)
# print(mlr_pipeops)    #  <DictionaryPipeOp>
class(mlr_pipeops)    #  "DictionaryPipeOp" "Dictionary" "R6" 
available_pipeops   = as.data.table(mlr_pipeops)
colnames(available_pipeops)

#' * Details for specific types of PipeOps (e.g., impute)
(imputing_pipeops <- available_pipeops[grepl("impute", key)])


