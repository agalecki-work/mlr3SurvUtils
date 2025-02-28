# R/zzz.R
#' @importFrom mlr3 mlr_learners
NULL

.onLoad <- function(libname, pkgname) {
  # Register the learner in mlr_learners dictionary
  mlr_learners$add("surv.finegray_coxph", LearnerSurvFineGrayCoxPH)
}

# Declare global variables to avoid R CMD check warnings
utils::globalVariables(c("..keep_cols", "..xtra_cols",  "traceon"))
utils::globalVariables(c(".", "everything", "statistic", "value",".Last", "set_col_roles"))