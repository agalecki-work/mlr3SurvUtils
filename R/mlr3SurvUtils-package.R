#' mlr3SurvUtils: A Package for Survival Analysis Utilities
#'
#' The \code{mlr3SurvUtils} package provides utilities and functions for conducting
#' survival analysis. It is designed to build on the capabilities of the mlr3 ecosystem, particularly
#' focusing on tasks and models specific to survival analysis.
#'
#' @section Main Features:
#' \itemize{
#'   \item Creating and managing survival tasks
#'   \item Specialized support for \code{mlr3} and \code{mlr3proba} interactions
#'   \item Tools for preprocessing and model evaluation tailored to survival data
#' }
#'
#' @section Functions:
#' The package provides the following key functions:
#' \describe{
#'   \item{\code{\link{createTaskSurv}}}{Creates mlr3 tasks under different scenarios.}
#'   \item{\code{\link{Descriptive_stats}}}{Compute descriptive statistics for survival data.}
#'   \item{\code{\link{create_cvglmnet_extra_args}}}{Prepare extra arguments for `cv.glmnet` that uses training data partition.}
#'   \item{\code{\link{run_example}}}{Run and render example scripts stored in the package.}
#' }
#'
#' @seealso
#' Refer to \code{\link[mlr3]{mlr3}} and \code{\link[mlr3proba]{mlr3proba}} for more information on integrating with the mlr3 framework for survival analysis.
#'
#' @keywords package
#' @name mlr3SurvUtils-package
NULL