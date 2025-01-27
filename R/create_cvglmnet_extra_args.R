
#' Create Extra Arguments for cv.glmnet from Training Partition
#'
#' This function prepares additional arguments for the `cv.glmnet` function, focusing on a designated training partition of a task. It generates fold IDs for cross-validation and penalty factors for features.
#'
#' @param task A task object from which a training partition is designated. The task is expected to have properties such as number of rows (`nrow`), column roles (`col_roles`), strata, row IDs (`row_ids`), and feature names (`feature_names`).
#' @param nfolds An integer indicating the number of folds to use in cross-validation. Default is 5.
#' @param seed An integer seed for random number generation to ensure reproducibility. Default is 123.
#' @param traceon Logical. If `TRUE`, enables detailed tracing output for debugging. Default is `FALSE`.
#'
#' @return A list containing: 
#'   - `foldid`: An integer vector assigning each observation to a fold for cross-validation.
#'   - `penalty.factor`: A named numeric vector indicating penalty factors for the features, with default values of 1 for each feature.
#'
#' @examples
#' \dontrun{
#' # Assuming `task` represents a dataset with a training partition
#' extra_args <- create_cvglmnet_extra_args(task, nfolds = 10, seed = 456)
#' print(extra_args$foldid)
#' print(extra_args$penalty.factor)
#' }
#'
#' @export
create_cvglmnet_extra_args <- function(task, nfolds = 5, seed = 123, traceon = FALSE) {
  traceit <- function(msg, variable = NULL) {
    if (isTRUE(traceon)) {
      cat(msg, "\n")
      if (!is.null(variable)) {
        print(variable)
      }
    }
  }

  traceit(sprintf("--- create_cvglmnet_extra_args() STARTS. nfolds: %d", nfolds))
  
  set.seed(seed)
  nrows = task$nrow
  foldid = integer(nrows)
  
  # Get strata and assign folds
  if ("stratum" %in% names(task$col_roles) && !is.null(task$strata)) {
    strata = task$strata
    traceit("Strata details", strata)
    filtered_indices = task$row_ids
    
    for (i in seq_len(nrow(strata))) {
      original_indices = unlist(strata$row_id[i])
      indices = which(filtered_indices %in% original_indices)
      foldid[indices] = sample(rep(1:nfolds, length.out = length(indices)))
    }
  } else {
    foldid = sample(rep(1:nfolds, length.out = nrows))
  }

  penalty_factor = rep(1, task$n_features)
  names(penalty_factor) = task$feature_names
  
  res = list(foldid = foldid, penalty.factor = penalty_factor)
  traceit("--- create_cvglmnet_extra_args() ENDS with result", res)
  
  return(res)
}
