#' Trace Function for Debugging
#'
#' This internal function prints trace messages to the console if tracing is enabled. It is designed to 
#' help with debugging by outputting specified messages and, optionally, the structure of a given variable.
#'
#' @param msg A character string representing the message to be printed.
#' @param variable An optional variable whose structure is printed to help with debugging; defaults to `NULL`.
#'
#' @details
#' The function is controlled by the `traceon` flag, which is set elsewhere in the code. When `traceon` is `TRUE`,
#' the function uses `cat` to print the provided message. If a variable is also provided, it prints the variable 
#' using `print()`. This is particularly useful for tracing control flow and inspecting variable states during development.
#'
#' @keywords internal
traceit <- function(msg, variable = NULL) {
  if (isTRUE(traceon)) {
    cat(msg, "\n")
    if (!is.null(variable)) {
      print(variable)
    }
  }
}