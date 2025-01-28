#' Run and List Example Scripts
#'
#' This function lists available example scripts or renders a specified script
#' as HTML using the `spin` function from the knitr package. It also optionally
#' generates an R script from the rendered R Markdown file using the `purl` function.
#' The HTML content, if generated, is automatically opened in the default web browser.
#'
#' @param script_prefix A character string representing the beginning of the
#'   script's name. If NULL, the function lists all available scripts.
#' @param purl A logical value indicating whether to generate an R script from the
#'   R Markdown file. If TRUE, a script is created with a "-purl" postfix. Defaults to FALSE.
#' @return If no prefix is given, returns a character vector of available script
#'   names. If a valid prefix is provided and the script is rendered, opens the
#'   HTML content in a web browser and returns the path to the HTML file.
#' @importFrom knitr spin purl
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#' @examples
#' # List all available example scripts
#' run_example()
#'
#' # Run and render an example script as HTML and open it in the default web browser
#' # Assuming a script named "01*.R" exists
#' \dontrun{
#' run_example("01")
#'
#' # Run and render an example script and also generate a purled R script
#' run_example("01", purl = TRUE)
#' }
#' @export
run_example <- function(script_prefix = NULL, purl = FALSE) {
  examples_dir <- system.file("examples", package = "mlr3SurvUtils")
  temp_dir <- tempdir() # Use a temporary directory for file generation
  
  # List all scripts if no prefix is provided
  if (is.null(script_prefix)) {
    scripts <- list.files(examples_dir, pattern = "^\\d{2}-.*\\.R$", full.names = FALSE)
    if (length(scripts) == 0) {
      message("No example scripts found.")
      return(character(0))
    }
    return(scripts)
  }

  # Render specific script
  scripts <- list.files(examples_dir, pattern = paste0("^", script_prefix, ".*\\.R$"), full.names = TRUE)
  
  if (length(scripts) == 0) {
    stop("No matching script found.")
  }
  
  if (length(scripts) > 1) {
    message("Multiple scripts match the given prefix; using the first one.")
    scripts <- scripts[1]
  }
  
  # Ensure knitr and rmarkdown are available
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("The 'knitr' package is required but not installed.")
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("The 'rmarkdown' package is required but not installed.")
  }
  
  # Spin the R script into an R Markdown file in a temporary directory
  rmd_file <- knitr::spin(scripts, knit = FALSE)
  rmd_file_temp <- file.path(temp_dir, basename(rmd_file))
  file.rename(rmd_file, rmd_file_temp)

  # Render the file to HTML in a temporary directory
  html_file <- rmarkdown::render(rmd_file_temp, output_format = "html_document", quiet = TRUE, output_dir = temp_dir)
  
  # Purl the spun R Markdown file if purl is TRUE
  if (purl) {
    purl_file <- sub("\\.Rmd$", "-purl.R", basename(rmd_file_temp))
    purl_file_temp <- file.path(temp_dir, purl_file)
    knitr::purl(rmd_file_temp, output = purl_file_temp, quiet = TRUE)
    message("Purled script created: ", purl_file_temp)
  }
  
  # Open the HTML file in the default web browser
  utils::browseURL(html_file)

  # Return the path to the HTML file
  return(html_file)
}
