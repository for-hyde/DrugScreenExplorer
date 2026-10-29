#' Generate a quality report of the screen data
#'
#' This function generates a customized Rmarkdown based html report for
#' priliminary quality assurance.
#'
#' @param screenData a data table that contains the screen results generated
#' by \code{readScreen()} function.
#' @param dir a character string specifying the directory in which the report
#' will be generated. By default, the report is created in the current working
#' directory.
#' @param showCode if \code{TRUE}, the generated HTML report will include the
#' underlying R code. Default is \code{FALSE}.
#' @param title a character string specifying the title for the html report
#' @param author a character string specifying the author name
#' @param ifPlatePlot Logical; if \code{TRUE}, the report includes plate-level
#' heatmap plots. Note that enabling heatmap plotting can significantly increase
#' rendering time for large datasets. Default is \code{FALSE}.
#' @export makeReport
#' @import BiocStyle tidyr dplyr gridExtra ggplot2  tools
#' @return An HTML report named \code{report.html} created within a subdirectory
#' named \code{report} inside the specified (or default) directory.
#' @examples
#' # load processed data
#' data(screenData_normalized)
#'
#' # make html report
#' makeReport(screenData_normalized, showCode = TRUE,
#'            title = 'Report example',
#'            author = 'Author Name', ifPlatePlot = FALSE)
#'

makeReport <- function(screenData,
                       dir = ".",
                       showCode = FALSE,
                       title = "Report for my drug screening project",
                       author = "Author Name",
                       ifPlatePlot = FALSE,
                       reportDate = Sys.Date()) {
  #input validation
  checkmate::assert_data_frame(screenData)
  checkmate::assert_names(names(screenData),
                          must.include = c("sampleID", "fileName", "wellType",
                                           "value", "rowID", "colID", "normVal"))
  checkmate::assert_path_for_output(dir, overwrite = TRUE)
  checkmate::assert_logical(showCode, len = 1)
  checkmate::assert_string(title)
  checkmate::assert_string(author)
  checkmate::assert_logical(ifPlatePlot, len = 1)
  checkmate::assert_date(reportDate, len = 1)
  report_dir <- normalizePath(file.path(dir, paste0("report_", reportDate)),
                              winslash = "/", mustWork = FALSE)
  # Create the report directory if it doesn't exist
  dir.create(file.path(report_dir, "plots"), recursive = TRUE,
             showWarnings = FALSE)
  # save the screenData object to a file in report directory
  saveRDS(screenData, file = file.path(report_dir, "screenData.rds"),
          compress = TRUE)
  # copy the template Rmd file to the report directory
  temp_rmd <- system.file("template.Rmd", package = "DrugScreenExplorer")
  if (!nzchar(temp_rmd) || !file.exists(temp_rmd)) {
    stop("The package report template could not be found")
  }
  report_rmd <- file.path(report_dir, "report.Rmd")
  file.copy(temp_rmd, report_rmd, overwrite = TRUE)

  # render html with parameters
  rmarkdown::render(
    input = report_rmd,
    output_file = "report.html",
    output_dir = report_dir,
    knit_root_dir = report_dir,
    params = list(showCode = showCode,
                  set_title = title,
                  set_author = author,
                  ifPlatePlot = ifPlatePlot,
                  set_date = reportDate),
    quiet = TRUE
  )

  report_file <- file.path(report_dir, "report.html")
  if (!file.exists(report_file)) {
    stop("Report rendering completed without creating report.html")
  }

  invisible(report_file)
}
