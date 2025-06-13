
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
                       ifPlatePlot = FALSE) {

  # create a directoy for report
  dir.create(file.path(dir, "report"), showWarnings = FALSE)
  dir.create(file.path(dir, "report/plots"), showWarnings = FALSE)

  # save the screenData object to a file
  saveRDS(screenData, file = file.path(dir, "report/screenData.rds"),
          compress = TRUE)

  # template file
  tempRmd <- system.file("template.Rmd", package = "DrugScreenExplorer")
  file.copy(tempRmd, file.path(dir, "report/report.Rmd"),
            overwrite = TRUE)

  # render html with parameters
  rmarkdown::render(
    input = file.path(dir, "report/report.Rmd"), output_file = "report.html",
    params = list(showCode = showCode,
                  set_title = title,
                  set_author = author,
                  ifPlatePlot = ifPlatePlot), quiet = TRUE)
}




