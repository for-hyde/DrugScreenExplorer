
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

  if (!is.character(dir) || length(dir) != 1 || is.na(dir) ||
      !dir.exists(dir)) {
    stop("dir must be an existing directory")
  }

  report_dir <- normalizePath(file.path(dir, "report"),
                              winslash = "/", mustWork = FALSE)
  dir.create(file.path(report_dir, "plots"), recursive = TRUE,
             showWarnings = FALSE)

  # save the screenData object to a file
  saveRDS(screenData, file = file.path(report_dir, "screenData.rds"),
          compress = TRUE)

  # Make Pandoc available when RStudio's bundled binary is not on PATH.
  pandoc_dir <- Sys.getenv("RSTUDIO_PANDOC", unset = "")
  if (!nzchar(pandoc_dir) || !file.exists(file.path(pandoc_dir, "pandoc"))) {
    pandoc_path <- Sys.which("pandoc")
    if (nzchar(pandoc_path)) {
      pandoc_dir <- dirname(pandoc_path)
    } else {
      machine_dir <- if (identical(Sys.info()[["machine"]], "arm64")) {
        "aarch64"
      } else {
        "x86_64"
      }
      rstudio_pandoc <- file.path(
        "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
        machine_dir
      )
      if (file.exists(file.path(rstudio_pandoc, "pandoc"))) {
        pandoc_dir <- rstudio_pandoc
      }
    }
  }

  if (nzchar(pandoc_dir) && file.exists(file.path(pandoc_dir, "pandoc"))) {
    old_rstudio_pandoc <- Sys.getenv("RSTUDIO_PANDOC", unset = NA_character_)
    Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
    if (is.na(old_rstudio_pandoc)) {
      on.exit(Sys.unsetenv("RSTUDIO_PANDOC"), add = TRUE)
    } else {
      on.exit(Sys.setenv(RSTUDIO_PANDOC = old_rstudio_pandoc), add = TRUE)
    }
    rmarkdown::find_pandoc(dir = pandoc_dir, cache = FALSE)
  }

  if (!rmarkdown::pandoc_available()) {
    stop(
      "Pandoc was not found. Install Pandoc or set RSTUDIO_PANDOC to its directory."
    )
  }

  # template file
  tempRmd <- system.file("template.Rmd", package = "DrugScreenExplorer")
  if (!nzchar(tempRmd) || !file.exists(tempRmd)) {
    stop("The package report template could not be found")
  }
  report_rmd <- file.path(report_dir, "report.Rmd")
  file.copy(tempRmd, report_rmd, overwrite = TRUE)

  # render html with parameters
  rmarkdown::render(
    input = report_rmd,
    output_file = "report.html",
    output_dir = report_dir,
    knit_root_dir = report_dir,
    params = list(showCode = showCode,
                  set_title = title,
                  set_author = author,
                  ifPlatePlot = ifPlatePlot),
    quiet = TRUE
  )

  report_file <- file.path(report_dir, "report.html")
  if (!file.exists(report_file)) {
    stop("Report rendering completed without creating report.html")
  }

  invisible(report_file)
}
