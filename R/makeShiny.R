
#' Generate a Ready-to-Run Shiny Application Directory
#'
#' @description
#' This function prepares a self-contained directory structure for a Shiny
#' application designed for drug screen analysis. It processes the input data,
#' bundles it, and includes the necessary Shiny application script (`shinyAPP.R`).
#'
#' The resulting 'shiny' directory contains everything needed to launch an
#' interactive web interface. This interface provides
#' comprehensive access to the `DrugScreenExplorer` package functionality through
#' an intuitive graphical user interface (GUI), enabling analysis workflows
#' from data import to response effect calculation via point-and-click.
#'
#' @param rawDir A character string specifying the path to the directory
#'   containing the raw experimental data files.
#' @param plateAnnotationFile A character string specifying the path to the
#'   plate annotation file (e.g., a TSV or CSV file).
#' @param wellAnnotationFile A character string specifying the path to the
#'   well annotation file (e.g., a TSV or CSV file).
#' @param dir A character string specifying the path to the **parent** directory
#'   where the 'shiny' application directory should be created. Defaults to the
#'   current working directory (`.`).
#'
#' @return
#' This function creates a new directory named 'shiny' inside the directory
#' specified by the `dir` argument. This 'shiny' directory will contain:
#' \itemize{
#'   \item \code{shinyAPP.R}: The R script required to launch the Shiny application.
#'   \item \code{ShinyData.zip}: A compressed archive containing the structured
#'     data processed from the inputs, ready for use by the Shiny application.
#' }
#'
#' @export makeShiny
#'
#' @examples
#'

#' \donttest{
#'
#'  # Create a temporary directory for the output
#'  temp_output_dir <- tempdir()
#'
#'  # Get example data file paths from the package
#'  rawFolder <- system.file('testData/rawData',
#'                           package = 'DrugScreenExplorer')
#'  plateFile <- system.file('testData/plateAnno/plateAnno.tsv',
#'                          package = 'DrugScreenExplorer')
#'  wellFile  <- system.file('testData/wellAnno/wellAnno.tsv',
#'                          package = 'DrugScreenExplorer')
#'
#'  # Check if example data exists before running
#'  makeShiny(rawDir = rawFolder,
#'            plateAnnotationFile = plateFile,
#'            wellAnnotationFile = wellFile,
#'            dir = temp_output_dir)
#' }
#'
#' @importFrom fs path dir_exists file_exists dir_create file_copy path_abs

makeShiny <- function(rawDir,
                      plateAnnotationFile,
                      wellAnnotationFile,
                      dir = ".") {

    # --- Input Validation ---
    stopifnot(
        "`rawDir` must be a character string." =
            is.character(rawDir), length(rawDir) == 1,
        "`plateAnnotationFile` must be a character string." =
            is.character(plateAnnotationFile), length(plateAnnotationFile) == 1,
        "`wellAnnotationFile` must be a character string." =
            is.character(wellAnnotationFile), length(wellAnnotationFile) == 1,
        "`dir` must be a character string." =
            is.character(dir), length(dir) == 1
    )

    # Use absolute paths for clearer error messages if needed
    rawDir <- fs::path_abs(rawDir)
    plateAnnotationFile <- fs::path_abs(plateAnnotationFile)
    wellAnnotationFile <- fs::path_abs(wellAnnotationFile)
    outputDir <- fs::path_abs(dir)

    # Check existence of inputs
    if (!fs::dir_exists(rawDir)) {
        stop("Specified `rawDir` does not exist: ", rawDir)
    }
    if (!fs::file_exists(plateAnnotationFile)) {
        stop("Specified `plateAnnotationFile` does not exist: ", plateAnnotationFile)
    }
    if (!fs::file_exists(wellAnnotationFile)) {
        stop("Specified `wellAnnotationFile` does not exist: ", wellAnnotationFile)
    }
    if (!fs::dir_exists(outputDir)) {
        stop("Specified output directory `dir` does not exist: ", outputDir)
    }


    # --- Directory Setup ---
    # Define the target shiny directory path
    shiny_dir_path <- fs::path(outputDir, "shiny")

    # Create the shiny directory (fs::dir_create doesn't warn if it exists)
    fs::dir_create(shiny_dir_path)

    # Call the function to process data and create the zip file
    # Assuming createShinyData handles zipping internally and saves 'ShinyData.zip'
    createShinyData(
        rawDir = rawDir,
        plateAnnotationFile = plateAnnotationFile,
        wellAnnotationFile = wellAnnotationFile,
        dir = shiny_dir_path, # Save directly into the shiny dir
        filename = "ShinyData" # Base name, assuming .zip is added by createShinyData
    )

    # Check if the data zip file was created successfully
    expected_zip_file <- fs::path(shiny_dir_path, "ShinyData.zip")
    if (!fs::file_exists(expected_zip_file)) {
        stop("Failed to create the Shiny data zip file: ", expected_zip_file)
    }

    # --- Copy Shiny App Template ---
    # Locate the template Shiny app script within the package
    template_shiny_script <- system.file("shinyAPP.R",
                                         package = "DrugScreenExplorer")

    if (template_shiny_script == "" || !fs::file_exists(template_shiny_script)) {
        stop("Could not find the shinyAPP.
         R template within the DrugScreenExplorer package.")
    }

    # Define the destination path for the script
    destination_script_path <- fs::path(shiny_dir_path, "shinyAPP.R")

    # Copy the template script
    fs::file_copy(template_shiny_script,
                  destination_script_path,
                  overwrite = TRUE)

    # --- Final Message ---
    message("Successfully created Shiny application directory at: \n",
            shiny_dir_path)

    # --- Return ---
    # Return NULL invisibly as the function is used for its side effects
    invisible(NULL)
}




