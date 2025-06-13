#' @name runShiny
#'
#' @title Launch the DrugScreenExplorer Shiny Application
#'
#' @description
#' \code{runShiny} launches the Shiny Application for DrugScreenExplorer.
#' This interactive web interface provides comprehensive access to all package
#' functionality through an intuitive graphical user interface. Designed for users
#' of all skill levels, the application enables complete analysis of drug screening
#' experiments - from data import to response effect calculation - through a
#' streamlined point-and-click workflow.
#'
#' @return The function starts up the Shiny application.
#'
#' @details
#' The function searches for the Shiny application files in the installed package
#' directory. If the application cannot be found, try reinstalling the package.
#'
#' @export runShiny
#' @examples
#' # Launch the Shiny application:
#' # runDrugScreenExplorer()

runShiny <- function() {

  # Get the path to the Shiny application directory within the installed package
  appDir <- system.file("shinyAPP.R", package = "DrugScreenExplorer")

  # Check if the application directory was found
  if (appDir == "") {
    # If not found, stop with an informative error message
    stop(
      "Could not find Shiny application directory. ",
      "Try re-installing 'DrugScreenExplorer'.",
      call. = FALSE
    )
  }

  # Run the Shiny application from the found directory in normal display mode
  shiny::runApp(appDir, display.mode = "normal")
}


#' Prepare Data for Shiny Application
#'
#' @description
#' This function prepares the data used in the Shiny Application. It gathers raw
#' data files, plate annotation, and well annotation files, organizes them into
#' a specific directory structure within a temporary location, and then compresses
#' this structure into a zip archive.
#'
#' @param rawDir a character string specifying the location of the raw data folder.
#' @param plateAnnotationFile a character string specifying the location of the
#' plate annotation file.
#' @param wellAnnotationFile a character string specifying the location of the
#' well annotation file.
#' @param dir A character string specifying the path to the output directory
#' where the final zip file will be saved. Defaults to the current working
#' directory (`.`).
#' @param filename a character string specify the zipped file name. defaulted as
#' "ShinyData"
#'
#' @return NULL (invisibly). The function primarily works via side effects,
#' creating a zip file in the specified output directory (`dir`).
#' It prints messages to the console indicating the progress and success/failure
#' of file copying operations.
#'
#' @importFrom zip zip
#' @export createShinyData
#' @examples
#' rawFolder <- system.file('testData/rawData',
#'                           package = 'DrugScreenExplorer')
#' plateFile <- system.file('testData/plateAnno/plateAnno.tsv',
#'                          package = 'DrugScreenExplorer')
#' wellFile  <- system.file('testData/wellAnno/wellAnno.tsv',
#'                          package = 'DrugScreenExplorer')
#' createShinyData(rawDir = rawFolder,
#'                 plateAnnotationFile = plateFile,
#'                 wellAnnotationFile = wellFile)
#' # return a zipped file names as "ShinyData.zip" in the working directory
#'
createShinyData <- function(rawDir,
                            plateAnnotationFile,
                            wellAnnotationFile,
                            dir = ".",
                            filename = "ShinyData") {

  temp <- tempdir()

  # generate file structure
  dir.create(file.path(temp, "/plateAnno"))
  dir.create(file.path(temp, "/wellAnno"))
  dir.create(file.path(temp, "/rawData"))

  # copy all files in rawDir to new directory
  allFiles <- list.files(rawDir, full.names = TRUE)
  # Check if there are any files to copy
  if (length(allFiles) > 0) {
    # Copy all files from rawDir to the new folder in rawDat
    for(file in allFiles) {
      file.copy(from = file,
                to = file.path(temp, "/rawData"),
                overwrite = TRUE,
                recursive = TRUE)
    }
    cat(paste0("Successfully copied files from: \n", rawDir, "\n"))
  } else {
    cat(paste0("No files found in: ", rawDir, "\n"))
  }

  # copy plateAnnotationFile
  ifexist <- file.exists(plateAnnotationFile)
  if (ifexist) {
    file.copy(from = plateAnnotationFile,
              to = file.path(temp, "/plateAnno"),
              overwrite = TRUE,
              recursive = TRUE)
    cat(paste0("Successfully copied file from: \n",
               plateAnnotationFile))
  } else {
    cat(paste0("No files found in: ", plateAnnotationFile, "\n"))
  }

  # copy wellAnnotationFile
  ifexist <- file.exists(wellAnnotationFile)
  if (ifexist) {
    file.copy(from = wellAnnotationFile,
              to = file.path(temp, "/wellAnno"),
              overwrite = TRUE,
              recursive = TRUE)
    cat(paste0("\n Successfully copied file from: \n",
               wellAnnotationFile))
  } else {
    cat(paste0("No files found in: ", wellAnnotationFile, "\n"))
  }

  # compress the profile
  temp_dir <- getwd()
  setwd(temp)
  zip::zip(paste0(filename, ".zip"),
           files = c("plateAnno/", "wellAnno/", "rawData/"),
           mode = "mirror")
  setwd(temp_dir)

  # remove zip to working directory
  file.copy(from = file.path(temp, paste0(filename, ".zip")),
            to = file.path(dir, paste0(filename, ".zip")))
  file.remove(file.path(temp, paste0(filename, ".zip")))

  # delete all generated profiles in the temp directory
  unlink(file.path(temp, "plateAnno/"), recursive = TRUE)
  unlink(file.path(temp, "wellAnno/"), recursive = TRUE)
  unlink(file.path(temp, "rawData/"), recursive = TRUE)

}


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
#' # Create a temporary directory for the output
#' temp_output_dir <- tempdir()
#'
#' # Get example data file paths from the package
#' rawFolder <- system.file('testData/rawData', package = 'DrugScreenExplorer')
#' plateFile <- system.file('testData/plateAnno/plateAnno.tsv',
#'                          package = 'DrugScreenExplorer')
#' wellFile  <- system.file('testData/wellAnno/wellAnno.tsv',
#'                          package = 'DrugScreenExplorer')
#'
#' # Check if example data exists before running
#' makeShiny(rawDir = rawFolder,
#'            plateAnnotationFile = plateFile,
#'            wellAnnotationFile = wellFile,
#'            dir = temp_output_dir)
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






