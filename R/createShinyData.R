
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
        message("Successfully copied files from: \n", rawDir, "\n")
    } else {
        message("No files found in: ", rawDir, "\n")
    }
    
    # copy plateAnnotationFile
    ifexist <- file.exists(plateAnnotationFile)
    if (ifexist) {
        file.copy(from = plateAnnotationFile,
                  to = file.path(temp, "/plateAnno"),
                  overwrite = TRUE,
                  recursive = TRUE)
        message("Successfully copied file from: \n", plateAnnotationFile)
    } else {
        message("No files found in: ", plateAnnotationFile, "\n")
    }
    
    # copy wellAnnotationFile
    ifexist <- file.exists(wellAnnotationFile)
    if (ifexist) {
        file.copy(from = wellAnnotationFile,
                  to = file.path(temp, "/wellAnno"),
                  overwrite = TRUE,
                  recursive = TRUE)
        message("\n Successfully copied file from: \n", wellAnnotationFile)
    } else {
        message("No files found in: ", wellAnnotationFile, "\n")
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







