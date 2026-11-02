#'
#' Read and assemble a drug screen dataset
#'
#' This function creates a data frame that contains the screening result from
#' a whole drug screen experiment. Drugs and samples are annotated based on
#' the drug and sample annotation files.
#'
#' @param rawDir a character string specifying the location of the raw data
#' folder.
#' @param plateAnnotationFile a character string specifying the location of
#' the plate annotation file.
#' @param wellAnnotationFile a character string specifying the location of the
#' well annotation file.
#' @param rowRange a numeric vector of length two, specifying the range of the
#' rows (separated by delimiters) that contain the actual values in the raw data
#' files. The default value is NULL, which means the function will automatically
#' detect the row range. If the user wants to manually set the row range, they
#' can provide a numeric vector of length two, indicating the starting and ending
#' rows of the actual values.
#' @param colRange a numeric vector of length one or two, specifying the range
#' of the columns (separated by delimiters) that contain the actual values in
#' the raw data files. The default value is NULL, which means the function will
#' automatically detect the column range.
#' @param sep a character string specifying the delimiters for extracting values
#' in the text file. If automatic parsing is used, the function will automatically
#' detect the delimiters. The default value is a semicolon (";").
#' @param setManualRanges a logical value, whether to manually set the row and
#' column ranges for extracting the actual values from the raw data files.
#' The default value is \code{FALSE}. If \code{TRUE}, the user MUST provide the
#' row and column ranges using the \code{rowRange} and \code{colRange} arguments.
#' @param negWell a character vector or a character string of well IDs or drug
#' names (if the 'name' column is present in well annotation), which specifies
#' the negative control wells
#' @param posWell a character vector or a character string of well IDs or drug
#' names (if the 'name' column is present in well annotation), which specifies
#' the positive control wells
#' @param normalization a logical value to indicate whether to calculate percent
#' inhibition based on control wells on the plate. Default is FALSE.
#' @param method a character string, either 'negatives' or 'NPI',
#' specifying the method for calculating percent inhibition values. If
#' 'negatives', each measurement on the plate will be divided by the median of
#' the values of negative control wells on the same plate. If 'NPI', a
#' 'normalized percent inhibition (NPI)' is applied in a per-plate basis.
#' For an inhibition assay, this method divides the difference between each
#' measurement on a plate and the median measurement of the positive controls
#' on that plate by the difference between the median measurement of negative
#' controls and positive controls on that well. The default value is 'negatives'.
#' @param discardLayer an integer value. To avoid the impact of incubation
#' effect on the negative controls, a certain number of the exterior wells on
#' the plate can be ignored when summarising the measurement of controls.
#' 0 means do not discard any wells. A value above zero indicates
#' \code{N} layers of wells from the edge should be discarded.
#' @param commaAsDecimal a logical value, whether commas is used as decimal
#' in the input data. Default is FALSE.
#' @param allowNA a logical value, whether to allow NA (missing or empty) values
#' in the input data. Default is FALSE.
#' @param bySample a logical value, indicates whether to normalize the viability
#' by sample rather than by plate. This is for situations where multiple samples
#' are on the same plate.
#' @import tibble dplyr readr stringr tools
#' @importFrom stringr str_detect
#' @export
#' @return A data frame with each row represent a well on a plate and the
#' following columns:
#' \item{wellID}{well identifier}
#' \item{rowID}{row identifier}
#' \item{colID}{column identifier}
#' \item{value}{raw signal intensity from a plate reader}
#' \item{fileName}{input files names (without extension names) for the plates}
#' \item{drug}{drug name, if this column is present in the well annotation file}
#' \item{concentration}{drug concentration, if this column is present in the
#' well annotation file}
#' \item{wellType}{the type of wells, can be either neg (negative controls),
#' pos (positive controls), or sample (drug wells)}
#' \item{normVal}{normalized viability based on control wells on the plate,
#' if the normalization step is performed by setting \code{normalization = TRUE}}
#' Depends on the user-specified plate and well annotations, other columns,
#' such as sampleID, patientID, batch and so on, in the annotation files
#' can also be present in the output table.
#' @examples
#' rawFolder <- system.file('testData/rawData',
#'                          package = 'DrugScreenExplorer')
#' plateFile <- system.file('testData/plateAnno/plateAnno.tsv',
#'                          package = 'DrugScreenExplorer')
#' wellFile  <- system.file('testData/wellAnno/wellAnno.tsv',
#'                          package = 'DrugScreenExplorer')
#' screenData <- readScreen(rawDir = rawFolder,
#'                         plateAnnotationFile = plateFile,
#'                         wellAnnotationFile = wellFile,
#'                         rowRange = c(3,18), colRange = 2,
#'                         sep = '[;\t]', commaAsDecimal = FALSE,
#'                         negWell = c('DMSO','PBS'), posWell = c())
#'
#' # Example with combined data
#' rawFolder <- system.file('testData/rawDataCombine',
#'                          package = 'DrugScreenExplorer')
#' plateFile <- system.file('testData/plateAnnoCombine/plateAnnoCombine.tsv',
#'                          package = 'DrugScreenExplorer')
#' wellFile  <- system.file('testData/wellAnnoCombine/wellAnnoCombine.tsv',
#'                          package = 'DrugScreenExplorer')
#' screenData <- readScreen(rawDir = rawFolder,
#'                          plateAnnotationFile = plateFile,
#'                          wellAnnotationFile = wellFile,
#'                          rowRange = c(7, 22),
#'                          colRange = 2,
#'                          sep = ',', commaAsDecimal = FALSE,
#'                          negWell = c("DMSO_DMSO"),
#'                          posWell = c())
#' # see vignette for more instructions


readScreen <- function(rawDir,
                       plateAnnotationFile,
                       wellAnnotationFile,
                       sep=";",
                       setManualRanges = FALSE,
                       rowRange = NULL,
                       colRange = NULL,
                       negWell = c(),
                       posWell = c(),
                       normalization = FALSE,
                       method = "negatives",
                       discardLayer = 0,
                       commaAsDecimal = FALSE,
                       allowNA = FALSE,
                       bySample = FALSE) {
  #Input validation
  checkmate::assert_character(rawDir, len = 1, any.missing = FALSE)
  checkmate::assert_directory_exists(rawDir, access = "r")
  checkmate::assert_character(plateAnnotationFile, len = 1, any.missing = FALSE)
  checkmate::assert_file_exists(plateAnnotationFile, access = "r")
  checkmate::assert_character(wellAnnotationFile, len = 1, any.missing = FALSE)
  checkmate::assert_file_exists(wellAnnotationFile, access = "r")
  checkmate::assert_character(sep, len = 1, any.missing = FALSE)
  checkmate::assert_logical(setManualRanges, len = 1, any.missing = FALSE)
  if (setManualRanges) {
    checkmate::assert_integerish(rowRange, len = 2, lower = 1,
                                any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_integerish(colRange, min.len = 1, max.len = 2, lower = 1,
                                  any.missing = FALSE, null.ok = TRUE)
  }
  checkmate::assert_logical(normalization, len = 1, any.missing = FALSE)
  checkmate::assert_character(negWell, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_character(posWell, any.missing = FALSE, null.ok = TRUE)
  if (normalization) {
    checkmate::assert_choice(method, c("negatives", "NPI"))
    checkmate::assert_integerish(discardLayer, len = 1, lower = 0, any.missing = FALSE)
  }
  checkmate::assert_logical(commaAsDecimal, len = 1, any.missing = FALSE)
  checkmate::assert_logical(allowNA, len = 1, any.missing = FALSE)
  checkmate::assert_logical(bySample, len = 1, any.missing = FALSE)

  # if commaAsDecimal is TRUE, set decimal mark to ",", otherwise "."
  deciMark <- ifelse(commaAsDecimal, ",", ".")
  
  # Read plate annotation file, function in helper.R
  sampleAnno <- readPlateAnno(plateAnnotationFile, deciMark)
  rawFileList <- lapply(sampleAnno$fileName, function(x) paste0(rawDir, "/", x))
  
  # Read well annotation file, function in helper.R
  wellAnno <- readWellAnno(wellAnnotationFile, deciMark)
  # Plate size from well annotation file, function in helper.R
  wellDims <- getWellDims(wellAnno)
  
  # If a rowRange and column Range are provided, set setManualRanges to TRUE. 
  if (!(is.null(rowRange)) || !(is.null(colRange))) {
    setManualRanges <- validateRanges(rowRange, colRange)
  }
  
  if (setManualRanges) {
    screenData <- parseManual(rawFileList, rowRange, colRange, sep,
                              deciMark, wellDims)
    screenData <- checkManualHeaders(screenData)
  } else {
    screenData <- parseAutomatic(rawFileList, deciMark, wellDims)
    screenData <- addHeaderAndIndex(screenData, TRUE, TRUE)
  }
  # Pivot to long
  screenData <- convertPlateToLong(screenData)

  # Merge with sampleAnno
  screenData <- dplyr::left_join(screenData, sampleAnno, by = "fileName")
  # Remove ".txt" from fileName if present
  screenData <- dplyr::mutate(
    screenData, 
    fileName = tools::file_path_sans_ext(fileName),#remove endings from fileName,
    value = as.numeric(value), # convert value to numeric
    )
  # Merge with drugAnno
  screenData <- joinWellAnno(screenData, wellAnno, negWell, posWell)
  
  # Perform normalization if requested
  if (normalization){
    screenData <- normalizePlate(
      screenData,
      method = method,
      discardLayer = discardLayer,
      bySample = bySample
    )
  }
  
  return(screenData)
}
