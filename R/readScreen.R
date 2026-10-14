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
#' @param rowRange either an integer vector with length of two or an integer
#' value.
#' If it's an vector, it should specify the range of the lines that contain
#' the actual values.
#' If it's an integer values, only the starting line is indicated and the end
#' line is the end of the file.
#' @param colRange either an integer vector with length of two or an integer
#' value.
#' If it's an vector, it should specify the range of the columns (separated by
#' delimiters) that contain the actual values.
#' If it's an integer, only the starting column is indicated and the end column
#' is the last column in the file.
#' @param sep a character string specifying the delimiters for extracting values
#' in the text file.
#' A regular expression can be used. The default value is using either tab or
#' semicolon as delimiter.
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
#' @param discardLayer an integer values. To avoid the impact of incubation
#' effect on the negative controls, a certain number of the exterior wells on
#' the plate can be ignored when summarising the measurement of controls.
#' 0 means do not discard any control wells. A value above zero indicates
#' the \code{N} layers of wells on the edge should be discarded.
#' @param commaAsDecimal a logical value, whether commas is used as decimal
#' in the input data. Default is FALSE.
#' @param allowNA a logical value, whether to allow NA (missing or empty) values
#' in the input data. Default is FALSE.
#' @param bySample a logic value, indicates whether to normalize the viability
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
                       sep,
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

  # argument matching
  # If normalization == TRUE, perform checks.
  if (normalization) {
    method <- match.arg(method, c("negatives", "NPI"))
    stopifnot(is.numeric(discardLayer))
  }

  # read sample annotation files
  deciMark <- ifelse(commaAsDecimal, ",", ".")

  # Read plate annotation file
  sampleAnno  <- read_delim(
    plateAnnotationFile,
    delim = "\t",
    locale = locale(decimal_mark = deciMark),
    col_types = cols()
  )
  # Check all names are present in the plate annotation file
  if (!all(c("sampleID", "fileName") %in% colnames(sampleAnno))) {
    stop("The plate annotation file must contain the following columns: sampleID and fileName")
  }

  rawFileList <- sampleAnno$fileName

  # check if there are duplicated file names
  allFiles <- vapply(rawFileList, basename, "c")
  if (any(duplicated(allFiles))){
    stop("Duplicated file names found")
  }

  # if rowRange and colRange are not provided, set setManualRanges = TRUE to prompt user to provide them
  if (!(is.null(rowRange)) || !(is.null(colRange))) {
    #validate that rowRange and colRange are formatted correctly.
    #Both must be vector of length 2 with a starting and ending value. 
    if (length(rowRange) != 2 || length(colRange) != 2) {
      stop("rowRange and colRange must be vectors of length 2, specifying the starting and ending values.")
    }
    setManualRanges <- TRUE
  }

  # If setManualRanges = TRUE, ensure rowRange and colRange are provided 
  if (setManualRanges) {
    if (is.null(rowRange) || is.null(colRange)) {
      stop("rowRange and colRange must be provided when setManualRanges is TRUE")
    }
    screenData <- lapply(rawFileList, function(eachFile) {
      plateData <- tryCatch({
        dt <- data.table::fread(
          paste0(rawDir,"/",eachFile),
          sep = sep,
          dec = deciMark,
          skip = rowRange[1] - 1,
          nrows = rowRange[2] - rowRange[1] + 1,
          select = colRange[1]:colRange[2],
        )
        convert_plate_to_long(dt) %>% 
          dplyr::mutate(fileName = eachFile)
      }, error = function(e) {
        # Just modify the error message without creating a new error
        e$message <- sprintf("Error reading %s: %s", eachFile, e$message)
        stop(e)
      }) 
    #Automatic working
    }) %>% bind_rows()
    } else {#If setManualRanges = False, attempt to automatically detect the data. 
        screenData <- lapply(rawFileList, function(eachFile) {
          plateData <- tryCatch({
            extract_plate_block(
              file_path = paste0(rawDir,"/",eachFile)
            ) %>% dplyr::mutate(fileName = eachFile)
          }, error = function(e){
            e$message <- sprintf("Error reading %s: %s", eachFile, e$message)
            stop(e)
          })

        }) %>% bind_rows()

    }
  # add sample annotations to screenData
  screenData <- left_join(screenData, sampleAnno, by = "fileName")

  # read drug annotation files
  drugAnno <- read_delim(
    wellAnnotationFile,
    delim = "\t",
    locale = locale(decimal_mark = deciMark),
    col_types = cols()
  )

  # detect two-drug combination
  # select colnames containing "drug1", but not "drug1a"
  ind <- str_detect(colnames(screenData), "^drug\\d$")

  # only two drug combination allowed
  if (sum(ind) > 1)
    stop("More than two drugs are detected:
         a maximum two-drug combination is allowed.")

  # if the second drug is not named as "drug2", rename it to drug2
  if (sum(ind) == 1 & !"drug2" %in% colnames(screenData)) {
    warning("The second drug is renamed as 'drug2'. ")
    colnames(screenData)[ind] <- "drug2"
  }

  # select the second concentration in a similar format "^concentration\\d$
  ind <- str_detect(colnames(screenData), "^concentration\\d$")

  # only two drug combination allowed
  if (sum(ind) > 1)
    stop("More than two concentrations are detected:
         a maximum two-drug combination is allowed.")

  if (sum(ind) == 1 & !"concentration2" %in% colnames(screenData)) {
    warning("The concentration for the second drug is renamed as 'concentration2'.")
    colnames(screenData) <- "concentration2"
  }

  #
  # if combination drug
  # if ("drug2" %in% colnames(drugAnno))
  #   drugAnno[,"combination"] <- paste0(drugAnno$drug, "_", drugAnno$drug2)

  # add drug annotation information
  if ("plateID" %in% colnames(drugAnno)) {
    screenData <- left_join(screenData, drugAnno, by = c("wellID", "plateID"))
  } else {
    screenData <- left_join(screenData, drugAnno, by = "wellID")
  }

  # add well type information
  if (length(intersect(negWell, posWell)) != 0){
    stop("Overlap in postive and negative control well list")
  }

  # Can I remove wellID here?

  # Scenario for drug combination
  if ("drug2" %in% colnames(screenData)){

    # Both drug_drug2 amd drug2_drug work
    combs <- cbind(paste0(drugAnno$drug, "_", drugAnno$drug2),
                   paste0(drugAnno$drug2, "_", drugAnno$drug))

    screenData <-
      dplyr::mutate(screenData,
                    wellType = case_when(
                      (wellID %in% negWell | combs[,1] %in% negWell |
                         combs[,2] %in% negWell) ~ "neg",
                      (wellID %in% posWell | combs[,1] %in% posWell |
                         combs[,2] %in% posWell) ~ "pos",
                      .default = "sample"))
  } else if ("drug" %in% colnames(screenData)) {
    screenData <- dplyr::mutate(
      screenData,
      wellType = case_when(wellID %in% negWell | drug %in% negWell ~ "neg",
                           wellID %in% posWell | drug %in% posWell ~ "pos",
                           .default = "sample"))
  } else {
    screenData <-
      dplyr::mutate(screenData,
                    wellType = case_when(wellID %in% negWell ~ "neg",
                                         wellID %in% posWell ~ "pos",
                                         .default = "sample"))
  }

  if (normalization) {
    # normalizing plates
    screenData <- normalizePlate(
      screenData = screenData,
      method = method,
      discardLayer = discardLayer,
      bySample = bySample)
  }

  if ("batch" %in% colnames(screenData)) {
    # batch as factor
    screenData$batch <- factor(screenData$batch)
  }

  # transfer concentration to numeric values
  if ("concentration" %in% colnames(screenData)) {
    # concentration as numeric
    screenData$concentration <-
      as.numeric(as.character(screenData$concentration))
    if ("concentration2" %in% colnames(screenData)){
      screenData$concentration2 <-
        as.numeric(as.character(screenData$concentration2))
    }
  }

  screenData <- ungroup(screenData)  #%>% mutate_if(is.character, factor)

  # clean up file name
  screenData <- screenData %>%
    dplyr::mutate(fileName = basename(fileName)) %>%
    dplyr::mutate(fileName = tools::file_path_sans_ext(fileName))

  return(screenData)
}







