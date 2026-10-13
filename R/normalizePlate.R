#' Per-plate normalization
#'
#' This function calculates the percent inhibition or normalized viability values
#' for each plate based on the control wells on the same plate.
#'
#' @param screenData a data frame that contains the output from the
#' \code{readScreen()} function.
#' @param method a character string, either 'negatives' or 'NPI', specifying
#' the method of per-plate normalization.
#' If 'negatives', the raw signals on the plate will be divided by the median
#' signal intensity of negative control wells on the same plate
#' \eqn{x_{normalized} = x / Median(x_{neg})}.
#' If 'NPI', a 'normalized percent inhibition (NPI)' is applied in a per-plate
#' basis.
#' NPI measures the extent to which the signal of interest is diminished
#' compared to a positive control:
#' \eqn{x_{normalized} = (x - Median(x_{pos}))/(Median(x_{neg}) - Median(x_{pos}))}.
#' The default value is 'negatives'.
#' @param discardLayer an integer value. To avoid the impact of incubation
#' effect on the negative controls,
#' a certain number of the exterior layers on the plate can be ignored when
#' calculating the median of control wells.
#' 0 means do not discard any control wells.
#' @param bySample a logical value, indicates whether normalized the viability
#' by sample rather than by plate.
#' This is for the situation that multiple samples are on the same plate.
#' @import dplyr tibble
#' @export
#' @return
#' This function will add one column, \code{normVal}, which is the normalized
#' viability based on user-specified normalization method, to the input
#' data.frame.
#' @examples
#'
#' rawFolder <- system.file('testData/rawData',
#'                           package = 'DrugScreenExplorer')
#' plateFile <- system.file('testData/plateAnno/plateAnno.tsv',
#'                          package = 'DrugScreenExplorer')
#' wellFile <- system.file('testData/wellAnno/wellAnno.tsv',
#'                          package = 'DrugScreenExplorer')
#' # Read in screen data without normalization
#' screenData <- readScreen(rawDir = rawFolder,
#'                          plateAnnotationFile = plateFile,
#'                          wellAnnotationFile = wellFile,
#'                          rowRange = c(3,18),
#'                          colRange = 2,
#'                          sep = '[;\t]',
#'                          negWell = c('DMSO','PBS'),
#'                          posWell = c(),
#'                          normalization = FALSE)
#'
#' # Perform a normalization step separately
#' screenData <- normalizePlate(screenData,
#'                              method = 'negatives',
#'                              discardLayer = 2)

normalizePlate <- function(screenData,
                           method = "negatives",
                           discardLayer = 0L,
                           bySample = FALSE) {

  #- argument checking
  method <- match.arg(method, c("negatives", "NPI"))
  stopifnot(is.numeric(discardLayer))

  if (!is.logical(bySample)) {
    stop("bySample must be a logical value, either TRUE or FALSE")
  }
  if (!all(c("sampleID", "fileName", "wellType", "value") %in%
           colnames(screenData))) {
    stop("screenData must contain the following columns: sampleID, fileName,
         wellType, and value")
  }
  if (!is.numeric(discardLayer) || discardLayer < 0) {
    stop("discardLayer must be a non-negative integer")
  }

  if (discardLayer > 0) {

    # generate a list of wellIDs that need to be ignored when summarizing
    # controls measurements

    nCol <- length(unique(screenData$colID))
    nRow <- length(unique(screenData$rowID))

    #- A restriction on discardLayer, in case all negatives are removed
    if (discardLayer > pmin(nCol, nRow) / 2)
      stop("discardLayer is too large.
           All negatives will be removed with discardLayer = ", discardLayer)

    disCol <- genColIDs(nCol)[c(seq(1, discardLayer),
                                seq(nCol - discardLayer + 1, nCol))]
    disRow <- genRowIDs(nRow)[c(seq(1, discardLayer),
                                seq(nRow - discardLayer + 1, nRow))]
  } else {
    disCol <- c()
    disRow <- c()
  }

  #  normOnePlate, function to normalize the values with two methods:
  #  "negatives" and "NPI".
  #  the name may be misleading

  normOnePlate <- function(onePlate) {

    negMed <- median(dplyr::filter(
      onePlate, wellType == "neg", !colID %in% disCol, !rowID %in%
        disRow)$value, na.rm = TRUE)
    posMed <- median(dplyr::filter(
      onePlate, wellType == "pos", !colID %in% disCol, !rowID %in%
        disRow)$value, na.rm = TRUE)

    if (method == "negatives") {
      if (is.na(negMed)) {
        stop("No negative control wells found on the speficied region of the plate")
      } else {
        onePlate <- dplyr::mutate(onePlate, normVal = value / negMed)
      }
    } else if (method == "NPI") {
      if (is.na(negMed) | is.na(posMed)) {
        stop("No negative and positive control wells found on the specified
             region of the plate")
      } else {
        onePlate <- dplyr::mutate(onePlate,
                                  normVal = (value - posMed)/(negMed - posMed))
      }
    } else stop("Please choose a normalization method,
                either \"negatives\" or \"NPI\"")
    return(onePlate)
  }

  if (bySample) {

    #? if possible, one plate has several samples: whether the neg will be shared.
    screenData <- group_by(screenData, sampleID) %>%
      do(normOnePlate(.)) %>%
      ungroup()
  } else {
    screenData <- group_by(screenData, fileName) %>%
      do(normOnePlate(.)) %>%
      ungroup()
  }
  return(screenData)
}




