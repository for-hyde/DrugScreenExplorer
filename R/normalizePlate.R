#' Per-plate normalization
#'
#' This function calculates the normalized viability values
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
#' \eqn{x_{normalized} = (x - Median(x_{pos}))/(Median(x_{neg}) - Median(x_{pos}))}. # nolint
#' The default value is 'negatives'.
#' @param discardLayer an integer value. To avoid the impact of incubation
#' effect on the negative controls,
#' a certain number of the exterior layers on the plate can be ignored when
#' calculating the median of control wells.
#' 0 means do not discard any control wells.
#' @param bySample a logical value, indicates whether normalized the viability
#' by sample rather than by plate.
#' This is for the situation that multiple samples are on the same plate.
#' @import dplyr tibble checkmate
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

  # Input validation
  checkmate::assert_data_frame(screenData)
  checkmate::assert_names(names(screenData),
                          must.include = c("sampleID",
                                           "fileName", "wellType", "value",
                                           "rowID", "colID"))
  checkmate::assert_choice(method, c("negatives", "NPI"))
  checkmate::assert_integerish(discardLayer, lower = 0, len = 1)
  checkmate::assert_logical(bySample, len = 1)

  # If discard layer is greater than 0, generate a list of wellIDs
  # to ignore when summarizing control measurements
  if (discardLayer > 0) {
    discardList <- findDiscardLayers(screenData, discardLayer)
    disCol <- discardList$colID
    disRow <- discardList$rowID
  } else {
    disCol <- c()
    disRow <- c()
  }
  # group by sampleID or fileName, then apply the normalization function to each group
  split_col <- if (bySample) screenData$sampleID else screenData$fileName

  split_list <- split(screenData, split_col)
  results_list <- lapply(split_list, function(plate) {
    medians <- getControlMedians(plate, disCol, disRow, method)
    if (method == "negatives") {
      plate <- plate |>
        dplyr::mutate(normVal = value / medians$medNeg)
    } else {
      plate <- plate |>
        dplyr::mutate(normVal = (value - medians$medPos)/(medians$medNeg - medians$medPos))
    }
    plate
  })
  dplyr::bind_rows(results_list)
  # extract median values for controls, ensure discarded edges do 
  # not remove all values.
}