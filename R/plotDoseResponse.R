#' Plot Dose-Response Curves for Drug Screening Data
#'
#' Generates dose-response curves for specified compounds from screening data,
#' with options for different response metrics, logarithmic scaling, and IC50 model fitting.
#'
#' @param screenData A data frame containing drug screening data. Must include
#'   columns: `drug`, `concentration`, and one of `value`, `normVal`, or
#'   `normVal.cor` depending on the `whichResponse` parameter. Typically, it is
#'   the output of `readScreen()` function.
#' @param drugList A character vector of drug names to plot. Drugs not found in
#'   `screenData` will be silently ignored.
#' @param logDose Logical indicating whether to use logarithmic x-axis for
#'   concentration values. Default is `TRUE`.
#' @param whichResponse Character string specifying which response values to use.
#'   Must be one of: "viability" (raw values), "normalized", or "adjusted".
#'   Default is "viability".
#' @param fitModel Logical indicating whether to fit and display IC50 curves.
#'   Default is `TRUE`.
#' @param showPoints Logical indicating whether to show raw data points.
#'   Default is `TRUE`.
#' @param pointSize Numeric value for point size. Default is 2.
#' @param lineSize Numeric value for line size. Default is 0.8.
#' @param title Plot title. If `NULL`, uses a descriptive default title.
#' @param subtitle Plot subtitle. If `NULL`, uses a descriptive default subtitle.
#' @param caption Plot caption. If `NULL`, uses a descriptive default caption.
#'
#' @return A ggplot object displaying dose-response curves for the specified compounds.
#'
#' @examples
#'
#' library(DrugScreenExplorer)
#' rawFolder <- system.file('testData/rawData',
#'                          package = 'DrugScreenExplorer')
#' plateFile <- system.file('testData/plateAnno/plateAnno.tsv',
#'                          package = 'DrugScreenExplorer')
#' wellFile  <- system.file('testData/wellAnno/wellAnno.tsv',
#'                          package = 'DrugScreenExplorer')
#'
#' screenData <- readScreen(rawDir = rawFolder,
#'                          plateAnnotationFile = plateFile,
#'                          wellAnnotationFile = wellFile,
#'                          normalization = TRUE,
#'                          rowRange = c(3,18), colRange = 2,
#'                          sep = '[;\t]', commaAsDecimal = FALSE,
#'                          negWell = c('DMSO','PBS'))
#'
#' plotDoseResponse(screenData,
#'                  drugList = c("A-1210477", "Palbociclib", "Motolimod"),
#'                  logDose = TRUE,
#'                  whichResponse = "normalized",
#'                  fitModel = TRUE,
#'                  showPoints = TRUE,
#'                  pointSize = 2,
#'                  lineSize = 0.8,
#'                  title = NULL,
#'                  subtitle = NULL,
#'                  caption = NULL)
#'
#'
#' @importFrom dplyr filter mutate select
#' @importFrom ggplot2 ggplot aes geom_point scale_x_log10 scale_x_continuous
#'   scale_color_viridis_d scale_fill_viridis_d ylab xlab ggtitle geom_smooth
#'   labs theme_minimal theme element_text element_line element_blank
#'   element_rect
#' @importFrom scales trans_breaks trans_format math_format
#' @importFrom stats complete.cases
#' @importFrom rlang sym
#' @export
#'
plotDoseResponse <- function(screenData,
                             drugList,
                             logDose = TRUE,
                             whichResponse = c("viability", "normalized", "adjusted"),
                             fitModel = TRUE,
                             showPoints = TRUE,
                             pointSize = 2,
                             lineSize = 0.8,
                             title = NULL,
                             subtitle = NULL,
                             caption = NULL) {

  # Input validation
  if (!is.data.frame(screenData)) {
    stop("screenData must be a data frame")
  }

  if (!is.character(drugList)) {
    stop("drugList must be a character vector")
  }

  required_cols <- c("drug", "concentration")
  if (!all(required_cols %in% colnames(screenData))) {
    stop(paste("screenData must contain columns:", paste(required_cols, collapse = ", ")))
  }

  # Match whichResponse argument
  whichResponse <- match.arg(whichResponse)

  # Determine target column
  target <- switch(
    whichResponse,
    "normalized" = {
      if (!"normVal" %in% colnames(screenData)) {
        stop("Normalized values (normVal) cannot be found in screenData")
      }
      "normVal"
    },
    "adjusted" = {
      if (!"normVal.cor" %in% colnames(screenData)) {
        stop("Incubation effects adjusted values (normVal.cor) cannot be found in screenData")
      }
      "normVal.cor"
    },
    "viability" = {
      if (!"value" %in% colnames(screenData)) {
        stop("Raw viability values (value) cannot be found in screenData")
      }
      "value"
    }
  )

  # Filter and prepare data
  plotData <- screenData %>%
    filter(drug %in% drugList) %>%
    mutate(val = !!sym(target)) %>%
    select(drug, concentration, val) %>%
    filter(complete.cases(.))

  # Check if any data remains after filtering
  if (nrow(plotData) == 0) {
    stop("No data available for the specified drugs after filtering")
  }

  # Create base plot
  p <- ggplot(data = plotData,
              aes(x = concentration, y = val, col = drug, fill = drug))

  # Add points if requested
  if (showPoints) {
    p <- p + geom_point(shape = 1, size = pointSize, alpha = 0.7)
  }

  # Add model fit if requested
  if (fitModel) {
    p <- p + geom_smooth(method = "fitIC50",
                         se = FALSE,
                         alpha = 0.2,
                         linewidth = lineSize,
                         method.args = list(logDose = ifelse(logDose, 10, NULL)))
  }

  # Handle x-axis scaling
  if (logDose) {
    p <- p + scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    )
  } else {
    p <- p + scale_x_continuous()
  }

  # Set y-axis label based on response type
  y_label <- switch(whichResponse,
                    "viability" = "Viability",
                    "normalized" = "Normalized Viability",
                    "adjusted" = "Adjusted Viability"
  )

  # Set default titles and captions
  default_title <- "Dose-Response Curves for Selected Compounds"
  default_subtitle <- if (fitModel) {
    "Points represent raw data, lines represent IC50 model fits"
  } else {
    "Raw data points only (no model fitting)"
  }
  default_caption <- if (fitModel) {
    "IC50 models fitted using a 4-parameter logistic model"
  } else {
    "No model fitting performed"
  }

  # Apply styling and labels
  p <- p +
    ylab(y_label) +
    xlab("Concentration") +
    labs(color = "Compound", fill = "Compound") +
    scale_color_viridis_d(option = "plasma", end = 0.85) +
    scale_fill_viridis_d(option = "plasma", end = 0.85, alpha = 0.2) +
    ggtitle(title %||% default_title) +
    labs(subtitle = subtitle %||% default_subtitle,
         caption = caption %||% default_caption) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 10),
      plot.caption = element_text(hjust = 1, color = "gray50", size = 9, face = "italic"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "gray80", linewidth = 0.5),
      panel.grid.major = element_line(linewidth = 0.25, color = "gray90")
    )

  return(p)
}

# Helper function for NULL coalescing
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}
