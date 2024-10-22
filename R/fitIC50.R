#' Fitting 4 parameter log-logistic (4PL) models.
#'
#' Use the dr4pl package to perform IC50 curve fitting. The outputs can be directly used for geom_smooth() in ggplot2.
#'
#' @param formula formula for the curve fitting in the form of 'response ~ concentration'.
#' @param data a data frame that contains concentration values and normalized viability values or percent inhibition values.
#' @param weights not used, mainly for geom_smooth() purpose.
#' @param logDose a numeric value indicates the base of a log function if concentrations have been log-transformed.
#' The argument will transform the concentrations back.
#' The default value is NULL (indicating no log transformation has been applied).
#' @param ... further arguments to be passed to \textit{dr4pl} function in \textit{dr4pl} package.
#' @export fitIC50
#' @import dr4pl
#' @import rlang
#' @return fitIC50 returns an object of class 'fitIC50', which is a list containing the following components:
#' \item{model}{the input dose-response table}
#' \item{formula}{the formula used for the model fitting}
#' \item{parm_fit}{the fitted parameters for a 4-paramater logistic model}
#' \item{logDose}{a numeric value specifying the base of log transformation for concentration,
#' if the concentration is log transformed. The default value is NULL (not log transformed).}
#' @examples
#' # create an example dose-response table
#' doseTab <- data.frame(viability = c(1.0, 0.95, 0.5, 0.2, 0.1),
#'                       concentration = c(0.001, 0.01, 0.1, 1, 10))
#'
#' # fit a four-parameter logistic model using dr4pl package
#' fitIC50(viability ~ concentration, data = doseTab)
#'

fitIC50 <- function(formula, data = NULL, weights = NULL, logDose = NULL, ...) {

  #- check
  if (!rlang::is_formula(formula))
    stop("The input should be a formula")

  if (!is.null(data)) {
    modelFrame <- model.frame(formula, data)
  } else {
    modelFrame <- model.frame(formula)
  }

  if (!is.null(logDose)) {
    modelFrame[, 2] <- logDose^modelFrame[, 2]
    message("Concentration is transformed back with the given base:", logDose)
  }

  parm_fit <- dr4pl(modelFrame[, 2], modelFrame[, 1], ...)
  newModel <- list(formula = formula,
                   parm_fit = parm_fit,
                   logDose = logDose,
                   model = modelFrame)

  class(newModel) <- "fitIC50"
  return(newModel)
}




