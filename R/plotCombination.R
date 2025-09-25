

plotCombination_bliss <- function(obj, drug1, drug2, whichIndex = NULL) {

  # which value to plot
  if (is.null(whichIndex)) {
    # the default orders
    whichIndex <-  c("CI", "logCI")
    whichIndex <- intersect(whichIndex, colnames(obj))[1]

  } else {

    if (!whichIndex %in% colnames(obj))
      stop(whichIndex, " cannot be found in obj.")
  }

  # rename
  drugA <- drug1
  drugB <- drug2

  # check
  if(!drugA %in% unique(obj$drug))
    stop("drug1 cannot be found in the drug column of the provided obj")

  if(!drugB %in% unique(obj$drug2))
    stop("drug2 cannot be found in the drug2 column of the provided obj")

  plotTab <- filter(obj, drug == drugA & drug2 == drugB) %>%
    group_by(concentration, concentration2) %>%
    summarise(val = mean(!!sym(whichIndex)))

  if (nrow(plotTab) == 0)
    stop("drug combination cannot be found in the given obj")

  # prepare data, fill NAs
  plotTab <- expand.grid(concentration  = unique(plotTab$concentration),
                         concentration2 = unique(plotTab$concentration2)) %>%
    as.data.frame() %>%
    left_join(., plotTab, by = c("concentration", "concentration2")) %>%
    arrange(concentration, concentration2) %>%
    mutate(concentration2 = as.factor(concentration2),
           concentration = as.factor(concentration))

  #
  subtitle <- switch(whichIndex,
                     "CI" = "CI < 1 (Blue) indicates Synergy",
                     "logCI" = "logCI < 0 (Blue) indicates Synergy")

  midpoint <- switch(whichIndex,
                     "CI" = 1,
                     "logCI" = 0)

  legendName <- switch(whichIndex,
                       "CI" = "Combination\nIndex (CI)",
                       "logCI" = "Combination\nIndex (logCI)")

  # plot
  p <- ggplot(plotTab,
              aes(x = concentration,
                  y = concentration2,
                  fill = val)) +

    geom_tile(color = "gray", linewidth = 0.8) +

    labs(x = "Concentration2", y = "Concentration", fill = "Value") +

    theme_bw() +

    theme(axis.text = element_text(size = 8),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          plot.margin = margin(5, 5, 5, 5),
          panel.grid = element_blank(),
          legend.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))  +

    labs(title = "The Effects of Drug Combination",
         subtitle = subtitle,
         x = drugA,
         y = drugB) +

    # add NA to the NA cells
    geom_text(data = plotTab[is.na(plotTab$val),],
              aes(x = concentration,
                  y = concentration2,
                  label = "NA"), # The label to plot
              color = "gray20",
              size = 3) +

    # add values
    geom_text(data = plotTab[!is.na(plotTab$val),],
              aes(x = concentration,
                  y = concentration2,
                  label = format(round(val, 3), nsmall = 3)),
              color = "gray20",
              size = 3) +

    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = midpoint,
                         na.value = "transparent",
                         name = legendName)


  return(p)

}


plotCombination_bayesynergy <- function(obj, whichIndex) {

  # check
  if (is.null(whichIndex)) {
    # the default orders
    whichIndex <-  c("synergyScore", "antagonismScore")
    whichIndex <- intersect(whichIndex, colnames(obj))[1]

  } else {

    if (!whichIndex %in% colnames(obj))
      stop(whichIndex, " cannot be found in obj.")
  }

  # derive plot data

  plotTab <- obj %>%
    group_by(drug, drug2) %>%
    summarise(val = mean(!!sym(whichIndex)))

  # draw plot
  legendName <- subtitle <- switch(whichIndex,
                                   "synergyScore" = "Synergy Score",
                                   "antagonismScore" = "Antagonism Score")


  p <- ggplot(plotTab,
              aes(x = drug,
                  y = drug2,
                  fill = val)) +

    geom_tile(color = "gray", linewidth = 0.8) +

    labs( fill = "Value") +

    theme_bw() +

    theme(axis.text = element_text(size = 8),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          plot.margin = margin(5, 5, 5, 5),
          panel.grid = element_blank(),
          legend.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))  +

    labs(title = "The Effects of Drug Combination",
         subtitle = subtitle,
         x = "Drug A",
         y = "Drug B") +

    # add NA to the NA cells
    geom_text(data = plotTab[is.na(plotTab$val),],
              aes(x = drug,
                  y = drug2,
                  label = "NA"), # The label to plot
              color = "gray20",
              size = 3) +

    # add values
    geom_text(data = plotTab[!is.na(plotTab$val),],
              aes(x = drug,
                  y = drug2,
                  label = format(round(val, 3), nsmall = 3)),
              color = "gray20",
              size = 3) +

    scale_fill_gradient(low = "white", high = "blue",
                         na.value = "transparent",
                         name = legendName)

  return(p)

}

#' Plot Drug Combination Effects
#'
#' Generates a heatmap visualization for drug combination screening data using
#' the output of `summariseScreenCombination()`.
#'
#' @param obj A data.frame containing drug combination screening results, typically
#'   the output from `summariseScreenCombination()`. Must contain columns for
#'   drug identifiers, concentrations, and at least one of the supported synergy
#'   score columns.
#'
#' @param whichIndex Character. The name of the column in `obj` to use for
#'   plotting. If NULL (default), the function will attempt to use the first
#'   available metric from: "CI", "synergyScore", "logCI", "antagonismScore".
#'
#' @param drug1 Character. Name of the first drug to plot.
#'   Required when `whichIndex` is "CI" or "logCI".
#'
#' @param drug2 Character. Name of the second drug to plot.
#'   Required when `whichIndex` is "CI" or "logCI".
#'
#' @return A ggplot object showing a heatmap of the drug combination effects.
#'
#' @details
#' This function provides visualizations for different drug combination screening
#' analysis methods:
#'
#' \strong{Bliss method:} When combination effects are computed by `bliss` method
#' in `summariseScreenCombination()`, this function generates a concentration-
#' response heatmap for a specific drug pair using Combination Index (CI) or
#' log(CI) values. Requires specification of `drug1` and `drug2`.
#'
#' \strong{Bayesynergy method:} When combination effects are computed by `bayesynergy`
#' method in `summariseScreenCombination()`, this function generates a comprehensive
#' heatmap showing synergy or antagonism scores across all tested drug combinations.
#' Does not require `drug1` and `drug2` parameters.
#'
#'
#' @examples
#'
#' data(screenData_combine_normalized)
#' obj <- summariseScreenCombination(screenData_combine_normalized,
#'                                   negDrug = "DMSO",
#'                                   method = "bliss")
#' plotCombination(obj, whichIndex = "CI", drug1 = "Venetoclax", drug2 = "DCA")
#'
#'
#' @export

plotCombination <- function(obj, whichIndex = NULL, drug1 = NULL, drug2 = NULL) {

  if (is.null(whichIndex)) {
    # the default orders
    whichIndex <-  c("CI", "synergyScore", "logCI", "antagonismScore")
    whichIndex <- intersect(whichIndex, colnames(obj))[1]
  } else {
    if (!whichIndex[1] %in% colnames(obj))
      stop(whichIndex, "cannot be found in the colnames of obj")
  }

  # check drug1 and drug2
  if (whichIndex %in% c("CI", "logCI")) {
    if (any(is.null(drug1), is.null(drug2)))
      stop("drug1 and drug2 need to be specified for bliss method")
  }

  # make plot
  if (whichIndex %in% c("CI", "logCI")){
    p <- plotCombination_bliss(obj, drug1, drug2, whichIndex = whichIndex)
  }

  if (whichIndex %in% c("synergyScore", "antagonismScore")){
    p <- plotCombination_bayesynergy(obj, whichIndex = whichIndex)
  }

  return(p)

}




