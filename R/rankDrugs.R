#' Rank drugs against a selected sample and group summary
#'
#' This function compares a selected sample's drug score to
#' the summary score for a chosen group and returns the ranked drugs, along with
#' a waterfall-style graph.
#'
#' @param screenData A data frame containing drug screening data with at least
#'   the columns `sampleID`, `drug`, `wellType`, and a score column such as
#'   `DSS`, `AUC`, `IC50`, `meanViab`, or their corrected variants.
#' @param score Character string naming the score to rank by.
#' @param sampleID Character string identifying the sample used for comparison.
#' @param group Character string naming the grouping variable to compare within.
#'   Use `"All samples"` for global comparison.
#' @param summary Character string specifying the summary statistic used to
#'   aggregate the group score: either `"mean"` or `"median"`.
#' @param useCorrected Logical. If `TRUE`, prefer the corrected score column
#'   when available (e.g. `DSS.cor` instead of `DSS`).
#' @param topN Integer indicating the number of top-ranked drugs to return.
#' @param plot Logical. If `TRUE`, return a ggplot object together with the
#'   ranked table; otherwise return only the table.
#'
#' @return If `plot = TRUE`, a list with components `rankData` and `plot`.
#'   Otherwise, a data frame containing the ranked results.
#'
#' @export
rankDrugs <- function(screenData,
                      score = c("DSS", "AUC", "IC50", "meanViab"),
                      sampleID,
                      group = "All samples",
                      summary = c("mean", "median"),
                      useCorrected = FALSE,
                      topN = 30L,
                      plot = TRUE) {
  checkmate::assert_data_frame(screenData, .var.name = "screenData")
  checkmate::assert_choice(score, choices = c("DSS", "AUC", "IC50", "meanViab"),
                           .var.name = "score")
  checkmate::assert_choice(summary, choices = c("mean", "median"),
                           .var.name = "summary")
  checkmate::assert_flag(useCorrected, .var.name = "useCorrected")
  checkmate::assert_integerish(topN, len = 1, lower = 1, .var.name = "topN")
  checkmate::assert_flag(plot, .var.name = "plot")

  requiredCols <- c("sampleID", "drug", "wellType")
  if (!all(requiredCols %in% colnames(screenData))) {
    stop("screenData must contain sampleID, drug, and wellType columns.")
  }

  if (missing(sampleID) || is.null(sampleID) || length(sampleID) != 1L) {
    stop("A single sampleID must be supplied.")
  }

  if (!sampleID %in% screenData$sampleID) {
    stop("Selected sampleID is not present in screenData.")
  }

  if (!is.null(group) && group != "All samples" && !group %in% colnames(screenData)) {
    stop("The requested grouping variable is not present in screenData.")
  }

  score <- match.arg(score)
  summary <- match.arg(summary)
  topN <- as.integer(topN)

  scoreCol <- score
  if (useCorrected && paste0(score, ".cor") %in% colnames(screenData)) {
    scoreCol <- paste0(score, ".cor")
  }

  if (!scoreCol %in% colnames(screenData)) {
    stop("Selected score is not available in the current data.")
  }

  data <- screenData %>%
    dplyr::filter(wellType == "sample") %>%
    dplyr::mutate(
      groupValue = if (group == "All samples") {
        "All samples"
      } else {
        as.character(.data[[group]])
      }
    ) %>%
    dplyr::select(sampleID, drug, groupValue, dplyr::all_of(scoreCol)) %>%
    dplyr::distinct()

  groupSummary <- data %>%
    dplyr::group_by(groupValue, drug) %>%
    dplyr::summarise(
      groupScore = if (summary == "mean") {
        mean(.data[[scoreCol]], na.rm = TRUE)
      } else {
        median(.data[[scoreCol]], na.rm = TRUE)
      },
      .groups = "drop"
    )

  sampleScores <- data %>%
    dplyr::filter(sampleID == .env$sampleID)

  if (nrow(sampleScores) == 0) {
    stop("Selected sample has no score data in the current data set.")
  }

  names(sampleScores)[which(names(sampleScores) == scoreCol)] <- "sampleScore"

  rankData <- sampleScores %>%
    dplyr::left_join(groupSummary, by = c("groupValue", "drug")) %>%
    dplyr::mutate(
      diff = sampleScore - groupScore,
      rank = dplyr::dense_rank(dplyr::desc(diff))
    ) %>%
    dplyr::arrange(dplyr::desc(diff)) %>%
    head(topN)

  if (plot) {
    df <- rankData %>%
      dplyr::mutate(drug = factor(drug, levels = unique(drug)))

    p <- ggplot2::ggplot(df, ggplot2::aes(x = drug, y = diff, fill = diff > 0)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c("TRUE" = "steelblue", "FALSE" = "salmon"),
        guide = "none"
      ) +
      ggplot2::labs(
        x = "Drug",
        y = paste0(scoreCol, " - ", summary, " group"),
        title = paste0("Waterfall ranking for sample ", sampleID),
        subtitle = if (group == "All samples") {
          "Compared to global summary"
        } else {
          paste0("Compared to ", group, " summary")
        }
      ) +
      ggplot2::theme_minimal()

    return(list(rankData = rankData, plot = p))
  }

  rankData
}
