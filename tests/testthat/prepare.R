s <- dir("../../R/")
sapply(paste0("../../R/", s), source)

# prepare expected data for unit testing

# # Unit test for readScreen function
# screenData <- readScreen(rawDir = "testData/rawData/",
#                          plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
#                          wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
#                          rowRange = c(3,18),
#                          colRange = 2,
#                          sep = "[;\t]",
#                          negWell <- c("DMSO","PBS"),
#                          posWell = c())
# #
# screenData_norm <- readScreen(rawDir = "testData/rawData/",
#                               plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
#                               wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
#                               rowRange = c(3,18),
#                               colRange = 2,
#                               sep = "[;\t]",
#                               negWell <- c("DMSO","PBS"),
#                               posWell = c(),
#                               normalization = TRUE,
#                               method = "negatives")
# # NPI, Afatinib as positive
# screenData_norm_NPI <- readScreen(rawDir = "testData/rawData/",
#                                   plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
#                                   wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
#                                   rowRange = c(3,18),
#                                   colRange = 2,
#                                   sep = "[;\t]",
#                                   negWell <- c("DMSO","PBS"),
#                                   posWell = c("Afatinib"),
#                                   normalization = TRUE,
#                                   method = "NPI")
# # # Discard layer
# screenData_norm_discard <- readScreen(rawDir = "testData/rawData/",
#                                       plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
#                                       wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
#                                       rowRange = c(3,18), colRange = 2,
#                                       sep = "[;\t]",
#                                       negWell <- c("DMSO","PBS"), posWell = c(),
#                                       normalization = TRUE,
#                                       method = "negatives",
#                                       discardLayer = 2)
#
# save(screenData, screenData_norm, screenData_norm_discard, screenData_norm_discard,
#      file = "testData/expectedOutput/screenData.RData")

# #Unit test for fitting edge effect
#load("tests/testthat/testData/expectedOutput/screenData.RData")

# edgeFactor_loess <- fitEdgeEffect(screenData_norm,
#                                   method = "loess",
#                                   useNeg = TRUE,
#                                   useLowConcentrations = 0)$edgeFactor
#
# edgeFactor_loess_allWell <- fitEdgeEffect(screenData_norm,
#                                           method = "loess",
#                                           useNeg = FALSE,
#                                           useLowConcentrations = 0)$edgeFactor
#
# edgeFactor_sigmoid <- fitEdgeEffect(screenData_norm,
#                                     method = "sigmoid",
#                                     useNeg = TRUE,
#                                     useLowConcentrations = 0)$edgeFactor
#
# edgeFactor_sigmoid_allWell <- fitEdgeEffect(screenData_norm,
#                                             method = "sigmoid",
#                                             useNeg = FALSE,
#                                             useLowConcentrations = 0)$edgeFactor
#
# screenOnePlate <- dplyr::filter(screenData_norm,
#                                 fileName == "P0007_12PB0265")
#
# edgeVal1 <- estimateOnePlate(screenOnePlate,
#                              nLayer =1,
#                              onlyNeg = FALSE)
#
# edgeVal2 <- estimateOnePlate(screenOnePlate,
#                              nLayer = 2,
#                              onlyNeg = TRUE)
#
# save(edgeFactor_loess, edgeFactor_loess_allWell, edgeFactor_sigmoid,
#      edgeFactor_sigmoid_allWell, screenOnePlate, edgeVal1, edgeVal2,
#      file = "testData/expectedOutput/edgeFactor.RData")
#
# # Unit test for correcting and estimate edge effect
# screenData_edgeEstimated <- fitEdgeEffect(screenData_norm, method = "loess", useNeg = TRUE, useLowConcentrations = 0)
# edgeCorr_bliss <- correctEdgeEffect(screenData_edgeEstimated, correctMethod = "bliss")
# edgeCorr_linear <- correctEdgeEffect(screenData_edgeEstimated, correctMethod = "linear")
# save(screenData_edgeEstimated, edgeCorr_bliss, edgeCorr_linear, file = "testData/expectedOutput/edgeCorrected.RData")
#
# # Unit tests for summarizing screen data
# screenSub <- dplyr::filter(screenData_norm, patientID %in% c("P0010","P0007"))
# sumData_average <- summariseScreen(screenSub, method = "average")
# sumData_AUC <- summariseScreen(screenSub, method = "AUC")
# screenSub_edgeCorr <- correctEdgeEffect(screenSub)
# sumData_AUC_corr <- summariseScreen(screenSub_edgeCorr, method = "AUC")
# sumData_IC50 <- summariseScreen(screenSub_edgeCorr, method = "IC50")
#
# tabDose1 <- dplyr::filter(screenData_norm, sampleID == "11PB0010", name == "Venetoclax")
# tabDose2 <- dplyr::filter(screenData_norm, sampleID == "11PB0010", name == "Ibrutinib")
# fitRes1 <- sumIC50(normVal ~ concentration, tabDose1)
# fitRes2 <- sumIC50(normVal ~ concentration, tabDose2)
# save(screenSub, sumData_average, sumData_AUC, screenSub_edgeCorr, sumData_AUC_corr, sumData_IC50,
#      tabDose1, tabDose2, fitRes1, fitRes2, file = "testData/expectedOutput/summarisedData.RData")
#
#
# get expect results for summariseScreenCombination function

#
# screenDataCombine <- readScreen(rawDir = "testData/rawDataCombine/",
#                                 plateAnnotationFile = "testData/plateAnnoCombine/plateAnnoCombine.tsv",
#                                 wellAnnotationFile = "testData/wellAnnoCombine/wellAnnoCombine.tsv",
#                                 rowRange = c(7,22),
#                                 colRange = 2,
#                                 sep = ",",
#                                 negWell <- c("DMSO_DMSO"),
#                                 posWell = c(),
#                                 normalization = T)
#
#
# sumData_bliss <- summariseScreenCombination(screenDataCombine,
#                                             negDrug = "DMSO",
#                                             method = "bliss")
#
# sumData_bayesynergy <- summariseScreenCombination(screenDataCombine,
#                                                   negDrug = "DMSO",
#                                                   method = "bayesynergy")
#
# save(sumData_bliss, sumData_bayesynergy,
#      file = "testData/expectedOutput/summarisedDataCombine.RData")
#
#
#
#
#




