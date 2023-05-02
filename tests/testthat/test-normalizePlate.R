# expectScreen <- readScreen(rawDir = "testData/rawData/",
#                            plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
#                            wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
#                            rowRange = c(3,18),
#                            colRange = 2,
#                            sep = "[;\t]",
#                            negWell = c("DMSO","PBS"),
#                            posWell = c("Afatinib"))
#
# expectScreen_norm <- readScreen(rawDir = "testData/rawData/",
#                                 plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
#                                 wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
#                                 rowRange = c(3,18),
#                                 colRange = 2,
#                                 sep = "[;\t]",
#                                 negWell = c("DMSO","PBS"),
#                                 posWell = c("Afatinib"),
#                                 normalization = TRUE,
#                                 method = "negatives")
#
# expectScreen_norm_NPI<- readScreen(rawDir = "testData/rawData/",
#                                    plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
#                                    wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
#                                    rowRange = c(3,18),
#                                    colRange = 2,
#                                    sep = "[;\t]",
#                                    negWell <- c("DMSO","PBS"),
#                                    posWell = c("Afatinib"),
#                                    normalization = TRUE,
#                                    method = "NPI")
#
# expectScreen_norm_discard <- readScreen(rawDir = "testData/rawData/",
#                                         plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
#                                         wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
#                                         rowRange = c(3,18),
#                                         colRange = 2,
#                                         sep = "[;\t]",
#                                         negWell <- c("DMSO","PBS"),
#                                         posWell = c("Afatinib"),
#                                         normalization = TRUE,
#                                         method = "negatives",
#                                         discardLayer = 2L)
#
# save(list = c("expectScreen", "expectScreen_norm", "expectScreen_norm_NPI",
#               "expectScreen_norm_discard"),
#      file = "testData/expectedOutput/screenData.RData")
#
# sapply(paste0("../../R/", dir("../../R/")), source)

#- test normalization
#- Using Afatinib as positive control for NPI
#- derive data
testScreen <- readScreen(rawDir = "testData/rawData/",
                         plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
                         wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
                         rowRange = c(3,18),
                         colRange = 2,
                         sep = "[;\t]",
                         negWell = c("DMSO","PBS"),
                         posWell = c("Afatinib"))
#- norm
testScreen_norm <- normalizePlate(testScreen, method = "negative")
testScreen_norm_NPI <- normalizePlate(testScreen, method = "NPI")
testScreen_norm_discard <- normalizePlate(testScreen, method = "negative",
                                          discardLayer = 2L)


#- load expected results
load("testData/expectedOutput/screenData.RData")

test_that("Read the whole screen data", {
  expect_equal(expectScreen, testScreen)
})

test_that("Test the 'negative' and 'NPI' methods for normalization", {
  expect_equal(expectScreen_norm, testScreen_norm)
  expect_equal(testScreen_norm_NPI, testScreen_norm_NPI)
})

test_that("Test the 'discardLayer' for discarding 2 outer layers", {
  expect_equal(testScreen_norm_discard, expectScreen_norm_discard)
})






