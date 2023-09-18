
# method = negatives

testScreen <- readScreen(rawDir = "testData/rawData/",
                         plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
                         wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
                         rowRange = c(3,18),
                         colRange = 2,
                         sep = "[;\t]",
                         negWell = c("DMSO","PBS"),
                         posWell = c())

testScreen_norm <- normalizePlate(testScreen, method = "negative")

#-method = NPI
testScreen_NPI <- readScreen(rawDir = "testData/rawData/",
                             plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
                             wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
                             rowRange = c(3, 18),
                             colRange = 2,
                             sep = "[;\t]",
                             negWell = c("DMSO","PBS"),
                             posWell = c("Afatinib"))

testScreen_norm_NPI <- normalizePlate(testScreen_NPI, method = "NPI")

# discardLayer = 2L
testScreen_norm_discard <- normalizePlate(testScreen,
                                          method = "negative",
                                          discardLayer = 2L)

#- load expected results
load("testData/expectedOutput/screenData.RData")

test_that("Read the whole screen data", {
  expect_equal(screenData, testScreen)
})

test_that("Test the 'negative' and 'NPI' methods for normalization", {
  expect_equal(screenData_norm, testScreen_norm)
  expect_equal(screenData_norm_NPI, testScreen_norm_NPI)
})

test_that("Test the 'discardLayer' for discarding 2 outer layers", {
  expect_equal(testScreen_norm_discard, screenData_norm_discard)
})








