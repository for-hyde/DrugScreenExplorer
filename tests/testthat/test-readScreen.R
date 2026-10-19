
load("testData/expectedOutput/screenData.RData")
rawDir = "testData/rawData/"
plateAnno = "testData/plateAnno/plateAnno.tsv"
wellAnno = "testData/wellAnno/wellAnno.tsv"

testScreenAutoload <- readScreen(rawDir = rawDir,
                                 plateAnnotationFile = plateAnno,
                                 wellAnnotationFile = wellAnno,
                                 sep = ";",
                                 negWell = c("DMSO","PBS")
                                 )

testhat("Read screen data with minimal input from user.",
        expect_equal(testScreenAutoload, screenData))

testScreen <- readScreen(rawDir = "testData/rawData/",
                         plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
                         wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
                         sep = ";",
                         setManualRanges = TRUE,
                         rowRange = c(3,18),
                         colRange = 2,
                         negWell = c("DMSO","PBS"),
                         posWell = c())

testthat("Manual loading functioning",
         expect_equal(testScreen, screenData))

testScreen_norm <- readScreen(rawDir = "testData/rawData/",
                              plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
                              wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
                              sep = ";",
                              negWell = c("DMSO","PBS"),
                              posWell = c(),
                              normalization = TRUE,
                              method = "negatives")
testthat("Loading with normalization functioning",
         expect_equal(testScreen_norm, screenData_norm))

testScreen_norm_discard <- readScreen(rawDir = "testData/rawData/",
                                      plateAnnotationFile = "testData/plateAnno/plateAnno.tsv",
                                      wellAnnotationFile = "testData/wellAnno/wellAnno.tsv",
                                      sep = ";",
                                      setManualRanges = FALSE,
                                      negWell <- c("DMSO","PBS"),
                                      posWell = c(),
                                      normalization = TRUE,
                                      method = "negatives",
                                      discardLayer = 2)

test_that("Read the whole screen data, with normalization, discarding 2 outer layers", {
  expect_equal(testScreen_norm_discard, screenData_norm_discard)
})



