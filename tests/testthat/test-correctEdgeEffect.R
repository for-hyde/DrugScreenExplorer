
load("testData/expectedOutput/edgeCorrected.RData")

testEdgeCorr_bliss  <- correctEdgeEffect(screenData_edgeEstimated, correctMethod = "bliss")
testEdgeCorr_linear <- correctEdgeEffect(screenData_edgeEstimated, correctMethod = "linear")

test_that("Edge effect correction with bliss multiplicative model", {
  expect_equal(testEdgeCorr_bliss, edgeCorr_bliss)
})

test_that("Edge effect correction with linear correction model", {
  expect_equal(testEdgeCorr_linear, edgeCorr_linear)
})

test_that("Check function arguments", {

  expect_error(
    correctEdgeEffect(as.matrix(screenData_edgeEstimated), correctMethod = "bliss"),
    "screenData should be a data frame"
  )
})


