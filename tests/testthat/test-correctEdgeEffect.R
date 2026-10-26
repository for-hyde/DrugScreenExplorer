load("testData/expectedOutput/edgeCorrected.RData")

#Load and run data
testEdgeCorr_bliss  <- correctEdgeEffect(screenData_edgeEstimated, correctMethod = "bliss")
testEdgeCorr_linear <- correctEdgeEffect(screenData_edgeEstimated, correctMethod = "linear")

test_that("Edge effect correction with bliss multiplicative model", {
  expect_equal(testEdgeCorr_bliss, edgeCorr_bliss)
})

test_that("Edge effect correction with linear correction model", {
  expect_equal(testEdgeCorr_linear, edgeCorr_linear)
})

#Toy data frame for testing
valid_screen_data <- data.frame(
  normVal = c(0.8, 1.0),
  sampleID = c("sample-1", "sample-1"),
  fileName = c("plate-1.csv", "plate-1.csv"),
  wellType = c("negative", "sample"),
  edgeFactor = c(0.9, 1.1)
)

test_that("valid input returns corrected values", {
  result <- correctEdgeEffect(valid_screen_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(valid_screen_data))
  expect_true("normVal.cor" %in% names(result))
  expect_equal(
    result$normVal.cor,
    valid_screen_data$normVal / valid_screen_data$edgeFactor
  )
})

test_that("estimateMethod must be supported", {
  bad_inputs <- list("invalid", 1, NA_character_)
  for (input in bad_inputs) {
    expect_error(
      correctEdgeEffect(valid_screen_data, estimateMethod = input)
    )
  }
  expect_silent(
    correctEdgeEffect(valid_screen_data, estimateMethod = "loess")
  )
  expect_silent(
    correctEdgeEffect(valid_screen_data, estimateMethod = "sigmoid")
  )
})

test_that("correctMethod must be supported", {
  bad_inputs <- list("invalid", 1, NA_character_)
  for (input in bad_inputs) {
    expect_error(
      correctEdgeEffect(valid_screen_data, correctMethod = input)
    )
  }
  expect_silent(
    correctEdgeEffect(valid_screen_data, correctMethod = "bliss")
  )
  expect_silent(
    correctEdgeEffect(valid_screen_data, correctMethod = "linear")
  )
})

test_that("useNeg must be a single non-missing logical value", {
  bad_inputs <- list(1, "invalid", c(TRUE, FALSE), NA)
  for (input in bad_inputs) {
    expect_error(
      correctEdgeEffect(valid_screen_data, useNeg = input)
    )
  }
  expect_silent(
    correctEdgeEffect(valid_screen_data, useNeg = TRUE)
  )
  expect_silent(
    correctEdgeEffect(valid_screen_data, useNeg = FALSE)
  )
})

test_that("useLowConcentrations must be a single non-negative numeric value", {
  bad_inputs <- list(-1, "invalid", c(0, 1), NA)
  for (input in bad_inputs) {
    expect_error(
      correctEdgeEffect(valid_screen_data, useLowConcentrations = input)
    )
  }
  expect_silent(
    correctEdgeEffect(valid_screen_data, useLowConcentrations = 0)
  )
  expect_silent(
    correctEdgeEffect(valid_screen_data, useLowConcentrations = 1)
  )
})

test_that("span must be a single non-negative numeric value", {
  bad_inputs <- list(0, "invalid", c(0, 1), NA)
  for (input in bad_inputs) {
    expect_error(
      correctEdgeEffect(valid_screen_data, span = input)
    )
  }
  expect_silent(
    correctEdgeEffect(valid_screen_data, span = 0.5)
  )
  expect_silent(
    correctEdgeEffect(valid_screen_data, span = 1)
  )
})

test_that("If span > 1, a warning is issued", {
  expect_warning(correctEdgeEffect(valid_screen_data, span = 1.5))
})
