#Load prior data
load("testData/expectedOutput/screenData.RData")

test_that("Function is evaluating correctly", {
          #negative norm works
          expect_equal(normalizePlate(screenData), screenData_norm)
          expect_equal(normalizePlate(screenData, discardLayer = 2),
                       screenData_norm_discard)
          #NPI norm works
          expect_equal(normalizePlate(screenData_pos, method = "NPI"),
                       screenData_norm_pos)
})

toy_df <- data.frame(
  "sampleID" = c("test", "test", "test"),
  "fileName" = c("test", "test", "test"),
  "wellType" = c("sample", "neg", "pos"),
  "value" = c(150, 200, 100),
  "rowID" = c("A0", "A0", "A0"),
  "colID" = c("01", "02", "03")
)

norm_toy <- normalizePlate(toy_df)
npi_toy <- normalizePlate(toy_df, method = "NPI")

test_that("function evaluates when given clean data",{
  expect_silent(normalizePlate(toy_df))
  expect_equal(norm_toy$normVal[[1]], 0.75)
  expect_equal(npi_toy$normVal[[1]], 0.5)
})

test_that("Invalid Inputs are flagged", {
  expect_error(normalizePlate(screenData, method = "inverse_binomial"))
  #screenData missing negatives
  expect_error(normalizePlate(screenData, method = "NPI"))
  expect_error(normalizePlate(screenData, method = TRUE))
  expect_error(normalizePlate(screenData, discardLayer = 0.4))
  expect_error(normalizePlate(screenData, discardLayer = -1))
  expect_error(normalizePlate(screenData, bySample = "no"))
  #If provided, works
  expect_silent(normalizePlate(
    screenData,
    method = "negatives",
    discardLayer = 2,
    bySample = TRUE
  ))
})

test_that("Edge cases are correctly accounted for",
  #Too many wells discarded
  expect_error(normalizePlate(screenData, discardLayer = 18))
)
