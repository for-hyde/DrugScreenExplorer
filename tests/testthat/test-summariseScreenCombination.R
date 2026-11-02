# Test for combined data

load("testData/expectedOutput/summarisedDataCombine.RData")

#Test proper loading
testSumData_bliss <- summariseScreenCombination(
  screenDataCombine,
  negDrug = c("DMSO"),
  method = c("bliss")
)

test_that("Synergy or antagonism effects of two-drug combination", {
  expect_equal(sumData_bliss, testSumData_bliss)
})

#Test inputs correctly reject
test_that("Inputs are correctly validated", {
  expect_error(
    summariseScreenCombination(
      screenDataCombine,
      negDrug = c("DMSO"),
      method = c("False")
    )
  )
  expect_error(
    summariseScreenCombination(
      screenDataCombine,
      negDrug = c("TTTT"),
      method = c("Bliss")
    )
  )
  expect_error(
    summariseScreenCombination(
      NotADataFrame,
      negDrug = c("DMSO"),
      method = c("Bliss")
    )
  )
})
