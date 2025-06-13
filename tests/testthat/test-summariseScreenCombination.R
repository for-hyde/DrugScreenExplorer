# Test for combined data

load("testData/expectedOutput/summarisedDataCombine.RData")

testSumData_bliss <- summariseScreenCombination(
  screenDataCombine,
  negDrug = c("DMSO"),
  method = c("bliss")
)

test_that("Synergy or antagonism effects of two-drug combination", {
  expect_equal(sumData_bliss, testSumData_bliss)
})


