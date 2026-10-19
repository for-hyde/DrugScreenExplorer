load("testData/expectedOutput/summarisedData.RData")

testSumData_average <-
  suppressWarnings(summariseScreen(screenSub, method = "average"))
testSumData_AUC <-
  suppressWarnings(summariseScreen(screenSub, method = "AUC"))
testSumData_AUC_corr <-
  suppressWarnings(summariseScreen(screenSub_edgeCorr, method = "AUC"))
testSumData_IC50 <-
  suppressWarnings(summariseScreen(screenSub_edgeCorr, method = "IC50"))
sumData_DSS <-
  suppressWarnings(summariseScreen(screenSub, method = "DSS"))

testFitRes1 <-
  suppressWarnings(sumIC50(normVal ~ concentration, tabDose1))
testFitRes2 <-
  suppressWarnings(sumIC50(normVal ~ concentration, tabDose2))

test_that("Summarise screen use averaged viability", {
  expect_equal(sumData_average, testSumData_average)
})

test_that("Summarise screen use trepazoidal AUC value", {
  expect_equal(sumData_AUC, testSumData_AUC)
})

test_that("Summarise screen use trepazoidal AUC value on edge corrected viabilities", {
  expect_equal(sumData_AUC_corr, testSumData_AUC_corr)
})

test_that("Summarise screen use IC50 fitting", {
  expect_equal(testSumData_IC50, sumData_IC50)
})

test_that("Test 1 for sumIC50 function", {
  expect_equal(testFitRes1, fitRes1)
})

test_that("Test 2 for sumIC50 function", {
  expect_equal(testFitRes2, fitRes2)
})
