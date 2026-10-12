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

test_that("summariseScreen calculates DSS for the saved screen data", {
  expect_true("DSS" %in% names(sumData_DSS))
  expect_equal(
    nrow(sumData_DSS),
    nrow(screenSub)
  )

  sample_rows <- sumData_DSS$wellType == "sample"
  expect_true(any(is.finite(sumData_DSS$DSS[sample_rows])))
  expect_true(all(is.na(sumData_DSS$DSS[!sample_rows])))
})

test_that("summariseScreen adds corrected DSS when corrected viability is present", {
  result <- summariseScreen(
    summarise_screen_data,
    method = "DSS",
    dssType = "DSS1"
  )

  expect_true(all(c("DSS", "DSS.cor") %in% names(result)))
  expect_equal(
    unique(result$DSS[result$sampleID == "sample_1" & result$drug == "DrugA"]),
    94 / 270,
    tolerance = 1e-12
  )
  expect_equal(
    unique(result$DSS[result$sampleID == "sample_2" & result$drug == "DrugA"]),
    108 / 270,
    tolerance = 1e-12
  )
  expect_equal(
    unique(result$DSS.cor[result$sampleID == "sample_1" & result$drug == "DrugA"]),
    99.5 / 270,
    tolerance = 1e-12
  )
  expect_equal(
    unique(result$DSS.cor[result$sampleID == "sample_2" & result$drug == "DrugA"]),
    113.5 / 270,
    tolerance = 1e-12
  )
  expect_true(all(is.na(result$DSS[result$drug == "DMSO"])))
  expect_true(all(is.na(result$DSS.cor[result$drug == "DMSO"])))
})

test_that("summariseScreen forwards the DSS type", {
  results <- lapply(
    c("DSS1", "DSS2", "DSS3"),
    function(type) {
      summariseScreen(
        summarise_screen_data,
        method = "DSS",
        dssType = type
      )
    }
  )

  dss_values <- vapply(
    results,
    function(result) {
      unique(result$DSS[result$sampleID == "sample_1" &
                           result$drug == "DrugA"])
    },
    numeric(1)
  )

  expect_equal(dss_values[1], 94 / 270, tolerance = 1e-12)
  expect_equal(
    dss_values[2],
    (94 / 270) / log10(80),
    tolerance = 1e-12
  )
  expect_equal(
    dss_values[3],
    ((94 / 270) / log10(80)) * (2 / 3),
    tolerance = 1e-12
  )
})

test_that("summariseScreen validates DSS arguments", {
  expect_error(
    summariseScreen(
      summarise_screen_data,
      method = "DSS",
      dssType = "invalid"
    ),
    "Invalid DSS type"
  )
  expect_error(
    summariseScreen(
      summarise_screen_data,
      method = "DSS",
      dssThreshold = 100
    ),
    "dssThreshold"
  )
})
