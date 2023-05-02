
#-------------------------------------------------------------------------------
#- fitIC50 test
#-------------------------------------------------------------------------------

test_that("Test the output of fitIC50 to that directly using dr4pl", {

  doseTab <- data.frame(viability = c(1.0, 0.95, 0.5, 0.2, 0.1),
                        concentration = c(0.001, 0.01, 0.1, 1, 10))

  expectedResult <- dr4pl(viability ~ concentration, data = doseTab)
  testResult <- fitIC50(viability ~ concentration, data = doseTab)$parm_fit

  expect_equal(expectedResult$parameters, testResult$parameters)

})


test_that("Test argument: logDose ", {

  doseTab <- data.frame(viability = c(1.0, 0.95, 0.5, 0.2, 0.1),
                        concentration = c(0.001, 0.01, 0.1, 1, 10))

  expect_message(fitIC50(viability ~ concentration, data = doseTab, logDose = 2),
                 "Concentration is transformed back with the given base:2")

})
