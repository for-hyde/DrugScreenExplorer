
#-------------------------------------------------------------------------------
#- sumIC50 test, including predict.fitIC50 and calcAUC
#-------------------------------------------------------------------------------


test_that("Test sumIC50 including internal functions: predict.fitIC50 and calcAUC)",{

  doseTab <- data.frame(viability = c(1.0, 0.95, 0.5, 0.2, 0.1),
                        concentration = c(0.001, 0.01, 0.1, 1, 10))

  IC50 <- sumIC50(viability ~ concentration, data = doseTab,
                  minConc = 0, maxConc = 15, n = 100)

  expect_equal(as.numeric(IC50["AUC"]), 0.1905, tolerance = 0.001)


  #- expect warning
  doseTab <- data.frame(viability = c(1, 1, 1, 1, 1),
                        concentration = c(0.001, 0.01, 0.1, 1, 10))

  expect_warning(sumIC50(viability ~ concentration, data = doseTab,
                         minConc = 0, maxConc = 15, n = 100),
                 regexp = "Curve fitting failed, NA values generated. ")

})


#-------------------------------------------------------------------------------

