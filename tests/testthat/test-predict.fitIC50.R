
#-------------------------------------------------------------------------------
#- predict.fitIC50 test
#-------------------------------------------------------------------------------

test_that("The predicted value using IC50 should be equal to the mean of Lowerlimit
          and UpperLimit", {

  doseTab <- data.frame(viability = c(1.0, 0.95, 0.5, 0.2, 0.1),
                        concentration = c(0.001, 0.01, 0.1, 1, 10))

  fit  <- fitIC50(viability ~ concentration, data = doseTab)
  testResult <- predict.fitIC50(fit, newdata = fit$parm_fit$parameters[2])
  testResult <- as.numeric(testResult)

  expectedResult <- mean(fit$parm_fit$parameters[c(1, 4)])
  expect_equal(testResult, expectedResult)

})


test_that("An extreme lower concentration (e.g., 0) results in the Lowerlimit", {

  doseTab <- data.frame(viability = c(1.0, 0.95, 0.5, 0.2, 0.1),
                        concentration = c(0.001, 0.01, 0.1, 1, 10))

  fit  <- fitIC50(viability ~ concentration, data = doseTab)
  testResult <- predict.fitIC50(fit, newdata = 0)
  testResult <- as.numeric(testResult)

  expectedResult <- as.numeric(fit$parm_fit$parameters[1])
  expect_equal(testResult, expectedResult)

})

test_that("The object should be the output of fitIC50", {

  obj = list()
  expect_error(predict.fitIC50(obj),
               "object should be an output of fitIC50 function: an object of 'fitIC50' class")
})






