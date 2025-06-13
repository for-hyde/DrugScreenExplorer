

# plot raw ATP count distribution for each well type

test_that("plotTypeDist", {

  data(screenData)
  output <- plotTypeDist(screenData)
  expect_true(is(output[[1]], "ggplot"))

})


