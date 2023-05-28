#- test list for plotPlate

# load processed data

test_that("plotPlate", {

  data('screenData_normalized')

  ## - plot for layout
  output <- plotPlate(screenData_normalized, plotPlate = 'all', plotType = 'layout')
  expect_true(is(output, "list"))
  expect_true(is(output[[1]], "ggplot"))

  #- test argument: plotPlate
  ## - plot for layout

  #- Check: using a filename list including a random name, say "A".
  plates <- c(unique(screenData_normalized$fileName)[1], "A")

  expect_warning(plotPlate(screenData_normalized,
                           plotPlate = plates,
                           plotType = 'layout'),
                 "The following plates cannot be found: A")

  output <- plotPlate(screenData_normalized,
                      plotPlate = plates,
                      plotType = 'layout')

  expect_true(is(output[[1]], "ggplot"))

  #- Check: no useful name

  expect_error(plotPlate(screenData_normalized,
                         plotPlate = "A",
                         plotType = 'layout'),
               "No plate found")

  ##- check: plotType

  plotZscore <- plotPlate(screenData_normalized,
                          plotPlate = "all",
                          plotType = 'zscore')

  expect_true(is(plotZscore[[1]], "ggplot"))

  ##- check viability
  plotViability <- plotPlate(screenData_normalized,
                             plotPlate = "all",
                             plotType = 'viability')

  expect_true(is(plotViability[[1]], "ggplot"))

})

















