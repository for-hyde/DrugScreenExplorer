#- test list for readPlate
#- 1, rowRange & colRange
#- 2, sep
#- 3, allowNA

load("testData/expectedOutput/expected_values.RData")

test_that("Read one 384-well plate", {
  expect_identical(readPlate("testData/example_plates/plate1.txt",
                             rowRange = c(3,18), colRange =2, sep = ";", commaAsDecimal = FALSE ),
                   plate1)
})


test_that("Read one 1536-well plate", {
  expect_identical(readPlate("testData/example_plates/plate2.txt",
                             rowRange = c(1,32), colRange =1, sep = "\t", commaAsDecimal = FALSE ),
                   plate2)
})

# 3- allowNA

test_that("test the argument about allowNA", {

  #- Temporary profile
  temp_file <- tempfile()

  #- Import plate profile
  plate <- readLines("testData/example_plates/plate1.txt")

  #- Randomly generate a non-numeric input, replace 856480 with X
  plate <- gsub("856480", "X", plate)
  writeLines(plate, temp_file)

  expect_warning(readPlate(temp_file,
                           rowRange = c(3, 18),
                           colRange =2,
                           sep = ";",
                           commaAsDecimal = FALSE,
                           allowNA = T))

  expect_error(readPlate(temp_file,
                         rowRange = c(3, 18),
                         colRange =2,
                         sep = ";",
                         commaAsDecimal = FALSE,
                         allowNA = F))

  #- Clean the temporary profile
  unlink(temp_file)

})

