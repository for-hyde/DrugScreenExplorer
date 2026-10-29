#Load data
load("testData/expectedOutput/screenData.RData")

#Create tempdir, save report to files.
test_output_dir <- tempfile("test_report_dir_")
dir.create(test_output_dir)
#Ensure folder is deletect upon closure
on.exit(unlink(test_output_dir, recursive = TRUE), add = TRUE)

makeReport(
  screenData = screenData_norm,
  dir = test_output_dir,
  title = "Test Report",
  author = "F Hyde",
  reportDate = date("2026-01-01")
)

# Input validations working
test_that(
  "Incorrect inputs are signalled to user", {
    expect_error(makeReport(screenData, test_output_dir)) #Not normalized
    expect_error(makeReport("Not a dataframe", test_output_dir))
    expect_error(makeReport(screenData_norm, dir = 7)) #Incorrect dir formatting
    expect_error(makeReport(screenData_norm, dir = "/User/InvalidFolder"))
    expect_error(makeReport(screenData_norm, test_output_dir, title = 12))
    expect_error(makeReport(screenData_norm, test_output_dir,
                            reportDate = "Today"))
  }
)

test_that("Output files are correctly generated", {
  report_dir <- list.files(test_output_dir, full.names = TRUE)

  expect_true(dir.exists(report_dir))
  expect_equal(basename(report_dir), "report_2026-01-01")

  report_files <- list.files(report_dir, full.names = TRUE,
                             pattern = "\\.html$")

  html_content <- paste(readLines(report_files, warn = FALSE),
                        fixed = TRUE, collapse = "\n")

  expect_true(grepl("F Hyde", html_content))
  expect_true(grepl("Test Report", html_content))
  }
)
