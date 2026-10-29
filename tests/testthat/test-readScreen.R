load("testData/expectedOutput/readScreenData.RData")
rawDir = "testData/rawData/"
plateAnno = "testData/plateAnno/plateAnno.tsv"
wellAnno = "testData/wellAnno/wellAnno.tsv"

testScreenAutoload <- readScreen(rawDir = rawDir,
                                 plateAnnotationFile = plateAnno,
                                 wellAnnotationFile = wellAnno,
                                 negWell = c("DMSO","PBS")
                                 )

test_that("Read screen data with minimal input from user.",
        expect_equal(testScreenAutoload, screenData_auto))

testScreenManualload <- readScreen(
  rawDir = rawDir,
  plateAnnotationFile = plateAnno,
  wellAnnotationFile = wellAnno,
  sep = ";",
  rowRange = c(2,18),
  colRange = c(1,25),
  negWell = c("DMSO", "PBS")
)

test_that("Manual imput will result in identical dataframe",
          expect_equal(testScreenManualload, screenData_auto))

testScreenNorm <- readScreen(
  rawDir = rawDir,
  plateAnnotationFile = plateAnno,
  wellAnnotationFile = wellAnno,
  negWell = c("DMSO","PBS"),
  normalization = TRUE
)

test_that("Calling normalization from loading works as intended",
          expect_equal(testScreenNorm, screenData_norm))

testScreenNormDiscard <- readScreen(
  rawDir = rawDir,
  plateAnnotationFile = plateAnno,
  wellAnnotationFile = wellAnno,
  negWell = c("DMSO","PBS"),
  normalization = TRUE,
  discardLayer = 2
)

test_that("Discard layer normalization functioning",
          expect_equal(testScreenNormDiscard, screenData_norm_discard))

testScreenNormNPI <- readScreen(
  rawDir = rawDir,
  plateAnnotationFile = plateAnno,
  wellAnnotationFile = wellAnno,
  negWell = c("DMSO", "PBS"),
  posWell = c("Afatinib"),
  normalization = TRUE,
  method = "NPI"
)

test_that("Normalization using NPI from loading working correctly",
          expect_equal(testScreenNormNPI, screenData_norm_NPI))


test_that("readScreen returns the documented tabular structure", {
  expect_s3_class(testScreenAutoload, "data.frame")
  expect_named(testScreenAutoload, names(screenData_auto))
  expect_equal(dim(testScreenAutoload), dim(screenData_auto))
  expect_equal(nrow(testScreenAutoload), nrow(screenData_auto))
  expect_equal(ncol(testScreenAutoload), ncol(screenData_auto))
  expect_true(all(c("wellID", "rowID", "colID", "value",
                    "fileName", "wellType") %in% names(testScreenAutoload)))
})

test_that("normalization adds numeric normalized values without changing dimensions", {
  expect_s3_class(testScreenNorm, "data.frame")
  expect_equal(dim(testScreenNorm), dim(screenData_norm))
  expect_true("normVal" %in% names(testScreenNorm))
  expect_type(testScreenNorm$normVal, "double")
  expect_equal(testScreenNorm$normVal, screenData_norm$normVal, tolerance = 1e-12)
})

test_that("boundary options are accepted", {
  expect_silent(
    readScreen(
      rawDir = rawDir,
      plateAnnotationFile = plateAnno,
      wellAnnotationFile = wellAnno,
      sep = ";",
      negWell = character(),
      posWell = character(),
      normalization = FALSE,
      discardLayer = 0L,
      commaAsDecimal = FALSE,
      allowNA = FALSE,
      bySample = FALSE
    )
  )
})

test_that("invalid manual ranges warn and fall back to automatic parsing", {
  expect_warning(
    automatic_from_invalid_range <- readScreen(
      rawDir = rawDir,
      plateAnnotationFile = plateAnno,
      wellAnnotationFile = wellAnno,
      sep = ";",
      rowRange = c(0L, 18L),
      colRange = 2L,
      negWell = c("DMSO", "PBS")
    ),
    "positive integers"
  )
  expect_equal(automatic_from_invalid_range, screenData_auto)

  expect_warning(
    incomplete_range <- readScreen(
      rawDir = rawDir,
      plateAnnotationFile = plateAnno,
      wellAnnotationFile = wellAnno,
      sep = ";",
      rowRange = 3L,
      colRange = integer(),
      negWell = c("DMSO", "PBS")
    ),
    "Rows and columns must be specified"
  )
  expect_equal(incomplete_range, screenData_auto)
})

test_that("invalid input arguments are rejected", {
  expect_error(readScreen(rawDir = 1, plateAnnotationFile = plateAnno,
                          wellAnnotationFile = wellAnno))
  expect_error(readScreen(rawDir = "does-not-exist", plateAnnotationFile = plateAnno,
                          wellAnnotationFile = wellAnno))
  expect_error(readScreen(rawDir = rawDir, plateAnnotationFile = "missing.tsv",
                          wellAnnotationFile = wellAnno))
  expect_error(readScreen(rawDir = rawDir, plateAnnotationFile = plateAnno,
                          wellAnnotationFile = "missing.tsv"))
  expect_error(readScreen(rawDir = rawDir, plateAnnotationFile = plateAnno,
                          wellAnnotationFile = wellAnno, sep = NA_character_),
                )
  expect_error(readScreen(rawDir = rawDir, plateAnnotationFile = plateAnno,
                          wellAnnotationFile = wellAnno, setManualRanges = 1)
                )
  expect_error(readScreen(rawDir = rawDir, plateAnnotationFile = plateAnno,
                          wellAnnotationFile = wellAnno, normalization = 1)
                )
  expect_error(readScreen(rawDir = rawDir, plateAnnotationFile = plateAnno,
                          wellAnnotationFile = wellAnno, negWell = 1)
                )
  expect_error(readScreen(rawDir = rawDir, plateAnnotationFile = plateAnno,
                          wellAnnotationFile = wellAnno, posWell = 1)
                )
  expect_error(readScreen(rawDir = rawDir, plateAnnotationFile = plateAnno,
                          wellAnnotationFile = wellAnno, commaAsDecimal = 1)
                )
  expect_error(readScreen(rawDir = rawDir, plateAnnotationFile = plateAnno,
                          wellAnnotationFile = wellAnno, allowNA = 1)
                )
  expect_error(readScreen(rawDir = rawDir, plateAnnotationFile = plateAnno,
                          wellAnnotationFile = wellAnno, bySample = 1)
                )
})

test_that("normalization arguments are validated when normalization is enabled", {
  expect_error(
    readScreen(rawDir = rawDir, plateAnnotationFile = plateAnno,
               wellAnnotationFile = wellAnno, normalization = TRUE,
               method = "unsupported"),

  )
  expect_error(
    readScreen(rawDir = rawDir, plateAnnotationFile = plateAnno,
               wellAnnotationFile = wellAnno, normalization = TRUE,
               discardLayer = -1)

  )
  expect_error(
    readScreen(rawDir = rawDir, plateAnnotationFile = plateAnno,
               wellAnnotationFile = wellAnno, normalization = TRUE,
               discardLayer = c(0, 1)),

  )
})

test_that("normalization output is numerically accurate", {
  expect_equal(testScreenNorm$normVal, screenData_norm$normVal, tolerance = 1e-10)
  expect_equal(testScreenNormDiscard$normVal,
               screenData_norm_discard$normVal,
               tolerance = 1e-10)
})
