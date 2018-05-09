context("test-manifest.R")

test_that("generate_manifest creates 0-row data frame", {
  dat <- generate_manifest()
  expect_true(is.data.frame(dat))
  expect_equal(nrow(dat), 0)
})

test_that("generate_manifest includes correct columns", {
  standard_cols <- c("path", "parent", "name", "used", "executed")
  annotation_cols <- c("study", "assay", "tissue")
  expected_combo <- c(standard_cols, annotation_cols)
  
  dat1 <- generate_manifest()
  dat2 <- generate_manifest(keys = annotation_cols)

  ## Example with duplicate keys
  dat3 <- generate_manifest(keys = c(annotation_cols, "study", "parent"))
  
  expect_equal(names(dat1), standard_cols)
  expect_equal(names(dat2), c(standard_cols, annotation_cols))
  expect_equal(names(dat3), c(standard_cols, annotation_cols))
  
})

test_that("write_manifest writes to excel file", {
  tmp <- tempfile()
  on.exit(unlink(tmp))

  x <- data.frame(x = 1:5)
  write_manifest(x, tmp)

  expect_equal(openxlsx::read.xlsx(tmp), x)
})
