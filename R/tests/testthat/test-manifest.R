context("test-manifest.R")

test_that("generate_manifest_template creates 0-row data frame", {
  dat <- generate_manifest_template()
  expect_true(is.data.frame(dat))
  expect_equal(nrow(dat), 0)
})

test_that("generate_manifest_template includes correct columns", {
  standard_cols <- c("path", "parent", "name", "used", "executed")
  annotation_cols <- c("study", "assay", "tissue")
  expected_combo <- c(standard_cols, annotation_cols)
  
  dat1 <- generate_manifest_template()
  dat2 <- generate_manifest_template(keys = annotation_cols)

  ## Example with duplicate keys
  dat3 <- generate_manifest_template(keys = c(annotation_cols, "study", "parent"))
  
  expect_equal(names(dat1), standard_cols)
  expect_equal(names(dat2), c(standard_cols, annotation_cols))
  expect_equal(names(dat3), c(standard_cols, annotation_cols))
  
})

test_that("generate_key/value_description includes correct columns", {
  dat <- with_mock(
    get_synapse_annotations = function() {
      load(system.file("testdata", "annotations.RData", package = "syndccutils"))
      return(annotations)
    },
    get_synapse_annotations()
  )
  key_desc_names <- c("key", "description", "columnType", "module")
  val_desc_names <- c("key", "value", "valueDescription", "source", "module")
  expect_equal(names(generate_key_description(dat)), key_desc_names)
  expect_equal(names(generate_value_description(dat)), val_desc_names)
})

test_that("no duplicated rows in generate_key_description output", {
  dat <- with_mock(
    get_synapse_annotations = function() {
      load(system.file("testdata", "annotations.RData", package = "syndccutils"))
      return(annotations)
    },
    get_synapse_annotations()
  )
  desc <- generate_key_description(dat)
  expect_false(any(duplicated(desc)))
})

test_that("write_manifest writes to excel file", {
  x1 <- data.frame(x = 5)
  x2 <- list(x1, x1)

  tmp1 <- tempfile()
  tmp2 <- tempfile()
  on.exit(unlink(c(tmp1, tmp2)))
  
  write_manifest(x1, tmp1)
  write_manifest(x2, tmp2)

  expect_equal(openxlsx::read.xlsx(tmp1), x1)
  expect_equal(
    lapply(1:2, function(x) {openxlsx::read.xlsx(tmp2, sheet = x)}),
    x2
  )
})

test_that("write_manifest writes passes ... to write.xlsx", {
  tmp <- tempfile()
  on.exit(unlink(tmp))

  dat <- data.frame(x = 1:5, y = 6:10)
  write_manifest(dat, tmp, colNames = FALSE)
  dat2 <- openxlsx::read.xlsx(tmp)

  expect_false(any(c("x", "y") %in% names(dat2)))
  
})
