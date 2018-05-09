context("test-annotations.R")

test_that("get_synapse_annotations provides data frame with the correct column names", {

  ## Requires synapse login, so skip on CRAN/CI
  skip_on_cran()
  skip_on_travis()
  
  expected_names <- c(
    "key",
    "description",
    "columnType",
    "maximumSize",
    "value",
    "valueDescription",
    "source",
    "module"
  )
  dat <- get_synapse_annotations()

  expect_true(is.data.frame(dat))
  expect_true(nrow(dat) > 0)
  expect_equal(expected_names, names(dat))
  expect_false(any(c("ROW_ID", "ROW_VERSION") %in% names(dat)))
})


test_that("get_annotation_keys uses synapse annotations if source is unspecified", {
  output <- c("consortium", "fileFormat", "fundingAgency", "resourceType")

  ## Uses local data instead of calling the real get_synapse_annotations(); this
  ## way we can test get_annotation_keys() behavior for the case when `source`
  ## is unspecified without requiring a Synapse login
  with_mock(
    get_synapse_annotations = function() {
      load(system.file("testdata", "annotations.RData", package = "syndccutils"))
      return(annotations)
    },
    expect_equal(get_annotation_keys("sageCommunity"), output)
  )
  
})

test_that("get_annotation_keys uses user-supplied source if provided", {
  mysource <- data.frame(
    key = c("a", "b", "c"),
    module = c("x", "x", "y"),
    stringsAsFactors = FALSE
  )

  expect_equal(get_annotation_keys("x", mysource), c("a", "b"))
  expect_equal(get_annotation_keys("y", mysource), c("c"))
  expect_equal(get_annotation_keys(source = mysource), c("a", "b", "c"))
})
