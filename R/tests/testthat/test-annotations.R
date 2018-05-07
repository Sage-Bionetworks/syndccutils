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
