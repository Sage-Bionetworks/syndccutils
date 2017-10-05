context("Table construction/modification functions")

test_that("build_tablequery builds correct query for single column, one row", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y, ~query,
        "foo", "bar", "SELECT * FROM test_synID WHERE ( ( col_x = 'foo' ) )"
    )

    test_result <- tibble::tibble(col_x = "foo", col_y = "bar") %>%
        dplyr::rowwise() %>%
        dplyr::mutate(query = build_tablequery("test_synID", col_x))

    expect_equal(test_result, expected_result)
})


test_that("build_tablequery builds correct query for single column, multiple rows", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y, ~query,
        "foo", "bar", "SELECT * FROM test_synID WHERE ( ( col_x = 'foo' ) )",
        "baz", "bar", "SELECT * FROM test_synID WHERE ( ( col_x = 'baz' ) )"
    )

    test_result <- tibble::tibble(col_x = c("foo", "baz"), col_y = "bar") %>%
        dplyr::rowwise() %>%
        dplyr::mutate(query = build_tablequery("test_synID", col_x))

    expect_equal(test_result, expected_result)
})


test_that("build_tablequery builds correct query for multiple columns", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y, ~query,
        "foo", "bar", "SELECT * FROM test_synID WHERE ( ( col_x = 'foo' ) AND ( col_y = 'bar' ) )"
    )

    test_result <- tibble::tibble(col_x = "foo", col_y = "bar") %>%
        dplyr::rowwise() %>%
        dplyr::mutate(query = build_tablequery("test_synID", col_x, col_y))

    expect_equal(test_result, expected_result)
})
