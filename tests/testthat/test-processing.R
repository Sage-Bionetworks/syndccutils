context("Munging based on missing/bad values")

test_that("add_missing_placeholder replaces NA values with default string", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y,
        "foo", "Not Annotated",
        "Not Annotated", "bar"
    )

    test_result <- tibble::tibble(col_x = c("foo", NA),
                                  col_y = c(NA, "bar")) %>%
        add_missing_placeholder()

    expect_equal(test_result, expected_result)
})

test_that("add_missing_placeholder replaces NA values with specified string", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y,
        "foo", "unknown",
        "unknown", "bar"
    )

    test_result <- tibble::tibble(col_x = c("foo", NA),
                                  col_y = c(NA, "bar")) %>%
        add_missing_placeholder(placeholder = "unknown")

    expect_equal(test_result, expected_result)
})

test_that("add_missing_placeholder replaces NA values with default string for selected keys", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y,
        "foo", NA,
        "Not Annotated", "bar"
    )

    test_result <- tibble::tibble(col_x = c("foo", NA),
                                  col_y = c(NA, "bar")) %>%
        add_missing_placeholder(replace_keys = c("col_x"))

    expect_equal(test_result, expected_result)
})

test_that("filter_by_key removes rows for which specified annotation key values are missing", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y,
        "bar", "baz"
    )

    test_result <- tibble::tibble(col_x = c("foo", "bar"),
                                  col_y = c(NA, "baz")) %>%
        filter_by_key(filter_keys = c("col_x", "col_y"))

    expect_equal(test_result, expected_result)
})

test_that("filter_by_key removes rows for which specified annotation key values are missing or bad (default)", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y,
        "bar", "baz"
    )

    test_result <- tibble::tibble(col_x = c("foo", "bar", "null", "y"),
                                  col_y = c(NA, "baz", "x", "Not Applicable")) %>%
        filter_by_key(filter_keys = c("col_x", "col_y"))

    expect_equal(test_result, expected_result)
})

test_that("filter_by_key removes rows for which specified annotation key values are missing or bad (custom)", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y,
        "null", "x",
        "y", "Not Applicable"
    )

    test_result <- tibble::tibble(col_x = c("foo", "bar", "null", "y"),
                                  col_y = c(NA, "baz", "x", "Not Applicable")) %>%
        filter_by_key(filter_keys = c("col_x", "col_y"), bad_values = c("bar"))

    expect_equal(test_result, expected_result)
})

context("Targeted combination of columns")

test_that("augment_values combines column values as expected (one pair)", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y,
        "foo", "foo - bar",
        "bar", "bar - baz"
    )

    test_result <- tibble::tibble(col_x = c("foo", "bar"),
                                  col_y = c("bar", "baz")) %>%
        augment_values(augment_keys = list(col_y = "col_x"))

    expect_equal(test_result, expected_result)
})

test_that("augment_values combines column values as expected (multiple pairs)", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y, ~col_xx, ~col_yy,
        "foo", "foo - bar", "foo1", "foo1 - bar1",
        "bar", "bar - baz", "bar1", "bar1 - baz1"
    )

    test_result <- tibble::tibble(col_x = c("foo", "bar"),
                                  col_y = c("bar", "baz"),
                                  col_xx = c("foo1", "bar1"),
                                  col_yy = c("bar1", "baz1")) %>%
        augment_values(augment_keys = list(col_y = "col_x", col_yy = "col_xx"))

    expect_equal(test_result, expected_result)
})


test_that("create_synapse_links correctly builds Synapse links (one pair)", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y,
        "syn111", "<a href='https://www.synapse.org/#!Synapse:syn111' target='_blank'>foo</a>",
        "syn222", "<a href='https://www.synapse.org/#!Synapse:syn222' target='_blank'>bar</a>"
    )

    test_result <- tibble::tibble(col_x = c("syn111", "syn222"),
                                  col_y = c("foo", "bar")) %>%
        create_synapse_links(link_keys = list(col_y = "col_x"))

    expect_equal(test_result, expected_result)
})

test_that("create_synapse_links correctly builds Synapse links (multiple pairs)", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y,
        "syn111", "<a href='https://www.synapse.org/#!Synapse:syn111' target='_blank'>foo</a>",
        "syn222", "<a href='https://www.synapse.org/#!Synapse:syn222' target='_blank'>bar</a>"
    ) %>%
        dplyr::bind_cols(
            dplyr::tribble(
                ~col_xx, ~col_yy,
                "syn333", "<a href='https://www.synapse.org/#!Synapse:syn333' target='_blank'>foo1</a>",
                "syn444", "<a href='https://www.synapse.org/#!Synapse:syn444' target='_blank'>bar1</a>"
            )
        )

    test_result <- tibble::tibble(col_x = c("syn111", "syn222"),
                                  col_y = c("foo", "bar"),
                                  col_xx = c("syn333", "syn444"),
                                  col_yy = c("foo1", "bar1")) %>%
        create_synapse_links(link_keys = list(col_y = "col_x", col_yy = "col_xx"))

    expect_equal(test_result, expected_result)
})


context("Table summarization")

test_that("count_values correctly returns number of distinct values based on group and count keys", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y,
        "foo", 2L,
        "bar", 1L
    )

    test_result <- tibble::tibble(col_x = c("foo", "foo", "bar"),
                                  col_y = c("a", "b", "c")) %>%
        count_values(group_keys = c("col_x"), count_keys = c("col_y"))

    expect_equal(test_result, expected_result)
})

test_that("list_values correctly returns number of distinct values based on group and count keys", {
    expected_result <- dplyr::tribble(
        ~col_x, ~col_y,
        "bar", "<ul><li>c</li></ul>",
        "foo", "<ul><li>a</li><li>b</li></ul>"
    )

    test_result <- tibble::tibble(col_x = c("foo", "foo", "bar"),
                                  col_y = c("a", "b", "c")) %>%
        list_values(group_keys = c("col_x"), list_keys = c("col_y"))

    expect_equal(test_result, expected_result)
})

context("Table construction/modification")

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
