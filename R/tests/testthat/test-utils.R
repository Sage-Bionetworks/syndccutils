
context("utils - Modifying HTML output files")

mock_chart_filename <- "testdata/mock_chart.html"

test_that("get_script_lines returns expected dataframe of target line and attribute", {

    test_result <- get_script_lines(mock_chart_filename)

    expect_equal(names(test_result), c("target", "target_attr"))
    expect_equal(nrow(test_result), 6)
})

test_that("get_link_lines returns expected dataframe of target line and attribute", {
    test_result <- get_link_lines(mock_chart_filename)

    expect_equal(names(test_result), c("target", "target_attr"))
    expect_equal(nrow(test_result), 2)
})

test_that("path_replace_cdn correctly replaces local path with CDN path", {
    mock_path <- "mock_chart_files/htmlwidgets-0.9/htmlwidgets.js"
    expected_result <- "https://cdn-www.synapse.org/research/htmlwidgets-0.9/htmlwidgets.js"
    test_result <- path_replace_cdn(mock_path)

    expect_equal(test_result, expected_result)
})

test_that("path_replace_cdn correctly replaces local path with CDN path for a public asset", {
    mock_path <- "mock_chart_files/plotlyjs-1.29.2/plotly-latest.min.js"
    expected_result <- "https://cdn.plot.ly/plotly-1.29.2.min.js"
    test_result <- path_replace_cdn(mock_path)

    expect_equal(test_result, expected_result)
})

test_that("update_html_lines replaces path for all script lines", {
    mock_html_lines <- readr::read_lines(mock_chart_filename)
    mock_target_lines <- get_script_lines(mock_chart_filename)

    test_html_lines <- update_html_lines(mock_html_lines, mock_target_lines)
    test_updated_count <- sum(
        stringr::str_detect(test_html_lines, "<script src=\"https://cdn-www.synapse.org")
    )
    test_public_count <- sum(
        stringr::str_detect(test_html_lines, "<script src=\"https://(ajax.googleapis.com|cdn.plot.ly)")
    )
    test_target_count <- sum(
        stringr::str_detect(test_html_lines, "<script src=\"mock_chart_files/")
    )

    expect_equal(test_updated_count, 4)
    expect_equal(test_public_count, 2)
    expect_equal(test_target_count, 0)
})

test_that("update_html_lines replaces path for all link lines", {
    mock_html_lines <- readr::read_lines(mock_chart_filename)
    mock_target_lines <- get_link_lines(mock_chart_filename)

    test_html_lines <- update_html_lines(mock_html_lines, mock_target_lines)
    test_updated_count <- sum(
        stringr::str_detect(test_html_lines, "<link href=\"https://cdn-www.synapse.org/research/")
    )
    test_target_count <- sum(
        stringr::str_detect(test_html_lines, "<link href=\"mock_chart_files/")
    )

    expect_equal(test_updated_count, 2)
    expect_equal(test_target_count, 0)
})


test_that("fix_js_assets replaces all script/link lines and writes new HTML file", {
    expected_result <- file.path(
        dirname(mock_chart_filename),
        stringr::str_c("fixed_", basename(mock_chart_filename))
    )
    test_result <- fix_js_assets(mock_chart_filename,
                                 rename_file = TRUE)
    expect_equal(test_result, expected_result)
})

