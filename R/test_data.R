library(tidyverse)

mock_fileview_df <- tibble(
    id = str_c("syn", c(1:10)),
    projectId = str_c("syn", floor(seq(1, 3.5, length.out = 10)), "00"),
    name = str_c("file", c(1:10), ".dat"),
    id_annotation = str_c("sample_", floor(seq(1, 5.5, length.out = 10))),
    status_annotation = str_c("disease_", floor(seq(1, 3.5, length.out = 10))),
    data_annotation = str_c("assay_", rep(c(1:2), 5)),
    random_annotation = str_c("feature_", sample(LETTERS, 10, replace = TRUE)),
    incomplete_annotation = str_c("variable_", rep(c("X", NA), 5))
)

# paths to mocked files
mock_chart_filename <- "testdata/mock_chart.html"
