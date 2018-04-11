library(tibble)
library(dplyr)
library(stringr)
library(fs)
library(htmlwidgets)

mock_fileview_df <- tibble(
    id = str_c("syn", c(1:10)),
    projectId = str_c("syn", floor(seq(1, 3.5, length.out = 10)), "00"),
    center = str_c("Center ", floor(seq(1, 3.5, length.out = 10))),
    study = str_c("Study ", floor(seq(1, 3.5, length.out = 10)),
                  ".", rep(1:2, 5)),
    name = str_c("file", c(1:10), ".dat"),
    id_annotation = str_c("sample_", floor(seq(1, 5.5, length.out = 10))),
    status_annotation = str_c("disease_", floor(seq(1, 3.5, length.out = 10))),
    data_annotation = str_c("assay_", rep(c(1:2), 5)),
    random_annotation = str_c("feature_", sample(LETTERS, 10, replace = TRUE)),
    incomplete_annotation = str_c("variable_", rep(c("X", NA), 5))
)


# paths to mocked files
mock_chart_filename <- "mock_chart.html"
mock_datatable_filename <- "mock_datatable.html"

# create and save mock chart
plot_keys <- list(data_annotation = "Assay", status_annotation = "Disease")
mock_chart <- mock_fileview_df %>%
    plot_file_counts_by_annotationkey(plot_keys, chart_height = 300)

saveWidget(
  mock_chart,
  file.path(normalizePath("./testdata/"), mock_chart_filename),
  selfcontained = FALSE
)

# create and save mock table
group_keys <- c("data_annotation", "status_annotation")
count_cols <- c("id", "id_annotation")
list_cols <- "random_annotation"

mock_datafile_counts <- mock_fileview_df %>%
    summarize_by_annotationkey(
        annotation_keys = group_keys,
        table_id = "syn1234",
        count_cols = count_cols,
        list_cols = list_cols
    )

mock_datafile_counts_dt <- mock_datafile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()

saveWidget(
  mock_datafile_counts_dt,
  file.path(normalizePath("./testdata/"), mock_datatable_filename),
  selfcontained = FALSE
)

