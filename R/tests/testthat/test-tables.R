context("tables - Building summary tables")

test_that("summarize_by_annotationkey works...", {

    mock_view_df <- mock_fileview_df
    mock_view_df[mock_view_df$id == "syn2", "projectId"] <- NA
    test_result <- mock_view_df %>%
        summarize_by_annotationkey(
            annotation_keys = "projectId",
            table_id = "syn1234",
            synproject_key = "center",
            count_cols = "id",
            list_cols = "study",
            augment_keys = list(study = "center"),
            link_keys = list(center = "projectId"),
            filter_missing = TRUE
        )
    print(test_result)

    expect_false("syn2" %in% test_result$id)
})
