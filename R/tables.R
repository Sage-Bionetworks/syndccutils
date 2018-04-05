## Functions for building and formatting summary tables (data frames, markdown,
## or HTML datatables) based on Synapse annotation data


#' Format data frame as markdown table
#'
#' @param df
#' @param cols_as_code
#'
#' @export
as_wiki_markdown <- function(df, cols_as_code = c()) {

    if (length(cols_as_code)) {
        # first add in backticks for code blocks
        df <- df %>%
            mutate_at(cols_as_code, funs(stringr::str_c("`", ., "`")))
    }
    df %>% knitr::kable(format = "markdown")
}


#' Convert and format data frame as datatable widget.
#'
#' @param df
#' @param cols_as_code
#'
#' @export
as_datatable <- function(df, cols_as_code = c()) {
    js_formatting <- htmlwidgets::JS(
        "function(settings, json) {
            $(this.api().table().body()).css({
                'font-family': 'Roboto, Open Sans, sans-serif',
                'font-size': '13px'
            });
            $(this.api().table().header()).css({
                'font-family': 'Roboto, Open Sans, sans-serif',
                'font-size': '14px'
            });
            $(this.api().table().container()).css({
                'font-family': 'Roboto, Open Sans, sans-serif',
                'font-size': '14px'
            });
        }"
    )
    df %>%
        DT::datatable(escape = FALSE, rownames = FALSE,
                      options=list(
                          pageLength = min(nrow(df), 10),
                          dom = 'tp',
                          initComplete = js_formatting
                      )
        )
}


# Core summary tables (data files) ----------------------------------------

#' Count files in a Synapse file view, grouped by annotation keys.
#'
#' @param view_df data frame representing Synapse file view
#' @param annotation_keys character vector of grouping keys
#' @param table_id string representing Synapse ID for file view
#' @param synproject_key if 'projectId' is in `annotation_keys`, a string
#'   indicating a corresponding key to rename the column
#' @param count_cols character vector of keys for which to count files
#' @param list_cols character vector of keys for which to list values
#' @param augment_keys list mapping target columns (list names) for which to
#'   prepend values of corresponding meta columns (list values)
#' @param link_keys list mapping target columns (list names) for which to
#'   construct links from Synapse IDs in corresponding ID columns (list values)
#' @param filter_missing remove records with missing annotation values
#'
#' @export
#'
#' @examples
#' group_keys <- c("assay", "tumorType")
#' list_cols <- "study"
#' augment_keys <- list(study = "Center Name")
#' link_keys <- list(study = "Study")
#' fileview_df %>%
#'    summarize_by_annotationkey(
#'        annotation_keys = group_keys,
#'        table_id = master_fileview_id,
#'        count_cols = count_cols,
#'        list_cols = list_cols,
#'        augment_keys = augment_keys,
#'        link_keys = link_keys
#'    )
summarize_by_annotationkey <- function(
    view_df, annotation_keys, table_id, synproject_key = NULL,
    count_cols = NULL, list_cols = NULL, augment_keys = NULL, link_keys = NULL,
    filter_missing = TRUE, queryformat = "html", list_format = "html"
) {
    if (is.null(count_cols)) {
        count_cols <- "id"
    }

    if (filter_missing) {
        view_df <- view_df %>%
            filter_by_key(filter_keys = annotation_keys,
                          bad_values = c("null", "Not Applicable"))
    }

    if (!is.null(augment_keys)) {
        view_df <- view_df %>%
            augment_values(augment_keys = augment_keys)
    }

    if (!is.null(link_keys)) {
        view_df <- view_df %>%
            create_synapse_links(link_keys = link_keys)
    }

    query_keys <- annotation_keys
    if ("projectId" %in% annotation_keys & !is.null(synproject_key)) {
        annotation_keys <- c(synproject_key, annotation_keys)
    }

    count_df <- view_df %>%
        count_values(group_keys = annotation_keys,
                     count_keys = count_cols)

    if (!is.null(list_cols)) {
        list_df <- view_df %>%
            list_values(group_keys = annotation_keys,
                        list_keys = list_cols,
                        list_format = list_format)

        summary_df <- left_join(count_df, list_df, by = annotation_keys)
    } else {
        summary_df <- count_df
    }

    query_cols <- sapply(query_keys, as.name)
    summary_df <- summary_df %>%
        rowwise() %>%
        mutate(sourceFileview = table_id,
                      query = build_tablequery(sourceFileview,
                                               rlang::UQS(query_cols))) %>%
        add_queryview_column(format = queryformat) %>%
        select(-query, -sourceFileview) %>%
        ungroup()

    if ("projectId" %in% annotation_keys & !is.null(synproject_key)) {
        summary_df %>%
            select(-matches("projectId"))
    } else {
        summary_df
    }
}


# adapted from lines 29-33 in 'fileViewReporting.Rmd'
# TODO: this should be deprecated by a join with a Project View...
summarize_project_info <- function(view_df) {
    proj_info <-
        sapply(unique(c(view_df$projectId)),
               function(x) {
                   res = synGet(x)
                   annotations = synGetAnnotations(x)
                   c(
                       projectId = x,
                       projectName = ifelse(is.null(res$properties$name),"",res$properties$name),
                       Center = ifelse(is.null(res$properties$name),"",res$properties$name),
                       Program = ifelse(is.null(annotations$consortium[[1]]),"",annotations$consortium[[1]]),
                       Institution = ifelse(is.null(annotations$institution[[1]]),"",annotations$institution[[1]])
                   )
               })

    proj_info = data.frame(t(proj_info))
    proj_info
}

