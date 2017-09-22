library(tidyverse)
library(synapseClient)
library(DT)

# Table utility functions -------------------------------------------------

#' Construct a Synapse SQL-style table query using names/values of data frame
#' column(s) to compose 'WHERE' clauses
#'
#' @param table_id
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' test_df %>%
#'     rowwise() %>%
#'     mutate(query = build_tablequery(table_id, assay))
build_tablequery <- function(table_id, ...) {
    query_template <- "SELECT * FROM {id} WHERE ( {filters} )"
    dots <- substitute(list(...))[-1]
    list_names <- sapply(dots, deparse)
    list(...) %>%
        set_names(list_names) %>%
        map2(names(.), function(value, key) {
            filter_string <- glue::glue("( {key} = '{value}' )",
                                        key = key, value = value)
            filter_string
        }) %>%
        flatten_chr() %>%
        stringr::str_c(collapse = " AND ") %>%
        glue::glue(query_template, id = table_id, filters = .)
}


#' Construct a URL to view the results of a Synapse table query
#'
#' @param table_id
#' @param query_string
#'
#' @return
#' @export
#'
#' @examples
get_tablequery_url <- function(table_id, query_string) {
    query <- list(limit = 25,
                  sql = query_string,
                  isConsistent = TRUE,
                  offset = 0) %>%
        jsonlite::toJSON(auto_unbox = TRUE)
    query_encoded <- openssl::base64_encode(query)
    base_url <- "https://www.synapse.org/#!Synapse:{id}/tables/query/{query}"
    glue::glue(base_url, id = table_id, query = query_encoded)
}


#' Shortcut function that wraps `build_tablequery` and `get_tablequery_url`
#' to add a new column with link to view table query results
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
add_queryview_column <- function(df, format = c("markdown", "html")) {
    link_templates <- list(
        markdown = "[View]({url})",
        html = '<a href="{url}" target="_blank">View</a>'
    )
    link_template <- link_templates[[format]]
    df %>%
        mutate(viewFiles = get_tablequery_url(sourceFileview, query),
               viewFiles = glue::glue(link_template, url = viewFiles))
}


#' Format data frame as markdown table
#'
#' @param df
#' @param cols_as_code
#'
#' @return
#' @export
#'
#' @examples
as_wiki_markdown <- function(df, cols_as_code = c()) {

    if (length(cols_as_code)) {
        # first add in backticks for code blocks
        df <- df %>%
            dplyr::mutate_at(cols_as_code, funs(stringr::str_c("`", ., "`")))
    }
    df %>% knitr::kable(format = "markdown")
}


as_datatable <- function(df, cols_as_code = c()) {
    df %>%
        datatable(escape = FALSE, rownames = FALSE, height = 400,
                  options=list(
                      pageLength = min(nrow(df), 10),
                      dom = 'tp',
                      initComplete = JS("
                                    function(settings, json) {
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
                                    }
                                    ")
                  )
        )
}


format_summarytable_columns <- function(df, facet_cols = c()) {
    name_map <- tibble(name = names(df)) %>%
        mutate(formatted_name = case_when(
            name == "id" ~ "Files",
            name == "assay" & (name %in% facet_cols) ~ "Assay",
            name == "assay" & !(name %in% facet_cols) ~ "Assays",
            name == "diagnosis" & (name %in% facet_cols) ~ "Diagnosis",
            name == "diagnosis" & !(name %in% facet_cols) ~ "Diagnoses",
            name == "tumorType" & (name %in% facet_cols) ~ "Tumor Type",
            name == "tumorType" & !(name %in% facet_cols) ~ "Tumor Types",
            name == "individualID" & (name %in% facet_cols) ~ "Individual",
            name == "individualID" & !(name %in% facet_cols) ~ "Individuals",
            name == "cellLine" & (name %in% facet_cols) ~ "Cell Line",
            name == "cellLine" & !(name %in% facet_cols) ~ "Cell Lines",
            name == "specimenID" & (name %in% facet_cols) ~ "Specimen",
            name == "specimenID" & !(name %in% facet_cols) ~ "Specimens",
            name == "Center" & (name %in% facet_cols) ~ "Center",
            name == "Center" & !(name %in% facet_cols) ~ "Centers",
            name == "sourceFileview" ~ "Source Fileview",
            name == "viewFiles" ~ "View Files",
            TRUE ~ name
        )) %>%
        split(.$name) %>%
        map("formatted_name")
    plyr::rename(df, name_map)
}


# Core summary tables (data files) ----------------------------------------

#' Count files in a Synapse file view, grouped by two annotation keys.
#'
#' @param view_df data frame representing Synapse file view
#' @param annotation_keys character vector of grouping keys
#' @param table_id string representing Synapse ID for file view
#' @param count_cols character vector of keys for which to count files
#' @param filter_missing remove records with missing annotation values
#'
#' @return
#' @export
#'
#' @examples
#' group_keys <- c("assay", "tumorType")
#' summarize_datafiles_by_annotationkey(fileview_df, group_keys, fileview_id)
summarize_files_by_annotationkey <- function(
    view_df, annotation_keys, table_id, count_cols = NULL, filter_missing = TRUE
) {
    if (is.null(count_cols)) {
        count_cols <- c("id", "diagnosis", "individualID")
    }

    if (filter_missing) {
        view_df <- view_df %>%
            filter_at(vars(one_of(annotation_keys)),
                      all_vars(!is.na(.) & !(. %in% c("null", "Not Applicable"))))
    }

    query_keys <- annotation_keys
    if ("Center" %in% annotation_keys) {
        annotation_keys <- c("projectId", annotation_keys)
        query_keys[query_keys == "Center"] <- "projectId"
    }

    query_cols <- sapply(query_keys, as.name)
    view_df %>%
        dplyr::group_by(.dots = annotation_keys) %>%
        dplyr::summarise_at(count_cols, n_distinct) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(sourceFileview = table_id,
               query = build_tablequery(sourceFileview,
                                        rlang::UQS(query_cols))) %>%
        add_queryview_column(format = "html") %>%
        dplyr::select(-query, -matches("projectId")) %>%
        dplyr::ungroup()
}


# adapted from lines 29-33 in 'fileViewReporting.Rmd'
summarize_project_info <- function(view_df) {
    proj_info <-
        sapply(unique(c(view_df$projectId)),
               function(x) {
                   res = synGet(x)
                   c(
                       projectId = x,
                       Center = res@properties$name,
                       Program = res@annotations$consortium,
                       Institution = res@annotations$institution
                   )
               })

    proj_info = data.frame(t(proj_info))
    proj_info
}

