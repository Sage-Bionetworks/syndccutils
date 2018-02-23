library(tidyverse)
library(stringr)
library(synapser)
library(DT)

# Table utility functions -------------------------------------------------

#' Fill in missing values with default or specified string.
#'
#' @param df data frame nominally representing a Synapse table or file view
#' @param placeholder
#' @param replace_keys
#'
#' @return
#' @export
#'
#' @examples
add_missing_placeholder <- function(
    df, placeholder = "Not Annotated", replace_keys = NULL
) {

    if (!is.null(replace_keys)) {
        df %>%
            dplyr::mutate_at(.vars = replace_keys,
                             funs(replace(., is.na(.), placeholder)))
    } else {
        df %>%
            dplyr::mutate_all(funs(replace(., is.na(.), placeholder)))
    }
}


#' Remove rows with missing or bad values for specified keys.
#'
#' @param df data frame nominally representing a Synapse table or file view
#' @param filter_keys character vector of keys for which to check values
#' @param bad_values character vector specifying bad values to exclude
#'
#' @return
#' @export
#'
#' @examples
filter_by_key <- function(
    df, filter_keys, bad_values = c("null", "Not Applicable")
) {

    df %>%
        filter_at(vars(one_of(filter_keys)),
                  all_vars(!is.na(.) & !(. %in% bad_values)))
}


#' Augment values in one column with the value of another column.
#'
#' @param df data frame nominally representing a Synapse table or file view
#' @param augment_keys list mapping target columns (list names) for which to
#'   prepend values of corresponding meta columns (list values)
#'
#' @return
#' @export
#'
#' @examples
augment_values <- function(
    df, augment_keys
) {

    augment_keys %>%
        walk2(names(.), function(meta_key, target_key) {
            target_col <- as.name(target_key)
            meta_col <- as.name(meta_key)
            df <<- df %>%
                mutate(rlang::UQ(target_col) :=
                           ifelse(!is.na(rlang::UQ(target_col)),
                                  str_c(
                                      rlang::UQ(meta_col),
                                      rlang::UQ(target_col),
                                      sep = " — "
                                  ),
                                  rlang::UQ(target_col)))
        })
    df
}


#' Convert values to named Synapse URLs based on corresponding Synapse IDs.
#'
#' @param df data frame nominally representing a Synapse table or file view
#' @param link_keys list mapping target columns (list names) for which to
#'   construct links from Synapse IDs in corresponding ID columns (list values)
#'
#' @return
#' @export
#'
#' @examples
create_synapse_links <- function(
    df, link_keys
) {

    link_template <- "<a href='https://www.synapse.org/#!Synapse:{id}' target='_blank'>{target}</a>"
    link_keys %>%
        walk2(names(.), function(id_key, target_key) {
            target_col <- as.name(target_key)
            id_col <- as.name(id_key)
            df <<- df %>%
                mutate(rlang::UQ(target_col) :=
                           ifelse(!is.na(rlang::UQ(target_col)),
                                  glue::glue(
                                      link_template,
                                      id = rlang::UQ(id_col),
                                      target = rlang::UQ(target_col)
                                  ),
                                  rlang::UQ(target_col)))
        })
    df
}


#' Count distinct values within each group for specified keys.
#'
#' @param df data frame nominally representing a Synapse table or file view
#' @param group_keys character vector of keys by which to group
#' @param count_keys character vector of keys for which to count files
#'
#' @return
#' @export
#'
#' @examples
count_values <- function(
    df, group_keys, count_keys
) {

    group_cols <- sapply(group_keys, as.name)

    df %>%
        dplyr::group_by(rlang::UQS(group_cols)) %>%
        dplyr::summarise_at(count_keys, n_distinct) %>%
        dplyr::ungroup()
}


#' List distinct values within each group for specified keys
#'
#' @param df data frame nominally representing a Synapse table or file view
#' @param group_keys character vector of keys by which to group
#' @param list_keys character vector of keys for which to list values
#'
#' @return
#' @export
#'
#' @examples
list_values <- function(
    df, group_keys, list_keys, list_format = c("html", "csv")
) {
    list_format <- match.arg(list_format)
    sep_opts <- list(html = "</li><li>", csv = ",")
    start_opts <- list(html = "<ul><li>", csv = "")
    end_opts <- list(html = "</li></ul>", csv = "")

    group_cols <- sapply(group_keys, as.name)

    merge_strings <- function(x) {
        stringr::str_c(unique(x), collapse = sep_opts[[list_format]])
    }

    df %>%
        dplyr::group_by(rlang::UQS(group_cols)) %>%
        dplyr::summarise_at(list_keys, merge_strings) %>%
        dplyr::ungroup() %>%
        dplyr::mutate_at(.vars = list_keys,
                         funs(str_c(start_opts[[list_format]], .,
                                    end_opts[[list_format]], sep = "")))
}


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
add_queryview_column <- function(df, format = c("markdown", "html","raw")) {
    format <- match.arg(format)
    link_templates <- list(
        markdown = "[View]({url})",
        html = '<a href="{url}" target="_blank">View</a>',
        raw = '{url}'
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


#' Convert and format data frame as datatable widget.
#'
#' @param df
#' @param cols_as_code
#'
#' @return
#' @export
#'
#' @examples
as_datatable <- function(df, cols_as_code = c()) {

    df %>%
        datatable(escape = FALSE, rownames = FALSE,
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


#' Prettify column names based on annotation keys.
#'
#' @param df
#' @param facet_cols
#'
#' @return
#' @export
#'
#' @examples
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
            name == "study" & (name %in% facet_cols) ~ "Study",
            name == "study" & !(name %in% facet_cols) ~ "Studies",
            name == "softwareType" & (name %in% facet_cols) ~ "Software Type",
            name == "softwareType" & !(name %in% facet_cols) ~ "Software Types",
            name == "softwareLanguage" & (name %in% facet_cols) ~ "Software Language",
            name == "softwareLanguage" & !(name %in% facet_cols) ~ "Software Languages",
            name == "sourceFileview" ~ "Source Fileview",
            name == "viewFiles" ~ "View Files",
            TRUE ~ name
        )) %>%
        split(.$name) %>%
        map("formatted_name")
    plyr::rename(df, name_map)
}


# Core summary tables (data files) ----------------------------------------

#' Count files in a Synapse file view, grouped two annotation keys.
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
    view_df, annotation_keys, table_id, count_cols = NULL, filter_missing = TRUE, queryformat="html"
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
        add_queryview_column(format = queryformat) %>%
        dplyr::select(-query, -matches("projectId")) %>%
        dplyr::ungroup()
}


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
#' @return
#' @export
#'
#' @examples
#' group_keys <- c("assay", "tumorType")
#' list_cols <- "study"
#' augment_keys <- list(study = "Center Name")
#' link_keys <- list(study = "Study")
#' fileview_df %>%
#'    summarize_files_by_annotationkey_new(
#'        annotation_keys = group_keys,
#'        table_id = master_fileview_id,
#'        count_cols = count_cols,
#'        list_cols = list_cols,
#'        augment_keys = augment_keys,
#'        link_keys = link_keys
#'    )
summarize_files_by_annotationkey_new <- function(
    view_df, annotation_keys, table_id, synproject_key = NULL,
    count_cols = NULL, list_cols = NULL, augment_keys = NULL, link_keys = NULL,
    filter_missing = TRUE, queryformat="html"
) {
    if (is.null(count_cols)) {
        count_cols <- "id"
    }

    if (filter_missing) {
        view_df <- view_df %>%
            filter_at(vars(one_of(annotation_keys)),
                      all_vars(!is.na(.) & !(. %in% c("null", "Not Applicable"))))
    }

    if (!is.null(augment_keys)) {
        augment_keys %>%
            walk2(names(.), function(meta_key, target_key) {
                target_col <- as.name(target_key)
                meta_col <- as.name(meta_key)
                view_df <<- view_df %>%
                    mutate(rlang::UQ(target_col) :=
                               ifelse(!is.na(rlang::UQ(target_col)),
                                      str_c(
                                          rlang::UQ(meta_col),
                                          rlang::UQ(target_col),
                                          sep = " — "
                                      ),
                                      rlang::UQ(target_col)))
            })
    }

    link_template <- "<a href='https://www.synapse.org/#!Synapse:{id}' target='_blank'>{target}</a>"
    if (!is.null(link_keys)) {
        link_keys %>%
            walk2(names(.), function(id_key, target_key) {
                target_col <- as.name(target_key)
                id_col <- as.name(id_key)
                view_df <<- view_df %>%
                    mutate(rlang::UQ(target_col) :=
                               ifelse(!is.na(rlang::UQ(target_col)),
                                      glue::glue(
                                          link_template,
                                          id = rlang::UQ(id_col),
                                          target = rlang::UQ(target_col)
                                      ),
                                      rlang::UQ(target_col)))
            })
    }

    query_keys <- annotation_keys
    if ("projectId" %in% annotation_keys & !is.null(synproject_key)) {
        annotation_keys <- c(synproject_key, annotation_keys)
    }

    group_cols <- sapply(annotation_keys, as.name)
    query_cols <- sapply(query_keys, as.name)

    count_df <- view_df %>%
        dplyr::group_by(rlang::UQS(group_cols)) %>%
        dplyr::summarise_at(count_cols, n_distinct) %>%
        dplyr::ungroup()

    if (!is.null(list_cols)) {
        merge_strings <- function(x) {
            stringr::str_c(unique(x), collapse = "</li><li>")
        }

        list_df <- view_df %>%
            dplyr::group_by(rlang::UQS(group_cols)) %>%
            dplyr::summarise_at(list_cols, merge_strings) %>%
            dplyr::ungroup() %>%
            dplyr::mutate_at(.vars = list_cols,
                             funs(str_c("<ul><li>", ., "</li></ul>", sep = "")))

        summary_df <- left_join(count_df, list_df, by = annotation_keys)
    } else {
        summary_df <- count_df
    }

    summary_df <- summary_df %>%
        dplyr::rowwise() %>%
        dplyr::mutate(sourceFileview = table_id,
                      query = build_tablequery(sourceFileview,
                                               rlang::UQS(query_cols))) %>%
        add_queryview_column(format = queryformat) %>%
        dplyr::select(-query, -sourceFileview) %>%
        dplyr::ungroup()


    if ("projectId" %in% annotation_keys & !is.null(synproject_key)) {
        summary_df %>%
            dplyr::select(-matches("projectId"))
    } else {
        summary_df
    }
}


# adapted from lines 29-33 in 'fileViewReporting.Rmd'
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

