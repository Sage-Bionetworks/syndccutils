## Functions for munging, modifying, and summarizing Synapse annotation data in
## data frames


#' Fill in missing values with default or specified string.
#'
#' @param df data frame nominally representing a Synapse table or file view
#' @param placeholder Placeholder value to use. Defaults to "Not Annotated"
#' @param replace_keys Keys to replace with placeholder value
#'
#' @export
add_missing_placeholder <- function(
    df, placeholder = "Not Annotated", replace_keys = NULL
) {

    if (!is.null(replace_keys)) {
        df %>%
            mutate_at(.vars = replace_keys,
                             funs(replace(., is.na(.), placeholder)))
    } else {
        df %>%
            mutate_all(funs(replace(., is.na(.), placeholder)))
    }
}


#' Remove rows with missing or bad values for specified keys.
#'
#' @param df data frame nominally representing a Synapse table or file view
#' @param filter_keys character vector of keys for which to check values
#' @param bad_values character vector specifying bad values to exclude
#'
#' @export
filter_by_key <- function(
    df, filter_keys, bad_values = c("null", "Not Applicable")
) {

    df %>%
        filter_at(
            vars(one_of(filter_keys)),
            all_vars(!is.na(.) & !(. %in% bad_values))
        )
}


#' Augment values in one column with the value of another column.
#'
#' @param df data frame nominally representing a Synapse table or file view
#' @param augment_keys list mapping target columns (list names) for which to
#'   prepend values of corresponding meta columns (list values)
#'
#' @export
augment_values <- function(
    df, augment_keys
) {

    augment_keys %>%
        walk2(names(.), function(meta_key, target_key) {
            target_col <- as.name(target_key)
            meta_col <- as.name(meta_key)
            df <<- df %>%
                mutate(
                    UQ(target_col) :=
                        ifelse(!is.na(UQ(target_col)),
                               stringr::str_c(
                                   UQ(meta_col),
                                   UQ(target_col),
                                   sep = " - "
                               ),
                               UQ(target_col))
                )
        })
    df
}


#' Convert values to named Synapse URLs based on corresponding Synapse IDs.
#'
#' @param df data frame nominally representing a Synapse table or file view
#' @param link_keys list mapping target columns (list names) for which to
#'   construct links from Synapse IDs in corresponding ID columns (list values)
#'
#' @export
create_synapse_links <- function(
    df, link_keys
) {
    base_url <- "https://www.synapse.org/#!Synapse:"
    link_template <- glue::glue(
        "<a href='{base}{{id}}' target='_blank'>{{target}}</a>",
        base = base_url
    )
    link_keys %>%
        walk2(names(.), function(id_key, target_key) {
            target_col <- as.name(target_key)
            id_col <- as.name(id_key)
            df <<- df %>%
                mutate(
                    UQ(target_col) :=
                        ifelse(!is.na(UQ(target_col)),
                               glue::glue(
                                   link_template,
                                   id = UQ(id_col),
                                   target = UQ(target_col)
                               ),
                               UQ(target_col))
                )
        })
    df
}


#' Count distinct values within each group for specified keys.
#'
#' @param df data frame nominally representing a Synapse table or file view
#' @param group_keys character vector of keys by which to group
#' @param count_keys character vector of keys for which to count files
#'
#' @export
count_values <- function(
    df, group_keys, count_keys
) {

    group_cols <- sapply(group_keys, as.name)

    df %>%
        group_by(UQS(group_cols)) %>%
        summarise_at(count_keys, n_distinct) %>%
        ungroup()
}


#' List distinct values within each group for specified keys
#'
#' @param df data frame nominally representing a Synapse table or file view
#' @param group_keys character vector of keys by which to group
#' @param list_keys Which keys to list
#' @param list_format Format of keys
#'
#' @export
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
        group_by(UQS(group_cols)) %>%
        summarise_at(list_keys, merge_strings) %>%
        ungroup() %>%
        mutate_at(
            .vars = list_keys,
            funs(stringr::str_c(start_opts[[list_format]], .,
                                end_opts[[list_format]], sep = ""))
        )
}


#' Construct a Synapse SQL-style table query using names/values of data frame
#' column(s) to compose 'WHERE' clauses
#'
#' @param table_id Synapse ID of table
#' @param ... Additional query parameters
#'
#' @export
#'
#' @examples
#' \dontrun{
#' test_df %>%
#'     rowwise() %>%
#'     mutate(query = build_tablequery(table_id, assay))
#' }
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
#' @param table_id Synapse ID of table
#' @param query_string A string containing Synapse query
#'
#' @export
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
#' @param df Data frame to add a column to
#' @param format Output format (one of `"markdown"`, `"html"`, or `"raw"`)
#'
#' @export
add_queryview_column <- function(df, format = c("markdown", "html","raw")) {
    format <- match.arg(format)
    link_templates <- list(
        markdown = "[View]({url})",
        html = '<a href="{url}" target="_blank">View</a>',
        raw = '{url}'
    )
    link_template <- link_templates[[format]]
    df %>%
        mutate(viewFiles = get_tablequery_url(.data$sourceFileview, .data$query),
               viewFiles = glue::glue(link_template, url = .data$viewFiles))
}


#' Prettify column names based on annotation keys.
#'
#' @param df Data frame to modify
#' @param facet_cols Vector of names of facet columns
#'
#' @export
format_summarytable_columns <- function(df, facet_cols = c()) {
    # TODO: remove plyr dependency...
    name_map <- tibble::tibble(name = names(df)) %>%
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
