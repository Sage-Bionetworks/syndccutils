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
            name == "specimenID" & (name %in% facet_cols) ~ "Specimen",
            name == "specimenID" & !(name %in% facet_cols) ~ "Specimens",
            name == "sourceFileview" ~ "Source Fileview",
            name == "viewFiles" ~ "View Files",
            TRUE ~ name
        )) %>%
        split(.$name) %>%
        map("formatted_name")
    plyr::rename(df, name_map)
}


# Core summary tables (data files) ----------------------------------------

summarize_datafiles_by_assay <- function(view_df, table_id) {
    count_cols <- c("id", "diagnosis", "tumorType", "individualID",
                    "specimenID")
    view_df %>%
        group_by(assay) %>%
        summarise_at(count_cols, n_distinct) %>%
        rowwise() %>%
        mutate(sourceFileview = table_id,
               query = build_tablequery(sourceFileview, assay)) %>%
        add_queryview_column(format = "html") %>%
        select(-query)
}

summarize_datafiles_by_assay_and_tumortype <- function(view_df, table_id) {
    count_cols <- c("id", "diagnosis", "individualID",
                    "specimenID")
    view_df %>%
        group_by(assay, tumorType) %>%
        summarise_at(count_cols, n_distinct) %>%
        rowwise() %>%
        mutate(sourceFileview = table_id,
               query = build_tablequery(sourceFileview, assay, tumorType)) %>%
        add_queryview_column(format = "html") %>%
        select(-query)
}

# Project summary tables --------------------------------------------------

# adapted from lines 29-33 in 'fileViewReporting.Rmd'
summarize_project_info <- function(view_df,fv_synid) {
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

# adapted from lines 40-44 in 'fileViewReporting.Rmd'
summarize_project_datafile_counts <- function(view_df, project_info,fv_synid) {
    project_datafile_counts <- view_df %>%
        group_by(projectId) %>%
        summarize(
            Files = n_distinct(id),
            Assays = n_distinct(assay),
            TumorTypes = n_distinct(tumorType),
            Diseases = n_distinct(diagnosis),
            Specimens = n_distinct(specimenID),
            Patients = n_distinct(individualID)
        ) %>%
        inner_join(project_info, ., by = "projectId")
    ###adding query information
    fv_syn_query=paste('SELECT * FROM',fv_synid,'WHERE')
    dl_syn_query=paste('SELECT id FROM',fv_synid,'WHERE')

    project_datafile_counts%>% mutate(viewFiles=paste(fv_syn_query,'projectId =',paste0('"',projectId,'"')),
        downloadFiles=paste(dl_syn_query,'projectId =',paste0('"',projectId,'"')))
    }

# adapted from lines 49-53 in 'fileViewReporting.Rmd'
summarize_project_toolfile_counts <- function(view_df, project_info,fv_synid) {
    project_toolfile_counts <- view_df %>%
        group_by(projectId) %>%
        summarize(
            Files = n_distinct(id),
            ToolTypes = n_distinct(softwareType),
            ToolLanguages = n_distinct(softwareLanguage)
        ) %>%
        inner_join(project_info, ., by = "projectId") %>%
        mutate(Label = paste(Institution, Program, sep = '\n'))

    ###adding query information
    fv_syn_query=paste('SELECT * FROM',fv_synid,'WHERE')
    dl_syn_query=paste('SELECT id FROM',fv_synid,'WHERE')

    project_toolfile_counts%>% mutate(viewFiles=paste(fv_syn_query,'projectId =',paste0('"',projectId,'"')),
        downloadFiles=paste(dl_syn_query,'projectId =',paste0('"',projectId,'"')))

}

# adapted from lines 37-43 in 'toolTypeReporting.Rmd'
summarize_project_toollanguage_counts <- function(view_df, project_info,fv_synid) {
    project_toollanguage_counts = view_df %>%
        group_by(softwareLanguage, projectId) %>%
        summarize(Files = n_distinct(id)) %>%
        inner_join(project_info, ., by = "projectId") %>%
        mutate(Label = paste(Institution, Program, sep = '\n'))

    project_toollanguage_counts <- arrange(project_toollanguage_counts, desc(Files))

    ###adding query information
    fv_syn_query=paste('SELECT * FROM',fv_synid,'WHERE')
    dl_syn_query=paste('SELECT id FROM',fv_synid,'WHERE')

    project_toollanguage_counts%>% mutate(viewFiles=paste(fv_syn_query,'projectId =',paste0('"',projectId,'"'),'AND softwareLanguage =',paste0('"',softwareLanguage,'"')),
        downloadFiles=paste(dl_syn_query,'projectId =',paste0('"',projectId,'"'),'AND softwareLanguage =',softwareLanguage))

}


# Data type tables --------------------------------------------------------

# adapted from lines 36-38 in 'dataTypeReporting.Rmd'
summarize_assay_counts <- function(fileview_df,fv_synid) {
    assay_counts = fileview_df %>%
        group_by(assay) %>%
        summarize(
            Centers = n_distinct(projectId),
            Files = n_distinct(id),
            tumorType = n_distinct(tumorType),
            disease = n_distinct(diagnosis),
            specimens = n_distinct(specimenID),
            patients = n_distinct(individualID)
        )
    assay_counts <- arrange(assay_counts, desc(Centers))

    ###adding query information
    fv_syn_query=paste('SELECT * FROM',fv_synid,'WHERE')
    dl_syn_query=paste('SELECT id FROM',fv_synid,'WHERE')

    assay_counts%>% mutate(viewFiles=paste(fv_syn_query,'assay =',paste0('"',assay,'"')),
        downloadFiles=paste(dl_syn_query,'assay =',paste0('"',assay,'"')))
}

# adapted from lines 42-44 in 'dataTypeReporting.Rmd'
summarize_assay_stats <- function(fileview_df,fv_synid) {

    assay_stats = fileview_df %>%
        group_by(assay, tumorType, diagnosis) %>%
        summarize(
            Centers = n_distinct(projectId),
            Files = n_distinct(id),
            specimens = n_distinct(specimenID),
            patients = n_distinct(individualID)
        )

    assay_stats=arrange(assay_stats, desc(Centers))

    ###adding query information
    fv_syn_query=paste('SELECT * FROM',fv_synid,'WHERE')
    dl_syn_query=paste('SELECT id FROM',fv_synid,'WHERE')

    assay_stats%>% mutate(viewFiles=paste(fv_syn_query,'tumorType =',paste0('"',tumorType,'"'),'AND assay =',paste0('"',assay,'"'),'AND diagnosis =',paste0('"',diagnosis,'"')),
        downloadFiles=paste(dl_syn_query,'tumorType =',paste0('"',tumorType,'"'),'AND assay =',paste0('"',assay,'"'),'AND diagnosis =',paste0('"',diagnosis,'"')))
}


# adapted from lines 70-72 in 'dataTypeReporting.Rmd'
summarize_disease_counts <- function(fileview_df,fv_synid) {
    disease_counts = fileview_df %>%
        group_by(diagnosis) %>%
        summarize(
            Centers = n_distinct(projectId),
            Files = n_distinct(id),
            tumorType = n_distinct(tumorType),
            assay = n_distinct(assay),
            specimens = n_distinct(specimenID),
            patients = n_distinct(individualID)
        )

    disaese_counts <- arrange(disease_counts, desc(Centers))
    ###adding query information
    fv_syn_query=paste('SELECT * FROM',fv_synid,'WHERE')
    dl_syn_query=paste('SELECT id FROM',fv_synid,'WHERE')

    disease_counts%>% mutate(viewFiles=paste(fv_syn_query,'diagnosis =',paste0('"',diagnosis,'"')),
        downloadFiles=paste(dl_syn_query,'diagnosis =',paste0('"',diagnosis,'"')))
}


# adapted from lines 77-79 in 'dataTypeReporting.Rmd'
summarize_tumortype_counts <- function(fileview_df,fv_synid) {
    tt_counts = fileview_df %>%
        group_by(tumorType) %>%
        summarize(
            Centers = n_distinct(projectId),
            Files = n_distinct(id),
            disease = n_distinct(diagnosis),
            assay = n_distinct(assay),
            specimens = n_distinct(specimenID),
            patients = n_distinct(individualID)
        )

    tt_counts <- arrange(tt_counts, desc(Centers))
    ###adding query information
    fv_syn_query=paste('SELECT * FROM',fv_synid,'WHERE')
    dl_syn_query=paste('SELECT id FROM',fv_synid,'WHERE')

    assay_stats%>% mutate(viewFiles=paste(fv_syn_query,'tumorType =',paste0('"',tumorType,'"')),
        downloadFiles=paste(dl_syn_query,'tumorType =',paste0('"',tumorType,'"')))
}


# Patient summary tables --------------------------------------------------

summarize_assaycounts_by_patient <- function(fileview_df,fv_synid) {
    assay_patient <- fileview_df %>%
        group_by(individualID, assay) %>%
        tally() %>%
        ungroup() %>%
        right_join(., expand(., individualID, assay),
                   by = c("individualID", "assay")) %>%
        replace_na(list(n = 0)) %>%
        # need to be careful with column types
        mutate(n = as.integer(n))
    ###adding query information
    fv_syn_query=paste('SELECT * FROM',fv_synid,'WHERE')
    dl_syn_query=paste('SELECT id FROM',fv_synid,'WHERE')
    assay_patient%>% mutate(viewFiles=paste(fv_syn_query,'individualID =',paste0('"',individualID,'"'),'AND assay =',paste0('"',assay,'"')),
        downloadFiles=paste(dl_syn_query,'individualID =',paste0('"',individualID,'"'),'AND assay =',paste0('"',assay,'"')))

}


# adapted from lines 71-79 in 'pan_stanford_viz.Rmd'
summarize_assay_datafile_counts_by_patient <- function(fileview_df,fv_synid) {
    df_by_patient <- fileview_df %>%
        group_by(individualID, assay) %>%
        tally() %>%
        ungroup() %>%
        right_join(., expand(., individualID, assay),
                   by = c("individualID", "assay")) %>%
        replace_na(list(n = 0)) %>%
        # need to be careful with column types
        mutate(n = as.integer(n)) %>%
        spread(individualID, n)

    ###adding query information
    fv_syn_query=paste('SELECT * FROM',fv_synid,'WHERE')
    dl_syn_query=paste('SELECT id FROM',fv_synid,'WHERE')
    df_by_patient%>% mutate(viewFiles=paste(fv_syn_query,'individualID =',paste0('"',individualID,'"'),'AND assay =',paste0('"',assay,'"')),
        downloadFiles=paste(dl_syn_query,'individualID =',paste0('"',individualID,'"'),'AND assay =',paste0('"',assay,'"')))

}


# adapted from lines 81-89 in 'pan_stanford_viz.Rmd'
summarize_patient_datafile_counts_by_assay <- function(fileview_df,fv_synid) {

    stats_table <- fileview_df %>%
        group_by(individualID, assay) %>%
        tally() %>%
        ungroup() %>%
        right_join(., expand(., individualID, assay),
                   by = c("individualID", "assay")) %>%
        replace_na(list(n = 0)) %>%
        # need to be careful with column types
        mutate(n = as.integer(n)) %>%
        spread(assay, n)

    ###adding query information
    fv_syn_query=paste('SELECT * FROM',fv_synid,'WHERE')
    dl_syn_query=paste('SELECT id FROM',fv_synid,'WHERE')
    stats_table%>% mutate(viewFiles=paste(fv_syn_query,'individualID =',paste0('"',individualID,'"'),'AND assay =',paste0('"',assay,'"')),
            downloadFiles=paste(dl_syn_query,'individualID =',paste0('"',individualID,'"'),'AND assay =',paste0('"',assay,'"')))

}
