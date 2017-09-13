library(tidyverse)
library(synapseClient)

#' Collect all rows and columns from a Synapse table and return as values
#' in a data frame.
#'
#' @param table_id
#'
#' @return data frame with table values
#' @export
#'
#' @examples
get_table_df <- function(table_id) {

    syn_table_data <- synTableQuery(sprintf("select * from %s", table_id))
    return(syn_table_data@values)
}


#' Create a new table in Synapse within a specified project or update an
#' existing table.
#'
#' This is a convenience/utility function to simplify several operations within
#' the Synapse R client for creating or updating tables.
#'
#' @param project_id string containing the Synapse ID for the target project
#' @param table_name string containing the name of the table to be stored in
#'   Synapse
#' @param table_df data frame containing values to be stored in Synapse table
#'
#' @return Synapse `Table` object for stored table data
#' @export
#'
#' @examples
save_table <- function(project_id, table_name, table_df) {
    # check whether table exists for project
    project_query <- sprintf("select * from Entity where parentId == '%s'",
                             project_id)
    table_id <- synQuery(project_query) %>%
        # make sure that matched entities are tables
        filter(str_detect(Entity.concreteType, "TableEntity"),
               # then check whether any tables match the target name
               str_detect(Entity.name, table_name)) %>%
        .[["Entity.id"]]

    if (length(table_id)) {
        message(sprintf("updating table: %s", table_id))
        # if table exists, get data from Synapse before updating
        syn_table_data <- synTableQuery(sprintf("select * from %s", table_id))
        syn_table_df <- syn_table_data@values

        # check whether table values have changed at all before updating
        if (!all_equal(platform_workflow_df, syn_table_df) == TRUE) {
            # rather than try to conditionally update part of the table,
            # just wipe all rows and add the latest ones
            synDeleteRows(syn_table_data)
            schema <- synGet(table_id)
            update_table <- Table(schema, platform_workflow_df)
            syn_table <- synStore(update_table)
        }
    } else {
        # otherwise, create new schema and store table data
        message(sprintf("creating new table with name '%s'", table_name))
        table_colobject <- as.tableColumns(platform_workflow_df)
        cols <- table_colobject$tableColumns

        schema <- TableSchema(name = table_name, columns = cols,
                              parent = syn_project)
        syn_table <- Table(schema, platform_workflow_df)
        syn_table <- synStore(syn_table)
        message(sprintf("table stored as: %s",
                        properties(syn_table@schema)$id))
    }
    return(syn_table)
}


#' Save a static or dynamic chart to a file and store in Synapse.
#'
#' @param parent_id
#' @param chart_filename
#' @param plot_object
#' @param static
#'
#' @return
#' @export
#'
#' @examples
save_chart <- function(parent_id, chart_filename, plot_object, static = FALSE) {
    chart_widget <- plotly::as_widget(plot_object)
    htmlwidgets::saveWidget(chart_widget)
    syn_entity <- synStore(File(path = chart_filename, parentId = parent_id))
    return(syn_entity)
}

#' Convert a table to a wik istring
#'
#' #param tab
#' @param columnsAsCode
#'
#' @return
#' @export
table_as_wiki <- function(tab #table to populate
    ,columnsAsCode=c()#list of columns to format as courier in markdown
    ){

    #first add in backticks for code blocks
    for(i in columnsAsCode)
        tab[,i]<-sapply(tab[,i],function(x) paste0('`',x,'`'))

    rowstr=paste(
            paste0(names(tab),collapse='|'),'\n',
            paste0(rep('---',ncol(tab)),collapse='|'),'\n'
            )
    tabstr<-paste(apply(tab,1,function(x) paste0(x,collapse='|')),collapse='\n')
    return(paste0(rowstr,tabstr))
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

