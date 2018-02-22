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
    if (!dir.exists("html")) {
        dir.create("html")
    }
    defaultHeight <- plot_object$sizingPolicy$defaultHeight
    chartHeight <- plot_object$height
  #  if (!is.null(defaultHeight) && (defaultHeight != chartHeight)) {
  #      plot_object$sizingPolicy$defaultHeight <- chartHeight
  #  }
    chart_widget <- plotly::as_widget(plot_object)
    htmlwidgets::saveWidget(chart_widget, chart_filename)
    syn_entity <- synStore(File(path = chart_filename, parentId = parent_id))
    file.rename(chart_filename, file.path("html", chart_filename))
    return(syn_entity)
}

save_datatable <- function(parent_id, dt_filename, dt_widget) {
    if (!dir.exists("html")) {
        dir.create("html")
    }
    htmlwidgets::saveWidget(dt_widget, dt_filename)
    syn_entity <- synStore(File(path = dt_filename, parentId = parent_id))
    file.rename(dt_filename, file.path("html", dt_filename))
    return(syn_entity)
}

datatable_to_synapse <-function(dt, parent_id, table_name) {
    colnames(dt)<-sapply(colnames(dt),function(x) gsub(' ','_',x))
    tcols<-as.tableColumns(dt)$tableColumns
    tcols[[which(sapply(tcols,function(x) x$name)%in%c('viewFiles','View_Files'))]]$columnType<-'LINK'
    schema <- synapseClient::TableSchema(name=table_name,
            columns=tcols,
            parent=parent_id)
    syn_id <- synapseClient::synStore(synapseClient::Table(schema,dt))

    #all.rows <-synapseClient::synTableQuery(paste('select * from',syn_id))

    #synapseClient::synDeleteRows(all.rows)
    syn_id <-synapseClient::synStore(synapseClient::Table(schema,dt))

    return(syn_id)

}

simple_plots_wiki_string <-function(table_id, group_keys,count_cols,title ){


    md_string <- paste('${plot?query= select ',group_keys[1],',',group_keys[2],',',count_cols[1],' from ',
        table_id,' GROUP BY ',group_keys[1],',',group_keys[2],'&title=',title,'&type=BAR&barmode=STACK&showlegend=true&fill=',group_keys[2],'}',sep='')
    ##maybe we can aspire to glue some day
        #glue::glue('${plot?query=select [xaxis],', '[fill],', '[yaxis]', 'from [table]', 'GROUP BY [fill],', '[xaxis]','&title=[title]','&type=BAR&barmode=STACK&showlegend=true&fill=[fill]}',
       # table=table_id,
    #    xaxis=group_keys[1],
    #    yaxis=count_cols[1],
    #    fill=group_keys[2])
    return(md_string)

}


