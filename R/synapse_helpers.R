## Helper functions for getting data into and out of Synapse

source("R/utils.R")
library(synapser)


#' Collect all rows and columns from a Synapse table and return as values
#' in a data frame.
#'
#' @param table_id
#'
#' @return data frame with table values
#' @export
get_table_df <- function(table_id, cache = FALSE) {
    if (cache) {
        viewcache_dir <- "data/viewcache"
        if (!fs::dir_exists(viewcache_dir)) {
            fs::dir_create(viewcache_dir, recursive = TRUE)
        }
        view_file <- fs::path(viewcache_dir,
                              stringr::str_c(table_id, ".feather"))
        if (!fs::file_exists(view_file)) {
            syn_table_data <- synapser::synTableQuery(
                sprintf("select * from %s", table_id),
                includeRowIdAndRowVersion = FALSE
            )
            feather::write_feather(syn_table_data$asDataFrame(), view_file)
            return(syn_table_data$asDataFrame())
        } else {
            return(feather::read_feather(view_file))
        }
    } else {
        syn_table_data <- synapser::synTableQuery(
            sprintf("select * from %s", table_id),
            includeRowIdAndRowVersion = FALSE
        )
        syn_table_data$asDataFrame()
    }
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
save_table <- function(project_id, table_name, table_df) {
    # check whether table exists for project
    project_query <- sprintf("select * from Entity where parentId == '%s'",
                             project_id)
    table_id <- synapser::synQuery(project_query) %>%
        # make sure that matched entities are tables
        dplyr::filter(
            stringr::str_detect(Entity.concreteType, "TableEntity"),
            # then check whether any tables match the target name
            stringr::str_detect(Entity.name, table_name)) %>%
        .[["Entity.id"]]

    if (length(table_id)) {
        message(sprintf("updating table: %s", table_id))
        # if table exists, get data from Synapse before updating
        syn_table_data <- synapser::synTableQuery(
            sprintf("select * from %s", table_id)
        )
        syn_table_df <- as.data.frame(syn_table_data) %>%
            select(-ROW_ID, -ROW_VERSION)

        # check whether table values have changed at all before updating
        if (!all_equal(platform_workflow_df, syn_table_df) == TRUE) {
            # rather than try to conditionally update part of the table,
            # just wipe all rows and add the latest ones
            synDelete(syn_table_data$asRowSet())
            schema <- synapser::synGet(table_id)
            update_table <- synapser::Table(schema, platform_workflow_df)
            syn_table <- synapser::synStore(update_table)
        }
    } else {
        # otherwise, create new schema and store table data
        message(sprintf("creating new table with name '%s'", table_name))
        # TODO: as.tableColumns not part of 'synapser' package; rewrite
        table_colobject <- as.tableColumns(platform_workflow_df)
        cols <- table_colobject$tableColumns

        # TODO: TableSchema not part of 'synapser' package; rewrite
        schema <- TableSchema(name = table_name, columns = cols,
                              parent = syn_project)
        syn_table <- synapser::Table(schema, platform_workflow_df)
        syn_table <- synapser::synStore(syn_table)
        message(sprintf("table stored as: %s",
                        syn_table$properties$id))
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
#' @export
save_chart <- function(parent_id, chart_filename, plot_object, static = FALSE) {
    if (!dir.exists("html")) {
        dir.create("html")
    }

    chart_widget <- plotly::as_widget(plot_object)
    htmlwidgets::saveWidget(chart_widget, chart_filename,
                            selfcontained = TRUE)
    # fixed_chart_filename <- fix_js_assets(chart_filename)
    fixed_chart_filename <- chart_filename

    syn_entity <- synapser::synStore(
        synapser::File(path = fixed_chart_filename,
                       parentId = parent_id)
    )
    file.rename(fixed_chart_filename, file.path("html", fixed_chart_filename))
    return(syn_entity)
}


save_datatable <- function(parent_id, dt_filename, dt_widget) {
    if (!dir.exists("html")) {
        dir.create("html")
    }
    htmlwidgets::saveWidget(dt_widget, dt_filename,
                            selfcontained = TRUE)
    # fixed_dt_filename <- fix_js_assets(dt_filename)
    fixed_dt_filename <- dt_filename

    syn_entity <- synapser::synStore(
        synapser::File(path = fixed_dt_filename,
                       parentId = parent_id)
    )
    file.rename(fixed_dt_filename, file.path("html", fixed_dt_filename))
    return(syn_entity)
}


datatable_to_synapse <- function(dt, parent_id, table_name) {
    col.name <- sapply(colnames(dt), function(x)
        gsub(' ', '_', x))
    colnames(dt) <- col.name
    col.type <- sapply(1:ncol(dt), function(x) {
        switch(
            class(dt[, x][[1]]),
            character = 'STRING',
            integer = 'INTEGER',
            factor = 'STRING'
        )
    })

    tcols <- sapply(1:ncol(dt), function(x) {
        if (col.type[x] == 'STRING') {
            if (col.name[x] %in% c('viewFiles', 'View_Files'))
                Column(
                    name = col.name[[x]],
                    columnType = 'LINK',
                    maximumSize = as.integer(512)
                )
            else
                Column(
                    name = col.name[[x]],
                    columnType = col.type[x],
                    maximumSize = as.integer(256)
                )
        } else
            Column(name = col.name[[x]], columnType = col.type[x])
    })

    schema.obj <- synapser::Schema(
        name = table_name,
        columns = tcols,
        parent = synapser::synGet(parent_id)
    )

    tab <- synapser::Table(schema.obj, as.data.frame(dt))

    syn_id <- synapser::synStore(tab)
    #this is the jira:  https://sagebionetworks.jira.com/browse/SYNPY-603
    #    all.rows <-synapser::synTableQuery(paste('select * from',syn_id$tableId))

    #synDelete(all.rows$asRowSet())

    #    syn_id <-synapser::synStore(synapser::Table(schema,dt))

    return(syn_id)
}


simple_plots_wiki_string <- function(table_id, group_keys, count_cols, title) {
    # TODO: rewrite as 'glue' function
    md_string <- paste(
        '${plot?query= select ',
        group_keys[1],
        ',',
        group_keys[2],
        ',',
        count_cols[1],
        ' from ',
        table_id,
        ' GROUP BY ',
        group_keys[1],
        ',',
        group_keys[2],
        '&title=',
        title,
        '&type=BAR&barmode=STACK&showlegend=true&fill=',
        group_keys[2],
        '}',
        sep = ''
    )
    return(md_string)
}


