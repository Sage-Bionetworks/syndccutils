source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

# Script/template to create summary tables and charts for a "study"
synapseLogin()
# Config ------------------------------------------------------------------

synproject_id <- "syn5610418" # Synapse project for project Center
source_id <- "syn5610418" # Synapse folder associated with project
parent_id <- "syn10902919" # Center 'Reporting' folder where files should be stored
master_fileview_id <- "syn10903667" # Synapse fileview associated with project

# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)


# Summarize -------------------------------------------------------

# summarize by assay
table_filename <- glue::glue("{source_id}_DataFileCountsByAssay.html",
                             source_id = source_id)

summarize_datafiles_by_assay <- function(view_df, table_id) {
    count_cols <- c("id")
    view_df %>%
        group_by(assay) %>%
        summarise_at(count_cols, n_distinct) %>%
        rowwise() %>%
        mutate(sourceFileview = table_id,
               query = build_tablequery(sourceFileview, assay)) %>%
        add_queryview_column(format = "html") %>%
        select(-query)
}

datafile_counts_by_assay <- fileview_df %>%
    summarize_datafiles_by_assay(master_fileview_id)

datafile_counts_by_assay_dt <- datafile_counts_by_assay %>%
    format_summarytable_columns("assay") %>%
    as_datatable()

syn_dt_entity <- datafile_counts_by_assay_dt %>%
    save_datatable(parent_id, table_filename, .)


# summarize by species
files_by_species_table_filename <- glue::glue("{source_id}_DataFileCountsBySpecies.html",
                                              source_id = source_id)

summarize_datafiles_by_species <- function(view_df, table_id) {
    count_cols <- c("id")
    view_df %>%
        group_by(species) %>%
        summarise_at(count_cols, n_distinct) %>%
        rowwise() %>%
        mutate(sourceFileview = table_id,
               query = build_tablequery(sourceFileview, species)) %>%
        add_queryview_column(format = "html") %>%
        select(-query)
}

datafile_counts_by_species <- fileview_df %>%
    summarize_datafiles_by_species(master_fileview_id)

datafile_counts_by_species_dt <- datafile_counts_by_species %>%
    format_summarytable_columns(c("species")) %>%
    as_datatable()

syn_file_by_species_dt_entity <- datafile_counts_by_species_dt %>%
    save_datatable(parent_id, files_by_species_table_filename, .)

# summarize by species
files_by_dataType_table_filename <- glue::glue("{source_id}_DataFileCountsByDataType.html",
                                               source_id = source_id)

summarize_datafiles_by_dataType <- function(view_df, table_id) {
    count_cols <- c("id")
    view_df %>%
        group_by(dataType) %>%
        summarise_at(count_cols, n_distinct) %>%
        rowwise() %>%
        mutate(sourceFileview = table_id,
               query = build_tablequery(sourceFileview, dataType)) %>%
        add_queryview_column(format = "html") %>%
        select(-query)
}

datafile_counts_by_dataType <- fileview_df %>%
    summarize_datafiles_by_dataType(master_fileview_id)

datafile_counts_by_dataType_dt <- datafile_counts_by_dataType %>%
    format_summarytable_columns(c("dataType")) %>%
    as_datatable()

syn_file_by_dataType_dt_entity <- datafile_counts_by_dataType_dt %>%
    save_datatable(parent_id, files_by_dataType_table_filename, .)

# plot --------------------------------------------------------

## plot file counts
chart_filename <- glue::glue("{source_id}_AllDataFilesByCategory.html",
                             source_id = source_id)
plot_keys <- list(assay = "Assay",
                  species = "Species",
                  dataType = "Data Type")

chart <- fileview_df %>%
    plot_file_counts_by_annotationkey(plot_keys)

chart
syn_entity <- save_chart(parent_id, chart_filename, chart)

