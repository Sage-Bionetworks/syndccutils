source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

# Script/template to create summary tables and charts for a "study"
synapseLogin()
update_remote <- TRUE

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


#table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndTumorType.html",
#    source_id = project_id)

# create and save table
group_keys <- c("dataType")
count_cols <- c("id")

datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        count_cols = count_cols
    )

datafile_counts_dt <- datafile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()

if (update_remote) {
    syn_dt_entity <- datafile_counts_dt %>%
        save_datatable(parent_id, table_filename, .)
}

# view table
datafile_counts_dt
###-------



# summarize by species
files_by_species_table_filename <- glue::glue("{source_id}_DataFileCountsBySpecies.html",
                                              source_id = source_id)

group_keys=c('species')
datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        count_cols = count_cols
    )

datafile_counts_dt <- datafile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()

if (update_remote) {
    syn_dt_entity <- datafile_counts_dt %>%
        save_datatable(parent_id, table_filename, .)
}

# view table
datafile_counts_dt

# summarize by species
files_by_dataType_table_filename <- glue::glue("{source_id}_DataFileCountsByDataType.html",
                                               source_id = source_id)
group_keys=c('dataType')
datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        count_cols = count_cols
    )

datafile_counts_dt <- datafile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()

if (update_remote) {
    syn_dt_entity <- datafile_counts_dt %>%
        save_datatable(parent_id, table_filename, .)
}

# view table
datafile_counts_dt

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
if (update_remote) {
    syn_entity <- save_chart(parent_id, chart_filename, chart)
}
