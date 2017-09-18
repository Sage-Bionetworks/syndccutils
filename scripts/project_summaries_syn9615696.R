source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

# Script/template to create summary tables and charts for a "project"

# Config ------------------------------------------------------------------

synproject_id <- "syn7315808" # Synapse project for project Center
project_id <- "syn9615696" # Synapse folder associated with project
parent_id <- "syn10831920" # Center 'Reporting' folder where files should be stored
master_fileview_id <- "syn9636756" # Synapse fileview associated with project


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)


# Assays by patient -------------------------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssay.html",
                             source_id = project_id)
chart_filename <- glue::glue("{source_id}_AssayDataFilesByTumorType.html",
                             source_id = project_id)

# create and save table
datafile_counts_by_assay <- fileview_df %>%
    summarize_datafiles_by_assay(master_fileview_id)

datafile_counts_by_assay_dt <- datafile_counts_by_assay %>%
    format_summarytable_columns("assay") %>%
    as_datatable()

syn_dt_entity <- datafile_counts_by_assay_dt %>%
    save_datatable(parent_id, table_filename, .)

# create and save chart
syn_chart_entity <- fileview_df %>%
    plot_assay_counts_by_tumortype() %>%
    save_chart(parent_id, chart_filename, .)
