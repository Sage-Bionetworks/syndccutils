source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

# Config ------------------------------------------------------------------

project_id <-
parent_id <-
master_fileview_id <- "syn10531467"


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)


# Assays by patient -------------------------------------------------------

table_name <- "PatientDataFileCountsByAssay"
chart_filename <- glue::glue("{source_id}_AssaysByPatient.html",
                             source_id = master_fileview_id)

# create and save table
patient_datafile_counts_by_assay <- fileview_df %>%
    summarize_patient_datafile_counts_by_assay()

syn_table <- save_table(project_id, table_name,
                        patient_datafile_counts_by_assay)

# create and save chart
syn_entity <- patient_datafile_counts_by_assay %>%
    plot_assay_counts_by_patient() %>%
    save_chart(parent_id, chart_filename, .)
