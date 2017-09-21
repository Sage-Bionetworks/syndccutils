source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

# Script/template to create summary tables and charts for a "project"

# Config ------------------------------------------------------------------

synproject_id <- "syn7315808" # Synapse project for project Center
project_id <- "syn9615696" # Synapse folder associated with study
parent_id <- "syn10831920" # Center 'Reporting' folder where files should be stored
master_fileview_id <- "syn10838871" # Synapse fileview associated with project


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)


# Files by assay -------------------------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndTumorType.html",
                             source_id = project_id)

# create and save table
datafile_counts_by_assay_and_tumortype <- fileview_df %>%
    summarize_datafiles_by_assay_and_tumortype(master_fileview_id)

datafile_counts_by_assay_and_tumortype_dt <- datafile_counts_by_assay_and_tumortype %>%
    format_summarytable_columns(c("assay", "tumorType")) %>%
    as_datatable()
datafile_counts_by_assay_and_tumortype_dt
syn_dt_entity <- datafile_counts_by_assay_and_tumortype_dt %>%
    save_datatable(parent_id, table_filename, .)


# Individuals by assays by tumor type -------------------------------------

chart_filename <- glue::glue("{source_id}_IndividualsByAssayAndTumorType.html",
                             source_id = project_id)

# create and save chart
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type")
chart <- fileview_df %>%
    plot_sample_counts_by_annotationkey_2d(sample_key = "individualID",
                                           annotation_keys = plot_keys)
# chart
syn_chart_entity <- save_chart(parent_id, chart_filename, chart)


# Files by category -------------------------------------------------------

chart_filename <- glue::glue("{source_id}_DataFilesByCategory.html",
                             source_id = project_id)
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type",
                  projectName = "Study")

chart <- fileview_df %>%
    plot_file_counts_by_annotationkey(plot_keys)
# chart
syn_entity <- save_chart(parent_id, chart_filename, chart)
