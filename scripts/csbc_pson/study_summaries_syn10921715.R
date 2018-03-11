source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

# Script/template to create summary tables and charts for a "study"

# Config ------------------------------------------------------------------

synproject_id <- "syn9775595" # Synapse project for project Center
study_id <- "syn10921715" # Synapse folder associated with study
parent_id <- "syn10921753" # Center 'Reporting' folder where files should be stored
master_fileview_id <- "syn10890010" # Synapse fileview associated with study


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)


# Data files by assay and tumor type --------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndTumorType.html",
                             source_id = study_id)

# create and save table
group_keys <- c("assay", "tumorType")
count_cols <- c("id", "diagnosis", "individualID", "specimenID")

datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        count_cols = count_cols
    )

datafile_counts_dt <- datafile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()

syn_dt_entity <- datafile_counts_dt %>%
    save_datatable(parent_id, table_filename, .)

# view table
datafile_counts_dt


# Individuals by assays by tumor type -------------------------------------

chart_filename <- glue::glue("{source_id}_IndividualsByAssayAndTumorType.html",
                             source_id = study_id)

# create and save chart
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type")

chart <- fileview_df %>%
    plot_sample_counts_by_annotationkey_2d(sample_key = "individualID",
                                           annotation_keys = plot_keys)

syn_chart_entity <- save_chart(parent_id, chart_filename, chart)

# view chart
chart


# Files by category -------------------------------------------------------

chart_filename <- glue::glue("{source_id}_DataFilesByCategory.html",
                             source_id = study_id)

# create and save chart
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type")

chart <- fileview_df %>%
    plot_file_counts_by_annotationkey(plot_keys)

syn_entity <- save_chart(parent_id, chart_filename, chart)

# view chart
chart
