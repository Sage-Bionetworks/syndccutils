source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

# Script/template to create summary tables and charts for a "project"

synapseLogin()


# Config ------------------------------------------------------------------

synproject_id <- "syn7080714" # Synapse project for consortium
consortium_id <- "syn7080714" # Synapse folder associated with consortium
parent_id <- "syn10846095" # consortium 'Reporting' folder where files should be stored
master_fileview_id <- "syn9630847" # Synapse fileview associated with consortium data
master_tool_fileview_id <- "syn9898965" # Synapse fileview associated with consortium tools


# Collect data ------------------------------------------------------------

# fileview_df <- get_table_df(master_fileview_id)
fileview_df <- feather::read_feather("data/csbc_fileview.feather")
tool_fileview_df <- get_table_df(master_tool_fileview_id)


# Add Synapse project info --------------------------------------------

fileview_df <- fileview_df %>%
    left_join(summarize_project_info(.), by = "projectId")
tool_fileview_df <- tool_fileview_df %>%
    left_join(summarize_project_info(.), by = "projectId")


# Data files by assay and tumor type --------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndTumorType.html",
                             source_id = consortium_id)

# create and save table
group_keys <- c("assay", "tumorType")
count_cols <- c("id", "diagnosis", "individualID", "cellLine")

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
                             source_id = consortium_id)

# create and save chart
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type")
chart <- fileview_df %>%
    plot_sample_counts_by_annotationkey_2d(sample_key = "individualID",
                                           annotation_keys = plot_keys)
syn_chart_entity <- save_chart(parent_id, chart_filename, chart)

# view chart
chart

# Cell lines by assays and tumor type -------------------------------------

chart_filename <- glue::glue("{source_id}_CellLinesByAssayAndTumorType.html",
                             source_id = consortium_id)

# create and save chart
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type")

chart <- fileview_df %>%
    plot_sample_counts_by_annotationkey_2d(sample_key = "cellLine",
                                           annotation_keys = plot_keys)

syn_chart_entity <- save_chart(parent_id, chart_filename, chart)

# view chart
chart

# Data files by Center and assay ------------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByCenterAndAssay.html",
                             source_id = consortium_id)

# create and save table
group_keys <- c("Center", "assay")
count_cols <- c("id", "tumorType", "diagnosis")

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


# Data files by Center and assay (chart) ----------------------------------

chart_filename <- glue::glue("{source_id}_DataFilesByCenterAndAssay.html",
    source_id = consortium_id)


# create and save chart
chart <- syn_chart_entity <- fileview_df %>%
    plot_assay_counts_by_center()

syn_chart_entity <- save_chart(parent_id, chart_filename, chart)

# view chart
chart


# Files by category -------------------------------------------------------

chart_filename <- glue::glue("{source_id}_DataFilesByCategory.html",
                             source_id = consortium_id)

# create and save chart
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type",
                  diagnosis = "Diagnosis", species = "Species",
                  organ = "Organ", tissue = "Tissue",
                  dataType = "Data Type", study = "Study")

chart <- fileview_df %>%
    plot_file_counts_by_annotationkey(plot_keys, chart_height = 300)

syn_entity <- save_chart(parent_id, chart_filename, chart)

# view chart
chart


# Tool files by Center ----------------------------------------------------

table_filename <- glue::glue("{source_id}_ToolFileCountsByCenter.html",
    source_id = consortium_id)

# create and save table
group_keys <- "Center"
count_cols <- c("id", "softwareType", "softwareLanguage")

toolfile_counts <- tool_fileview_df %>%
    summarize_files_by_annotationkey(
        annotation_keys = group_keys,
        table_id = master_tool_fileview_id,
        count_cols = count_cols
    )

toolfile_counts_dt <- toolfile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()

syn_dt_entity <- toolfile_counts_by_center_dt %>%
    save_datatable(parent_id, table_filename, .)

# view table
toolfile_counts_dt


# Tool files by input -----------------------------------------------------

table_filename <- glue::glue("{source_id}_ToolFilesCountsByInput.html",
                                   source_id = consortium_id)

# create and save table
group_keys <- "inputDataType"
count_cols <- "id"

toolfile_counts <- tool_fileview_df %>%
    summarize_files_by_annotationkey(
        annotation_keys = group_keys,
        table_id = master_tool_fileview_id,
        count_cols = count_cols
    )

toolfile_counts_dt <- toolfile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()

syn_id_entity <-toolfile_counts_dt %>%
    save_datatable(parent_id, table_filename, .)

# view table
toolfile_counts_dt


# Tool file counts by output ----------------------------------------------

output_table_filename <- glue::glue("{source_id}_ToolFilesCountsByOutput.html",
                                    source_id = consortium_id)

# create and save table
group_keys <- "outputDataType"
count_cols <- "id"

toolfile_counts <- tool_fileview_df %>%
    summarize_files_by_annotationkey(
        annotation_keys = group_keys,
        table_id = master_tool_fileview_id,
        count_cols = count_cols
    )

toolfile_counts_dt <- toolfile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()

syn_id_entity <-toolfile_counts_dt %>%
    save_datatable(parent_id, table_filename, .)

# view table
toolfile_counts_dt


# Tool files by Center, input, and output (charts) ------------------------

input_chart_filename <- glue::glue("{source_id}_ToolFilesByInput.html",
                                   source_id = consortium_id)

output_chart_filename <- glue::glue("{source_id}_ToolFilesByOutput.html",
                                    source_id = consortium_id)



# create and save chart
chart1 <- tool_fileview_df %>%
    plot_tool_inputs()

# syn_chart1_entity <- save_chart(parent_id, input_chart_filename, chart1)

# view chart
chart1

# crete and save chart
chart2 <- tool_fileview_df %>%
    plot_tool_outputs()

# syn_chart2_entity <- save_chart(parent_id, output_chart_filename, chart2)

# view chart
chart2

