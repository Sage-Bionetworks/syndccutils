source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

# Script/template to create summary tables and charts for a "project"
update_remote <- TRUE

# Config ------------------------------------------------------------------

source_id <- "syn7349745" # Synapse ID associated with Center Wiki
parent_id <- "syn11213515" # Center 'Reporting' folder where files should be stored
master_data_fileview_id <- "syn9928160" # Synapse fileview associated with Center _data_
master_tool_fileview_id <- "syn9749627" # Synapse fileview associated with Center _tools_


# Collect data ------------------------------------------------------------

data_fileview_df <- get_table_df(master_data_fileview_id)
tool_fileview_df <- get_table_df(master_tool_fileview_id)

# Add Synapse project info --------------------------------------------

data_fileview_df <- data_fileview_df %>% left_join(summarize_project_info(.), by = "projectId")
tool_fileview_df <- tool_fileview_df %>% left_join(summarize_project_info(.), by = "projectId")
## synproject_df <- data_fileview_df %>% summarize_project_info()
## data_fileview_df <- data_fileview_df %>% left_join(synproject_df, by = "projectId")
## tool_fileview_df <- tool_fileview_df %>% left_join(synproject_df, by = "projectId")

# Data files by assay and tumor type --------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndTumorType.html",
                             source_id = source_id)

# create and save table
group_keys <- c("assay", "tumorType")
count_cols <- c("id", "diagnosis", "individualID", "specimenID")

datafile_counts <- data_fileview_df %>%
    summarize_files_by_annotationkey(
        annotation_keys = group_keys,
        table_id = master_data_fileview_id,
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


# Individuals by assays and tumor type ------------------------------------

chart_filename <- glue::glue("{source_id}_IndividualsByAssayAndTumorType.html",
                             source_id = source_id)

# create and save chart
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type")

chart <- data_fileview_df %>%
    plot_sample_counts_by_annotationkey_2d(sample_key = "individualID",
                                           annotation_keys = plot_keys)

if (update_remote) {
    syn_chart_entity <- save_chart(parent_id, chart_filename, chart)
}

# view chart
chart


# Files by category -------------------------------------------------------

chart_filename <- glue::glue("{source_id}_DataFilesByCategory.html",
                             source_id = source_id)

# create and save chart
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type",
                  projectName = "Study")

chart <- data_fileview_df %>%
    plot_file_counts_by_annotationkey(plot_keys, chart_height = 300)

if (update_remote) {
    # syn_entity <-
    save_chart(parent_id, chart_filename, chart)
}

# view chart
chart

# Tool files by input -----------------------------------------------------

input_table_filename <- glue::glue("{source_id}_ToolFilesCountsByInput.html",
                                   source_id = source_id)

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

if (update_remote) {
    syn_id_entity <-toolfile_counts_dt %>%
        save_datatable(parent_id, input_table_filename, .)
}

# view table
toolfile_counts_dt


# Tool file counts by output ----------------------------------------------

output_table_filename <- glue::glue("{source_id}_ToolFilesCountsByOutput.html",
                                    source_id = source_id)

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

if (update_remote) {
    syn_id_entity <-toolfile_counts_dt %>%
        save_datatable(parent_id, output_table_filename, .)
}

# view table
toolfile_counts_dt



# Tool files by Center, input, and output (charts) ------------------------

input_chart_filename <- glue::glue("{source_id}_ToolFilesByInput.html",
                                   source_id = source_id)

output_chart_filename <- glue::glue("{source_id}_ToolFilesByOutput.html",
                                    source_id = source_id)



# create and save chart
chart1 <- tool_fileview_df %>%
    plot_tool_inputs()

if (update_remote) {
    syn_chart1_entity <- save_chart(parent_id, input_chart_filename, chart1)
}

# view chart
chart1

# crete and save chart
chart2 <- tool_fileview_df %>%
    plot_tool_outputs()

if (update_remote) {
    syn_chart2_entity <- save_chart(parent_id, output_chart_filename, chart2)
}

# view chart
chart2

