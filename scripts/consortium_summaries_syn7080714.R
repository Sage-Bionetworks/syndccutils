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

fileview_df <- get_table_df(master_fileview_id)
tool_fileview_df <- get_table_df(master_tool_fileview_id)

# Collect consortium hierarchy info ---------------------------------------

hierarchy_id <- "syn10915872"
hierarchy_df <- get_table_df(hierarchy_id)


# Collect Synapse project info --------------------------------------------

synproject_df <- fileview_df %>%
    summarize_project_info()
tool_synproject_df <- tool_fileview_df %>%
    summarize_project_info()


# Add hierarchy and Synapse project info ----------------------------------

fileview_df <- fileview_df %>%
    left_join(hierarchy_df, by = c("study" = "Study Name")) %>%
    left_join(synproject_df, by = c("Center" = "projectId")) %>%
    rename(`Center Name` = `Center.y`)

tool_fileview_df <- tool_fileview_df %>%
    left_join(hierarchy_df, by = c("study" = "Study Name")) %>%
    left_join(tool_synproject_df, by = c("Center" = "projectId")) %>%
    rename(`Center Name` = `Center.y`)


# Data files by assay and tumor type --------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndTumorType.html",
                             source_id = consortium_id)

# create and save table
group_keys <- c("assay", "tumorType")
count_cols <- c("id", "diagnosis", "individualID", "cellLine")
list_cols <- "study"
augment_keys <- list(study = "Center Name")
link_keys <- list(study = "Study")

datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        count_cols = count_cols,
        list_cols = list_cols,
        link_keys = link_keys
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
group_keys <- c("projectId", "assay")
synproject_key <- "Center Name"
count_cols <- c("id", "tumorType", "diagnosis")

datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        synproject_key = synproject_key,
        count_cols = count_cols
    ) %>%
    rename(Center = `Center Name`)

datafile_counts_dt <- datafile_counts %>%
    format_summarytable_columns(c("Center", group_keys)) %>%
    as_datatable()

syn_dt_entity <- datafile_counts_dt %>%
    save_datatable(parent_id, table_filename, .)

# view table
datafile_counts_dt


# Data files by Center and assay (chart) ----------------------------------

chart_filename <- glue::glue("{source_id}_DataFilesByCenterAndAssay.html",
    source_id = consortium_id)


# create and save chart
plot_keys <- list(projectId = "Center", assay = "Assay")

chart <- fileview_df %>%
    plot_file_counts_by_annotationkey_2d(
        annotation_keys = plot_keys,
        synproject_key = "Center Name",
        filter_missing = TRUE,
        log_counts = TRUE
    )

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
group_keys <- "projectId"
synproject_key <- "Center Name"
count_cols <- c("id", "softwareType", "softwareLanguage")
list_cols <- "study"
link_keys <- list(study = "Study")

toolfile_counts <- tool_fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        table_id = master_tool_fileview_id,
        synproject_key = synproject_key,
        count_cols = count_cols,
        list_cols = list_cols,
        link_keys = link_keys
    ) %>%
    rename(Center = `Center Name`)

toolfile_counts_dt <- toolfile_counts %>%
    format_summarytable_columns(c("Center", group_keys)) %>%
    as_datatable()

syn_dt_entity <- toolfile_counts_dt %>%
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

