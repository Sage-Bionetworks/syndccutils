source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

# Script/template to create summary tables and charts for a "project"
synapseLogin()
# Config ------------------------------------------------------------------

synproject_id <- "syn7080714" # Synapse project for consortium
consortium_id <- "syn7080714" # Synapse folder associated with consortium
parent_id <- "syn10846095" # consortium 'Reporting' folder where files should be stored
master_fileview_id <- "syn9630847" # Synapse fileview associated with consortium
master_tool_fileview_id <- "syn9898965"


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)
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
    plot_sample_counts_by_annotationkey_2d(sample_key = "cellLine",
                                           annotation_keys = plot_keys)
chart
syn_chart_entity <- save_chart(parent_id, chart_filename, chart)



# Data files by Center ----------------------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByCenterAndAssay.html",
                             source_id = consortium_id)

# create and save table
datafile_counts_by_center_and_assay <- fileview_df %>%
    summarize_datafiles_by_center_and_assay(master_fileview_id)

datafile_counts_by_center_and_assay_dt <- datafile_counts_by_center_and_assay %>%
    format_summarytable_columns(c("Center", "assay")) %>%
    as_datatable()

datafile_counts_by_center_and_assay_dt
syn_dt_entity <- datafile_counts_by_center_and_assay_dt %>%
    save_datatable(parent_id, table_filename, .)


# Data files by Center and assay (chart) ----------------------------------

chart_filename <- glue::glue("{source_id}_DataFilesByCenterAndAssay.html",
    source_id = consortium_id)


##create and save chart
chart <- syn_chart_entity <- fileview_df %>%
    plot_assay_counts_by_center()
chart
syn_chart_entity <- save_chart(parent_id, chart_filename, chart)



# Tool files by Center ----------------------------------------------------

table_filename <- glue::glue("{source_id}_ToolFileCountsByCenter.html",
    source_id = consortium_id)

# create and save table
toolfile_counts_by_center <- tool_fileview_df %>%
    summarize_toolfiles_by_center(tool_fileview_id)

toolfile_counts_by_center_dt <- toolfile_counts_by_center %>%
    format_summarytable_columns(c("Center")) %>%
        as_datatable()
toolfile_counts_by_center_dt

syn_dt_entity <- toolfile_counts_by_center_dt %>%
    save_datatable(parent_id, table_filename, .)


# Tool files by Center, input, and output ---------------------------------

input_chart_filename <- glue::glue("{source_id}_ToolFilesByInput.html",
                                   source_id = consortium_id)

output_chart_filename <- glue::glue("{source_id}_ToolFilesByOutput.html",
                                    source_id = consortium_id)


# create and save chart
chart1 <- tool_fileview_df %>%
    plot_tool_inputs()
# chart1
syn_chart1_entity <- save_chart(parent_id, input_chart_filename, chart1)

chart2 <- tool_fileview_df %>%
    plot_tool_outputs()
chart2
syn_chart2_entity <- save_chart(parent_id,output_chart_filename,chart2)

