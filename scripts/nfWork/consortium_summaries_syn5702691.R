source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")
##NEXUS site
# Script/template to create summary tables and charts for a "project"

synapseLogin()


# Config ------------------------------------------------------------------

synproject_id <- "syn5702691" # Synapse project for consortium
consortium_id <- "syn5702691" # Synapse folder associated with consortium
parent_id <- "syn11405764" # consortium 'Reporting' folder where files should be stored
master_fileview_id <- "syn11391666" # Synapse fileview associated with consortium data

imported_fileview_id <-"syn11391664"

# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)


# Add Synapse project info --------------------------------------------

fileview_df <- fileview_df %>%
    left_join(summarize_project_info(.), by = "projectId")


#ok. in the code you have `list(projectId="Project")` — the function doesn’t know that “Center” ~ “Project”, you have to give it an actual column name :)

# Data files by assay and tumor type --------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndProject.html",
    source_id = consortium_id)

nf1_table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndNF1Genotype.html",
                             source_id = consortium_id)

nf2_table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndNF2Genotype.html",
    source_id = consortium_id)

# create and save table - NF1
group_keys <- c("assay", "nf1Genotype")
count_cols <- c("id", "cellType", "individualID")
list_cols <- c("projectName")
link_keys <-list(projectName="projectId")

datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        count_cols = count_cols,
        list_cols = list_cols,
        link_keys = link_keys
        ) %>%
    rename(Project = projectName)

datafile_counts_dt <- datafile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()

syn_dt_entity <- datafile_counts_dt %>%
    save_datatable(parent_id, nf1_table_filename, .)

# view table
datafile_counts_dt


# Data files by assay -----------------------------------------------------

# create and save table
group_keys <- "assay"
count_cols <- c("id", "tumorType", "diagnosis")
list_cols <- c("projectName")
link_keys <-list(projectName="projectId")

datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        list_cols = list_cols,
        link_keys = link_keys,
        count_cols = count_cols
    ) %>%
    rename(Project = projectName)

datafile_counts_dt <- datafile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()

syn_dt_entity <- datafile_counts_dt %>%
    save_datatable(parent_id, table_filename, .)

# view table
datafile_counts_dt

#Now do plots -------------------------------------------
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type",
    diagnosis = "Diagnosis", species = "Species",
    organ = "Organ", tissue = "Tissue",
    dataType = "Data Type", study = "Study")

chart_filename <- glue::glue("{source_id}_DataFilesByCenterAndAssay.html",
                             source_id = consortium_id)

chart <- fileview_df %>%
    plot_file_counts_by_annotationkey(plot_keys)

chart

syn_entity <- save_chart(parent_id, chart_filename, chart)



chart_filename <- glue::glue("{source_id}_AssayCountsByCenter.html",
    source_id = consortium_id)
# create and save chart
chart <- syn_chart_entity <- fileview_df %>%
    plot_assay_counts_by_center()

syn_chart_entity <- save_chart(parent_id, chart_filename, chart)

# view chart
chart

chart_filename <- glue::glue("{source_id}_annotationSummary.html",
    source_id = consortium_id)


chart <- syn_chart_entity <- fileview_df %>%
    get_annotation_summary()
syn_chart_entity <- save_chart(parent_id, chart_filename, chart)

# view chart
chart



