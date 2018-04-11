source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")
source("R/utils.R")
##CTF SITE
# Script/template to create summary tables and charts for a "project"

synLogin()
update_remote <- TRUE

# Config ------------------------------------------------------------------

synproject_id <- "syn6135075" # Synapse project for consortium
consortium_id <- "syn6135075" # Synapse folder associated with consortium
parent_id <- "syn10901483" # consortium 'Reporting' folder where files should be stored
master_fileview_id <- "syn11614206" # Synapse fileview associated with consortium data

# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)

# Add Synapse project info --------------------------------------------

fileview_df <- fileview_df %>%
    left_join(summarize_project_info(.), by = "projectId")


# Data files by assay and tumor type --------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndProject.html",
    source_id = consortium_id)

nf1_table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndNF1Genotype.html",
                             source_id = consortium_id)

nf2_table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndNF2Genotype.html",
    source_id = consortium_id)

# create and save table - NF1
group_keys <- c("assay", "diagnosis")
count_cols <- c("id", "cellType", "individualID")
list_cols <- c("Center")
link_keys <-list(Center="projectId")

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

if (update_remote) {
    syn_dt_entity <- datafile_counts_dt %>%
        save_datatable(parent_id, nf1_table_filename, .)
}

# view table
datafile_counts_dt

###now create and save table for NF2
group_keys <- c("assay", "tumorType")
count_cols <- c("id", "cellType", "individualID")

list_cols <- c("Center")
link_keys <-list(Center="projectId")

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

if (update_remote) {
    syn_dt_entity <- datafile_counts_dt %>%
        save_datatable(parent_id, nf2_table_filename, .)
}

# view table
datafile_counts_dt

group_keys <- c( "projectId","assay")
count_cols <- c("id", "cellType", "individualID")
synproject_key <- "Center"

datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        count_cols = count_cols,
        synproject_key = synproject_key
    )

datafile_counts_dt <- datafile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()


# view table
datafile_counts_dt

if (update_remote) {
    syn_dt_entity <- datafile_counts_dt %>%
        save_datatable(parent_id, table_filename, .)
}


#Now do plots -------------------------------------------
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type",
    diagnosis = "Diagnosis", species = "Species",
    organ = "Organ", tissue = "Tissue",
    dataType = "Data Type", study = "Study")

chart_filename <- glue::glue("{source_id}_DataFilesByAndAssay.html",
                             source_id = consortium_id)

chart <- fileview_df %>%
    plot_file_counts_by_annotationkey(plot_keys)

chart
if (update_remote) {
    syn_entity <- save_chart(parent_id, chart_filename, chart)
}

chart_filename <- glue::glue("{source_id}_DataFilesByCenterAndAssay.html",
    source_id = consortium_id)


# create and save chart
chart <- syn_chart_entity <- fileview_df %>%
    plot_assay_counts_by_center()

if (update_remote) {
    syn_chart_entity <- save_chart(parent_id, chart_filename, chart)
}

# view chart
chart


