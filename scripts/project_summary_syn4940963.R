source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")
##NTAP pNF cell culture
# Script/template to create summary tables and charts for a "project"

synapseLogin()


# Config ------------------------------------------------------------------

synproject_id <- "syn4940963" # Synapse project for project
consortium_id <- "syn4939478" # Synapse folder associated with consortium
parent_id <- "syn11270117" # consortium 'Reporting' folder where files should be stored
master_fileview_id <- "syn11270144" # Synapse fileview associated with consortium data


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)


# Add Synapse project info --------------------------------------------

fileview_df <- fileview_df %>%
    left_join(summarize_project_info(.), by = "projectId")

#----
#Try to figure out which assays are performed for which cell lines
chart_filename <- glue::glue("{source_id}_cellLinesByAssay.html",
    source_id = consortium_id)

# create and save chart
plot_keys <- list(assay = "Assay",specimenID = "Cell Line")

chart <- fileview_df %>%
    plot_sample_counts_by_annotationkey_2d(sample_key = "id",
        annotation_keys = plot_keys,filter_missing = FALSE)
syn_chart_entity <- save_chart(parent_id, chart_filename, chart)

# view chart
chart
