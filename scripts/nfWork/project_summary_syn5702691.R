source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")
##NEXUS work
# Script/template to create summary tables and charts for a "project"

synapseLogin()


# Config ------------------------------------------------------------------

synproject_id <- "syn5702691" # Synapse project for project
consortium_id <- synproject_id # Synapse folder associated with consortium
parent_id <- "syn11405764" # consortium 'Reporting' folder where files should be stored
master_fileview_id <- "syn11391666" # Synapse fileview associated with consortium data


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)


# Add Synapse project info --------------------------------------------

fileview_df <- fileview_df %>%
    left_join(summarize_project_info(.), by = "projectId")

# Collect consortium hierarchy info ---------------------------------------

hierarchy_id <- "syn11393267"
hierarchy_df <- get_table_df(hierarchy_id)


# Collect Synapse project info --------------------------------------------

synproject_df <- fileview_df %>%
    summarize_project_info()%>%select(projectId,projectName,Program)


# Add hierarchy and Synapse project info ----------------------------------

fileview_df <- fileview_df %>%
    left_join(hierarchy_df, by = c("study" = "Study Name")) %>%
    left_join(synproject_df, by = c("Project" = "projectName")) %>%
    rename(`Project Name` = `projectName`)

#----
#Try to figure out which assays are performed for which cell lines
#chart_filename <- glue::glue("{source_id}_cellLinesByAssay.html",
#    source_id = consortium_id)

# create and save chart
#plot_keys <- list(assay = "Assay",specimenID = "Cell Line")

#chart <- fileview_df %>%
#    plot_sample_counts_by_annotationkey_2d(sample_key = "id",
#        annotation_keys = plot_keys,filter_missing = FALSE)
#syn_chart_entity <- save_chart(parent_id, chart_filename, chart)

# view chart
#chart

#----now download data by cell line



##first plot: study type by agency
group_keys <- c("resourceType","fundingAgency")
count_cols <- c("study")

cell_line_counts <- fileview_df %>%
    summarize_files_by_annotationkey(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        count_cols = count_cols,
        queryformat= "raw"
    )

#store table to synapse
syn_id <- datatable_to_synapse( cell_line_counts,synproject_id, "Study Types By Agency")

#provide query string
wiki_string <- simple_plots_wiki_string(syn_id@schema@properties$id, group_keys,count_cols,title='Study Types')


##next plot: analysis and assay types
group_keys <-c("analysisType")#,"fundingAgency")
count_cols <-c('id')
list_cols <-c('study','Project Name')
link_keys <-list(`Project Name`='projectId.x',study='Study')


table_filename <- glue::glue("{source_id}_DataFileCountsByAnalysisType.html",
    source_id = consortium_id)

datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        count_cols = count_cols,
        table_id = master_fileview_id,
        list_cols = list_cols,
        link_keys = link_keys
    )%>%as_datatable()

datafile_counts
##prettier tables are here
#cell_line_counts_dt <- cell_line_counts %>%
 #   format_summarytable_columns(c("Center", group_keys)) %>%
#    as_datatable()

syn_dt_entity <- datafile_counts %>%
    save_datatable(parent_id, table_filename, .)

group_keys <-c("analysisType","fundingAgency")
count_cols <-c('id')
list_cols <-c('study','Project Name')
link_keys <-list(`Project Name`='projectId.x',study='Study')


table_filename <- glue::glue("{source_id}_DataFileCountsByAnalysisTypeAndFunder.html",
    source_id = consortium_id)

datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        count_cols = count_cols,
        table_id = master_fileview_id,
        list_cols = list_cols,
        link_keys = link_keys
    )%>%as_datatable()

datafile_counts
##prettier tables are here
#cell_line_counts_dt <- cell_line_counts %>%
#   format_summarytable_columns(c("Center", group_keys)) %>%
#    as_datatable()

syn_dt_entity <- datafile_counts %>%
    save_datatable(parent_id, table_filename, .)


group_keys <-c("dataType","fundingAgency")
count_cols <-c('id')
list_cols <-c('study','Project Name')
link_keys <-list(`Project Name`='projectId.x',study='Study')

table_filename <- glue::glue("{source_id}_DataFileCountsByDaataTypeAndFunder.html",
    source_id = consortium_id)

datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        count_cols = count_cols,
        table_id = master_fileview_id,
        list_cols = list_cols,
        link_keys = link_keys
    )%>%as_datatable()

datafile_counts
##prettier tables are here
#cell_line_counts_dt <- cell_line_counts %>%
#   format_summarytable_columns(c("Center", group_keys)) %>%
#    as_datatable()

syn_dt_entity <- datafile_counts %>%
    save_datatable(parent_id, table_filename, .)
