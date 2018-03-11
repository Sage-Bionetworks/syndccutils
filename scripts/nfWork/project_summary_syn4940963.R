source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")
##NTAP pNF cell culture
# Script/template to create summary tables and charts for a "project"

synLogin()


# Config ------------------------------------------------------------------

synproject_id <- "syn4940963" # Synapse project for project
consortium_id <- "syn4939478" # Synapse folder associated with consortium
parent_id <- "syn11270117" # consortium 'Reporting' folder where files should be stored
master_fileview_id <- "syn11270144" # Synapse fileview associated with consortium data


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)

cell_lines <- synTableQuery('SELECT distinct Sample FROM syn8496249')@values

fileview_df<-subset(fileview_df,specimenID%in%cell_lines$Sample)

# Add Synapse project info --------------------------------------------

fileview_df <- fileview_df %>%
    left_join(summarize_project_info(.), by = "projectId")

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

#table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndcellline.html",
#    source_id = consortium_id)


group_keys <- c("specimenID","assay")
count_cols <- c("id")

cell_line_counts <- fileview_df %>%
    summarize_files_by_annotationkey(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        count_cols = count_cols,
        queryformat= "raw"
    )

#store table to synapse
syn_id <- datatable_to_synapse(synproject_id, "Cell Line Counts", cell_line_counts)

#provide query string
wiki_string <- simple_plots_wiki_string(syn_id@schema@properties$id, group_keys,count_cols,title='Cell Line')

#provide markdown table???


##prettier tables are here
#cell_line_counts_dt <- cell_line_counts %>%
 #   format_summarytable_columns(c("Center", group_keys)) %>%
#    as_datatable()

#syn_dt_entity <- cell_line_counts_dt %>%
#    save_datatable(parent_id, table_filename, .)

# view table
#cell_line_counts_dt

