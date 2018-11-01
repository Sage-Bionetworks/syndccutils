source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")
library(magrittr)
library(dplyr)
library(purrr)
library(ggplot2)

##DHART SPORE Site
# Script/template to create summary tables and charts for a "project"

synLogin()
update_remote <- TRUE

# Config ------------------------------------------------------------------

synproject_id <- "syn5666132" # Synapse project for consortium
consortium_id <- "syn5666132" # Synapse folder associated with consortium
parent_id <- "syn12434311" # consortium 'Reporting' folder where files should be stored
master_fileview_id <- "syn11581437" # Synapse fileview associated with consortium data


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)
fileview_df$projectId <- "syn5666132"

hierarchy_id <- "syn11393267"
hierarchy_df <- get_table_df(hierarchy_id)
hierarchy_df <- hierarchy_df[hierarchy_df$`Consortium Name` =="DHART SPORE",]
colnames(hierarchy_df) <- c("projectId","studyId","study","consortium","consortiumName")
ind <- apply(hierarchy_df, 1, function(x) all(is.na(x)))
hierarchy_df <- hierarchy_df[!ind,]
hierarchy_df$consortium <- NULL
hierarchy_df$consortiumName <- NULL

# Add Synapse project info --------------------------------------------

new_fileview_df <- fileview_df %>%
  left_join(hierarchy_df, by = c("projectId","study"))

# Data files by assay and study --------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndStudy.html",
                             source_id = consortium_id)

# create and save table - by assay
group_keys <- c("assay","study")
count_cols <- c("id", "individualID")
list_cols <- c("studyName")
link_keys <-list(studyName="study")

datafile_counts <- new_fileview_df %>%
  summarize_by_annotationkey(
    annotation_keys = group_keys,
    table_id = master_fileview_id,
    count_cols = count_cols,
    queryformat='html')

datafile_counts_dt <- datafile_counts %>%
  format_summarytable_columns(group_keys) %>%
  as_datatable()

if (update_remote) {
  syn_dt_entity <- datafile_counts_dt %>%
    save_datatable(parent_id, table_filename, .)
}

# view table
datafile_counts_dt


# Samples (specimenID) by assay and study --------------------------------------

table_filename <- glue::glue("{source_id}_SampleCountsByAssayAndStudy.html",
                             source_id = consortium_id)

# create and save table - by assay
group_keys <- c("assay","study")
count_cols <- c("specimenID")
list_cols <- c("studyName")
link_keys <-list(studyName="study")

sample_counts <- new_fileview_df %>%
  summarize_by_annotationkey(
    annotation_keys = group_keys,
    table_id = master_fileview_id,
    count_cols = count_cols,
    queryformat='html')

sample_counts_dt <- sample_counts %>%
  format_summarytable_columns(group_keys) %>%
  as_datatable()

if (update_remote) {
  syn_dt_entity <- sample_counts_dt %>%
    save_datatable(parent_id, table_filename, .)
}

# view table
sample_counts_dt

#Now do plots -------------------------------------------
plot_keys <- list(assay = "Assay", 
                  species = "Species",
                  dataType = "Data Type", 
                  study = "Study")

chart_filename <- glue::glue("{source_id}_DataFilesByAssaySpeciesDataTypeStudy.html",
                             source_id = consortium_id)

chart <- new_fileview_df %>%
  plot_file_counts_by_annotationkey(plot_keys)

chart

if (update_remote) {
  syn_entity <- save_chart(parent_id, chart_filename, chart)
}


# create and save chart
chart <- syn_chart_entity <- new_fileview_df %>%
  plot_assay_counts_by_center()

if (update_remote) {
  syn_chart_entity <- save_chart(parent_id, chart_filename, chart)
}

# view chart
chart

# plot 2
plot_keys <- list(assay = "Assay", study = "Study")

chart_filename <- glue::glue("{source_id}_SamplesByAssayAndStudy.html",
                             source_id = consortium_id)

chart <- new_fileview_df %>%
  plot_sample_counts_by_annotationkey_2d(sample_key="specimenID",annotation_keys=plot_keys)

chart

if (update_remote) {
  syn_entity <- save_chart(parent_id, chart_filename, chart)
}