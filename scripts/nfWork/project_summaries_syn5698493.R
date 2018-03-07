source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")
#synodos LGG page
# Script/template to create summary tables and charts for a "study"
synLogin()
# Config ------------------------------------------------------------------

synproject_id <- "syn5698493" # Synapse project for project Center
source_id <- "syn5698493" # Synapse folder associated with project
parent_id <- "syn10923182" # Center 'Reporting' folder where files should be stored
master_fileview_id <- "syn11614205" # Synapse fileview associated with project

# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)


# Summarize -------------------------------------------------------

# summarize by assay
table_filename <- glue::glue("{source_id}samplesBySpeciesAssay.html",
                             source_id = source_id)


#table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndTumorType.html",
#    source_id = project_id)

# create and save table
group_keys <- c("assay","tumorType")
count_cols <- c("individualID")

datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
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
###-------


# summarize by species

# summarize by species
files_by_dataType_table_filename <- glue::glue("{source_id}_samplesBySpeciesDataType.html",
                                               source_id = source_id)
group_keys=c('dataType','tumorType')
datafile_counts <- fileview_df %>%
    summarize_files_by_annotationkey_new(
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

# plot --------------------------------------------------------

## plot file counts
chart_filename <- glue::glue("{source_id}_AllDataFilesByCategory.html",
                             source_id = source_id)
plot_keys <- list(assay = "Assay",
                  tumorType = "Tumor Type",
                  study = "Study")

chart <- fileview_df %>%
    plot_file_counts_by_annotationkey(plot_keys)

chart
syn_entity <- save_chart(parent_id, chart_filename, chart)


##plot samples by species, data type
chart_filename <- glue::glue("{source_id}_samplesByDataTypeSpeciesChart.html",
    source_id = source_id)

chart <- fileview_df%>%plot_sample_counts_by_annotationkey_2d(sample_key = "individualID",
    annotation_keys=list(dataType='Data Type',tumorType='Tumor Type'), filter_missing = FALSE)
chart
syn_entity <- save_chart(parent_id,chart_filename,chart)
