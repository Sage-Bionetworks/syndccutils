source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

#pNF Model Systems for Preclinical Drug Screening
synLogin()
update_remote <- TRUE

# Config ------------------------------------------------------------------

synproject_id <- "syn7217928" # Synapse Project for project Center
consortium_id <- "syn4939478" # Synapse folder associated with consortium
parent_id <- "syn12971487" # Center 'Reporting' folder where files should be stored
master_fileview_id <- "syn11581628" # Synapse fileview associated with project

# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)

# Summarize -------------------------------------------------------

# summarize by assay
table_filename <- glue::glue("{source_id}_DataFileCountsByAssay.html",
                             source_id = synproject_id)

# create and save table
group_keys <- c("specimenID","assay")
count_cols <- c("id")

datafile_counts <- fileview_df %>%
  summarize_by_annotationkey(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        count_cols = count_cols,
        queryformat='html'
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

###-------
# summarize by dataType
files_by_dataType_table_filename <- glue::glue("{source_id}_DataFileCountsByDataType.html",
                                               source_id = source_id)
group_keys=c('dataType')
datafile_counts <- fileview_df %>%
  summarize_by_annotationkey(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        count_cols = count_cols
    )

datafile_counts_dt <- datafile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()

# view table
datafile_counts_dt

if (update_remote) {
    syn_dt_entity <- datafile_counts_dt %>%
        save_datatable(parent_id, files_by_dataType_table_filename, .)
}

###-------

# plot --------------------------------------------------------

## plot file counts
chart_filename <- glue::glue("{source_id}_AllDataFilesByCategory.html",
                             source_id = source_id)
plot_keys <- list(assay = "Assay",
                  dataType = "Data Type",
                  cellType = "Cell Type")

chart <- fileview_df %>%
    plot_file_counts_by_annotationkey(plot_keys)

chart
if (update_remote) {
    syn_entity <- save_chart(parent_id, chart_filename, chart)
}
