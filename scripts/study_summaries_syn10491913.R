source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

# Script/template to create summary tables and charts for a "study"

# Config ------------------------------------------------------------------

synproject_id <- "syn7315808" # Synapse project for project Center
study_id <- "syn10491913" # Synapse folder associated with study
parent_id <- "syn10831920" # Center 'Reporting' folder where files should be stored
master_fileview_id <- "syn10531116" # Synapse fileview associated with study


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)


# Assays by patient -------------------------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssay.html",
                             source_id = study_id)
files_by_assay_and_tumortype_table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndTumorType.html",
                                                          source_id = study_id)
assay_by_tumor_chart_filename <- glue::glue("{source_id}_AssayDataFilesByTumorType.html",
                                            source_id = study_id)
patient_by_tumor_chart_filename <- glue::glue("{source_id}_PatientsByTumorType.html",
                                              source_id = study_id)

summarize_datafiles_by_assay_and_tumortype <- function(view_df, table_id) {
    count_cols <- c("id", "diagnosis", "individualID",
                    "specimenID")
    view_df %>%
        group_by(assay, tumorType) %>%
        summarise_at(count_cols, n_distinct) %>%
        rowwise() %>%
        mutate(sourceFileview = table_id,
               query = build_tablequery(sourceFileview, assay, tumorType)) %>%
        add_queryview_column(format = "html") %>%
        select(-query)
}


# create and save table
datafile_counts_by_assay <- fileview_df %>%
    summarize_datafiles_by_assay(master_fileview_id)

datafile_counts_by_assay_dt <- datafile_counts_by_assay %>%
    format_summarytable_columns("assay") %>%
    as_datatable()

datafile_counts_by_assay_and_tumortype <- fileview_df %>%
    summarize_datafiles_by_assay_and_tumortype(master_fileview_id)

datafile_counts_by_assay_and_tumortype_dt <- datafile_counts_by_assay_and_tumortype %>%
    format_summarytable_columns(c("assay", "tumorType")) %>%
    as_datatable()

syn_dt_entity <- datafile_counts_by_assay_dt %>%
    save_datatable(parent_id, table_filename, .)

syn_file_by_assay_and_tumortype_dt_entity <- datafile_counts_by_assay_and_tumortype_dt %>%
    save_datatable(parent_id, files_by_assay_and_tumortype_table_filename, .)


## Only change here from plot_assay_counts_by_tumortype
## is adding an ylab
my_plot_assay_counts_by_tumortype <- function(view_df) {
    p <- fileview_df %>%
        group_by(assay, tumorType) %>%
        tally() %>%
        ggplot(aes(x = tumorType, y = n)) +
        geom_col(aes(fill = assay)) + coord_flip() +
        scale_fill_viridis_d() +
        xlab("") +
        ylab("Number of Files")

    ggplotly(p, height = 500) %>%
        layout(margin = list(l = 150, r = 100, b = 55))
}

## Differs from my_plot_assay_counts_by_tumortype above in that
## tally -> summarize and the ylab is different

my_plot_patient_counts_by_tumortype <- function(view_df) {
    p <- fileview_df %>%
        group_by(assay, tumorType) %>%
        summarize(n = n_distinct(individualID)) %>%
        ggplot(aes(x = tumorType, y = n)) +
        geom_col(aes(fill = assay)) + coord_flip() +
        scale_fill_viridis_d() +
        xlab("") +
        ylab("Number of Patients")

    ggplotly(p, height = 500) %>%
        layout(margin = list(l = 150, r = 100, b = 55))
}

# create and save chart
syn_assay_by_tumor_chart_entity <- fileview_df %>%
    my_plot_assay_counts_by_tumortype() %>%
    save_chart(parent_id, assay_by_tumor_chart_filename, .)

syn_patient_by_tumor_chart_entity <- fileview_df %>%
    my_plot_patient_counts_by_tumortype() %>%
    save_chart(parent_id, patient_by_tumor_chart_filename, .)
