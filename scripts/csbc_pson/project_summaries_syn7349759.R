source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")
source("R/utils.R")
#This works on the CSBC PSOC site

# Script/template to create summary tables and charts for a "study"
synLogin()
# Config ------------------------------------------------------------------

synproject_id <- "syn7315805" # Synapse project for project Center
parent_id <- "syn11738140" # Center 'Reporting' folder where files should be stored
master_fileview_id <- "syn11448522" # Synapse fileview associated with project
tool_fileview_id <- "syn11957355"
# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)

##Start with scRNA seq project
source_id <- "syn11448532" # Synapse folder associated with project


# Assays by patient -------------------------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssay.html",
                             source_id = source_id)
files_by_assay_and_diagnosis_table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndDiagnosis.html",
                                                          source_id = source_id)
assay_by_diagnosis_chart_filename <- glue::glue("{source_id}_AssayDataFilesByDiagnosis.html",
                                            source_id = source_id)
patient_by_diagnosis_chart_filename <- glue::glue("{source_id}_PatientsByDiagnosis.html",
                                              source_id = source_id)


summarize_datafiles_by_assay_and_diagnosis <- function(view_df, table_id) {
    count_cols <- c("id", "individualID",
                    "specimenID")
    view_df %>%
        group_by(assay, diagnosis) %>%
        summarise_at(count_cols, n_distinct) %>%
        rowwise() %>%
        mutate(sourceFileview = table_id,
               query = build_tablequery(sourceFileview, assay, diagnosis)) %>%
        add_queryview_column(format = "html") %>%
        select(-query)
}


# create and save table
##datafile_counts_by_assay <- fileview_df %>%
##    summarize_datafiles_by_assay(master_fileview_id)

##datafile_counts_by_assay_dt <- datafile_counts_by_assay %>%
##    format_summarytable_columns("assay") %>%
##    as_datatable()

datafile_counts_by_assay_and_diagnosis <- fileview_df %>%
    summarize_datafiles_by_assay_and_diagnosis(master_fileview_id)

datafile_counts_by_assay_and_diagnosis_dt <- datafile_counts_by_assay_and_diagnosis %>%
    format_summarytable_columns(c("assay", "diagnosis")) %>%
    as_datatable()

##syn_dt_entity <- datafile_counts_by_assay_dt %>%
##    save_datatable(parent_id, table_filename, .)

syn_file_by_assay_and_diagnosis_dt_entity <- datafile_counts_by_assay_and_diagnosis_dt %>%
    save_datatable(parent_id, files_by_assay_and_diagnosis_table_filename, .)



chart<-plot_sample_counts_by_annotationkey_2d(fileview_df,sample_key='individualID',annotation_keys=c(tumorType='Tumor Type',egfrStatus='EGFR Status'))

syn_entity <- save_chart(parent_id, patient_by_diagnosis_chart_filename, chart)


# Files by category -------------------------------------------------------

chart_filename <- glue::glue("{source_id}_AllFilesByCategory.html",
                             source_id = source_id)

categories <- list(assay = "Assay", diagnosis = "Diagnosis",
                   species = "Species",
                   organ = "Organ", tissue = "Tissue",
                   dataType = "Data Type")

chart <- categories %>%
    map2(.y = names(.), function(annotation_prettykey, annotation_key) {
        p <- fileview_df %>%
            group_by(.dots = annotation_key) %>%
            tally() %>%
            ggplot(aes(x = 1, y = n)) +
            geom_col(aes_string(fill = annotation_key),
                     colour = "white") +
            scale_fill_viridis_d() +
            xlab(annotation_prettykey) +
            ylab("Number of Files") +
            theme_minimal() +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()) +
            guides(fill = FALSE)
        ggplotly(p, tooltip = c("y", "fill"),
                 width = 100 * length(categories) + 50,
                 height = 300)
    }) %>%
    subplot(shareY = TRUE, titleX = TRUE) %>%
    layout(showlegend = FALSE,
           font = list(family = "Roboto, Open Sans, sans-serif"))
# chart
syn_entity <- save_chart(parent_id, chart_filename, chart)





