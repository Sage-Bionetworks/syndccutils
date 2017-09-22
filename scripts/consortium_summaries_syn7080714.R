source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

# Script/template to create summary tables and charts for a "project"
synapseLogin()
# Config ------------------------------------------------------------------

synproject_id <- "syn7080714" # Synapse project for consortium
consortium_id <- "syn7080714" # Synapse folder associated with consortium
parent_id <- "syn10846095" # consortium 'Reporting' folder where files should be stored
master_fileview_id <- "syn9630847" # Synapse fileview associated with consortium
master_tool_fileview_id <- "syn9898965"


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)
tool_fileview_df <- get_table_df(master_tool_fileview_id)

# Add Synapse project info --------------------------------------------

fileview_df <- fileview_df %>%
    left_join(summarize_project_info(.), by = "projectId")
tool_fileview_df <- tool_fileview_df %>%
    left_join(summarize_project_info(.), by = "projectId")

# Data files by assay and tumor type --------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndTumorType.html",
                             source_id = consortium_id)

# create and save table
datafile_counts_by_assay_and_tumortype <- fileview_df %>%
    summarize_datafiles_by_assay_and_tumortype(master_fileview_id)

datafile_counts_by_assay_and_tumortype_dt <- datafile_counts_by_assay_and_tumortype %>%
    format_summarytable_columns(c("assay", "tumorType")) %>%
    as_datatable()
datafile_counts_by_assay_and_tumortype_dt
syn_dt_entity <- datafile_counts_by_assay_and_tumortype_dt %>%
    save_datatable(parent_id, table_filename, .)


# Individuals by assays by tumor type -------------------------------------

chart_filename <- glue::glue("{source_id}_IndividualsByAssayAndTumorType.html",
                             source_id = project_id)

# create and save chart
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type")
chart <- fileview_df %>%
    plot_sample_counts_by_annotationkey_2d(sample_key = "cellLine",
                                           annotation_keys = plot_keys)
chart
syn_chart_entity <- save_chart(parent_id, chart_filename, chart)



# Data files by Center ----------------------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByCenterAndAssay.html",
                             source_id = consortium_id)

# create and save table
datafile_counts_by_center_and_assay <- fileview_df %>%
    summarize_datafiles_by_center_and_assay(master_fileview_id)

datafile_counts_by_center_and_assay_dt <- datafile_counts_by_center_and_assay %>%
    format_summarytable_columns(c("Center", "assay")) %>%
    as_datatable()

datafile_counts_by_center_and_assay_dt
syn_dt_entity <- datafile_counts_by_center_and_assay_dt %>%
    save_datatable(parent_id, table_filename, .)


# Data files by Center and assay (chart) ----------------------------------

chart_filename <- glue::glue("{source_id}_DataFilesByCenterAndAssay.html",
    source_id = consortium_id)


##create and save chart
chart <- syn_chart_entity <- fileview_df %>%
    plot_assay_counts_by_center()
chart
syn_chart_entity <- save_chart(parent_id, chart_filename, chart)


# Files by category -------------------------------------------------------

chart_filename <- glue::glue("{source_id}_DataFilesByCategory.html",
                             source_id = consortium_id)
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type",
                  diagnosis = "Diagnosis", species = "Species",
                  organ = "Organ", tissue = "Tissue",
                  dataType = "Data Type", study = "Study")

chart <- fileview_df %>%
    plot_file_counts_by_annotationkey(plot_keys)
chart
syn_entity <- save_chart(parent_id, chart_filename, chart)


# Tool files by Center ----------------------------------------------------

table_filename <- glue::glue("{source_id}_ToolFileCountsByCenter.html",
    source_id = consortium_id)

# create and save table
toolfile_counts_by_center <- tool_fileview_df %>%
    summarize_toolfiles_by_center(tool_fileview_id)

toolfile_counts_by_center_dt <- toolfile_counts_by_center %>%
    format_summarytable_columns(c("Center")) %>%
        as_datatable()
toolfile_counts_by_center_dt

syn_dt_entity <- toolfile_counts_by_center_dt %>%
    save_datatable(parent_id, table_filename, .)


# Tool files by Center, input, and output ---------------------------------

input_chart_filename <- glue::glue("{source_id}_ToolFilesByInput.html",
                                   source_id = consortium_id)

input_table_filename <- glue::glue("{source_id}_ToolFilesCountsByInput.html",
    source_id = consortium_id)

output_chart_filename <- glue::glue("{source_id}_ToolFilesByOutput.html",
                                    source_id = consortium_id)

output_table_filename <- glue::glue("{source_id}_ToolFilesCountsByOutput.html",
    source_id = consortium_id)

# create and save chart
chart1 <- tool_fileview_df %>%
    plot_tool_inputs()
# chart1
syn_chart1_entity <- save_chart(parent_id, input_chart_filename, chart1)

chart2 <- tool_fileview_df %>%
    plot_tool_outputs()
chart2
syn_chart2_entity <- save_chart(parent_id,output_chart_filename,chart2)

##now do tables for input/output types
toolfile_counts_by_input <- tool_fileview_df %>%
    summarize_toolfiles_by_input(tool_fileview_id) %>%
    format_summarytable_columns(c("inputDataType")) %>%
    as_datatable()

syn_id_entity <-toolfile_counts_by_input %>% save_datatable(parent_id,input_table_filename,.)

toolfile_counts_by_output <- tool_fileview_df %>%
    summarize_toolfiles_by_output(tool_fileview_id) %>%
    format_summarytable_columns(c("outputDataType")) %>%
    as_datatable()

syn_od_entity <-toolfile_counts_by_output %>% save_datatable(parent_id,output_table_filename,.)

#create and save charts
# tool_fileview_df %>% inner_join(summarize_project_info(tool_fileview_df),by='projectId') %>%
#     plot_tool_inputs() %>%
#     save_chart(parent_id,input_chart_filename,.)
#
# tool_fileview_df %>% inner_join(summarize_project_info(tool_fileview_df),by='projectId') %>%
#     plot_tool_outputs() %>%
#     save_chart(parent_id,output_chart_filename,.)

# #===========================================
# ## my_format_summarytable_columns adds projectName -> study Name to mapping
# my_format_summarytable_columns <- function(df, facet_cols = c()) {
#     name_map <- tibble(name = names(df)) %>%
#         mutate(formatted_name = case_when(
#             name == "id" ~ "Files",
#             name == "assay" & (name %in% facet_cols) ~ "Assay",
#             name == "assay" & !(name %in% facet_cols) ~ "Assays",
#             name == "projectName" & (name %in% facet_cols) ~ "Study",
#             name == "projectName" & !(name %in% facet_cols) ~ "Studies",
#             name == "diagnosis" & (name %in% facet_cols) ~ "Diagnosis",
#             name == "diagnosis" & !(name %in% facet_cols) ~ "Diagnoses",
#             name == "tumorType" & (name %in% facet_cols) ~ "Tumor Type",
#             name == "tumorType" & !(name %in% facet_cols) ~ "Tumor Types",
#             name == "individualID" & (name %in% facet_cols) ~ "Individual",
#             name == "individualID" & !(name %in% facet_cols) ~ "Individuals",
#             name == "specimenID" & (name %in% facet_cols) ~ "Specimen",
#             name == "specimenID" & !(name %in% facet_cols) ~ "Specimens",
#             name == "sourceFileview" ~ "Source Fileview",
#             name == "viewFiles" ~ "View Files",
#             TRUE ~ name
#         )) %>%
#         split(.$name) %>%
#         map("formatted_name")
#     plyr::rename(df, name_map)
# }
#
# files_by_assay_and_tumortype_table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndTumorType.html",
#                                                           source_id = consortium_id)
#
# datafile_counts_by_assay_and_tumortype <- fileview_df %>%
#     summarize_datafiles_by_assay_and_tumortype(master_fileview_id)
#
# datafile_counts_by_assay_and_tumortype_dt <- datafile_counts_by_assay_and_tumortype %>%
#     format_summarytable_columns(c("assay", "tumorType")) %>%
#     as_datatable()
#
# syn_file_by_assay_and_tumortype_dt_entity <- datafile_counts_by_assay_and_tumortype_dt %>%
#     save_datatable(parent_id, files_by_assay_and_tumortype_table_filename, .)
#
#
# # Files by study, assay, and tumor type -----------------------------------
#
# summarize_datafiles_by_study_assay_and_tumortype <- function(view_df, table_id) {
#     count_cols <- c("id", "diagnosis", "individualID",
#                     "specimenID")
#     view_df %>%
#         group_by(assay, tumorType, projectName) %>%
#         summarise_at(count_cols, n_distinct) %>%
#         rowwise() %>%
#         mutate(sourceFileview = table_id,
#                query = build_tablequery(sourceFileview, assay, tumorType, projectName)) %>%
#         add_queryview_column(format = "html") %>%
#         select(-query)
# }
#
# files_by_study_assay_and_tumortype_table_filename <- glue::glue("{source_id}_DataFileCountsByStudyAssayAndTumorType.html",
#                                                                 source_id = consortium_id)
#
# datafile_counts_by_study_assay_and_tumortype <- fileview_df %>%
#     summarize_datafiles_by_study_assay_and_tumortype(master_fileview_id)
#
# datafile_counts_by_study_assay_and_tumortype_dt <- datafile_counts_by_study_assay_and_tumortype %>%
#     my_format_summarytable_columns(c("projectName", "assay", "tumorType")) %>%
#     as_datatable()
#
# syn_file_by_study_assay_and_tumortype_dt_entity <- datafile_counts_by_study_assay_and_tumortype_dt %>%
#     save_datatable(parent_id, files_by_study_assay_and_tumortype_table_filename, .)
#
#
# # Assays by tumor type ----------------------------------------------------
#
# ## Only change here from plot_assay_counts_by_tumortype
# ## is adding an ylab
# my_plot_assay_counts_by_tumortype <- function(view_df) {
#     p <- fileview_df %>%
#         group_by(assay, tumorType) %>%
#         tally() %>%
#         ggplot(aes(x = tumorType, y = n)) +
#         geom_col(aes(fill = assay)) + coord_flip() +
#         scale_fill_viridis_d() +
#         xlab("") +
#         ylab("Number of Files")
#
#     ggplotly(p, height = 500) %>%
#         layout(margin = list(l = 150, r = 100, b = 55))
# }
#
# # chart_filename <- glue::glue("{source_id}_AssayDataFilesByTumorType.html",
# #                              source_id = project_id)
# #
# # # create and save chart
# # chart <- fileview_df %>%
# #     plot_assay_counts_by_tumortype()
# #
# # syn_chart_entity <- save_chart(parent_id, chart_filename, chart)
#
# assay_by_tumor_chart_filename <- glue::glue("{source_id}_AssayDataFilesByTumorType.html",
#                                             source_id = consortium_id)
#
#
# # create and save chart
# chart <- fileview_df %>%
#     my_plot_assay_counts_by_tumortype()
# # chart
# syn_entity <- save_chart(parent_id, assay_by_tumor_chart_filename, chart)
#
#
# # Patients by tumor type --------------------------------------------------
#
# ## Differs from my_plot_assay_counts_by_tumortype above in that
# ## tally -> summarize and the ylab is different
#
# my_plot_patient_counts_by_tumortype <- function(view_df) {
#     p <- fileview_df %>%
#         group_by(assay, tumorType) %>%
#         summarize(n = n_distinct(individualID)) %>%
#         ggplot(aes(x = tumorType, y = n)) +
#         geom_col(aes(fill = assay)) + coord_flip() +
#         scale_fill_viridis_d() +
#         xlab("") +
#         ylab("Number of Patients")
#
#     ggplotly(p, height = 500) %>%
#         layout(margin = list(l = 150, r = 100, b = 55))
# }
#
# patient_by_tumor_chart_filename <- glue::glue("{source_id}_PatientsByTumorType.html",
#                                               source_id = consortium_id)
#
# chart <- fileview_df %>%
#     my_plot_patient_counts_by_tumortype()
# # chart
# syn_entity <- save_chart(parent_id, patient_by_tumor_chart_filename, chart)
#
# # Files by category -------------------------------------------------------
#
# chart_filename <- glue::glue("{source_id}_AllFilesByCategory.html",
#                              source_id = consortium_id)
#
# categories <- list(assay = "Assay", tumorType = "Tumor Type",
#                    diagnosis = "Diagnosis", species = "Species",
#                    organ = "Organ", tissue = "Tissue",
#                    dataType = "Data Type", study = "Study")
#
# chart <- categories %>%
#     map2(.y = names(.), function(annotation_prettykey, annotation_key) {
#         p <- fileview_df %>%
#             group_by(.dots = annotation_key) %>%
#             tally() %>%
#             ggplot(aes(x = 1, y = n)) +
#             geom_col(aes_string(fill = annotation_key),
#                      colour = "white") +
#             scale_fill_viridis_d() +
#             xlab(annotation_prettykey) +
#             ylab("Num. Files") +
#             theme_minimal() +
#             theme(axis.text.x = element_blank(),
#                   axis.ticks.x = element_blank()) +
#             guides(fill = FALSE)
#         ggplotly(p, tooltip = c("y", "fill"),
#                  width = 100 * length(categories) + 50,
#                  height = 300)
#     }) %>%
#     subplot(shareY = TRUE, titleX = TRUE) %>%
#     layout(showlegend = FALSE,
#            font = list(family = "Roboto, Open Sans, sans-serif"))
# # chart
# syn_entity <- save_chart(parent_id, chart_filename, chart)
#
#
# # Individuals by assay + category -----------------------------------------
#
# chart_filename <- glue::glue("{source_id}_PatientsByAssayAndCategories.html",
#                              source_id = consortium_id)
#
# categories <- list(diagnosis = "Diagnosis", species = "Species",
#                    organ = "Organ", tissue = "Tissue",
#                    dataType = "Data Type", study = "Study")
# p <- categories %>%
#     map2(.y = names(.), function(annotation_prettykey, annotation_key) {
#         fileview_df %>%
#             group_by(.dots = c("assay", annotation_key)) %>%
#             summarize(num_individuals = n_distinct(individualID)) %>%
#             ungroup() %>%
#             rename_(.dots = list(temp = annotation_key)) %>%
#             complete(temp = c(unique(temp), "NA"), assay) %>%
#             replace_na(list(num_individuals = 0)) %>%
#             rename_(.dots = setNames("temp", annotation_key)) %>%
#             gather(category, value, -assay, -num_individuals)
#     }) %>%
#     bind_rows() %>%
#     mutate(value = fct_relevel(value, "NA", after = Inf)) %>%
#     ggplot(aes(x = value, y = assay)) +
#     geom_tile(aes(fill = num_individuals)) +
#     scale_fill_viridis_c("Num. Individuals") +
#     scale_x_discrete(expand = c(0, 0)) +
#     scale_y_discrete(expand = c(0, 0)) +
#     xlab("") +
#     ylab("") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1),
#           legend.position = "bottom",
#           strip.text.x = element_text(face = "bold"),
#           strip.background = element_blank(),
#           panel.spacing.x = unit(3, "lines")) +
#     facet_grid(. ~ category, space = "free", scales = "free", switch = "y")
#
# chart <- ggplotly(p, tooltip = c("x", "y", "fill"),
#                   width = 100 * length(categories) + 200,
#                   height = 50 * n_distinct(fileview_df$assay) + 200) %>%
#     layout(font = list(family = "Roboto, Open Sans, sans-serif"),
#            margin = list(b = 150)) %>%
#     style(xgap = 5, ygap = 5)
#
# # chart
# syn_entity <- save_chart(parent_id, chart_filename, chart)
