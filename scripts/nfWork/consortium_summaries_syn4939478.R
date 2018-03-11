source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")
##NTAP SITE
# Script/template to create summary tables and charts for a "project"

synLogin()


# Config ------------------------------------------------------------------

synproject_id <- "syn4939478" # Synapse project for consortium
consortium_id <- "syn4939478" # Synapse folder associated with consortium
parent_id <- "syn10622387" # consortium 'Reporting' folder where files should be stored
master_fileview_id <- "syn8077013" # Synapse fileview associated with consortium data


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)

hierarchy_id <- "syn11393267"
hierarchy_df <- get_table_df(hierarchy_id)


# Add Synapse project info --------------------------------------------

synproject_df <- fileview_df %>%
    summarize_project_info()


new_fileview_df <- fileview_df %>%
    left_join(synproject_df, by = "projectId") %>%
    left_join(hierarchy_df, by = c("study" = "Study Name")) %>%
    rename(studyId = "Study")

#ok. in the code you have `list(projectId="Project")` — the function doesn’t know that “Center” ~ “Project”, you have to give it an actual column name :)

# Data files by assay and tumor type --------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndProject.html",
    source_id = consortium_id)

nf1_table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndNF1Genotype.html",
                             source_id = consortium_id)

nf2_table_filename <- glue::glue("{source_id}_DataFileCountsByAssayAndNF2Genotype.html",
    source_id = consortium_id)

# create and save table - NF1
group_keys <- c("assay", "nf1Genotype")
count_cols <- c("id", "cellType", "individualID")
list_cols <- c("projectName")
link_keys <-list(projectName="projectId")

datafile_counts <- new_fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
        count_cols = count_cols,
        list_cols = list_cols,
       link_keys = link_keys,
        queryformat='html'
        ) %>%
    rename(Project = projectName)

datafile_counts_dt <- datafile_counts %>%
    format_summarytable_columns(group_keys) %>%
    as_datatable()

syn_dt_entity <- datafile_counts_dt %>%
    save_datatable(parent_id, nf1_table_filename, .)

# view table
datafile_counts_dt


# Data files by assay -----------------------------------------------------

# create and save table
group_keys <- "assay"
count_cols <- c("id", "tumorType", "diagnosis")
list_cols <- c("projectName")
link_keys <-list(projectName="projectId")

datafile_counts <- new_fileview_df %>%
    summarize_files_by_annotationkey(
        annotation_keys = c(group_keys,list_cols),
        table_id = master_fileview_id,
    #    list_cols = list_cols,
    #    link_keys = link_keys,
        count_cols = count_cols,
        queryformat='raw'
    ) %>%
    rename(Project = projectName)

datafile_counts_dt <- datafile_counts %>%
    format_summarytable_columns(c(group_keys,list_cols)) #%>%
    #as_datatable()

syn_dt_entity <- datatable_to_synapse(datafile_counts_dt,synproject_id, 'Assays By Project')
#    save_datatable(parent_id, table_filename, .)
tcolnames<-names(as.data.frame(syn_dt_entity))
wiki_string <- simple_plots_wiki_string(syn_dt_entity$tableId,tcolnames[1:2],tcolnames[3],title='Assays By Project')
# view table

datafile_counts <- new_fileview_df %>%
    summarize_files_by_annotationkey_new(
        annotation_keys = group_keys,
        table_id = master_fileview_id,
            list_cols = list_cols,
            link_keys = link_keys,
        count_cols = count_cols,
        queryformat='html'
    ) %>%
    rename(Project = projectName)

datafile_counts_dt <- datafile_counts %>%
    format_summarytable_columns(c(group_keys,list_cols)) %>% as_datatable()

syn_dt_entity <-datafile_counts_dt %>% save_datatable(parent_id, table_filename, .) #datatable_to_synapse(datafile_counts_dt,synproject_id, 'Assays By Project')

datafile_counts_dt

#Now do plots -------------------------------------------
plot_keys <- list(assay = "Assay", tumorType = "Tumor Type",
    diagnosis = "Diagnosis", species = "Species",
    organ = "Organ", tissue = "Tissue",
    dataType = "Data Type", study = "Study")

chart_filename <- glue::glue("{source_id}_DataFilesByCenterAndAssay.html",
                             source_id = consortium_id)

chart <- new_fileview_df %>%
    plot_file_counts_by_annotationkey(plot_keys)

chart

syn_entity <- save_chart(parent_id, chart_filename, chart)



# create and save chart
chart <- syn_chart_entity <- new_fileview_df %>%
    plot_assay_counts_by_center()

syn_chart_entity <- save_chart(parent_id, chart_filename, chart)

# view chart
chart

chart_filename <- glue::glue("{source_id}_annotationSummary.html",
    source_id = consortium_id)


chart <- syn_chart_entity <- new_fileview_df %>%
    get_annotation_summary()
syn_chart_entity <- save_chart(parent_id, chart_filename, chart)

# view chart
chart


##next plot: analysis and assay types
group_keys <-c("analysisType")#,"fundingAgency")
count_cols <-c('id')
list_cols <-c('study','projectName')
link_keys <-list(`projectName`='projectId',study='studyId')


table_filename <- glue::glue("{source_id}_DataFileCountsByAnalysisType.html",
    source_id = consortium_id)

datafile_counts <- new_fileview_df %>%
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

