
# Project summary tables --------------------------------------------------

# adapted from lines 29-33 in 'fileViewReporting.Rmd'
summarize_project_info <- function(view_df) {
    proj_info <-
        sapply(unique(c(view_df$projectId)),
               function(x) {
                   res = synGet(x)
                   c(
                       projectId = x,
                       Center = res@properties$name,
                       Program = res@annotations$consortium,
                       Institution = res@annotations$institution
                   )
               })

    proj_info = data.frame(t(proj_info))
    proj_info
}

# adapted from lines 40-44 in 'fileViewReporting.Rmd'
summarize_project_datafile_counts <- function(view_df, project_info) {
    project_datafile_counts <- view_df %>%
        group_by(projectId) %>%
        summarize(
            Files = n_distinct(id),
            Assays = n_distinct(assay),
            TumorTypes = n_distinct(tumorType),
            Diseases = n_distinct(diagnosis),
            Specimens = n_distinct(specimenID),
            Patients = n_distinct(individualID)
        ) %>%
        inner_join(project_info, ., by = "projectId")
    project_datafile_counts
}

# adapted from lines 49-53 in 'fileViewReporting.Rmd'
summarize_project_toolfile_counts <- function(view_df, project_info) {
    project_toolfile_counts <- view_df %>%
        group_by(projectId) %>%
        summarize(
            Files = n_distinct(id),
            ToolTypes = n_distinct(softwareType),
            ToolLanguages = n_distinct(softwareLanguage)
        ) %>%
        inner_join(project_info, ., by = "projectId") %>%
        mutate(Label = paste(Institution, Program, sep = '\n'))
    project_toolfile_counts
}

# adapted from lines 37-43 in 'toolTypeReporting.Rmd'
summarize_project_toollanguage_counts <- function(view_df, project_info) {
    project_toollanguage_counts = view_df %>%
        group_by(softwareLanguage, projectId) %>%
        summarize(Files = n_distinct(id)) %>%
        inner_join(project_info, ., by = "projectId") %>%
        mutate(Label = paste(Institution, Program, sep = '\n'))

    arrange(project_toollanguage_counts, desc(Files))
}

# Data type tables --------------------------------------------------------

# adapted from lines 36-38 in 'dataTypeReporting.Rmd'
summarize_assay_counts <- function(fileview_df) {
    assay_counts = fileview_df %>%
        group_by(assay) %>%
        summarize(
            Centers = n_distinct(projectId),
            Files = n_distinct(id),
            tumorType = n_distinct(tumorType),
            disease = n_distinct(diagnosis),
            specimens = n_distinct(specimenID),
            patients = n_distinct(individualID)
        )
    arrange(assay_counts, desc(Centers))
}

# adapted from lines 42-44 in 'dataTypeReporting.Rmd'
summarize_assay_stats <- function(fileview_df) {
    assay_stats = fileview_df %>%
        group_by(assay, tumorType, diagnosis) %>%
        summarize(
            Centers = n_distinct(projectId),
            Files = n_distinct(id),
            specimens = n_distinct(specimenID),
            patients = n_distinct(individualID)
        )

    arrange(assay_stats, desc(Centers))
}

# adapted from lines 70-72 in 'dataTypeReporting.Rmd'
summarize_disease_counts <- function(fileview_df) {
    disease_counts = fileview_df %>%
        group_by(diagnosis) %>%
        summarize(
            Centers = n_distinct(projectId),
            Files = n_distinct(id),
            tumorType = n_distinct(tumorType),
            assay = n_distinct(assay),
            specimens = n_distinct(specimenID),
            patients = n_distinct(individualID)
        )

    arrange(disease_counts, desc(Centers))
}

# adapted from lines 77-79 in 'dataTypeReporting.Rmd'
summarize_tumortype_counts <- function(fileview_df) {
    tt_counts = fileview_df %>%
        group_by(tumorType) %>%
        summarize(
            Centers = n_distinct(projectId),
            Files = n_distinct(id),
            disease = n_distinct(diagnosis),
            assay = n_distinct(assay),
            specimens = n_distinct(specimenID),
            patients = n_distinct(individualID)
        )

    arrange(tt_counts, desc(Centers))
}

# Patient summary tables --------------------------------------------------

summarize_assay_counts_by_patient <- function(fileview_df) {
    fileview_df %>%
        group_by(individualID, assay) %>%
        tally() %>%
        ungroup() %>%
        right_join(., expand(., individualID, assay),
                   by = c("individualID", "assay")) %>%
        replace_na(list(n = 0)) %>%
        # need to be careful with column types
        mutate(n = as.integer(n)) %>%
        spread(individualID, n)
}

summarize_patient_counts_by_assay <- function(fileview_df) {
    fileview_df %>%
        group_by(individualID, assay) %>%
        tally() %>%
        ungroup() %>%
        right_join(., expand(., individualID, assay),
                   by = c("individualID", "assay")) %>%
        replace_na(list(n = 0)) %>%
        # need to be careful with column types
        mutate(n = as.integer(n)) %>%
        spread(assay, n)
}
