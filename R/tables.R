
# Project summary tables --------------------------------------------------

summarize_project_info <- function(fileview_df, toolview_df) {
    proj_info <-
        sapply(unique(c(fileview_df$projectId, toolview_df$projectId)),
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

summarize_project_file_counts <- function(fileview_df) {
    file_counts <- fileview_df %>%
        group_by(projectId) %>%
        summarize(
            Files = n_distinct(id),
            Assays = n_distinct(assay),
            tumorType = n_distinct(tumorType),
            disease = n_distinct(diagnosis),
            specimens = n_distinct(specimenID),
            patients = n_distinct(individualID)
    )

    files_with_info = inner_join(proj_info, file_counts, by = "projectId")
    files_with_info
}

summarize_project_tool_counts <- function(toolview_df) {
    tool_counts = toolview_df %>%
        group_by(projectId) %>%
        summarize(
            Files = n_distinct(id),
            ToolType = n_distinct(softwareType),
            ToolLanguage = n_distinct(softwareLanguage)
        )

    tools_with_info = inner_join(proj_info, tool_counts, by = "projectId")
    tools_with_info
}

# Data type tables --------------------------------------------------------

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


# Tool type tables --------------------------------------------------------

summarize_language_counts <- function(toolview_df, project_info_df) {
    lang_counts = toolview_df %>%
        group_by(softwareLanguage, projectId) %>%
        summarize(Files = n_distinct(id))

    files_with_info = inner_join(project_info_df, lang_counts,
                                 by = "projectId")

    files_with_info = mutate(files_with_info,
                             Label = paste(Institution, Program, sep = '\n'))

    arrange(files_with_info, desc(Files))
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
