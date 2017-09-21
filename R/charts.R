library(tidyverse)
library(ggplot2)
library(viridis)
library(plotly)
library(forcats)



# Core summary charts (data files) ----------------------------------------

#' Plot the breakdown of files from a file view according to distinct values
#' within each specified annotation key (i.e., category).
#'
#' @param view_df
#' @param annotation_keys
#'
#' @return
#' @export
#'
#' @examples
#' plot_keys <- list(assay = "Assay", tumorType = "Tumor Type",
#'                   diagnosis = "Diagnosis", species = "Species",
#'                   organ = "Organ", tissue = "Tissue",
#'                   dataType = "Data Type", study = "Study")
#' plot_file_counts_by_annotationkey(fileview_df, plot_keys)
plot_file_counts_by_annotationkey <- function(view_df, annotation_keys) {
    chart <- annotation_keys %>%
        map2(.y = names(.), function(annotation_prettykey, annotation_key) {
            p <- view_df %>%
                group_by(.dots = annotation_key) %>%
                tally() %>%
                ggplot(aes(x = 1, y = n)) +
                geom_col(aes_string(fill = annotation_key),
                         colour = "white", size = 0.2) +
                scale_fill_viridis_d() +
                xlab(annotation_prettykey) +
                ylab("Num. Files") +
                theme_minimal() +
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank()) +
                guides(fill = FALSE)
            ggplotly(p, tooltip = c("y", "fill"),
                     width = 100 * length(annotation_keys) + 50,
                     height = 300)
        }) %>%
        subplot(shareY = TRUE, titleX = TRUE) %>%
        layout(showlegend = FALSE,
               font = list(family = "Roboto, Open Sans, sans-serif"))
    chart
}


#' Plot the breakdown of samples in a file view based on distinct values
#' within two specified annotation keys.
#'
#' @param view_df
#' @param sample_key string indicating sample type
#' @param annotation_keys
#' @param filter_missing remove records with missing annotation values
#'
#' @return
#' @export
#'
#' @examples
#' plot_keys <- list(assay = "Assay", tumorType = "Tumor Type")
#' plot_sample_counts_by_annotationkey_2d(fileview_df, "cellLine", plot_keys)
plot_sample_counts_by_annotationkey_2d <- function(
    view_df, sample_key = c("individualID", "specimenID", "cellLine"),
    annotation_keys, filter_missing = TRUE
) {
    # TODO: add some check to make sure length(annotation_keys) == 2
    if (filter_missing) {
        view_df <- view_df %>%
            filter_at(vars(one_of(c(names(annotation_keys), sample_key))),
                      all_vars(!is.na(.) & !(. %in% c("null", "Not Applicable"))))
    }
    sample_labels <- list(individualID = "Individuals",
                          specimenID = "Specimens",
                          cellLine = "Cell Lines")
    p <- view_df %>%
        dplyr::group_by(.dots = names(annotation_keys)) %>%
        dplyr::summarize(n = !!! n_distinct(sample_key)) %>%
        ggplot2::ggplot(aes_string(x = names(annotation_keys)[2], y = "n")) +
        ggplot2::geom_col(aes_string(fill = names(annotation_keys[1]))) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_viridis_d(annotation_keys[[1]]) +
        ggplot2::xlab("") +
        ggplot2::ylab(glue::glue("Number of {label}",
                                 label = sample_labels[[sample_key]]))

    plotly::ggplotly(p, height = 500) %>%
        plotly::layout(margin = list(l = 150, r = 100, b = 55))
}

plot_assay_counts_by_center <- function(merged_df) {
    p <- merged_df %>%
        group_by(Center,assay) %>%
        tally() %>%
        ggplot(aes(x = assay, y = n)) +
        geom_col(aes(fill = Center)) + coord_flip() +
        scale_fill_viridis_d() +
        scale_y_log10() +
        xlab("") +
        ylab("")

    ggplotly(p, height = 500) %>%
        layout(margin = list(l = 150, r = 100, b = 55))
}

# old functions -----------------------------------------------------------

plot_assay_stats_by_tumortype <- function(assay_stats) {
    #adding in a text line for better
    assay_stats$text = paste('Assay:',assay_stats$assay,
        '\nTumor Type:',assay_stats$tumorType,
        '\nFiles:',assay_stats$Files)

    p <- ggplot(assay_stats,aes(text=text)) +
        geom_bar(aes(x = assay, fill = tumorType, y = Files),
                 stat = 'identity',
                 position = 'dodge') +
        ggtitle('Files by assay,tumor type') +
        scale_fill_viridis(discrete = TRUE) + xlab("") +
        scale_y_log10() +
        coord_flip() +
        ylab("") +
        theme(
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 315, hjust = 1)
        )

    ggplotly(p,tooltip='text',width = 1000) %>%
        layout(margin = list(l = 100, r = 100, b = 55))
}

plot_assay_stats_by_disease <- function(assay_stats) {
    assay_stats$text = paste('Assay:',assay_stats$assay,
        '\nDiagnosis:',assay_stats$diagnosis,
        '\nFiles:',assay_stats$Files)

    p = ggplot(assay_stats,aes(text=text)) +
        geom_bar(aes(x = assay, fill = diagnosis, y = Files),
                 stat = 'identity',
                 position = 'dodge') +
        ggtitle('Files by assay,diagnosis') +
        scale_fill_viridis(discrete = TRUE) +
        xlab("") +
        scale_y_log10() +
        ylab("") +
        coord_flip() +
        theme(
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 315, hjust = 1)
        )

    ggplotly(p, tooltip='text',width = 1000) %>%
        layout(margin = list(l = 100, r = 100, b = 55))
}

# adapted from lines 48-54 in 'toolTypeReporting.Rmd'
plot_project_toollanguage_counts_by_center <- function(project_toollanguage_counts) {

    project_toollanguage_counts$text=paste('Center:',project_toollanguage_counts$Label,'\nLanguage',project_toollanguage_counts$softwareLanguage,'\nFiles:',project_toollanguage_counts$Files)

    p <- ggplot(project_toollanguage_counts,aes(text='text')) +
        geom_bar(aes(x = Label, fill = softwareLanguage, y = Files),
                 stat = 'identity',
                 position = 'dodge') +
        ggtitle('Files/links by Center') +
        scale_fill_viridis(discrete = TRUE) +
        xlab("") +
        coord_flip() +
        ylab("") +
        theme(
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 315, hjust = 1)
        )

    ggplotly(p, tooltip='text',width = 1000) %>%
        layout(margin = list(l = 100, r = 100, b = 55))
}

# adapted from lines 28-44 in 'pan_stanford_viz.Rmd'
plot_assay_counts_by_patient <- function(patient_datafile_counts_by_assay) {
    p <- patient_datafile_counts_by_assay %>%
        gather(assay, Files, -individualID) %>%
        mutate(individualID = fct_rev(individualID)) %>%
        ggplot(aes(individualID)) +
        geom_bar(aes(fill = assay), alpha = 0.8, colour = "white",
                 position = position_stack(reverse = TRUE)) +
        scale_fill_viridis(discrete = TRUE) +
        coord_flip() +
        xlab("") +
        ylab("Number of Assays") +
        theme(plot.title = element_text(face = "bold"),
              legend.title = element_blank())

    ggplotly(p, width = 800) %>%
        layout(margin = list(l = 100, r = 250))
}

# adapted from lines 49-66 in 'pan_stanford_viz.Rmd'
plot_patient_counts_by_assay <- function(assay_datafile_counts_by_patient) {
    p <- assay_datafile_counts_by_patient %>%
        gather(individualID, Files, -assay) %>%
        ggplot(aes(assay)) +
        geom_bar(aes(fill = individualID), alpha = 0.8, colour = "white",
                 position = position_stack(reverse = TRUE)) +
        scale_fill_viridis(discrete = TRUE) +
        coord_flip() +
        xlab("") +
        ylab("Number of Patients") +
        theme(plot.title = element_text(face = "bold"),
              legend.title = element_blank())

    ggplotly(p, width = 800) %>%
        layout(margin = list(l = 150, r = 250))
}


# adapted from lines 59-70 of 'fileViewReporting.Rmd'
plot_project_file_counts_by_center <- function(project_stats) {

    project_stats <- mutate(project_stats,
                            Label = paste(Institution, Program, sep = '\n'))
    project_stats$text <- paste('Institution:',
                                project_stats$Institution,
                                '\nFiles:',
                                project_stats$Files)

    p <- ggplot(project_stats, aes(text = text)) +
        geom_bar(aes(x = Label, y = Files, fill = Center), stat = 'identity') +
        scale_y_log10() +
        ggtitle('Files uploaded by Center') +
        scale_fill_viridis(discrete = TRUE) +
        xlab("") +
        ylab("") +
        coord_flip() +
        theme(
            plot.title = element_text(face = "bold"),
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 315, hjust = 1)
        )

    ggplotly(p, tooltip = 'text', width = 1400) %>%
        layout(margin = list(l = 280, r = 85, b = 55))
}
