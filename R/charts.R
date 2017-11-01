library(tidyverse)
library(ggplot2)
library(viridis)
library(plotly)
library(forcats)


custom_theme_bw <- function() {
    theme_bw() +
        theme(axis.title = element_text(face = "bold"),
              legend.title = element_text(face = "bold"),
              plot.title = element_text(face = "bold"))
}

# Core summary charts (data files) ----------------------------------------

#' Plot the breakdown of files from a file view according to distinct values
#' within each specified annotation key (i.e., category).
#'
#' @param view_df
#' @param annotation_keys
#' @param replace_missing String to use for missing annotation values (defaults to "Not Annotated").
#' @param chart_height Height of the chart in pixels (optional, defaults to automatic sizing).
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
plot_file_counts_by_annotationkey <- function(
    view_df, annotation_keys, replace_missing = "Not Annotated",
    chart_height = NULL
) {

    chart <- annotation_keys %>%
        map2(.y = names(.), function(annotation_prettykey, annotation_key) {
            key_col <- as.name(annotation_key)
            plot_df <- view_df %>%
                dplyr::group_by(.dots = annotation_key) %>%
                dplyr::tally() %>%
                dplyr::mutate_at(.vars = annotation_key,
                                 funs(replace(., is.na(.), replace_missing))) %>%
                dplyr::mutate(UQ(key_col) := fct_relevel(
                    UQ(key_col), replace_missing, after = 0L
                )) %>%
                dplyr::mutate(label = glue::glue(
                    "<b>{value}:</b>\n{count} files",
                    value = rlang::UQ(key_col),
                    count = n
                ))

            p <- plot_df %>%
                ggplot2::ggplot(aes(x = 1, y = n, text = label)) +
                ggplot2::geom_col(aes_(fill = as.name(annotation_key)),
                                  position = position_stack(reverse = FALSE),
                                  colour = "white", size = 0.2) +
                ggplot2::scale_fill_viridis_d() +
                ggplot2::xlab(annotation_prettykey) +
                ggplot2::ylab("Number of Files") +
                ggplot2::scale_x_continuous(expand = c(0, 0)) +
                ggplot2::scale_y_continuous(expand = c(0, 0)) +
                custom_theme_bw() +
                ggplot2::theme(axis.text.x = element_blank(),
                               axis.ticks.x = element_blank()) +
                ggplot2::guides(fill = FALSE)

            ggplotly(p, tooltip = "text",
                     width = 100 * length(annotation_keys) + 50,
                     height = chart_height)
        }) %>%
        plotly::subplot(shareY = TRUE, titleX = TRUE) %>%
        plotly::layout(showlegend = FALSE,
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

    fill_vals <- unique(view_df[[names(annotation_keys)[1]]])
    bar_vals <- unique(view_df[[names(annotation_keys)[2]]])
    num_bars <- length(bar_vals)

    fill_margin <- max(purrr::map_int(fill_vals, stringr::str_length))
    bar_margin <- max(purrr::map_int(bar_vals, stringr::str_length))

    sample_labels <- list(individualID = "Individuals",
                          specimenID = "Specimens",
                          cellLine = "Cell Lines",
                          id = "Files")
    plot_df <- view_df %>%
        dplyr::group_by(.dots = names(annotation_keys)) %>%
        dplyr::summarize(n = n_distinct(rlang::UQ(as.name(sample_key)))) %>%
        ungroup() %>%
        dplyr::mutate(label = glue::glue(
            "<b>{assay}:</b>\n{count} {samples}",
            assay = rlang::UQ(as.name(names(annotation_keys)[1])),
            count = n,
            samples = stringr::str_to_lower(sample_labels[[sample_key]]))
        )

    p <- plot_df %>%
        ggplot2::ggplot(aes_string(x = names(annotation_keys)[2], y = "n",
                                   text = "label")) +
        ggplot2::geom_col(aes_string(fill = names(annotation_keys[1])),
                          colour = "white", size = 0.2) +
        ggplot2::scale_fill_viridis_d(annotation_keys[[1]]) +
        ggplot2::xlab("") +
        ggplot2::ylab(glue::glue("Number of {label}",
                                 label = sample_labels[[sample_key]])) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::coord_flip() +
        custom_theme_bw()

    plotly::ggplotly(p, tooltip = 'text', height = num_bars * 50 + 155) %>%
        plotly::layout(margin = list(l = 10 + bar_margin * 6,
                                     r = 10 + fill_margin * 6,
                                     b = 55),
                       font = list(family = "Roboto, Open Sans, sans-serif"),
                       legend = list(tracegroupgap = 10, yanchor = "top"))
}


plot_file_counts_by_annotationkey_2d <- function(
    view_df, annotation_keys, synproject_key = NULL,
    filter_missing = TRUE, log_counts = FALSE
) {
    # TODO: add some check to make sure length(annotation_keys) == 2
    if (filter_missing) {
        view_df <- view_df %>%
            filter_at(vars(one_of(c(names(annotation_keys), synproject_key))),
                      all_vars(!is.na(.) & !(. %in% c("null", "Not Applicable"))))
    }

    if ("projectId" %in% names(annotation_keys) & !is.null(synproject_key)) {
        annotation_keys <- annotation_keys %>%
            plyr::rename(list("projectId" = synproject_key))
    }

    fill_vals <- unique(view_df[[names(annotation_keys)[1]]])
    bar_vals <- unique(view_df[[names(annotation_keys)[2]]])
    print(bar_vals)
    num_bars <- length(bar_vals)

    fill_margin <- max(purrr::map_int(fill_vals, stringr::str_length))
    bar_margin <- max(purrr::map_int(bar_vals, stringr::str_length))

    print(bar_margin)
    group_cols <- sapply(names(annotation_keys), as.name)

    replace_missing <- "Not Annotated"
    plot_df <- view_df %>%
        dplyr::group_by(rlang::UQS(group_cols)) %>%
        dplyr::summarize(n = n_distinct(id)) %>%
        ungroup() %>%
        dplyr::mutate_at(.vars = names(annotation_keys),
                         funs(replace(., is.na(.), replace_missing))) %>%
        dplyr::mutate(label = glue::glue(
            "<b>{assay}:</b>\n{count} files",
            assay = rlang::UQ(as.name(names(annotation_keys)[1])),
            count = n)
        ) %>%
        I

    scale_note <- ""
    if (log_counts) {
        plot_df <- plot_df %>%
            mutate(n = ifelse(n > 0, log10(n), n))
        scale_note <- " (log10)"
    }

    p <- plot_df %>%
        ggplot2::ggplot(aes_(x = rlang::UQ(group_cols[[2]]), y = as.name("n"),
                                   text = as.name("label"))) +
        ggplot2::geom_col(aes_(fill = rlang::UQ(group_cols[[1]])),
                          colour = "white", size = 0.2) +
        ggplot2::scale_fill_viridis_d(annotation_keys[[1]]) +
        ggplot2::xlab("") +
        ggplot2::ylab(glue::glue("Number of Files{scale}", scale = scale_note)) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::coord_flip() +
        custom_theme_bw()

    plotly::ggplotly(p, tooltip = 'text', height = num_bars * 50 + 155) %>%
        plotly::layout(margin = list(l = 10 + bar_margin * 6,
                                     r = 10 + fill_margin * 6,
                                     b = 55),
                       font = list(family = "Roboto, Open Sans, sans-serif"),
                       legend = list(tracegroupgap = 10, yanchor = "top"))
}

get_annotation_summary <-function(merged_df){
    replace_missing <- "Not Annotated"

    p <- merged_df %>%
        dplyr::mutate_at(.vars = c('assay'),
            funs(replace(., is.na(.), replace_missing))) %>%
        group_by(assay,Center) %>%
        tally() %>%
        ggplot(aes(x = Center, y = n)) +
        geom_col(aes(fill = assay)) + coord_flip() +
        scale_fill_viridis_d() +
        #   scale_y_log10() +
        xlab("") +
        ylab("")

    ggplotly(p, height = 500) %>%
        layout(margin = list(l = 350, r = 100, b = 55))
}

plot_assay_counts_by_center <- function(merged_df) {
    p <- merged_df %>%
        group_by(Center,assay) %>%
        tally() %>%
        ggplot(aes(x = assay, y = n)) +
        geom_col(aes(fill = Center)) + coord_flip() +
        scale_fill_viridis_d() +
     #   scale_y_log10() +
        xlab("") +
        ylab("")

    ggplotly(p, height = 500) %>%
        layout(margin = list(l = 150, r = 100, b = 55))
}

plot_tool_inputs <- function(merged_df){
    p<- merged_df %>%
        group_by(Center,inputDataType) %>%
        tally () %>%
        ggplot(aes(x=inputDataType,y=n)) +
        geom_col(aes(fill=Center)) + coord_flip() +
        scale_fill_viridis_d() +
        xlab("") +
        ylab("")

    ggplotly(p,height=300) %>%
        layout(margin=list(l = 150, r=100, b=55))

}

plot_tool_outputs <- function(merged_df){
    p<- merged_df %>%
        group_by(Center,outputDataType) %>%
        tally () %>%
        ggplot(aes(x=outputDataType,y=n)) +
        geom_col(aes(fill=Center)) + coord_flip() +
        scale_fill_viridis_d() +
        xlab("") +
        ylab("")

    ggplotly(p,height=300) %>%
        layout(margin=list(l = 150, r=100, b=55))

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
plot_project_toollanguage_counts_by_center <- function(
    project_toollanguage_counts
) {
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
     #   scale_y_log10() +
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
