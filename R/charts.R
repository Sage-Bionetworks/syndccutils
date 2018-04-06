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
                group_by(.dots = annotation_key) %>%
                tally() %>%
                mutate_at(.vars = annotation_key,
                                 funs(replace(., is.na(.), replace_missing))) %>%
                mutate(UQ(key_col) := fct_relevel(
                    UQ(key_col), replace_missing, after = 0L
                )) %>%
                mutate(label = glue::glue(
                    "<b>{value}:</b>\n{count} files",
                    value = rlang::UQ(key_col),
                    count = n
                ))

            p <- plot_df %>%
                ggplot(aes(x = 1, y = n, text = label)) +
                geom_col(aes_(fill = as.name(annotation_key)),
                                  position = position_stack(reverse = FALSE),
                                  colour = "white", size = 0.2) +
                scale_fill_viridis_d() +
                xlab(annotation_prettykey) +
                ylab("Number of Files") +
                scale_x_continuous(expand = c(0, 0)) +
                scale_y_continuous(expand = c(0, 0)) +
                custom_theme_bw() +
                theme(axis.text.x = element_blank(),
                               axis.ticks.x = element_blank()) +
                guides(fill = FALSE)

            plotly::ggplotly(p, tooltip = "text",
                     width = 100 * length(annotation_keys) + 50,
                     height = chart_height)
        }) %>%
        plotly::subplot(shareY = TRUE, titleX = TRUE) %>%
        plotly::layout(showlegend = FALSE,
                       font = list(family = "Roboto, Open Sans, sans-serif")) %>%
        plotly::config(displayModeBar = F)
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

    fill_margin <- max(map_int(fill_vals, stringr::str_length))
    bar_margin <- max(map_int(bar_vals, stringr::str_length))

    sample_labels <- list(individualID = "Individuals",
                          specimenID = "Specimens",
                          cellLine = "Cell Lines",
                          id = "Files")

    replace_missing <- "Not Annotated"
    plot_df <- view_df %>%
        group_by(.dots = names(annotation_keys)) %>%
        summarize(n = n_distinct(rlang::UQ(as.name(sample_key)))) %>%
        ungroup() %>%
        mutate_at(.vars = names(annotation_keys),
                         funs(replace(., is.na(.), replace_missing))) %>%
        mutate_at(.vars = names(annotation_keys),
                         funs(forcats::fct_infreq(.))) %>%
        mutate_at(.vars = names(annotation_keys),
                         funs(forcats::fct_rev(.))) %>%
        # mutate_at(.vars = names(annotation_keys),
        #                  funs(forcats::fct_relevel(., "Not Annotated"))) %>%
        mutate(label = glue::glue(
            "<b>{assay}:</b>\n{count} {samples}",
            assay = rlang::UQ(as.name(names(annotation_keys)[1])),
            count = n,
            samples = stringr::str_to_lower(sample_labels[[sample_key]]))
        )

    p <- plot_df %>%
        ggplot(aes_string(x = names(annotation_keys)[2], y = "n",
                                   text = "label")) +
        geom_col(aes_string(fill = names(annotation_keys[1])),
                          colour = "white", size = 0.2) +
        scale_fill_viridis_d(annotation_keys[[1]]) +
        xlab("") +
        ylab(glue::glue("Number of {label}",
                                 label = sample_labels[[sample_key]])) +
        scale_y_continuous(expand = c(0, 0)) +
        coord_flip() +
        custom_theme_bw()

    plotly::ggplotly(p, tooltip = 'text', height = num_bars * 40 + 155) %>%
        plotly::layout(margin = list(l = 10 + bar_margin * 6,
                                     r = 10 + fill_margin * 0,
                                     b = 55),
                       font = list(family = "Roboto, Open Sans, sans-serif"),
                       legend = list(tracegroupgap = 3, traceorder = "reversed",
                                     yanchor = "top", y = 1)) %>%
        plotly::config(displayModeBar = F)
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
    num_bars <- length(bar_vals)

    fill_margin <- max(map_int(fill_vals, stringr::str_length))
    bar_margin <- max(map_int(bar_vals, stringr::str_length), na.rm = TRUE)

    group_cols <- sapply(names(annotation_keys), as.name)

    replace_missing <- "Not Annotated"
    plot_df <- view_df %>%
        group_by(rlang::UQS(group_cols)) %>%
        summarize(n = n_distinct(id)) %>%
        ungroup() %>%
        mutate_at(.vars = names(annotation_keys),
                         funs(replace(., is.na(.), replace_missing))) %>%
        mutate_at(.vars = names(annotation_keys),
                           funs(forcats::fct_infreq(.))) %>%
        mutate_at(.vars = names(annotation_keys),
                         funs(forcats::fct_rev(.))) %>%
        mutate_at(.vars = names(annotation_keys),
                         funs(forcats::fct_relevel(., "Not Annotated"))) %>%
        mutate(label = glue::glue(
            "<b>{fill_val}:</b>\n{count} files",
            fill_val = rlang::UQ(as.name(names(annotation_keys)[1])),
            count = n)
        )

    scale_note <- ""
    if (log_counts) {
        plot_df <- plot_df %>%
            mutate(n = ifelse(n > 0, log10(n), n))
        scale_note <- " (log10)"
    }

    p <- plot_df %>%
        ggplot(aes_(x = group_cols[[2]], y = as.name("n"),
                                   text = as.name("label"))) +
        geom_col(aes_(fill = group_cols[[1]]),
                          colour = "white", size = 0.2) +
        scale_fill_viridis_d(annotation_keys[[1]]) +
        guides(fill = guide_legend(reverse = T)) +
        xlab("") +
        ylab(glue::glue("Number of Files{scale}", scale = scale_note)) +
        scale_y_continuous(expand = c(0, 0)) +
        coord_flip() +
        custom_theme_bw()

    plotly::ggplotly(p, tooltip = 'text', height = num_bars * 40 + 155) %>%
        plotly::layout(margin = list(l = 10 + bar_margin * 6,
                                     r = 10 + fill_margin * 0,
                                     b = 55),
                       font = list(family = "Roboto, Open Sans, sans-serif"),
                       legend = list(tracegroupgap = 3, traceorder = "reversed",
                                     xanchor = "left", yanchor = "bottom",
                                     x = 0, y = 100)) %>%
        plotly::config(displayModeBar = F)
}



plot_study_counts_by_annotationkey_2d <- function(
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
    num_bars <- length(bar_vals)

    fill_margin <- max(map_int(fill_vals, stringr::str_length))
    bar_margin <- max(map_int(bar_vals, stringr::str_length))

    group_cols <- sapply(names(annotation_keys), as.name)

    replace_missing <- "Not Annotated"
    plot_df <- view_df %>%
        group_by(rlang::UQS(group_cols)) %>%
        summarize(n = n_distinct(study)) %>%
        ungroup() %>%
        mutate_at(.vars = names(annotation_keys),
            funs(replace(., is.na(.), replace_missing))) %>%
        mutate(label = glue::glue(
            "<b>{assay}:</b>\n{count} studies",
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
        ggplot(aes_(x = rlang::UQ(group_cols[[2]]), y = as.name("n"),
            text = as.name("label"))) +
        geom_col(aes_(fill = rlang::UQ(group_cols[[1]])),
            colour = "white", size = 0.2) +
        scale_fill_viridis_d(annotation_keys[[1]]) +
        xlab("") +
        ylab(glue::glue("Number of Studies{scale}", scale = scale_note)) +
        scale_y_continuous(expand = c(0, 0)) +
        coord_flip() +
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
        mutate_at(.vars = c('assay'),
            funs(replace(., is.na(.), replace_missing))) %>%
        group_by(assay,Center) %>%
        tally() %>%
        ggplot(aes(x = Center, y = n)) +
        geom_col(aes(fill = assay)) + coord_flip() +
        scale_fill_viridis_d() +
        #   scale_y_log10() +
        xlab("") +
        ylab("")

    plotly::ggplotly(p, height = 500) %>%
        plotly::layout(margin = list(l = 350, r = 100, b = 55))
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

    plotly::ggplotly(p, height = 500) %>%
        plotly::layout(margin = list(l = 150, r = 100, b = 55)) %>%
        plotly::config(displayModeBar = F)
}

plot_tool_inputs <- function(merged_df){
    p<- merged_df %>%
        group_by(Center,inputDataType) %>%
        tally() %>%
        ggplot(aes(x=inputDataType,y=n)) +
        geom_col(aes(fill=Center)) + coord_flip() +
        scale_fill_viridis_d() +
        xlab("") +
        ylab("")

    plotly::ggplotly(p,height=300) %>%
        plotly::layout(margin=list(l = 150, r=100, b=55)) %>%
        plotly::config(displayModeBar = F)

}

plot_tool_outputs <- function(merged_df){
    p<- merged_df %>%
        group_by(Center,outputDataType) %>%
        tally() %>%
        ggplot(aes(x=outputDataType,y=n)) +
        geom_col(aes(fill=Center)) + coord_flip() +
        scale_fill_viridis_d() +
        xlab("") +
        ylab("")

    plotly::ggplotly(p,height=300) %>%
        layout(margin=list(l = 150, r=100, b=55)) %>%
        plotly::config(displayModeBar = F)

}




