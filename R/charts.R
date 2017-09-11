
plot_assay_stats_by_tumortype <- function(assay_stats) {
    p <- ggplot(assay_stats) +
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

    ggplotly(p, width = 1000) %>%
        layout(margin = list(l = 100, r = 100, b = 55))
}

plot_assay_stats_by_disease <- function(assay_stats) {
    p = ggplot(assay_stats) +
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

    ggplotly(p, width = 1000) %>%
        layout(margin = list(l = 100, r = 100, b = 55))
}

# adapted from lines 48-54 in 'toolTypeReporting.Rmd'
plot_project_toollanguage_counts_by_center <- function(
    project_toollanguage_counts
) {
    p <- ggplot(project_toollanguage_counts) +
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

    ggplotly(p, width = 1000) %>%
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

