
plot_assay_stats_by_tumortype <- function(assay_stats) {
    #adding in a text line for better
    assay_stats$text = paste('Assay:',assay_stats$Assays,
        '\nTumor Type:',assay_stats$tumorType,
        '\nFiles:',assay_stats$Files)

    p <- ggplot(assay_stats,aes(text=text)) +
        geom_bar(aes(x = Assays, fill = tumorType, y = Files),
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
    assay_stats$text = paste('Assay:',assay_stats$Assays,
        '\nDiagnosis:',assay_stats$diagnosis,
        '\nFiles:',assay_stats$Files)

    p = ggplot(assay_stats,aes(text='text')) +
        geom_bar(aes(x = Assays, fill = diagnosis, y = Files),
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
