library(tidyverse)
library(ggplot2)
library(viridis)
library(plotly)
library(forcats)



# Core summary charts (data files) ----------------------------------------

plot_assay_counts_by_tumortype <- function(view_df) {
    p <- fileview_df %>%
        group_by(assay, tumorType) %>%
        tally() %>%
        ggplot(aes(x = tumorType, y = n)) +
        geom_col(aes(fill = assay)) + coord_flip() +
        scale_fill_viridis_d() +
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
