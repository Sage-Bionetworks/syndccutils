source("R/charts.R")
source("R/tables.R")
source("R/synapse_helpers.R")

# Script/template to create summary tables and charts for a "study"

# Config ------------------------------------------------------------------

synproject_id <- "syn7315808" # Synapse project for project Center
study_id <- "syn10491911" # Synapse folder associated with study
parent_id <- "syn10831920" # Center 'Reporting' folder where files should be stored
master_fileview_id <- "syn10531467" # Synapse fileview associated with study


# Collect data ------------------------------------------------------------

fileview_df <- get_table_df(master_fileview_id)


# Files by assay -------------------------------------------------------

table_filename <- glue::glue("{source_id}_DataFileCountsByAssay.html",
                             source_id = study_id)

# create and save table
datafile_counts_by_assay <- fileview_df %>%
    summarize_datafiles_by_assay(master_fileview_id)

datafile_counts_by_assay_dt <- datafile_counts_by_assay %>%
    format_summarytable_columns("assay") %>%
    as_datatable()

syn_dt_entity <- datafile_counts_by_assay_dt %>%
    save_datatable(parent_id, table_filename, .)

# Assays by tumor type ----------------------------------------------------

chart_filename <- glue::glue("{source_id}_AssayDataFilesByTumorType.html",
                             source_id = study_id)

# create and save chart
chart <- fileview_df %>%
    plot_assay_counts_by_tumortype()

syn_chart_entity <- save_chart(parent_id, chart_filename, chart)


# Files by category -------------------------------------------------------

chart <- list(assay = "Assay", tumorType = "Tumor Type") %>%
    map2(.y = names(.), function(annotation_prettykey, annotation_key) {
        p <- fileview_df %>%
            group_by(.dots = annotation_key) %>%
            tally() %>%
            ggplot(aes(x = 1, y = n)) +
            geom_col(aes_string(fill = annotation_key),
                     colour = "white") +
            scale_fill_viridis_d() +
            xlab(annotation_prettykey) +
            ylab("Num. Files") +
            theme_minimal() +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()) +
            guides(fill = FALSE)
        ggplotly(p, tooltip = c("y", "fill"),
                 width = 100 * length(categories) + 50,
                 height = 300)
    }) %>%
    subplot(shareY = TRUE, titleX = TRUE) %>%
    layout(showlegend = FALSE,
           font = list(family = "Roboto, Open Sans, sans-serif"))
chart


# Individuals by assay + category -----------------------------------------

categories <- list(tumorType = "Tumor Type")
p <- categories %>%
    map2(.y = names(.), function(annotation_prettykey, annotation_key) {
        fileview_df %>%
            group_by(.dots = c("assay", annotation_key)) %>%
            summarize(num_individuals = n_distinct(individualID)) %>%
            ungroup() %>%
            rename_(.dots = list(temp = annotation_key)) %>%
            complete(temp = c(unique(temp), "NA"), assay) %>%
            replace_na(list(num_individuals = 0)) %>%
            rename_(.dots = setNames("temp", annotation_key)) %>%
            gather(category, value, -assay, -num_individuals)
    }) %>%
    bind_rows() %>%
    mutate(value = fct_relevel(value, "NA", after = Inf)) %>%
    ggplot(aes(x = value, y = assay)) +
    geom_tile(aes(fill = num_individuals)) +
    scale_fill_viridis_c("Num. Individuals") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text.x = element_text(face = "bold"),
          strip.background = element_blank(),
          panel.spacing.x = unit(3, "lines")) +
    facet_grid(. ~ category, space = "free", scales = "free", switch = "y")

chart <- ggplotly(p, tooltip = c("x", "y", "fill"),
                  width = 100 * length(categories) + 200,
                  height = 50 * n_distinct(fileview_df$assay) + 200) %>%
    layout(font = list(family = "Roboto, Open Sans, sans-serif"),
           margin = list(b = 150)) %>%
    style(xgap = 5, ygap = 5) %>%
    I

chart
