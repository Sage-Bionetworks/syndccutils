library(tidyverse)
library(feather)
library(plotly)
library(DT)
library(kableExtra)
library(streamgraph)
library(synapser)

source("../R/synapse_helpers.R")
source("../R/tables.R")
source("../R/charts.R")

synLogin()
csbc_summary_df <- get_table_df("syn11968325", cache = TRUE)
csbc_summary_df <- csbc_summary_df %>% 
    mutate_at(.vars = vars(dplyr::matches("(createdOn|modifiedOn)")),
              .funs = funs(lubridate::as_datetime(floor(. / 1000))))

project_vars <- c("projectId", "name_project", "institution", "consortium",
                  "grantNumber", "grantType", "teamMembersProfileId",
                  "teamProfileId", "createdOn_project", "modifiedOn_project",
                  "publication_count", "publication_geodata_produced")

count_vars <- c("fileId", "individualID", "specimenID", "cellLine",
                "assay", "tool")

center_study_summary_df <- csbc_summary_df %>% 
    mutate(tool = ifelse(!is.na(softwareLanguage), study, NA)) %>% 
    group_by(.dots = c(project_vars, "study")) %>% 
    summarise_at(.vars = count_vars,
                 .funs = funs(n_distinct(., na.rm = TRUE))) %>% 
    replace_na(list(study = "Not Annotated")) %>% 
    mutate(study = ifelse(fileId == 0, NA, study))

center_summary_df <- center_study_summary_df %>% 
    group_by(projectId) %>% 
    summarise(study = n_distinct(study, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(
        center_study_summary_df %>% 
            select(-study) %>% 
            group_by(.dots = project_vars) %>% 
            summarise_all(sum),
        by = "projectId"
    )


plot_df <- center_study_summary_df %>% 
    mutate(sample = individualID + specimenID + cellLine) %>% 
    group_by(name_project, consortium, grantType) %>% 
    summarise(avg_files = mean(fileId),
              study = n_distinct(study, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(study) %>% 
    mutate(name_project = fct_inorder(name_project),
           label = glue::glue(
               "<b>{value}:</b>\n{count} studies\n{avg} files per study",
               value = name_project,
               count = study,
               avg = avg_files
           )
    )

p <- plot_df %>% 
    ggplot(aes(x = name_project, y = study, text = label)) +
    geom_col(aes(fill = avg_files)) +
    geom_point(aes(colour = grantType), y = 0.1) +
    coord_flip() +
    scale_colour_colorblind() +
    guides(alpha = FALSE, fill = FALSE, colour = guide_legend(title = NULL)) +
    xlab("") +
    ylab("Studies in Synapse") +
    scale_y_continuous(expand = c(0, 0)) +
    facet_grid(consortium ~ ., 
               scales = "free_y", space = "free_y", drop = TRUE) +
    theme_bw() +
    theme(strip.text.x = element_text(face = "bold"),
          strip.text.y = element_text(face = "bold"),
          strip.background.y = element_blank(),
          legend.title = element_blank())

shinyServer(function(input, output) {
    
    output$centersBox <- renderInfoBox({
        centers <- n_distinct(center_study_summary_df$projectId)
        infoBox(
            "Centers", centers, icon = icon("university"),
            color = "light-blue", fill = TRUE
        )
    })
    
    output$filesBox <- renderInfoBox({
        files <- sum(center_study_summary_df$fileId)
        infoBox(
            "Files", files, icon = icon("file"),
            color = "light-blue", fill = TRUE
        )
    })
    
    output$samplesBox <- renderInfoBox({
        samples <- sum(
            sum(center_study_summary_df$individualID),
            sum(center_study_summary_df$cellLine),
            sum(center_study_summary_df$specimenID)
        )
        infoBox(
            "Samples", samples, icon = icon("tag"),
            color = "light-blue", fill = TRUE
        )
    })
    
    output$pubsBox <- renderInfoBox({
        pubs <- csbc_summary_df %>% 
            group_by(projectId) %>% 
            summarise(value = unique(publication_count)) %>% 
            pull(value) %>%
            sum()
        infoBox(
            "Publications", pubs, icon = icon("pencil"),
            color = "light-blue", fill = TRUE
        )
    })
    
    output$kp_activity <- plotly::renderPlotly({
        ggplotly(p, tooltip = "text") %>%
            layout(legend = list(orientation = 'h',
                                 y = 1, x = 0, yanchor = "bottom"),
                   margin = list(l = 500, b = 55)) %>% 
            plotly::config(displayModeBar = F)  
    })
    
    output$files_per_month <- streamgraph::renderStreamgraph({
        sg_facet_chr <- input$sg_facet
        sg_facet <- as.name(input$sg_facet)
        
        plot_df <- csbc_summary_df %>% 
            select(createdOn_file, rlang::UQ(sg_facet)) %>% 
            mutate(month = as.Date(cut(createdOn_file, breaks = "month"))) %>% 
            filter(!is.na(month)) %>% 
            group_by(month, rlang::UQ(sg_facet)) %>% 
            tally() %>% 
            ungroup() %>%
            complete(month, rlang::UQ(sg_facet)) %>% 
            replace_na(list(n = 0L)) 
        
        if (input$sg_cumulative) {
            plot_df <- plot_df %>% 
                group_by(rlang::UQ(sg_facet)) %>%
                mutate(n = cumsum(n)) %>% 
                ungroup()
        } 
        plot_df <- plot_df %>% 
            mutate(n = log10(n + 1)) %>% 
            rename(sg_facet = rlang::UQ(sg_facet))
        
        streamgraph(as.list(plot_df), sg_facet, "n", "month", offset = "zero", 
                    interpolate = "step") %>%
            sg_axis_x(1, "month", "%m-%Y") %>%
            sg_fill_brewer("PuOr")
    })

    output$center_summary <- DT::renderDT({
        center_summary_df %>% 
            select(
                Center = name_project,
                Studies = study,
                Files = fileId,
                Assays = assay,
                Individuals = individualID,
                Specimens = specimenID,
                `Cell Lines` = cellLine,
                Tools = tool
            ) %>% 
            datatable(
                selection = list(
                    mode = 'single'
                ),
                options = list(
                    scrollX = TRUE,
                    autoWidth = F,
                    dom = "tip"
                ),
                rownames = FALSE
            ) %>%
            formatStyle(
                'Files',
                backgroundColor = styleEqual(0.0, 'lightpink')
            )
    }, server = FALSE
    )

    observeEvent(input$center_summary_rows_selected, {
        output$center_name <- renderText({
            center_row <- input$center_summary_rows_selected
            center_summary_df[[center_row, "name_project"]]
        })
    })
    
    observeEvent(input$center_summary_rows_selected, {
        output$center_metadata <- function() {
            center_row <- input$center_summary_rows_selected
            selected_center <- center_summary_df[[center_row, "projectId"]]
            center_data_df <- center_summary_df %>%
                filter(projectId == selected_center) %>%
                select(project_vars) %>%
                select(-dplyr::matches("(On_project)")) %>%
                mutate(`Synapse Project` = projectId,
                       `Synapse Team` = glue::glue("{team_id} ({team_size} members)",
                                                   team_id = teamProfileId,
                                                   team_size = str_split(
                                                       teamMembersProfileId,
                                                       ", ", simplify = TRUE
                                                   ) %>%
                                                       na.omit() %>% 
                                                       length())) %>%
                create_synapse_links(list(`Synapse Project` = "projectId",
                                          `Synapse Team` = "teamProfileId")) %>%
                mutate(`Synapse Team` = str_replace(`Synapse Team`,
                                                    "#!Synapse", "#!Team")) %>%
                select(-projectId, -name_project,
                       -teamProfileId, -teamMembersProfileId) %>%
                gather(field, val) %>%
                mutate(field = str_to_title(field),
                       field = case_when(
                           field == "Grantnumber" ~ "Grant Number",
                           field == "Granttype" ~ "Grant Type",
                           field == "Publication_count" ~ "Publications",
                           field == "Publication_geodata_produced" ~ "GEO Datasets",
                           TRUE ~ field
                       ),
                       field = str_c("<b>", field, "</b>")
                ) 
            
            knitr::kable(center_data_df, "html", escape = FALSE, col.names = NULL,
                         align = c('r', 'l')) %>% 
                kable_styling("striped", full_width = T)
        }
    })
    
    observeEvent(input$center_summary_rows_selected, {
        output$center_details <- plotly::renderPlotly({
            if (length(input$center_summary_rows_selected)) {
                center_row <- input$center_summary_rows_selected
                selected_center <- center_summary_df[[center_row, "projectId"]]
                center_df <- csbc_summary_df %>% 
                    filter(projectId == selected_center)
                
                # plot_keys <- list(assay = "Assays", tumorType = "Tumor Types",
                #           diagnosis = "Diagnoses", species = "Species",
                #           organ = "Organs", tissue = "Tissues",
                #           dataType = "Data Types", study = "Studies")
                # 
                # chart <- center_df %>%
                #     plot_file_counts_by_annotationkey(plot_keys, chart_height = 300)
                # chart
                
                p <- center_df %>%
                    mutate(tool = ifelse(!is.na(softwareLanguage), study, NA)) %>%
                    select(
                        Studies = study,
                        `Tumor Types` = tumorType,
                        Species = species,
                        Assays = assay,
                        Tools = tool
                    ) %>%
                    gather(attr, val) %>%
                    group_by(attr, val) %>%
                    tally() %>%
                    replace_na(list(val = "Not Annotated")) %>%
                    group_by(attr) %>%
                    mutate(files = sum(n)) %>%
                    ungroup() %>%
                    dplyr::mutate(label = glue::glue(
                        "<b>{value}:</b>\n{count} of {total} files",
                        value = val,
                        count = n,
                        total = files)
                    ) %>%
                    ggplot(aes(x = 1, y = n, text = label)) +
                    geom_col(aes(fill = val), 
                             position = position_stack(reverse = FALSE)) +
                    guides(fill = FALSE) +
                    scale_fill_viridis_d() +
                    scale_y_continuous(expand = c(0, 0)) +
                    xlab("") +
                    ylab("Fraction") +
                    facet_grid(. ~ attr, scales = "free_y") +
                    theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank()) %>%
                    I
                ggplotly(p, tooltip = "text") %>%
                    layout(showlegend = FALSE) %>%
                    plotly::config(displayModeBar = F)
            }
        })
    })
})