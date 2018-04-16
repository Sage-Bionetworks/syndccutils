cat(file=stderr(), "reading data...", "\n")

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
    ggplot(aes(x = name_project, y = study)) +
    geom_col(aes(fill = avg_files, text = label), 
             colour = "slategray", size = 0.3, alpha = 1) +
    geom_point(y = 0.2, colour = "white", size = 2) +
    geom_point(aes(colour = grantType), y = 0.2, size = 1.5) +
    coord_flip() +
    scale_colour_colorblind() +
    guides(alpha = FALSE, fill = FALSE, colour = guide_legend(title = NULL)) +
    xlab("") +
    ylab("Studies in Synapse") +
    scale_y_continuous(expand = c(0, 0)) +
    facet_grid(consortium ~ ., 
               scales = "free_y", space = "free_y", drop = TRUE) +
    theme_bw() +
    theme(strip.text.y = element_text(face = "bold"),
          legend.title = element_blank())