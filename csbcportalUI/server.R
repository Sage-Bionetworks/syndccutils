server <- function(input, output, session) {

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
            datatable(selection = list(
                mode = 'single'
            ),
            options = list(
                scrollX = TRUE
            ),
            rownames = FALSE
            ) %>%
            formatStyle(
                'Files',
                backgroundColor = styleEqual(0.0, 'lightpink')
            )
    }, server = FALSE
    )
}

