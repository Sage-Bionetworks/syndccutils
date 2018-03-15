shinyUI(fluidPage(
    theme = shinythemes::shinytheme("lumen"),
    titlePanel('CSBC/PS-ON Knowledge Portal'),
    sidebarLayout(
        sidebarPanel(
            'About\n  Welcome to the CSBC/PS-ON Knowledge Portal Dashboard!'
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Portal at a Glance"
                ),
                tabPanel("KP Over Time",
                         DT::dataTableOutput('center_summary', width = "100%")
                ),
                tabPanel("Center Summaries"
                )
            )
        )
    )
))


