library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(streamgraph)

dashboardPage(
    dashboardHeader(
        title = "CSBC/PS-ON Knowledge Portal",
        titleWidth = 350
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Portal at a Glance", tabName = "kp_overview", 
                     icon = icon("dashboard")),
            menuItem("KP Over Time", tabName = "kp_trends",
                     icon = icon("line-chart")),
            menuItem("Center Summaries", tabName = "center_summaries",
                     icon = icon("list"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "kp_overview",
                    fluidRow(
                        infoBoxOutput("centersBox", width = 3),
                        infoBoxOutput("filesBox", width = 3),
                        infoBoxOutput("samplesBox", width = 3),
                        infoBoxOutput("pubsBox", width = 3)
                    ),
                    fluidRow(
                        box(
                            title = "Center Activity", width = 12, height = 675,
                            solidHeader = FALSE, status = "warning",
                            plotly::plotlyOutput("kp_activity", 
                                                 height = "600px")
                        )
                    )
            ),
            tabItem(tabName = "kp_trends",
                    fluidRow(
                        box(
                            width = 3, status = "primary",
                            selectInput("sg_facet", label = "Group files by:",
                                        choices = c("projectId", "assay"), 
                                        selected = "projectId"),
                            radioButtons("sg_cumulative", 
                                         label = "Aggregation:",
                                         choiceNames = c(
                                             "Month-by-month", 
                                             "Cumulative"
                                         ),
                                         choiceValues = c(FALSE, TRUE),
                                         selected = TRUE)
                        ),
                        box(
                            title = "Files Added", width = 9, height = 500,
                            status = "warning",
                            streamgraph::streamgraphOutput("files_per_month")
                            
                        )
                    )
            ),
            tabItem(tabName = "center_summaries",
                    fluidRow(
                        box(
                            title = "Center Snapshot", width = 12,
                            status = "primary",
                            div(style = 'overflow-x: scroll', 
                                DT::dataTableOutput('center_summary')
                            )
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Center Details", width = 4, height = 500,
                            solidHeader = FALSE, status = "info",
                            textOutput("center_name_msg"),
                            h4(strong(textOutput("center_name"))),
                            tableOutput("center_metadata")
                        ),
                        box(
                            title = "Center Data", width = 8, height = 500,
                            solidHeader = FALSE, status = "warning",
                            plotly::plotlyOutput("center_details")
                        )
                    )
            )
        )
    )
)