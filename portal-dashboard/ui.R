library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(streamgraph)

dashboardPage(
    dashboardHeader(
        title = "CSBC/PS-ON Knowledge Portal"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Portal at a Glance", tabName = "kp_overview", 
                     icon = icon("dashboard")),
            menuItem("KP Over Time", icon = icon("th"), tabName = "kp_trends"),
            menuItem("Center Summaries", icon = icon("th"), 
                     tabName = "center_summaries")
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
                            title = "Center Activity", width = 12,
                            solidHeader = TRUE, status = "primary",
                            plotly::plotlyOutput("kp_activity")
                        )
                    )
            ),
            tabItem(tabName = "kp_trends",
                    fluidRow(
                        box(
                            width = 3,
                            selectInput("sg_facet", label = "Group files by:",
                                        choices = c("projectId", "assay"), 
                                        selected = "projectId"),
                            radioButtons("sg_cumulative", label = "Aggregation:",
                                         choiceNames = c("Month-by-month", "Cumulative"),
                                         choiceValues = c(FALSE, TRUE),
                                         selected = TRUE)
                        ),
                        box(
                            title = "Files Added", width = 9,
                            streamgraph::streamgraphOutput("files_per_month", 
                                                           height = "100%")
                            
                        )
                    )
            ),
            tabItem(tabName = "center_summaries",
                    fluidRow(
                        box(
                            title = "Table", width = 12,
                            solidHeader = TRUE, status = "primary",
                            DT::dataTableOutput("center_summary", 
                                                width = "100%")
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Center Details", width = 4,
                            solidHeader = TRUE, status = "info",
                            h4(textOutput("center_name")),
                            tableOutput("center_metadata")
                        ),
                        box(
                            title = "Center Data", width = 8,
                            solidHeader = TRUE, status = "info",
                            plotly::plotlyOutput("center_details")
                        )
                    )
            )
        )
    )
)