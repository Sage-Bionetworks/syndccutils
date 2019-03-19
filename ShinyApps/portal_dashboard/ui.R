dashboardPage(
  dashboardHeader(
    title = "CSBC/PS-ON Knowledge Portal",
    titleWidth = 350
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Portal at a Glance", tabName = "kp_overview", 
               icon = icon("dashboard")),
      menuItem("KP Details", tabName = "consortium_summaries", 
               icon = icon("bar-chart")),
      menuItem("KP Over Time", tabName = "kp_trends",
               icon = icon("line-chart")),
      menuItem("Center Summaries", tabName = "center_summaries",
               icon = icon("list"))
    )
  ),
  dashboardBody(
    tags$head(
      singleton(
        includeScript("www/readCookie.js")
      ),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "kp_overview",
              fluidRow(
                infoBoxOutput("centersBox", width = 3) %>% 
                  shinycssloaders::withSpinner(proxy.height = "125px"),
                infoBoxOutput("filesBox", width = 3),
                infoBoxOutput("samplesBox", width = 3),
                infoBoxOutput("pubsBox", width = 3)
              ),
              fluidRow(
                box(
                  title = "Program Content", width = 12, height = 250,
                  solidHeader = FALSE, status = "warning",
                  fluidRow(
                    column(width = 2, offset = 8,
                           div(class = "blue-square"),
                           span(style = "font-size:12px", p("CSBC"))
                    ),
                    column(width = 2,
                           div(class = "orange-square"),
                           span(style = "font-size:12px", p("PS-ON"))
                    )
                  ),
                  column(width = 3, pierOutput("consortium_files")),
                  column(width = 3, pierOutput("consortium_studies")),
                  column(width = 3, pierOutput("consortium_cancers")),
                  column(width = 3, pierOutput("consortium_data"))
                )
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
      tabItem(tabName = "consortium_summaries",
              fluidRow(
                box(
                  title = "Portal Snapshot", width = 12,
                  status = "primary", height = 400,
                  div(style = 'overflow-x: scroll', 
                      plotly::plotlyOutput('consortium_summary') %>% 
                        shinycssloaders::withSpinner()
                  )
                )
              ),
              fluidRow(
                box(
                  width = 3, status = "primary",
                  radioButtons("sample_type",
                               label = "Sample Type",
                               choices = config$sampletype_display,
                               selected = "individualID"),
                  selectInput("sample_group_select", 
                              label = "Annotation Key 1\n(y-axis split)",
                              choices = config$annotationkey_display,
                              selected = "tumorType"),
                  selectInput("sample_fill_select", 
                              label = "Annotation Key 2\n(fill color)",
                              choices = config$annotationkey_display,
                              selected = "assay")
                ),
                box(
                  title = "Sample Breakdown", width = 9,
                  status = "warning",
                  plotly::plotlyOutput("annotationkey_counts", height = "100%") %>% 
                    shinycssloaders::withSpinner()
                  
                )
              )
      ),
      tabItem(tabName = "kp_trends",
              fluidRow(
                box(
                  width = 3, status = "primary",
                  selectInput("sg_facet", label = "Group files by:",
                              choices = config$annotationkey_display, 
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
                  plotly::plotlyOutput("files_per_month") %>% 
                    shinycssloaders::withSpinner()
                  
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
