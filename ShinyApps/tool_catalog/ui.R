dashboardPage(
  dashboardHeader(title = "CSBC/PS-ON Tool Catalog",titleWidth = 300),
  dashboardSidebar(width = 300,
    uiOutput("input_ui"),
    uiOutput("output_ui"),
    uiOutput("center_ui")
  ),
  dashboardBody(
    tags$head(
     singleton(
       includeScript("www/readCookie.js")
     )
    ),
    h3("Software/Tool Type"),
    fluidRow(
      tabBox(
        id = "tab",width = "1000px",
        tabPanel("Script", DT::dataTableOutput('table_script')), 
        tabPanel("Package Binary", DT::dataTableOutput('table_binary')), 
        tabPanel("Package Library", DT::dataTableOutput('table_library')),
        tabPanel("Web Application", DT::dataTableOutput('table_web')),
        tabPanel("Other", DT::dataTableOutput('table_other'))
      )
    )
  )
)