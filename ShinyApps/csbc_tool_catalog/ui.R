dashboardPage(
  # tags$head(
  #   singleton(
  #     includeScript("www/readCookie.js")
  #   )
  # ),
  # 
  # # Application title
  # uiOutput("title"),
  
  dashboardHeader(title = "CSBC Tool Catalog"),
  dashboardSidebar(
    checkboxGroupInput("inputDataType", "Input Data Type",
                       choiceNames = input_list,
                       choiceValues = input_list, 
                       selected = input_list),
    checkboxGroupInput("outputDataType", "Output Data Type",
                       choiceNames = output_list,
                       choiceValues = output_list, 
                       selected = output_list),
    checkboxGroupInput("center", "Center",
                       choiceNames = center_list,
                       choiceValues = center_list, 
                       selected = center_list)
  ),
  dashboardBody(
    h3("Software/Tool Types"),
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