function(input, output, session) {
  
   session$sendCustomMessage(type="readCookie",
                             message=list(name='org.sagebionetworks.security.user.login.token'))
   
   foo <- observeEvent(input$cookie, {
     synLogin(sessionToken=input$cookie)
     withProgress(message = 'Loading data...',
                 {source("getData.R")})
    
    output$input_ui <- renderUI({
      pickerInput(inputId = "inputDataType", 
                label = "Input Data Type",
                selected = input_list,
                choices = sort(input_list), options = list(`actions-box` = TRUE), 
                multiple = TRUE)
    })
    output$output_ui <- renderUI({
      pickerInput(inputId = "outputDataType", 
                  label = "Output Data Type",
                  selected = output_list,
                  choices = sort(output_list), options = list(`actions-box` = TRUE), 
                  multiple = TRUE)
    })
    output$center_ui <- renderUI({
      checkboxGroupInput("center", "Center",
                          choiceNames = center_list,
                          choiceValues = center_list,
                          selected = center_list)

    })
    # Filter data based on selections
    dat <- reactive({
      # validate input
      validate(need(length(input$inputDataType) != 0, "Please select at least one input data type."))
      validate(need(length(input$outputDataType) != 0, "Please select at least one output data type."))
      validate(need(length(input$center) != 0, "Please select at least one center."))
      
      # filter by selected
      selected_id_input <- unique(unlist(input_search[input$inputDataType],use.names = FALSE))
      selected_id_output <- unique(unlist(output_search[input$outputDataType],use.names = FALSE))
      selected_id_center <- unique(unlist(center_search[input$center],use.names = FALSE))
      
      selected_ids <- Reduce(intersect,list(selected_id_input,selected_id_output,selected_id_center))
      
      temp <- tool_info[selected_ids,]
      temp <- temp[,c("methodName","centerName","inputDataType","outputDataType","softwareLanguage","softwareType")]
      temp
    })
    
    tempDat <- reactiveValues(df1=NULL,df2=NULL,df3=NULL,df4=NULL, df5=NULL)
    
    dataModal <- function(selected_id,failed = FALSE) {
      selected_dat <- tool_info[selected_id,]
      
      modalDialog(
        title = selected_dat$methodName,
        if(selected_dat$centerName != "N/A")
            tags$p(
              tags$strong("Center"),
              tags$br(),
              tags$a(href=paste0("https://www.synapse.org/#!Synapse:",center_info$center[center_info$centerName == selected_dat$centerName]),
                     target="_blank",
                     selected_dat$centerName)
            ),
        
        if(selected_dat$synapseSite != "N/A")
          tags$p(
            tags$strong("Synapse Site"),
            tags$br(),
            tags$a(href=paste0("https://www.synapse.org/#!Synapse:",selected_dat$synapseSite),
                   target="_blank",
                   selected_dat$synapseSite)
          ),
        
        if(selected_dat$URL != "N/A")
            tags$p(
              tags$strong("Website"),
              tags$br(),
              tags$a(href=selected_dat$URL,target="_blank",selected_dat$URL)
            ),
        
        tags$p(tags$strong("Abstract")),
        tags$p(unlist(abstracts_list[selected_id])),
        
        # if(selected_dat$PMID != "N/A"){
        #   pmid_list <- strsplit(selected_dat$PMID,",")[[1]]
        # }
        # if(selected_dat$PMID != "N/A")
        #   tags$p(
        #      tags$strong("Publication"),
        #   #s   for (pmid in pmid_list)
        #        tags$a(href=paste0('https://www.ncbi.nlm.nih.gov/pubmed/',trimws(pmid)),target="_blank",trimws(pmid)),
        #        tags$br()
        #   ),
        
        footer = modalButton("Close"),
        size = "l",
        easyClose = TRUE
      )
    }
    
    output$table_script <- DT::renderDataTable(DT::datatable({
          temp <- dat()
          temp <- temp[temp$softwareType=="script",]
          temp$softwareType <- NULL
          tempDat$df1 <- temp
          temp
      },escape = c(TRUE,TRUE,FALSE,FALSE,TRUE),selection = 'single',rownames = FALSE,options = list(dom = 't')))
    
    output$table_binary <- DT::renderDataTable(DT::datatable({
      temp <- dat()
      temp <- temp[temp$softwareType=="packageBinary",]
      temp$softwareType <- NULL
      tempDat$df2 <- temp
      temp
    },escape = c(TRUE,TRUE,FALSE,FALSE,TRUE),selection = 'single',rownames = FALSE,options = list(dom = 't')))
    
    output$table_library <- DT::renderDataTable(DT::datatable({
      temp <- dat()
      temp <- temp[temp$softwareType=="packageLibrary",]
      temp$softwareType <- NULL
      tempDat$df3 <- temp
      temp
    },escape = c(TRUE,TRUE,FALSE,FALSE,TRUE),selection = 'single',rownames = FALSE,options = list(dom = 't')))
    
    output$table_web <- DT::renderDataTable(DT::datatable({
      temp <- dat()
      temp <- temp[temp$softwareType=="web application",]
      temp$softwareType <- NULL
      tempDat$df4 <- temp
      temp
    },escape = c(TRUE,TRUE,FALSE,FALSE,TRUE),selection = 'single',rownames = FALSE,options = list(dom = 't')))
    
    output$table_other <- DT::renderDataTable(DT::datatable({
      temp <- dat()
      temp <- temp[temp$softwareType=="other",]
      temp$softwareType <- NULL
      tempDat$df5 <- temp
      temp
    },escape = c(TRUE,TRUE,FALSE,FALSE,TRUE),selection = 'single',rownames = FALSE,options = list(dom = 't')))
    
    # observe events
    observeEvent(input$table_script_rows_selected,
                 {
                   selected_id <- row.names(tempDat$df1[input$table_script_rows_selected,])
                   showModal(dataModal(selected_id = selected_id))
                 })
    
    observeEvent(input$table_binary_rows_selected,
                 {
                   selected_id <- row.names(tempDat$df2[input$table_binary_rows_selected,])
                   showModal(dataModal(selected_id = selected_id))
                 })
    
    observeEvent(input$table_library_rows_selected,
                 {
                   selected_id <- row.names(tempDat$df3[input$table_library_rows_selected,])
                   showModal(dataModal(selected_id = selected_id))
                 })
    
    observeEvent(input$table_web_rows_selected,
                 {
                   selected_id <- row.names(tempDat$df4[input$table_web_rows_selected,])
                   showModal(dataModal(selected_id = selected_id))
                 })
    
    observeEvent(input$table_other_rows_selected,
                 {
                   selected_id <- row.names(tempDat$df5[input$table_other_rows_selected,])
                   showModal(dataModal(selected_id = selected_id))
                 })
  })
}