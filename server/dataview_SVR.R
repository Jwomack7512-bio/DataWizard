dataViewFunction <- function(input,output,session){  

    #reads in the initial dataframe in the input data tab       
      data <- reactive({
        req(input$data)
        fread(input$data$datapath, na.strings=c("", NA))
      })
      
      
      #changes the checkbox in UI to have checkboxes for all column names
      observeEvent(data(), {
        updatePickerInput(session, "checkbox", choices = names(data()), selected=names(data()))
      })
      
      #anytime the checkbox variable is changed the arranges will be updated with the selected checkboxs
      observeEvent(input$checkbox, {
        updateSelectInput(session, "arrange1", choices = input$checkbox)
        updateSelectInput(session, "arrange2", choices = input$checkbox)
        
      })
      
      #Shows filters in a reactive way using shinyjs depending on the selection of the filter number scrollbar
      observeEvent(input$FILTER_NUMBER_OPTIONS, {
        if(input$FILTER_NUMBER_OPTIONS=="Four"){
          shinyjs::show("filter_div_2")
          shinyjs::show("filter_div_3")
          shinyjs::show("filter_div_4")}
        else if(input$FILTER_NUMBER_OPTIONS=="Three"){
          shinyjs::show("filter_div_2")
          shinyjs::show("filter_div_3")
          shinyjs::hide("filter_div_4")
        }
        else if(input$FILTER_NUMBER_OPTIONS=="Two"){
          shinyjs::show("filter_div_2")
          shinyjs::hide("filter_div_3")
          shinyjs::hide("filter_div_4")
        }
        else{
          shinyjs::hide("filter_div_2")
          shinyjs::hide("filter_div_3")
          shinyjs::hide("filter_div_4")
        }
      })
      
      observeEvent(input$ARRANGE_NUMBER_OPTIONS, {
        if(input$ARRANGE_NUMBER_OPTIONS=="Two"){
          shinyjs::show("sort_div")
          #updateSwitchInput(session, inputId="arrange_on_2", label='Sort', value=FALSE)
          
        }
        else{
          #updateSwitchInput(session, inputId="arrange_on_2", label='Sort', value=FALSE)
          shinyjs::hide("sort_div")
          
        }
      })
      
      
      #updates filter variable choices based on items selected in checkbox selct boxes
      observeEvent(input$checkbox, {
        updateSelectInput(session, "filter_var", choices = input$checkbox)
      })
      
      #updates filter_2 variable choices based on items selected in checkbox selct boxes
      observeEvent(input$checkbox, {
        updateSelectInput(session, "filter_var_2", choices = input$checkbox)
      })
      
      #updates filter_3 variable choices based on items selected in checkbox selct boxes
      observeEvent(input$checkbox, {
        updateSelectInput(session, "filter_var_3", choices = input$checkbox)
      })
      
      #updates filter_4 variable choices based on items selected in checkbox selct boxes
      observeEvent(input$checkbox, {
        updateSelectInput(session, "filter_var_4", choices = input$checkbox)
      })
      
      ##################################################
      # rv <- reactiveValues(filter_numerical = TRUE)
      # observeEvent(input$filter_sym, {
      #   if input$filter_sym = 
      # })
      
      #Here we try to add flag to detect if numerical or categorigacl was used. 
      #This way we can use the flags to stop the value of the filters from reseting each
      #time it is changed but that will take somethinking to do. 
      
      
      ##################################################
      
      #renders the output for the filter check value changing it from numerical to categorical if needed
      output$filter1 <- renderUI({
        if(input$filter_sym == "equal" || input$filter_sym == "not equal") {
          textInput("filter_text", "Categorical Input", value = "")
        }else{
          numericInput("filter_num", label="Numerical Input", value=1)
        }
      })
      
      #renders the output for the filter_2 check value changing it from numerical to categorical if needed
      output$filter2 <- renderUI({
        if(input$filter_sym_2 == "equal" || input$filter_sym_2 == "not equal") {
          textInput("filter_text_2", "Categorical Input", value = "")
        }else{
          numericInput("filter_num_2", label="Numerical Input", value=1)
        }
      })
      
      #renders the output for the filter_3 check value changing it from numerical to categorical if needed
      output$filter3 <- renderUI({
        if(input$filter_sym_3 == "equal" || input$filter_sym_3 == "not equal") {
          textInput("filter_text_3", "Categorical Input", value = "")
        }else{
          numericInput("filter_num_3", label="Numerical Input", value=1)
        }
      })
      
      #renders the output for the filter_4 check value changing it from numerical to categorical if needed
      output$filter4 <- renderUI({
        if(input$filter_sym_4 == "equal" || input$filter_sym_4 == "not equal") {
          textInput("filter_text_4", "Categorical Input", value = "")
        }else{
          numericInput("filter_num_4", label="Numerical Input", value=1)
        }
      })
      
      #This section changes the data to the specifics of the using with select, arrange, filter.
      df_sel <- reactive({
        df_sel <- data() %>%
          select(input$checkbox) %>%
          #if filter is on this chunk of code will filter the data by selected filter symbol
          {if(input$filter_on){
            if(input$filter_sym == ">"){
              filter(., .data[[input$filter_var]] > input$filter_num)
            }
            else if(input$filter_sym == "<"){
              filter(., .data[[input$filter_var]] < input$filter_num)
            }
            else if(input$filter_sym == "="){
              filter(., .data[[input$filter_var]] == input$filter_num)
            }
            else if(input$filter_sym == "!="){
              filter(., .data[[input$filter_var]] != input$filter_num)
            }
            else if(input$filter_sym == "equal"){
              filter(., .data[[input$filter_var]] == input$filter_text)
            }
            else if(input$filter_sym == "not equal"){
              filter(., .data[[input$filter_var]] != input$filter_text)
            }
            else if(input$filter_sym == "remove NA"){
              filter(., !is.na(.data[[input$filter_var]]))
            }
          }else .} %>%
          #filter 2
          {if(input$filter_on_2){
            if(input$filter_sym_2 == ">"){
              filter(., .data[[input$filter_var_2]] > input$filter_num_2)
            }
            else if(input$filter_sym_2 == "<"){
              filter(., .data[[input$filter_var_2]] < input$filter_num_2)
            }
            else if(input$filter_sym_2 == "="){
              filter(., .data[[input$filter_var_2]] == input$filter_num_2)
            }
            else if(input$filter_sym_2 == "!="){
              filter(., .data[[input$filter_var_2]] != input$filter_num_2)
            }
            else if(input$filter_sym_2 == "equal"){
              filter(., .data[[input$filter_var_2]] == input$filter_text_2)
            }
            else if(input$filter_sym_2 == "not equal"){
              filter(., .data[[input$filter_var_2]] != input$filter_text_2)
            }
            else if(input$filter_sym_2 == "remove NA"){
              filter(., !is.na(.data[[input$filter_var_2]]))
            }
          }else .} %>%
          #filter 3
          {if(input$filter_on_3){
            if(input$filter_sym_3 == ">"){
              filter(., .data[[input$filter_var_3]] > input$filter_num_3)
            }
            else if(input$filter_sym_3 == "<"){
              filter(., .data[[input$filter_var_3]] < input$filter_num_3)
            }
            else if(input$filter_sym_3 == "="){
              filter(., .data[[input$filter_var_3]] == input$filter_num_3)
            }
            else if(input$filter_sym_3 == "!="){
              filter(., .data[[input$filter_var_3]] != input$filter_num_3)
            }
            else if(input$filter_sym_3 == "equal"){
              filter(., .data[[input$filter_var_3]] == input$filter_text_3)
            }
            else if(input$filter_sym_3 == "not equal"){
              filter(., .data[[input$filter_var_3]] != input$filter_text_3)
            }
            else if(input$filter_sym_3 == "remove NA"){
              filter(., !is.na(.data[[input$filter_var_3]]))
            }
          }else .} %>%
          #filter 4
          {if(input$filter_on_4){
            if(input$filter_sym_4 == ">"){
              filter(., .data[[input$filter_var_4]] > input$filter_num_4)
            }
            else if(input$filter_sym_4 == "<"){
              filter(., .data[[input$filter_var_4]] < input$filter_num_4)
            }
            else if(input$filter_sym_4 == "="){
              filter(., .data[[input$filter_var_4]] == input$filter_num_4)
            }
            else if(input$filter_sym_4 == "!="){
              filter(., .data[[input$filter_var_4]] != input$filter_num_4)
            }
            else if(input$filter_sym_4 == "equal"){
              filter(., .data[[input$filter_var_4]] == input$filter_text_4)
            }
            else if(input$filter_sym_4 == "not equal"){
              filter(., .data[[input$filter_var_4]] != input$filter_text_4)
            }
            else if(input$filter_sym_4 == "remove NA"){
              filter(., !is.na(.data[[input$filter_var_4]]))
            } 
          } else .} %>%
          #sort using arrange 1 and arrange 2 with option to sort in descending order
          {if(input$arrange_on_1){
            #if second arrange is on then it uses the second arrange and the first to make a sort function
            if(input$arrange_on_2){
              #checks if descending sort is on first arrange and off on the second
              if(input$desc1 && !input$desc2){arrange(., desc(.data[[input$arrange1]]), .data[[input$arrange2]])
              }else if(input$desc1 && input$desc2){arrange(., desc(.data[[input$arrange1]]), desc(.data[[input$arrange2]]))
              }else if(!input$desc1 && input$desc2){arrange(., .data[[input$arrange1]], desc(.data[[input$arrange2]]))
              }else{arrange(., .data[[input$arrange1]], .data[[input$arrange2]])}
            }
            #if second arrange is off and first arrange is selected in descending order
            else if(input$desc1){arrange(., desc(.data[[input$arrange1]]))
            }else{arrange(., .data[[input$arrange1]])
            }
          } else .}
        
      })
      
      #creates excel like table taking in input values from the user slider data
      output$output_table <- renderRHandsontable({
        rhandsontable(df_sel(), width = input$rht_width_input, height = input$rht_height_input, readOnly=TRUE, contextMenu = FALSE) 
      })
      output$downloadData <- downloadHandler(
        filename = function(){
          paste("DataWizard", ".csv", sep = "")
        },
        content = function(file){
          write.csv(df_sel(), file, row.names = FALSE)
          
        }
      )}