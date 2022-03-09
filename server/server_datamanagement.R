#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#Tab: DataView
#DATAVIEW000

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#reads in the initial dataframe in the input data tab       
data <- reactive({
  req(input$data)
  #fread(input$data$datapath, na.strings=c("", NA))
  if(endsWith(input$data$datapath, ".csv")){
    read.csv(input$data$datapath)
  } else if(endsWith(input$data$datapath, ".txt")){
    read.table(input$data$datapath,header = T)
  }else if(endsWith(input$data$datapath, ".xls")){
    read_excel(input$data$datapath)
  } else if(endsWith(input$data$datapath, ".xlsx")){
    read_xlsx(input$data$datapath,sheet=1)
  }
})


#changes the checkbox in UI to have checkboxes for all column names
observeEvent(data(), {
  updatePickerInput(session, 
                    "checkbox", 
                    choices = names(data()), 
                    selected=names(data()))
})

#anytime the checkbox variable is changed the arranges will be updated with the selected checkboxs
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "arrange1", 
                    choices = input$checkbox)
  updateSelectInput(session, 
                    "arrange2", 
                    choices = input$checkbox)
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
  updateSelectInput(session, 
                    "filter_var",
                    choices = input$checkbox)
})

#updates filter_2 variable choices based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "filter_var_2", 
                    choices = input$checkbox)
})

#updates filter_3 variable choices based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "filter_var_3", 
                    choices = input$checkbox)
})

#updates filter_4 variable choices based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "filter_var_4", 
                    choices = input$checkbox)
})

#set reactive flag to tell when mutate has been used
rv <- reactiveValues(mutate_flag = FALSE)

#Setting a reactive value for data
v = reactiveValues(df_final = NULL)

#This section changes the data to the specifics of the using with select, arrange, filter.
df_sel <- reactive({
  #print(rv$mutate_flag)
  #print(head(v$df_final))
  #print(names(v$df_final))
  if(rv$mutate_flag==FALSE){
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
        else if(input$filter_sym == "between"){
          filter(., between(.data[[input$filter_var]], input$filter_num_min, input$filter_num_max))
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
        else if(input$filter_sym_2 == "between"){
          filter(., between(.data[[input$filter_var_2]], input$filter_num_min_2, input$filter_num_max_2))
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
        else if(input$filter_sym_3 == "between"){
          filter(., between(.data[[input$filter_var_3]], input$filter_num_min_3, input$filter_num_max_3))
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
        else if(input$filter_sym_4 == "between"){
          filter(., between(.data[[input$filter_var_4]], input$filter_num_min_4, input$filter_num_max_4))
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
    v$df_final = df_sel
    df_sel}
  else{
    #print("running this")
    df_sel <- v$df_final %>%
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
        else if(input$filter_sym == "between"){
          filter(., between(.data[[input$filter_var]], input$filter_num_min, input$filter_num_max))
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
        else if(input$filter_sym_2 == "between"){
          filter(., between(.data[[input$filter_var_2]], input$filter_num_min_2, input$filter_num_max_2))
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
        else if(input$filter_sym_3 == "between"){
          filter(., between(.data[[input$filter_var_3]], input$filter_num_min_3, input$filter_num_max_3))
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
        else if(input$filter_sym_4 == "between"){
          filter(., between(.data[[input$filter_var_4]], input$filter_num_min_4, input$filter_num_max_4))
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
    #v$df_final <- df_sel
    df_sel
    
  }
  
})

#----------------------------------------------------
#Mutate data
#MUTATE000
#this is done after all the filtering and selecting. I think that makes the most sense
#----------------------------------------------------
#updates log(x) variable options
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "mutate_trans_log", 
                    choices = input$checkbox)
})

#updates log(x+1) variable options
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "mutate_trans_logp1", 
                    choices = input$checkbox)
})

#updates sqrt(x) variable options
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "mutate_trans_sqrt", 
                    choices = input$checkbox)
})

#updates axpb variable options
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "mutate_trans_axpb", 
                    choices = input$checkbox)
})

#updates the input variable for Conditional Mutation tab in mutate Data tabbox
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "conditional_mutate_var", 
                    choices = input$checkbox)
}) 

#change choices on picker input/label on textInput if conditional checkbox is chosen
#removes "<"/">" for categorical input and changes label between "Numerical"/"Catergorical"
observeEvent(input$mutate_conditMut_isChar, {
  if(input$mutate_conditMut_isChar==0){
    updatePickerInput(session,
                      "conditional_mutate_condition", 
                      label = "Condition", 
                      choices =  c("=" = "==",
                                   "!=" = "!=", 
                                   "<" = "<", 
                                   ">" = ">"))
    
    updateTextInput(session,
                    "conditional_mutate_condition_text",
                    label="Numerical Input")
  }
  else if(input$mutate_conditMut_isChar == 1){
    updatePickerInput(session,
                      "conditional_mutate_condition", 
                      label = "Condition", 
                      choices =  c("=" = "==",
                                   "!=" = "!="))
    
    updateTextInput(session,
                    "conditional_mutate_condition_text",
                    label="Categorical Input")
  }
  
})

#updates the select input for conditional mutate options to select column
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "mutate_multicondit_select_column", 
                    choices = input$checkbox)
})


varChoices <- eventReactive(input$push_options_for_multiConditions,{
  varChoices <- sort(unique(df_sel()[[input$mutate_multicondit_select_column]]))
})

observeEvent(input$push_options_for_multiConditions,{
  output$multiple_outputs_select_colunms <- renderUI({
    req(input$mutate_multicondit_select_column)
    textInput(inputId="multiCondit_mutate_newColumnName",
              label="New Column Name",
              value=paste0(input$mutate_multicondit_select_column, "_mutate"))
  })
})

output$mulitple_outputs_categorical <- renderUI({
  req(input$mutate_multicondit_select_column)
  
  lev_with_spaces <- varChoices()
  lev <- gsub(" ", "_", varChoices())
  lapply(seq_along(lev), function(i){
    textInput(inputId = paste0("mutate_condit_", lev[i]),
              label=paste0("If value =  ", lev_with_spaces[i], " then change to:"),
              value=i-1
    )
  })
})

#updates the select input for edit column selection
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "data_edit_select_column", 
                    choices = input$checkbox)
})

#updates the select input for variable selection of column edit selection
observeEvent(input$data_edit_select_column, {
  updateSelectInput(session, 
                    "data_edit_select_value", 
                    choices = unique(sort(v$df_final[[input$data_edit_select_column]])))
})

observeEvent(input$edit_push,{
  rv$mutate_flag <- TRUE
  column_to_edit = input$data_edit_select_column
  value_to_change = input$data_edit_select_value
  value_to_change_to = input$data_edit_new_value
  temp_df <- v$df_final
  
  positions <- which(temp_df[[column_to_edit]]==value_to_change)
  for(i in positions){
    temp_df[[column_to_edit]][i] <- value_to_change_to
  }
  v$df_final <- temp_df
})

#updates the select input for edit column selection
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "edit_data_column_to_convert_numChar", 
                    choices = input$checkbox)
})

output$edit_typeofVar <- renderPrint({
  req(input$checkbox)
  out = typeof(df_to_use()[[input$edit_data_column_to_convert_numChar]])
  print(out)
})

observeEvent(input$numChar_convert_push, {
  rv$mutate_flag <- TRUE
  column_to_edit = input$edit_data_column_to_convert_numChar
  temp_df <- v$df_final
  if(input$edit_numChar_conversion_choice == "num2char"){
    temp_df[[column_to_edit]] <- as.character(temp_df[[column_to_edit]])
  }else if(input$edit_numChar_conversion_choice == "char2num"){
    temp_df[[column_to_edit]] <- as.numeric(temp_df[[column_to_edit]])
  }
  v$df_final <- temp_df
})


df_sel2 <- eventReactive(input$mutate_push, {
  #check for name error
  if(input$mutate_radio_options == "Transformations"){
    if(input$mutate_transformation_options == 'log'){
      colname = input$mutate_trans_log_name
    }
    else if(input$mutate_transformation_options == 'logp1'){
      colname = input$mutate_trans_logp1_name
    }
    else if(input$mutate_transformation_options == 'sqrt'){
      colname = input$mutate_trans_sqrt_name
    }
    else if(input$mutate_transformation_options == 'axpb'){
      colname = input$mutate_trans_axpb_name
    }
  }else if(input$mutate_radio_options == "Conditional_mutate"){
    colname = input$conditional_mutate_name
  }else if(input$mutate_radio_options == "Multiple_conditions"){
    colname = input$multiCondit_mutate_newColumnName
  }
  current_col_names <- colnames(df_sel()) #get current names in column
  if(colname %in% current_col_names){#this stops the program from crashing when the same name is used to create a column of data
    # #message popup
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Column Name already exists! Please change name')
  }
  else{#if name not in column proceed with code
    if(rv$mutate_flag){
      #if transformations is selected in the mutate data tabbox tab
      if(input$mutate_radio_options == "Transformations"){
        
        if(input$mutate_transformation_options == 'log'){
          t <- v$df_final %>%
            mutate(!!input$mutate_trans_log_name:=log10(.data[[input$mutate_trans_log]]))
        }
        else if(input$mutate_transformation_options == 'logp1'){
          if(input$logp1_custom_input==TRUE){
            t <- v$df_final %>%
              mutate(!!input$mutate_trans_logp1_name:=log10(.data[[input$mutate_trans_logp1]]+as.numeric(input$logp1_custom_input_value))) 
          }else{
            t <- v$df_final %>%
              mutate(!!input$mutate_trans_logp1_name:=log10(.data[[input$mutate_trans_logp1]]+1))
          }
        }
        else if(input$mutate_transformation_options == 'sqrt'){
          t <- v$df_final %>%
            mutate(!!input$mutate_trans_sqrt_name:=sqrt(.data[[input$mutate_trans_sqrt]]))
        }
        else if(input$mutate_transformation_options == 'axpb'){
          t <- v$df_final %>%
            mutate(!!input$mutate_trans_axpb_name:=(as.numeric(input$mutate_trans_axpb_a)*.data[[input$mutate_trans_axpb]]+as.numeric(input$mutate_trans_axpb_b)))
        }
        #--------end of inside if - else statements
        #--------beginning of conditional mutate elseif statement
      }else if(input$mutate_radio_options == "Conditional_mutate"){
        if(input$mutate_conditMut_isChar){
          # sets the variable and check var to be "var" to "'var'" so the string can be compared
          variable = paste0("'", v$df_final[[input$conditional_mutate_var]], "'")
          condition = input$conditional_mutate_condition
          check_var = paste0("'", input$conditional_mutate_condition_text, "'")
        }else{
          #does a numerical comparison, converting checkvar from string to numeric
          variable = v$df_final[[input$conditional_mutate_var]]
          condition = input$conditional_mutate_condition
          check_var = as.numeric(input$conditional_mutate_condition_text)
        }
        
        #create array for mutation replacing values depending on conditions
        TF_array <- evaluate_conditional_statements(variable, condition, check_var)
        col_to_add <- create_conditional_column(TF_array, input$conditional_mutate_value_condit_true, input$conditional_mutate_value_condit_false)
        
        t <- cbind(v$df_final, col_to_add) #bind column to dataframe
        names(t)[names(t)=="col_to_add"] <- input$conditional_mutate_name #add name to column that was added
      }
      else if(input$mutate_radio_options == "Multiple_conditions"){
        variables = paste0("'", v$df_final[[input$mutate_multicondit_select_column]], "'") #store variables as strings for eval
        
        check_vars_wihtout_spaces <- sort(unique(gsub(" ", "_", v$df_final[[input$mutate_multicondit_select_column]]))) #remove spaces to find correct input
        
        check_vars <- sort(unique(v$df_final[[input$mutate_multicondit_select_column]])) #get array of variables to compare with
        check_vars <- paste0("'", check_vars, "'") #make variables strings for comparison
        
        col_to_add <- vector()
        for(i in seq(length(check_vars))){ #for all unique values in the select input
          text_in <- paste0("input$mutate_condit_", check_vars_wihtout_spaces[i]) #create string for DA input
          text_in_evaluated <- eval(parse(text=text_in)) #evaluate the input to get its value
          
          bool<- evaluate_conditional_statements(variables, "==", check_vars[i]) #create vector of booleans if column of data matches the check varaiable
          for(j in seq(length(bool))){ #for all the var in the boolean vector
            if(bool[j]==TRUE){ #if the value is true then in add the change var to new array (ie. iris dataset, user wants setosa=1.  matches setosa with TRUE then places 1 in new vector everywhere setosa is)
              col_to_add[j] = text_in_evaluated
            }
          }
        }
        #print(col_to_add)
        t <- cbind(v$df_final, col_to_add) #bind new column of user wanted data to df
        names(t)[names(t)=="col_to_add"] <- input$multiCondit_mutate_newColumnName #give name to column
      }
      
      #assign values to reactive value  
      v$df_final <- t
      t #output t as the value of this function
    }
    #-----else if mutate flag has not been triggered, i.e mutate  button never been pushed
    #everything below this point should use df_sel() and df_sel2.  NOT v$df_final
    else{
      if(input$mutate_radio_options == "Transformations"){
        if(input$mutate_transformation_options == 'log'){
          df_sel2 <- df_sel()  %>%
            mutate(!!input$mutate_trans_log_name:=log10(.data[[input$mutate_trans_log]]))
        }
        else if(input$mutate_transformation_options == 'logp1'){
          if(input$logp1_custom_input==TRUE){
            df_sel2 <- df_sel() %>%
              mutate(!!input$mutate_trans_logp1_name:=log10(.data[[input$mutate_trans_logp1]]+as.numeric(input$logp1_custom_input_value))) 
          }else{
            df_sel2 <- df_sel() %>%
              mutate(!!input$mutate_trans_logp1_name:=log10(.data[[input$mutate_trans_logp1]]+1))
          }
        }
        else if(input$mutate_transformation_options == 'sqrt'){
          df_sel2 <- df_sel() %>%
            mutate(!!input$mutate_trans_sqrt_name:=sqrt(.data[[input$mutate_trans_sqrt]]))
        }
        else if(input$mutate_transformation_options == 'axpb'){
          df_sel2 <- df_sel() %>%
            mutate(!!input$mutate_trans_axpb_name:=(as.numeric(input$mutate_trans_axpb_a)*.data[[input$mutate_trans_axpb]]+as.numeric(input$mutate_trans_axpb_b)))
        }
        
      } #end if mutate_ratio_options = Transformations
      #--------end of inside if - else statements
      #--------beginning of conditional mutate elseif statement
      else if(input$mutate_radio_options == "Conditional_mutate"){
        #set up three individual statements to build a conditional statement
        #variable (condition) >
        if(input$mutate_conditMut_isChar){
          # sets the variable and check var to be "var" to "'var'" so the string can be compared
          variable = paste0("'", df_sel()[[input$conditional_mutate_var]], "'")
          condition = input$conditional_mutate_condition
          check_var = paste0("'", input$conditional_mutate_condition_text, "'")
        }else{
          #does a numerical comparison, converting checkvar from string to numeric
          variable = df_sel()[[input$conditional_mutate_var]]
          condition = input$conditional_mutate_condition
          check_var = as.numeric(input$conditional_mutate_condition_text)
        }
        
        #create array for mutation replacing values depending on conditions
        TF_array <- evaluate_conditional_statements(variable, condition, check_var)
        col_to_add <- create_conditional_column(TF_array, input$conditional_mutate_value_condit_true, input$conditional_mutate_value_condit_false)
        print(col_to_add)
        
        #bind column to dataframe
        df_sel2 <- cbind(df_sel(), col_to_add)
        names(df_sel2)[names(df_sel2)=="col_to_add"] <- input$conditional_mutate_name #give name to column
      }
      
      else if(input$mutate_radio_options == "Multiple_conditions"){
        variables = paste0("'", df_sel()[[input$mutate_multicondit_select_column]], "'") #store variables as strings for eval
        
        check_vars_wihtout_spaces <- sort(unique(gsub(" ", "_", df_sel()[[input$mutate_multicondit_select_column]]))) #remove spaces to find correct input
        
        check_vars <- sort(unique(df_sel()[[input$mutate_multicondit_select_column]])) #get array of variables to compare with
        check_vars <- paste0("'", check_vars, "'") #make variables strings for comparison
        
        col_to_add <- vector()
        for(i in seq(length(check_vars))){ #for all unique values in the select input
          text_in <- paste0("input$mutate_condit_", check_vars_wihtout_spaces[i]) #create string for DA input
          text_in_evaluated <- eval(parse(text=text_in)) #evaluate the input to get its value
          
          bool<- evaluate_conditional_statements(variables, "==", check_vars[i]) #create vector of booleans if column of data matches the check varaiable
          for(j in seq(length(bool))){ #for all the var in the boolean vector
            if(bool[j]==TRUE){ #if the value is true then in add the change var to new array (ie. iris dataset, user wants setosa=1.  matches setosa with TRUE then places 1 in new vector everywhere setosa is)
              col_to_add[j] = text_in_evaluated
            }
          }
        }
        #print(col_to_add)
        df_sel2 <- cbind(df_sel(), col_to_add) #bind new column of user wanted data to df
        names(df_sel2)[names(df_sel2)=="col_to_add"] <- input$multiCondit_mutate_newColumnName #give name to column
      }
      
      #set flag to true, signaling that mutate has been used, setting a specfic df for use at future steps
      rv$mutate_flag <- TRUE
      #assign the values of this calculated to the df_final reactive value
      v$df_final <- df_sel2
      #assign  the term of this equation as df_sel2
      df_sel2
    }
  }
  
  
})

#changes the original select choices picker input to match the mutated values if needed
observeEvent(input$mutate_push, {
  updatePickerInput(session,
                    "checkbox",
                    choices = names(df_sel2()),
                    selected=names(df_sel2()))
})

#ifelse doesn't seem to work in the table functions.  It seems to create a list that fails.
#using the if else in a reactive statement works tho.  
#So this is the final dataframe to use
df_to_use <- reactive({
  
  if(rv$mutate_flag){ #if the mutate button has been used we use df_sel2
    #df_sel2()
    df_sel()
    #v$df_final
  }else{df_sel()} #else we use the original data frame.
})



#_________________________________
#Table Outputs

#creates excel like table.  One option of conditional panel
output$output_rhandsometable <- renderRHandsontable({
  
  rhandsontable(df_to_use(), 
                width = input$rht_width_input, 
                height = input$rht_height_input, 
                readOnly=TRUE, 
                contextMenu = FALSE)
})

#creates a standard r table for data.  One option of conditional panel
output$output_rtable <- renderTable(df_to_use(),
                                    striped=TRUE,
                                    hover=TRUE,
                                    bordered=TRUE)

#creates a DT style table for data.  One option of conditional panel
output$output_DT_table <- renderDataTable(df_to_use(),
                                          options=list(
                                            scroller = TRUE,
                                            scrollX = TRUE,
                                            scrollY = "500px"
                                          ))

#mainly used to show is column is "num" or "str"
output$output_dataTable_structure <- renderPrint({str(df_to_use())})
                                            
#grabs labels from original data
vlabels <- reactive({data() %>% extract_variable_label()})

glimpse.out = reactive({
  df_to_use() %>%  ff_relabel(vlabels()) %>% ff_glimpse()
  # finalfit::ff_glimpse(df_to_use())
})
observe({
  print(glimpse.out())
})

# Column number to allow reactive justification
glimpse1_column_n = reactive(dim(glimpse.out()[[1]])[2])
glimpse2_column_n = reactive(dim(glimpse.out()[[2]])[2])

output$output_data_summary = renderDataTable({
  DT::datatable(glimpse.out()[[1]],
                rownames=FALSE, extensions = "FixedColumns",
                # Hard coded column names. Don't imagine ff_glimpse() changing
                colnames = c("", "N", "Missing N", "Missing %", "Mean", "SD", "Min", "25% quartile", "Median", "75% quartile", "Max"),
                options = list(dom = 't', 
                               scrollX = TRUE, 
                               paging=FALSE,
                               fixedColumns = list(leftColumns = 1, rightColumns = 0),
                               searching = FALSE,
                               columnDefs = list(list(className = 'dt-right', targets = 2:glimpse1_column_n()-1))
                )
  )
})

output$output_dataTable_text = renderDataTable({
  
  DT::datatable(glimpse.out()[[2]],
                rownames=FALSE, extensions = "FixedColumns", 
                colnames = c("", "N", "Missing N", "Missing %", "Levels N", "Levels", "Levels Count", "Levels %"),
                options = list(dom = 't', 
                               scrollX = TRUE, 
                               paging=FALSE,
                               fixedColumns = list(leftColumns = 1, rightColumns = 0),
                               searching = FALSE,
                               columnDefs = list(list(className = 'dt-right', targets = 2:glimpse2_column_n()-1))
                )
  )
})
# #mainly used to show is column is "num" or "str"
# output$output_dataTable_text <- renderPrint({str(df_to_use())})
# 
# #creates textoutput showing quick summary of data
# #mainly used to show 5 number summary/factoring
# output$output_data_summary <- renderPrint({summary(df_to_use())})

output$downloadData <- downloadHandler(
  filename = function(){
    paste(input$dataview_download_title, ".csv", sep = "")
  },
  content = function(file){
    write.csv(df_to_use(), file, row.names = FALSE)
    
  }
)



