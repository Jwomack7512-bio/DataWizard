#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

# Wilcox SERVER
# WILCOX000

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#updates wilcox values to match datamanagement tab
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "wilcox_input1",
                    choices = input$checkbox)
})

#updates wilcox values to match datamanagement tab
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "wilcox_input2",
                    choices = input$checkbox)
})

#updates wilcox group values (numerical) to match datamanagement tab
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "wilcox_group_numerical",
                    choices = input$checkbox)
})

#updates wilcox group values (categorical) to match datamanagement tab
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "wilcox_group_categorical",
                    choices = input$checkbox)
})


#output of wilcox results
output$wilcox_results <- renderPrint({
  
  #if numerical option is selected data is two numerical columns
  if(input$wilcox_data_import_options=='num_col'){
    #runs wilcox test for indepent sample
    if(input$wilcox_test_options == "independent"){
      #load data into variables
      x = df_to_use()[[input$wilcox_input1]]
      y=df_to_use()[[input$wilcox_input2]]
      #run independent test
      test <- wilcox.test(x, y, paired=FALSE)
    }else if(input$wilcox_test_options =="paired"){
      x = df_to_use()[[input$wilcox_input1]]
      y=df_to_use()[[input$wilcox_input2]]
      #run independent test
      test <- wilcox.test(x, y, paired=TRUE)}
  }
  #if grouped option is selected runs data based on a grouped column and numerical
  else if(input$wilcox_data_import_options=="group_col"){
    if(input$wilcox_test_options == "independent"){
      #load data into variables
      num_var = df_to_use()[[input$wilcox_group_numerical]]
      cat_var =df_to_use()[[input$wilcox_group_categorical]]
      #run independent test
      test <- wilcox.test(num_var~cat_var, paired=FALSE)
    }else if(input$wilcox_test_options =="paired"){
      num_var = df_to_use()[[input$wilcox_group_numerical]]
      cat_var =df_to_use()[[input$wilcox_group_categorical]]
      #run independent test
      test <- wilcox.test(num_var~cat_var, paired=TRUE)}
  }
  
  
  #print results of test to user
  print(test)
})


#outputs boxplot for wilcox data
output$wilcox_boxplot <- renderPlot({
  if(input$wilcox_data_import_options=='num_col'){
    #load data into variables
    x = df_to_use()[[input$wilcox_input1]]
    y=df_to_use()[[input$wilcox_input2]]
    #plot boxplot
    boxplot(x,y)
  }else if(input$wilcox_data_import_options=="group_col"){
    num_var = df_to_use()[[input$wilcox_group_numerical]]
    cat_var =df_to_use()[[input$wilcox_group_categorical]]
    boxplot(num_var~cat_var)
  }
})

