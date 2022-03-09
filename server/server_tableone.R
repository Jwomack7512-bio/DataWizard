
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
#**********************************************************************************************************

#TABITEM: TableOne - Table Statistics
# TABLEONE000

#**********************************************************************************************************
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################

#changes the picker in UI to have select options for all column names to allow user to select which categorical variables there are
observeEvent(input$checkbox, {
  updatePickerInput(session, "TABLEONE_CHOOSE_CATEGORICAL", choices = input$checkbox, selected=NULL)
})

#updates selectInput to df_to_use() options so user can select a strata variable if desired
observeEvent(input$TABLEONE_CHOOSE_CATEGORICAL, {
  updatePickerInput(session, "TABLEONE_CHOOSE_STRATA", choices = input$TABLEONE_CHOOSE_CATEGORICAL, selected=NULL)
})

observeEvent(input$TABLEONE_CHOOSE_CATEGORICAL, {
  updatePickerInput(session,
                    "TABLEONE_CHOOSE_NONNORMAL",
                    #this removes all the categorical values from the total list and only displays the continuous ones
                    choices = c(setdiff(input$TABLEONE_CHOOSE_CATEGORICAL, input$checkbox), setdiff(input$checkbox, input$TABLEONE_CHOOSE_CATEGORICAL)),
                    selected=NULL)
})
# tbl1 <- reactive({
#
#   catigorical_variables <- input$TABLEONE_CHOOSE_CATEGORICAL
#   strata_variable <- input$TABLEONE_CHOOSE_STRATA
#   if(input$TABLEONE_ACTIVATE_STRATA){
#     tbl1<- CreateTableOne(data = df_to_use(), factorVars = catigorical_variables, strata=strata_variable)
#   }else{tbl1 <- CreateTableOne(data=df_to_use(), factorVars = catigorical_variables)}
# })
#Creates the output summary in print form for tableone info (contains functions to change its output)
output$tableone_summary_output <-renderPrint({
  
  #allows user to select categorical variables based on the pickerinput and stores to variable
  catigorical_variables <- input$TABLEONE_CHOOSE_CATEGORICAL
  
  #stores strata variable from user select input
  strata_variable <- input$TABLEONE_CHOOSE_STRATA
  
  nonnormal_variables <- input$TABLEONE_CHOOSE_NONNORMAL
  
  
  if(input$TABLEONE_ACTIVATE_STRATA){
    tableone_output <- CreateTableOne(data = df_to_use(), factorVars = catigorical_variables, strata=strata_variable)
  }else{tableone_output <- CreateTableOne(data=df_to_use(), factorVars = catigorical_variables)}
  
  
  if(input$TABLEONE_ACTIVATE_SUBCLASSES == "Categorical Variables"){
    if(input$TABLEONE_SUMMARY_OPTIONS == "mean/sd"){
      print(tableone_output$CatTable)}
    else if(input$TABLEONE_SUMMARY_OPTIONS == "summary"){
      print(summary(tableone_output$CatTable))
    }}
  
  else if(input$TABLEONE_ACTIVATE_SUBCLASSES == "Continuous Variables"){
    if(input$TABLEONE_SUMMARY_OPTIONS == "mean/sd"){
      print(tableone_output$ContTable, nonnormal = nonnormal_variables)}
    else if(input$TABLEONE_SUMMARY_OPTIONS == "summary"){
      print(summary(tableone_output$ContTable))
    }}
  
  else{if(input$TABLEONE_SUMMARY_OPTIONS == "mean/sd"){
    print(tableone_output, nonnormal = nonnormal_variables)}
    else if(input$TABLEONE_SUMMARY_OPTIONS == "summary"){
      print(summary(tableone_output))
    }}
  #outputs to text the short output (mean/sd) or full summary depending on users needs/wants
  
  
  # output$tableone_sumamry_rhandsomeTable <- renderRHandsontable({
  #   if(input$TABLEONE_SUMMARY_OPTIONS == "mean/sd"){
  #     rhandsontable(print(tab1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE), readOnly=TRUE, contextMenu = FALSE)}
  #   else if(input$TABLEONE_SUMMARY_OPTIONS == "summary"){
  #     rhandsontable(print(summary(tab1), quote = FALSE, noSpaces = TRUE, printToggle = FALSE), readOnly=TRUE, contextMenu = FALSE)
  #     }
  #
  # })
  
})
