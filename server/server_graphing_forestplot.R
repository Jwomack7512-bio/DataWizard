#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#SUBTAB:  FOREST000 - FORESTPLOT
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#updates variables for all OR to use
observeEvent(input$checkbox, {
  updatePickerInput(session,
                    "forestplot_OR_select_variables",
                    choices = input$checkbox)
})

#updates btext based options based on OR select variables
observeEvent(input$forestplot_OR_select_variables, {
  updatePickerInput(session,
                    "forestplot_OR_select_text",
                    choices = input$forestplot_OR_select_variables,
                    selected = input$forestplot_OR_select_variables)
})

#updates numerical and selected variables in OR select var that aren't in OR_select_text
observeEvent(input$forestplot_OR_select_text, {
  updatePickerInput(session,
                    "forestplot_OR_select_num",
                    choices = input$forestplot_OR_select_variables,
                    #set diff takes the two difference between these two var and selects in for this option
                    selected=setdiff(input$forestplot_OR_select_variables, input$forestplot_OR_select_text) )
})

observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "forestplot_RAW_indepVar",
                    choices = input$checkbox,
                    #set diff takes the two difference between these two var and selects in for this option
                    selected=input$checkbox )
})

observeEvent(input$forestplot_RAW_indepVar, {
  updatePickerInput(session,
                    "forestplot_RAW_dependentVar",
                    choices = setdiff(input$checkbox,input$forestplot_RAW_indepVar))
})


# observe({
#   req(input$forestplot_RAW_dependentVar)
#   #print(glm(reformulate(input$forestplot_RAW_indepVar, input$forestplot_RAW_dependentVar), data = df_to_use()))
#   print(glm(as.formula(paste(input$forestplot_RAW_indepVar,'~', paste(input$forestplot_RAW_dependentVar, collapse = "+"))), data=df_to_use(), family=binomial()))
#   })

plotForestplotInput_from_OR <- function(){
  req(input$forestplot_OR_select_num)
  
  df1<-df_to_use()[input$forestplot_OR_select_text]
  df2<- df_to_use()[input$forestplot_OR_select_num]
  
  forestplot::forestplot(df1,df2,new_page = TRUE,
                         boxsize = 0.25, xlab="Adjusted Odds Ratio", lty.ci=1, xlog=T, zero=c(0.06, 1),
                         col=fpColors(box="black",line="black", summary="royalblue"),
                         txt_gp = fpTxtGp(label = list(gpar(fontfamily = "", fontface="bold.italic", cex=1.1),
                                                       gpar(fontfamily = ""), gpar(fontfamily = "")),
                                          ticks = gpar(fontfamily = "", fontface="plain",cex=1),
                                          xlab  = gpar(fontfamily = "", fontface="plain", cex = 1.2)))
}

plotForestplotInput_from_RawData <- function(){
  req(input$forestplot_RAW_dependentVar)
  
  d=input$forestplot_RAW_dependentVar
  #run regression model
  for(i in seq(length(d))){
    #print(i)
    if(i == 1){
      phrase = paste("factor(", d[i], ")", sep="")
    }else{
      phrase = paste(phrase, paste("+factor(", d[i], ")", sep=""), sep="")
    }
  }
  
  #reg<-glm(as.formula(paste(input$forestplot_RAW_indepVar,'~', paste(input$forestplot_RAW_dependentVar, collapse = "+"))), data=df_to_use())
  reg<-glm(as.formula(paste(input$forestplot_RAW_indepVar,'~', phrase, sep="")), data=df_to_use())
  
  #extract odds ratio
  odds.ratio <- round(exp(reg$coef), 2)[-1]  # this is correct output
  #extract confidence intervals
  CI <- round(exp(confint.default(reg,level = 0.90)), 2)[-1,]  # this is correct output
  #extract row names
  name.OR <- row.names(CI)
  #store total number of variables
  K<- length(name.OR) # number of total variables
  
  #### simple table ####
  
  tabletext <- matrix(NA, K, ncol=2)
  OR.table <- matrix(NA, K, ncol=3)
  
  for (i in 1:K) {
    tabletext[i,1] <- name.OR[i]
    tabletext[i,2] <- paste(odds.ratio[i]," (", CI[i,1], " - ", CI[i,2], ")", sep="")
    OR.table[i,1] <- odds.ratio[i]
    OR.table[i, 2:3] <- CI[i,]
  }
  
  #uses forestplot from the forestplot package.  Otherwise it runs something else that doesn't work
  forestplot::forestplot(tabletext,
                         OR.table,new_page = TRUE,
                         boxsize = 0.25, xlab="Adjusted Odds Ratio", lty.ci=1, xlog=T, zero=c(0.35, 1),  ## need to be able to adjust the range of "zeros"
                         col=fpColors(box="black",line="black", summary="royalblue"),
                         txt_gp = fpTxtGp(label = list(gpar(fontfamily = "", fontface="bold.italic", cex=1.1),
                                                       gpar(fontfamily = ""), gpar(fontfamily = "")),
                                          ticks = gpar(fontfamily = "", fontface="plain",cex=1),
                                          xlab  = gpar(fontfamily = "", fontface="plain", cex = 1.2))
  )
}


#plots data when button is pressed
observeEvent(input$forest_plot_button, {
  output$ForestPlot <- renderPlot({
    #plots OR option when data is imported in table form
    if(input$forest_data_load_options == 'OR'){
      print(plotForestplotInput_from_OR())
    }
    #plots table if data in raw form
    else if(input$forest_data_load_options == "RAW"){
      print(plotForestplotInput_from_RawData())
    }
  })
})

# output$multiview_tables <- renderUI({
#   req(input$multiView_selectTables)
#
#   out <- unlist(tableize(input$multiView_selectTables, input$multiView_patient_ID))
#   return(div(style="overflow-x:scroll",HTML(out),class="shiny-html-output"))
# })

#this outputs the multi gression table in a html output table generated by sjTable
output$forest_html_multi_regression_table <- renderUI({
  req(input$forestplot_RAW_dependentVar) #require dependent variable to be chosen so error not thrown
  #run linear model using a paste and as.formula to generate formula model
  reg <- glm(as.formula(paste(input$forestplot_RAW_indepVar,'~', paste(input$forestplot_RAW_dependentVar, collapse = "+"))), data=df_to_use())
  #out <- unlist(tab_model(reg))
  #tab_model stores regression model as a list of different html models
  out <- tab_model(reg)
  #if ran without extracting this element, then three tables are plotted
  out <- out$page.complete
  #returns html output in a div
  return(div(HTML(out), class="shiny-html-output"))
})