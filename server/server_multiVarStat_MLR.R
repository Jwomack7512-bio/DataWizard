
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

# MLR000 - Multiple Linear Regression Server

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#updates initial var tab
observeEvent(input$checkbox, {
  updatePickerInput(session,
                    "MLR_Select_D",
                    choices = input$checkbox)
})

#updates multi var select in MLR
observeEvent(input$MLR_Select_D, {
  updatePickerInput(session,
                    "MLR_Select_multVars",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$checkbox[!(input$checkbox %in% input$MLR_Select_D)])
})

observeEvent(input$MLR_Select_multVars, {
  
  updatePickerInput(session,
                    "MLR_joinVar1",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$MLR_Select_multVars)
})

observeEvent(input$MLR_Select_CategoricalVar, {
  original_choices = input$MLR_Select_multVars #original selected terms (these terms get fed into factor and multiply as well)
  factor_choices = vector() #initialize vector
  for(i in seq(length(input$MLR_Select_CategoricalVar))){ #for loop to add as.factor to each cat var
    factor_choices = c(factor_choices, paste("as.factor(",input$MLR_Select_CategoricalVar[i], ")", sep=""))
  }
  
  new_choices = original_choices[!original_choices %in% input$MLR_Select_CategoricalVar]
  if(length(input$MLR_Select_CategoricalVar > 0)){ #if any variables selected
    new_choices = c(new_choices, factor_choices) #add the factor choices to original
  }else{new_choices <- new_choices}
  
  updatePickerInput(session,
                    "MLR_joinVar1",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = new_choices)
})

observeEvent(input$MLR_Select_multVars, {
  updatePickerInput(session,
                    "MLR_joinVar2",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$MLR_Select_multVars)
})

observeEvent(input$MLR_Select_CategoricalVar, {
  original_choices = input$MLR_Select_multVars #original selected terms (these terms get fed into factor and multiply as well)
  factor_choices = vector() #initialize vector
  for(i in seq(length(input$MLR_Select_CategoricalVar))){ #for loop to add as.factor to each cat var
    factor_choices = c(factor_choices, paste("as.factor(",input$MLR_Select_CategoricalVar[i], ")", sep=""))
  }
  
  new_choices = original_choices[!original_choices %in% input$MLR_Select_CategoricalVar]
  if(length(input$MLR_Select_CategoricalVar > 0)){ #if any variables selected
    new_choices = c(new_choices, factor_choices) #add the factor choices to original
  }else{new_choices <- new_choices}
  
  updatePickerInput(session,
                    "MLR_joinVar2",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = new_choices)
})

observeEvent(input$MLR_Select_multVars, {
  updatePickerInput(session,
                    "MLR_Select_CategoricalVar",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$MLR_Select_multVars)
})

observeEvent(input$MLR_Select_multVars, {
  updateMultiInput(session,
                   "MLR_multiInput_finalVars",
                   #removes choice in anova_SELECT_D from inputs (basically removes the dependent var)
                   choices = my_MLR_choices(),
                   selected=NULL)
})

observeEvent(input$MLR_Select_CategoricalVar, {
  updateMultiInput(session,
                   "MLR_multiInput_finalVars",
                   #removes choice in anova_SELECT_D from inputs (basically removes the dependent var)
                   choices = my_MLR_choices(),
                   selected=NULL)
})

#This event adds multiply terms to a vector and stores those values as the output
#ex.  selectinput1: Var1, selectInput2: Var2. Press Button -> Result = "Var1*Var2"
MLR_multiply_terms <- eventReactive(input$MLR_join_together_button, {
  x1 = input$MLR_joinVar1 #set x1 var
  x2 = input$MLR_joinVar2 #set x2 var
  new_term = paste(x1, "*", x2, sep="") #paste values "x1*x2"
  values_to_multiply <<- append(values_to_multiply, new_term) #add string to vector
  return(values_to_multiply)
})

#This is a variable to get around the eventReactive in MLR_multiply_terms
#If I use MLR_multipl_terms directly in my_MLR_choices, the multiINput will not generate until a button is pressed
#which is unideal.  This gets around that by createding a variable outside of that button press and takes its value if pressed
vector_of_multipled_terms_to_transfer_MLR <- reactive({
  if(input$MLR_join_together_button==0){return(FALSE)} #if multiply button has not been pressed return false
  else{return(MLR_multiply_terms())}  #if button has been pressed, takes form of multiply terms
})

#This function provides the final output of values for MLR to be chosen in the multiinput
#It takes original selection, multiplied terms, and as.factor terms.
my_MLR_choices <- reactive({
  req(input$MLR_Select_multVars) #requires the first variable to be created
  #print(dbListTables(my_db())[!(dbListTables(my_db()) %in% input$join_first_table)])
  original_choices = input$MLR_Select_multVars #original selected terms (these terms get fed into factor and multiply as well)
  factor_choices = vector() #initialize vector
  for(i in seq(length(input$MLR_Select_CategoricalVar))){ #for loop to add as.factor to each cat var
    factor_choices = c(factor_choices, paste("as.factor(",input$MLR_Select_CategoricalVar[i], ")", sep=""))
  }
  
  #next block binds original choices to as.factor choices
  #this first line removes any repeats that have been factored
  #ex. choices = "var1, var2", var1 one selected as factor.  new_choices = "as.factor(var1), var2"
  new_choices = original_choices[!original_choices %in% input$MLR_Select_CategoricalVar]
  if(length(input$MLR_Select_CategoricalVar > 0)){ #if any variables selected
    new_choices = c(new_choices, factor_choices) #add the factor choices to original
  }else{new_choices <- new_choices}
  
  #this block adds the muliplied terms to the final list
  #ex. choices = "var1, var2, var3".  var1 and var2 selected to multiply.  newchoies = "var1, var2, var3, var1*var2"
  if(vector_of_multipled_terms_to_transfer_MLR()!=FALSE){
    new_choices = c(new_choices, vector_of_multipled_terms_to_transfer_MLR())
  }else{new_choices <- new_choices}
})

#when perform regression button is pressed event triggers
#this event runs all the the math for the regression
MLR_Output <- eventReactive(input$MLR_run_button, {
  y = input$MLR_Select_D # set first var y ~ stuff....
  X = input$MLR_multiInput_finalVars # "stuff" where it can be "x1 + x2 + as.factor(x3) + x4*x5 + ..."
  #for loop to put X in a Pasteable formula form
  phrase = "" #initalize
  for(i in seq(length(X))){
    if(i==length(X)){
      phrase = paste(phrase, X[i], sep="")
    }else(phrase = paste(phrase, X[i], "+", sep=""))
  }
  MLR_formula = paste(y, "~", phrase, sep="") #paste together final formula for regression
  out <- lm(as.formula(MLR_formula), data=df_to_use()) #run multi Linear regression
})

#this prints the "lm(y~x)" statement to a text output for the user to see
output$MLR_code_Out <- renderPrint({
  #req(input$MLR_multiInput_finalVars)
  req(input$MLR_Select_multVars)
  y = input$MLR_Select_D # set first var y ~ stuff....
  X = input$MLR_multiInput_finalVars # "stuff" where it can be "x1 + x2 + as.factor(x3) + x4*x5 + ..."
  #for loop to put X in a Pasteable formula form
  phrase = "" #initalize
  for(i in seq(length(X))){
    if(i==length(X)){
      phrase = paste(phrase, X[i], sep=" ")
    }else(phrase = paste(phrase, X[i], "+", sep=" "))
  }
  MLR_formula = paste("lm(", paste(y, "~", phrase, sep=" "), ")", sep="")
  print(MLR_formula)
})

#prints MLR summary to text output
output$MLR_summary <- renderPrint({
  req(MLR_Output())
  summary(MLR_Output())
})

output$MLR_Plots <- renderPlot({
  req(MLR_Output())
  layout(matrix(c(1, 2, 3, 4), 2, 2))
  #par(mfrow=c(4,1))
  plot(MLR_Output())
}, height=800
)

output$MLR_ResidualPlot <- renderPlot({
  req(MLR_Output())
  plot(MLR_Output()$fitted.values,
       MLR_Output()$residuals,
       xlab="Fitted Values",
       ylab= "Residuals")
  abline(0,0)
  
})

output$MLR_fittedPlot <- renderPlot({
  req(MLR_Output())
  plot(df_to_use()[[input$MLR_Select_D]],
       MLR_Output()$fitted.values,
       xlab="Y_observed",
       ylab="Fitted Values")
  abline(0,1)
  
})

output$MLR_OR_Plot <- renderPlot({
  req(MLR_Output())
  dep = input$MLR_Select_D
  exp = input$MLR_multiInput_finalVars
  out <- finalfit::ff_plot(df_to_use(), dependent=dep, explanatory = exp)
})

MLR_summaryFactorList <- eventReactive(input$MLR_run_button, {
  dep = input$MLR_Select_D
  exp = input$MLR_multiInput_finalVars
  df.out = df_to_use() %>%
    do(
      finalfit::summary_factorlist(.data, dependent = dep, explanatory = exp)
    )
})

MLR_finalfit_LM_data <- eventReactive(input$MLR_run_button, {
  dep = input$MLR_Select_D
  exp = input$MLR_multiInput_finalVars
  df.out = df_to_use() %>%
    do(
      finalfit.lm(.data, dependent = dep, explanatory = exp)
    )
})

# #creates a DT style table for data.  One option of conditional panel
output$MLR_summaryFactorList <- renderDataTable({
  req(MLR_Output())
  DT::datatable(MLR_summaryFactorList(),
                options=list(
                  scroller = TRUE,
                  scrollX = TRUE,
                  scrollY = "500px"))
})

output$download_MLR_summaryFactorList_csv <- downloadHandler(
  filename = function(){"MLR_summaryFactor.csv"}, 
  content = function(filename){
    write.csv(MLR_summaryFactorList(), filename, row.names = FALSE)
  }
)

output$download_MLR_summaryFactorList_pdf <- downloadHandler(
  filename = function(){"MLR_summaryFactor.pdf"}, 
  content = function(filename){
    write.csv(MLR_summaryFactorList(), filename, row.names = FALSE)
  }
)

output$MLR_GtSummary_table <- gt::render_gt({
  req(MLR_Output())
  #table <- MLR_Output() %>% as_gt()
  t1 <- tbl_regression(MLR_Output()) %>% as_gt()
})

output$MLR_finalfit_LM <- renderDataTable({
  req(MLR_Output())
  DT::datatable(MLR_finalfit_LM_data(),
                options=list(
                  scroller = TRUE,
                  scrollX = TRUE,
                  scrollY = "500px"))
})