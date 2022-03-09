#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

# LOGREG000 - Logistic Regression Tab

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________


# #updates initial var tab
observeEvent(input$checkbox, {
  updatePickerInput(session,
                    "LogReg_Select_D",
                    choices = input$checkbox)
})

#updates multi var select in MLR
observeEvent(input$LogReg_Select_D, {
  updatePickerInput(session,
                    "LogReg_Select_multVars",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$checkbox[!(input$checkbox %in% input$LogReg_Select_D)])
})

observeEvent(input$LogReg_Select_multVars, {
  
  updatePickerInput(session,
                    "LogReg_joinVar1",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$LogReg_Select_multVars)
})

observeEvent(input$LogReg_Select_CategoricalVar, {
  original_choices = input$LogReg_Select_multVars #original selected terms (these terms get fed into factor and multiply as well)
  factor_choices = vector() #initialize vector
  for(i in seq(length(input$LogReg_Select_CategoricalVar))){ #for loop to add as.factor to each cat var
    factor_choices = c(factor_choices, paste("as.factor(",input$LogReg_Select_CategoricalVar[i], ")", sep=""))
  }
  
  new_choices = original_choices[!original_choices %in% input$LogReg_Select_CategoricalVar]
  if(length(input$LogReg_Select_CategoricalVar > 0)){ #if any variables selected
    new_choices = c(new_choices, factor_choices) #add the factor choices to original
  }else{new_choices <- new_choices}
  
  updatePickerInput(session,
                    "LogReg_joinVar1",
                    #removes choice in LogReg_SELECT_D from inputs (basically removes the dependent var)
                    choices = new_choices)
})

observeEvent(input$LogReg_Select_multVars, {
  updatePickerInput(session,
                    "LogReg_joinVar2",
                    #removes choice in LogReg_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$LogReg_Select_multVars)
})

observeEvent(input$LogReg_Select_CategoricalVar, {
  original_choices = input$LogReg_Select_multVars #original selected terms (these terms get fed into factor and multiply as well)
  factor_choices = vector() #initialize vector
  for(i in seq(length(input$LogReg_Select_CategoricalVar))){ #for loop to add as.factor to each cat var
    factor_choices = c(factor_choices, paste("as.factor(",input$LogReg_Select_CategoricalVar[i], ")", sep=""))
  }
  
  new_choices = original_choices[!original_choices %in% input$LogReg_Select_CategoricalVar]
  if(length(input$LogReg_Select_CategoricalVar > 0)){ #if any variables selected
    new_choices = c(new_choices, factor_choices) #add the factor choices to original
  }else{new_choices <- new_choices}
  
  updatePickerInput(session,
                    "LogReg_joinVar2",
                    #removes choice in LogReg_SELECT_D from inputs (basically removes the dependent var)
                    choices = new_choices)
})

observeEvent(input$LogReg_Select_multVars, {
  updatePickerInput(session,
                    "LogReg_Select_CategoricalVar",
                    #removes choice in LogReg_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$LogReg_Select_multVars)
})

observeEvent(input$LogReg_Select_multVars, {
  updateMultiInput(session,
                   "LogReg_multiInput_finalVars",
                   #removes choice in anova_SELECT_D from inputs (basically removes the dependent var)
                   choices = my_LogReg_choices(),
                   selected=NULL)
})

observeEvent(input$LogReg_Select_CategoricalVar, {
  updateMultiInput(session,
                   "LogReg_multiInput_finalVars",
                   #removes choice in anova_SELECT_D from inputs (basically removes the dependent var)
                   choices = my_LogReg_choices(),
                   selected=NULL)
})

#Creates multiINput in "final selection" to allow users to pick final elements for MLR
# output$LogReg_multiInput_finalVars = renderUI(
#   multiInput(inputId = "LogReg_multiInput_finalVars",
#              label = "Select Final Variables for LogReg",
#              selected = "",
#              choices = my_LogReg_choices(),
#              options = list(
#                enable_search = FALSE,
#                non_selected_header = "Add To Model:",
#                selected_header = "In Model:"))
# )

#This event adds multiply terms to a vector and stores those values as the output
#ex.  selectinput1: Var1, selectInput2: Var2. Press Button -> Result = "Var1*Var2"
LogReg_multiply_terms <- eventReactive(input$LogReg_join_together_button, {
  x1 = input$LogReg_joinVar1 #set x1 var
  x2 = input$LogReg_joinVar2 #set x2 var
  new_term = paste(x1, "*", x2, sep="") #paste values "x1*x2"
  values_to_multiply_LogReg <<- append(values_to_multiply_LogReg, new_term) #add string to vector
  return(values_to_multiply_LogReg)
})

#This is a variable to get around the eventReactive in LogReg_multiply_terms
#If I use LogReg_multipl_terms directly in my_LogReg_choices, the multiINput will not generate until a button is pressed
#which is unideal.  This gets around that by createding a variable outside of that button press and takes its value if pressed
vector_of_multipled_terms_to_transfer_LogReg <- reactive({
  if(input$LogReg_join_together_button==0){return(FALSE)} #if multiply button has not been pressed return false
  else{return(LogReg_multiply_terms())}  #if button has been pressed, takes form of multiply terms
})

#This function provides the final output of values for LogReg to be chosen in the multiinput
#It takes original selection, multiplied terms, and as.factor terms.
my_LogReg_choices <- reactive({
  req(input$LogReg_Select_multVars) #requires the first variable to be created
  #print(dbListTables(my_db())[!(dbListTables(my_db()) %in% input$join_first_table)])
  original_choices = input$LogReg_Select_multVars #original selected terms (these terms get fed into factor and multiply as well)
  factor_choices = vector() #initialize vector
  for(i in seq(length(input$LogReg_Select_CategoricalVar))){ #for loop to add as.factor to each cat var
    factor_choices = c(factor_choices, paste("as.factor(",input$LogReg_Select_CategoricalVar[i], ")", sep=""))
  }
  
  #next block binds original choices to as.factor choices
  #this first line removes any repeats that have been factored
  #ex. choices = "var1, var2", var1 one selected as factor.  new_choices = "as.factor(var1), var2"
  new_choices = original_choices[!original_choices %in% input$LogReg_Select_CategoricalVar]
  if(length(input$LogReg_Select_CategoricalVar > 0)){ #if any variables selected
    new_choices = c(new_choices, factor_choices) #add the factor choices to original
  }else{new_choices <- new_choices}
  
  #this block adds the muliplied terms to the final list
  #ex. choices = "var1, var2, var3".  var1 and var2 selected to multiply.  newchoies = "var1, var2, var3, var1*var2"
  if(vector_of_multipled_terms_to_transfer_LogReg()!=FALSE){
    new_choices = c(new_choices, vector_of_multipled_terms_to_transfer_LogReg())
  }else{new_choices <- new_choices}
})

#when perform regression button is pressed event triggers
#this event runs all the the math for the regression
LogReg_Output <- eventReactive(input$LogReg_run_button, {
  y = input$LogReg_Select_D # set first var y ~ stuff....
  X = input$LogReg_multiInput_finalVars # "stuff" where it can be "x1 + x2 + as.factor(x3) + x4*x5 + ..."
  #for loop to put X in a Pasteable formula form
  phrase = "" #initalize
  for(i in seq(length(X))){
    if(i==length(X)){
      phrase = paste(phrase, X[i], sep="")
    }else(phrase = paste(phrase, X[i], "+", sep=""))
  }
  LogReg_formula = paste(y, "~", phrase, sep="") #paste together final formula for regression
  out <- glm(as.formula(LogReg_formula), family= binomial, data=df_to_use()) #run multi Linear regression
})

#this prints the "lm(y~x)" statement to a text output for the user to see
output$LogReg_code_Out <- renderPrint({
  #req(input$LogReg_multiInput_finalVars)
  req(input$LogReg_Select_multVars)
  y = input$LogReg_Select_D # set first var y ~ stuff....
  X = input$LogReg_multiInput_finalVars # "stuff" where it can be "x1 + x2 + as.factor(x3) + x4*x5 + ..."
  #for loop to put X in a Pasteable formula form
  phrase = "" #initalize
  for(i in seq(length(X))){
    if(i==length(X)){
      phrase = paste(phrase, X[i], sep=" ")
    }else(phrase = paste(phrase, X[i], "+", sep=" "))
  }
  LogReg_formula = paste("glm(", paste(y, "~", phrase, sep=" "), ")", sep="")
  print(LogReg_formula)
})

#prints LogReg summary to text output
output$LogReg_summary <- renderPrint({
  req(LogReg_Output)
  summary(LogReg_Output())
})

output$LogReg_odds_ratio <- renderPrint({
  req(LogReg_Output)
  exp(cbind("Odds ratio" = coef(LogReg_Output()), confint.default(LogReg_Output(), level = 0.95)))
  
})

output$LogReg_Plots <- renderPlot({
  req(LogReg_Output())
  layout(matrix(c(1, 2, 3, 4), 2, 2))
  #par(mfrow=c(4,1))
  plot(LogReg_Output())
}, height=800
)

output$LogReg_summaryFactorList_OR_Plot <- renderPlot({
  req(LogReg_Output())
  dep = input$LogReg_Select_D
  exp = input$LogReg_multiInput_finalVars
  out <- finalfit::ff_plot(df_to_use(), dependent = dep, explanatory = exp)
})

output$LogReg_GtSummary_table <- gt::render_gt({
  req(LogReg_Output())
  #table <- MLR_Output() %>% as_gt()
  t1 <- tbl_regression(LogReg_Output(), exponentiate = TRUE) %>% as_gt()
})

LogReg_summaryFactorList <- eventReactive(input$LogReg_run_button, {
  dep = input$LogReg_Select_D
  exp = input$LogReg_multiInput_finalVars
  df.out = df_to_use() %>%
    do(
      finalfit::summary_factorlist(.data, dependent = dep, explanatory = exp)
    )
})

LogReg_finalfit_data <- eventReactive(input$LogReg_run_button, {
  dep = input$LogReg_Select_D
  exp = input$LogReg_multiInput_finalVars
  df.out = df_to_use() %>%
    do(
      finalfit.glm(.data, dependent = dep, explanatory = exp)
    )
})

# #creates a DT style table for data.  One option of conditional panel
output$LogReg_summaryFactorList <- renderDataTable({
  req(LogReg_Output())
  DT::datatable(LogReg_summaryFactorList(),
                options=list(
                  scroller = TRUE,
                  scrollX = TRUE,
                  scrollY = "500px"))
})

output$download_LogReg_summaryFactorList_csv <- downloadHandler(
  filename = function(){"GLM_summaryFactor.csv"}, 
  content = function(filename){
    write.csv(LogReg_summaryFactorList(), filename, row.names = FALSE)
  }
)

output$download_LogReg_summaryFactorList_pdf <- downloadHandler(
  filename = function(){"GLM_summaryFactor.pdf"}, 
  content = function(filename){
    write.csv(LogReg_summaryFactorList(), filename, row.names = FALSE)
  }
)

output$LogReg_summaryFactorList_finalfit <- renderDataTable({
  req(LogReg_Output())
  DT::datatable(LogReg_finalfit_data(),
                options=list(
                  scroller = TRUE,
                  scrollX = TRUE,
                  scrollY = "500px"))
})
