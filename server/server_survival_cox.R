#server for Cox regression for survival

#updates initial var tab - time
observeEvent(input$checkbox, {
  updatePickerInput(session,
                    "COX_Select_time",
                    choices = input$checkbox)
})

#updates initial var tab - censoring variable
observeEvent(input$COX_Select_time, {
  updatePickerInput(session,
                    "COX_Select_censor",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$checkbox[!(input$checkbox %in% input$COX_Select_time)])
})

#updates initial var tab - covariates
observeEvent(input$COX_Select_censor, {
  updatePickerInput(session,
                    "COX_Select_covars",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$checkbox[!(input$checkbox %in% input$COX_Select_censor)])
})

#---------------------------------------------------------
observeEvent(input$COX_Select_covars, {
  updatePickerInput(session,
                    "COX_joinVar1",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$COX_Select_covars)
})

observeEvent(input$COX_Select_CategoricalVar, {
  original_choices = input$COX_Select_covars #original selected terms (these terms get fed into factor and multiply as well)
  factor_choices = vector() #initialize vector
  for(i in seq(length(input$COX_Select_CategoricalVar))){ #for loop to add as.factor to each cat var
    factor_choices = c(factor_choices, paste("as.factor(",input$COX_Select_CategoricalVar[i], ")", sep=""))
  }
  
  new_choices = original_choices[!original_choices %in% input$COX_Select_CategoricalVar]
  if(length(input$COX_Select_CategoricalVar > 0)){ #if any variables selected
    new_choices = c(new_choices, factor_choices) #add the factor choices to original
  }else{new_choices <- new_choices}
  
  updatePickerInput(session,
                    "COX_joinVar1",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = new_choices)
})

observeEvent(input$COX_Select_covars, {
  updatePickerInput(session,
                    "COX_joinVar2",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$COX_Select_covars)
})

observeEvent(input$COX_Select_CategoricalVar, {
  original_choices = input$COX_Select_covars #original selected terms (these terms get fed into factor and multiply as well)
  factor_choices = vector() #initialize vector
  for(i in seq(length(input$COX_Select_CategoricalVar))){ #for loop to add as.factor to each cat var
    factor_choices = c(factor_choices, paste("as.factor(",input$COX_Select_CategoricalVar[i], ")", sep=""))
  }
  
  new_choices = original_choices[!original_choices %in% input$COX_Select_CategoricalVar]
  if(length(input$COX_Select_CategoricalVar > 0)){ #if any variables selected
    new_choices = c(new_choices, factor_choices) #add the factor choices to original
  }else{new_choices <- new_choices}
  
  updatePickerInput(session,
                    "COX_joinVar2",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = new_choices)
})


#--------------------------------------------------
observeEvent(input$COX_Select_covars, {
  updatePickerInput(session,
                    "COX_Select_CategoricalVar",
                    choices = input$COX_Select_covars)
})

observeEvent(input$COX_Select_covars, {
  updateMultiInput(session,
                   "COX_multiInput_finalVars",
                   choices = my_COX_choices(),
                   selected=NULL)
})

observeEvent(input$COX_Select_CategoricalVar, {
  updateMultiInput(session,
                   "COX_multiInput_finalVars",
                   choices = my_COX_choices(),
                   selected=NULL)
})

#_____________________________________________


#This event adds multiply terms to a vector and stores those values as the output
#ex.  selectinput1: Var1, selectInput2: Var2. Press Button -> Result = "Var1*Var2"
COX_multiply_terms <- eventReactive(input$COX_join_together_button, {
  x1 = input$COX_joinVar1 #set x1 var
  x2 = input$COX_joinVar2 #set x2 var
  new_term = paste(x1, "*", x2, sep="") #paste values "x1*x2"
  values_to_multiply_COX <<- append(values_to_multiply_COX, new_term) #add string to vector
  return(values_to_multiply_COX)
})

#This is a variable to get around the eventReactive in MLR_multiply_terms
#If I use MLR_multipl_terms directly in my_MLR_choices, the multiINput will not generate until a button is pressed
#which is unideal.  This gets around that by createding a variable outside of that button press and takes its value if pressed
vector_of_multipled_terms_to_transfer_COX <- reactive({
  if(input$COX_join_together_button==0){return(FALSE)} #if multiply button has not been pressed return false
  else{return(COX_multiply_terms())}  #if button has been pressed, takes form of multiply terms
})

#This function provides the final output of values for MLR to be chosen in the multiinput
#It takes original selection, multiplied terms, and as.factor terms.
my_COX_choices <- reactive({
  req(input$COX_Select_covars) #requires the first variable to be created
  #print(dbListTables(my_db())[!(dbListTables(my_db()) %in% input$join_first_table)])
  original_choices = input$COX_Select_covars #original selected terms (these terms get fed into factor and multiply as well)
  factor_choices = vector() #initialize vector
  for(i in seq(length(input$COX_Select_CategoricalVar))){ #for loop to add as.factor to each cat var
    factor_choices = c(factor_choices, paste("as.factor(",input$COX_Select_CategoricalVar[i], ")", sep=""))
  }
  #next block binds original choices to as.factor choices
  #this first line removes any repeats that have been factored
  #ex. choices = "var1, var2", var1 one selected as factor.  new_choices = "as.factor(var1), var2"
  new_choices = original_choices[!original_choices %in% input$COX_Select_CategoricalVar]
  if(length(input$COX_Select_CategoricalVar > 0)){ #if any variables selected
    new_choices = c(new_choices, factor_choices) #add the factor choices to original
  }else{new_choices <- new_choices}
  
  #this block adds the muliplied terms to the final list
  #ex. choices = "var1, var2, var3".  var1 and var2 selected to multiply.  newchoies = "var1, var2, var3, var1*var2"
  if(vector_of_multipled_terms_to_transfer_COX()!=FALSE){
    new_choices = c(new_choices, vector_of_multipled_terms_to_transfer_COX())
  }else{new_choices <- new_choices}
})


#______________________________________


#when perform regression button is pressed event triggers
#this event runs all the the math for the regression
COX_Output <- eventReactive(input$COX_run_button, {
  time = input$COX_Select_time # set first var y ~ stuff....
  censor = input$COX_Select_censor 
  covar = input$COX_multiInput_finalVars# "stuff" where it can be "x1 + x2 + as.factor(x3) + x4*x5 + ..."
  #for loop to put X in a Pasteable formula form
  phrase = "" #initalize
  for(i in seq(length(covar))){
    if(i==length(covar)){
      phrase = paste(phrase, covar[i], sep="")
    }else(phrase = paste(phrase, covar[i], "+", sep=""))
  }
  COX_formula = paste("Surv(",time, ", ", censor, ")", "~", phrase, sep="") #paste together final formula for regression
  out <- coxph(as.formula(COX_formula), data=df_to_use()) #run cox regression
})

output$COX_code_Out <- renderPrint({
  time = input$COX_Select_time # set first var y ~ stuff....
  censor = input$COX_Select_censor 
  covar = input$COX_multiInput_finalVars# "stuff" where it can be "x1 + x2 + as.factor(x3) + x4*x5 + ..."
  #for loop to put X in a Pasteable formula form
  phrase = "" #initalize
  for(i in seq(length(covar))){
    if(i==length(covar)){
      phrase = paste(phrase, covar[i], sep="")
    }else(phrase = paste(phrase, covar[i], "+", sep=""))
  }
  COX_formula = paste("coxph(Surv(",time, ", ", censor, ")", "~", phrase, ")", sep="")
  print(COX_formula)
})

#prints MLR summary to text output
output$COX_summary <- renderPrint({
  req(COX_Output())
  summary(COX_Output())
})

