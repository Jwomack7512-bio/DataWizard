# #__________________________________________________________________________________________________________
# #__________________________________________________________________________________________________________
# #__________________________________________________________________________________________________________
# #__________________________________________________________________________________________________________
#
# # ANOVA000 - Anova Tab
#
# #__________________________________________________________________________________________________________
# #__________________________________________________________________________________________________________
# #__________________________________________________________________________________________________________
# #__________________________________________________________________________________________________________
#
#updates initial var tab
observeEvent(input$checkbox, {
  updatePickerInput(session,
                    "anova_Select_D",
                    choices = input$checkbox)
})

#updates multi var select in MLR
observeEvent(input$anova_Select_D, {
  updatePickerInput(session,
                    "anova_Select_multVars",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$checkbox[!(input$checkbox %in% input$anova_Select_D)])
})

observeEvent(input$anova_Select_multVars, {
  
  updatePickerInput(session,
                    "anova_joinVar1",
                    #removes choice in MLR_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$anova_Select_multVars)
})

observeEvent(input$anova_Select_CategoricalVar, {
  original_choices = input$anova_Select_multVars #original selected terms (these terms get fed into factor and multiply as well)
  factor_choices = vector() #initialize vector
  for(i in seq(length(input$anova_Select_CategoricalVar))){ #for loop to add as.factor to each cat var
    factor_choices = c(factor_choices, paste("as.factor(",input$anova_Select_CategoricalVar[i], ")", sep=""))
  }
  
  new_choices = original_choices[!original_choices %in% input$anova_Select_CategoricalVar]
  if(length(input$anova_Select_CategoricalVar > 0)){ #if any variables selected
    new_choices = c(new_choices, factor_choices) #add the factor choices to original
  }else{new_choices <- new_choices}
  
  updatePickerInput(session,
                    "anova_joinVar1",
                    #removes choice in anova_SELECT_D from inputs (basically removes the dependent var)
                    choices = new_choices)
})

observeEvent(input$anova_Select_multVars, {
  updatePickerInput(session,
                    "anova_joinVar2",
                    #removes choice in anova_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$anova_Select_multVars)
})

observeEvent(input$anova_Select_CategoricalVar, {
  original_choices = input$anova_Select_multVars #original selected terms (these terms get fed into factor and multiply as well)
  factor_choices = vector() #initialize vector
  for(i in seq(length(input$anova_Select_CategoricalVar))){ #for loop to add as.factor to each cat var
    factor_choices = c(factor_choices, paste("as.factor(",input$anova_Select_CategoricalVar[i], ")", sep=""))
  }
  
  new_choices = original_choices[!original_choices %in% input$anova_Select_CategoricalVar]
  if(length(input$anova_Select_CategoricalVar > 0)){ #if any variables selected
    new_choices = c(new_choices, factor_choices) #add the factor choices to original
  }else{new_choices <- new_choices}
  
  updatePickerInput(session,
                    "anova_joinVar2",
                    #removes choice in anova_SELECT_D from inputs (basically removes the dependent var)
                    choices = new_choices)
})

observeEvent(input$anova_Select_multVars, {
  updatePickerInput(session,
                    "anova_Select_CategoricalVar",
                    #removes choice in anova_SELECT_D from inputs (basically removes the dependent var)
                    choices = input$anova_Select_multVars)
})

observeEvent(input$anova_Select_multVars, {
  updateMultiInput(session,
                   "anova_multiInput_finalVars",
                   #removes choice in anova_SELECT_D from inputs (basically removes the dependent var)
                   choices = my_anova_choices(),
                   selected=NULL)
})

observeEvent(input$anova_Select_CategoricalVar, {
  updateMultiInput(session,
                   "anova_multiInput_finalVars",
                   #removes choice in anova_SELECT_D from inputs (basically removes the dependent var)
                   choices = my_anova_choices(),
                   selected=NULL)
})

#Creates multiINput in "final selection" to allow users to pick final elements for MLR
# output$anova_multiInput_finalVars = renderUI(
#   multiInput(inputId = "anova_multiInput_finalVars",
#              label = "Select Final Variables for anova",
#              selected = "",
#              choices = my_anova_choices(),
#              options = list(
#                enable_search = FALSE,
#                non_selected_header = "Add To Model:",
#                selected_header = "In Model:"))
# )

#This event adds multiply terms to a vector and stores those values as the output
#ex.  selectinput1: Var1, selectInput2: Var2. Press Button -> Result = "Var1*Var2"
anova_multiply_terms <- eventReactive(input$anova_join_together_button, {
  x1 = input$anova_joinVar1 #set x1 var
  x2 = input$anova_joinVar2 #set x2 var
  new_term = paste(x1, "*", x2, sep="") #paste values "x1*x2"
  values_to_multiply_anova <<- append(values_to_multiply_anova, new_term) #add string to vector
  return(values_to_multiply_anova)
})

#This is a variable to get around the eventReactive in anova_multiply_terms
#If I use anova_multipl_terms directly in my_anova_choices, the multiINput will not generate until a button is pressed
#which is unideal.  This gets around that by createding a variable outside of that button press and takes its value if pressed
vector_of_multipled_terms_to_transfer_anova <- reactive({
  if(input$anova_join_together_button==0){return(FALSE)} #if multiply button has not been pressed return false
  else{return(anova_multiply_terms())}  #if button has been pressed, takes form of multiply terms
})

#This function provides the final output of values for anova to be chosen in the multiinput
#It takes original selection, multiplied terms, and as.factor terms.
my_anova_choices <- reactive({
  req(input$anova_Select_multVars) #requires the first variable to be created
  #print(dbListTables(my_db())[!(dbListTables(my_db()) %in% input$join_first_table)])
  original_choices = input$anova_Select_multVars #original selected terms (these terms get fed into factor and multiply as well)
  factor_choices = vector() #initialize vector
  for(i in seq(length(input$anova_Select_CategoricalVar))){ #for loop to add as.factor to each cat var
    factor_choices = c(factor_choices, paste("as.factor(",input$anova_Select_CategoricalVar[i], ")", sep=""))
  }
  
  #next block binds original choices to as.factor choices
  #this first line removes any repeats that have been factored
  #ex. choices = "var1, var2", var1 one selected as factor.  new_choices = "as.factor(var1), var2"
  new_choices = original_choices[!original_choices %in% input$anova_Select_CategoricalVar]
  if(length(input$anova_Select_CategoricalVar > 0)){ #if any variables selected
    new_choices = c(new_choices, factor_choices) #add the factor choices to original
  }else{new_choices <- new_choices}
  
  #this block adds the muliplied terms to the final list
  #ex. choices = "var1, var2, var3".  var1 and var2 selected to multiply.  newchoies = "var1, var2, var3, var1*var2"
  if(vector_of_multipled_terms_to_transfer_anova()!=FALSE){
    new_choices = c(new_choices, vector_of_multipled_terms_to_transfer_anova())
  }else{new_choices <- new_choices}
})

#
observeEvent(input$anova_run_button, {
  updateTabsetPanel(session,
                    "anova_tabbox",
                    selected = "ANOVA Table")
})
#when perform regression button is pressed event triggers
#this event runs all the the math for the regression
anova_Output <- eventReactive(input$anova_run_button, {
  
  y = input$anova_Select_D # set first var y ~ stuff....
  X = input$anova_multiInput_finalVars # "stuff" where it can be "x1 + x2 + as.factor(x3) + x4*x5 + ..."
  #for loop to put X in a Pasteable formula form
  phrase = "" #initalize
  for(i in seq(length(X))){
    if(i==length(X)){
      phrase = paste(phrase, X[i], sep="")
    }else(phrase = paste(phrase, X[i], "+", sep=""))
  }
  anova_formula = paste(y, "~", phrase, sep="") #paste together final formula for anova
  out <- aov(as.formula(anova_formula), data=df_to_use()) #run anova
})


get_factors <- reactive ({
  y = input$anova_Select_D # set first var y ~ stuff....
  X = input$anova_multiInput_finalVars # "stuff" where it can be "x1 + x2 + as.factor(x3) + x4*x5 + ..."
  #for loop to put X in a Pasteable formula form
  phrase = "" #initalize
  for(i in seq(length(X))){
    if(i==length(X)){
      phrase = paste(phrase, X[i], sep="")
    }else(phrase = paste(phrase, X[i], "+", sep=""))
  }
  f = paste(y, "~", phrase, sep="")
  factorstr <- as.character(formula(f))[3]
  return(sub("\\s","",unlist(strsplit(factorstr,"[*+:]"))))
})

output$anova_dataview <- renderPrint({
  req(input$checkbox)
  print(df_to_use())
}) #end output$anova_dataview

output$anova_table <- renderPrint({
  aov.model = anova_Output()
  print(aov.model)
  br()
  br()
  print(summary(aov.model))
  cat("Coefficients"); cat("\n")
  print(aov.model$coefficients)
  
  #BoxPlot Tab Server
  output$anova_boxPlot <- renderPlot({
    y = input$anova_Select_D # set first var y ~ stuff....
    X = input$anova_multiInput_finalVars # "stuff" where it can be "x1 + x2 + as.factor(x3) + x4*x5 + ..."
    #for loop to put X in a Pasteable formula form
    phrase = "" #initalize
    for(i in seq(length(X))){
      if(i==length(X)){
        phrase = paste(phrase, X[i], sep="")
      }else(phrase = paste(phrase, X[i], "+", sep=""))
    }
    f = paste(y, "~", phrase, sep="") #paste together final formula for anova
    
    boxplot(as.formula(f),data=df_to_use(),
            ylab=y, xlab=paste(X, collapse= " "))
  }) #end output$anova_boxPlot
  
  #   #Residual Tab Server
  output$anova_residualPlot <- renderPlot({
    dataset = df_to_use()
    aov.model<-anova_Output()
    pv<-(ad.test(aov.model$residuals)$p.value)
    layout(matrix(c(1,2,3,4),2,2, byrow = TRUE))
    
    hist(aov.model$residuals, col="cyan", main=paste("Anderson Darling P-value= ", round(pv,4)))
    plot(aov.model$fitted, aov.model$residuals);  abline(h=0, col="red")
    plot(aov.model, which=2)
    plot(aov.model$residuals)
  }) #end output$anova_residualPlot
  
  #Anova Turkey Tab - Actual Test Results
  output$anova_turkey <- renderPrint({
    dataset = df_to_use()
    aov.model<-anova_Output()
    out <-HSD.test(aov.model, get_factors(), " ")
    print(out)
  }) #end output$anova_turkey
  
  #Turkey Plot Tab Server
  output$anova_TurkeyPlot <- renderPlot({
    dataset = df_to_use()
    aov.model<-anova_Output()
    out<-HSD.test(aov.model, get_factors())
    par(cex=1, mar=c(3,8,1,1))
    bar.group(out$groups,horiz=T,col="cyan",
              xlim=c(0,max(out$means[,1]*1.2)),las=1)
  }) #end output$anova_turkeyPlot
  
  #Anova Turkey Tab - Actual Test Results
  output$anova_turkey2 <- renderPrint({
    dataset = df_to_use()
    aov.model<-anova_Output()
    out <-TukeyHSD(aov.model, data = dataset)
    print(out)
  }) #end output$anova_turkey
  
  output$anova_TurkeyPlot2 <- renderPlot({
    dataset = df_to_use()
    aov.model<-anova_Output()
    out <-TukeyHSD(aov.model, data = dataset)
    plot(out)
  }) #end output$anova_turkeyPlot
  
  # #functions used to display data in anova display
  # get_resp_var <- reactive({
  #   return(as.character(formula(input$modstr))[2])
  # })
  #
  
  #
  # #show data tab
  
  #
  # # #Obserbe event will update when button is pressed, rendering the data for each tab.
  # observeEvent(input$anova_button, {
  #
  
  
  
}) #end observe event
