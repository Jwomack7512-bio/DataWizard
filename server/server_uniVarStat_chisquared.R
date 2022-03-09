#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

# CHISQUARED SERVER
# CHISQ000

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#adds values to chi squared input 1 for whateverhappens in the data management tab
#the user should select a row of categorical variables with 2 var in it
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "chisquared_input1",
                    choices = input$checkbox)
})

#adds values to chidquared input 2 for whateverhappens in the data management tab
#Same as the previous 2 different categorical variables
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "chisquared_input2",
                    choices = input$checkbox)
})

#creates a reactive data frame from the two selected columns of the data which will be used
cs_data <- reactive({
  
  if(input$cs_load_options=='cs_col_data'){
    dat <- data.frame(df_to_use()[[input$chisquared_input1]],
                      df_to_use()[[input$chisquared_input2]])
    colnames(dat) <- c(input$chisquared_input1, input$chisquared_input2)
    table(dat)}
  else if(input$cs_load_options=="cs_table_data"){
    dat <- read.csv(input$cs_table_data$datapath,
                    header=TRUE,
                    row.names=1)
  }
})

#output contingency table
output$cs_contingency_table= renderPrint({
  if(input$cs_load_options=='cs_col_data'){
    #adds sums to the table completing the contingency table
    tabled_dat <- addmargins(cs_data())
    print(tabled_dat)}
  else if(input$cs_load_options=="cs_table_data"){
    print(addmargins(as.matrix(cs_data())))
  }
  
  
})

#outputs a proportion table
output$cs_prop_table = renderPrint({
  if(input$cs_load_options=='cs_col_data'){
    #turns data into table form of counts. (2x2)
    prop_tabled_dat <- prop.table(cs_data(),1)
    #adds sums to the table completing the contingency table
    #prop_tabled_dat <- addmargins(prop_tabled_dat)
    print(round(prop_tabled_dat,3))
  }
  else if(input$cs_load_options=="cs_table_data"){
    print(round(prop.table(as.matrix(cs_data()),1),3))
  }
  
})

#output of chisquared results
output$cs_results <- renderPrint({
  
  user_data <- cs_data()
  
  if(input$cs_yates_correction_box){cs <- chisq.test(user_data, correct=TRUE)}
  else{ cs <- chisq.test(user_data, correct=FALSE)}
  print(cs)
})

#output plots (mosiac)
output$cs_mosiac_plot <- renderPlot({
  #create table data
  if(input$cs_load_options=='cs_col_data'){
    tabled_dat <- cs_data()
  }
  else if(input$cs_load_options=="cs_table_data"){tabled_dat = as.matrix(cs_data())}
  
  #create mosaic plot
  mosaic(tabled_dat, gp=shading_max, main="Mosaic Plot")
  #mosaicplot(tabled_dat, shade=TRUE)
})

#plot stacked barplot of data
output$cs_barplot <- renderPlot({
  
  x <- cs_data()
  
  levI <- nrow(x)
  levJ <- ncol(x)
  dosu <- as.vector(t(x))
  
  gokei <- c()
  bunbo <- c()
  for(i in 1:levI)
  {
    ds <- c()
    for(j in 1:levJ)
    {
      ds <- c(ds, dosu[(i-1)*levJ+j])
    }
    gokei <- c(gokei, sum(ds))
    bunbo <- c(bunbo, rep(sum(ds), levJ))
  }
  hyohir <- dosu/bunbo
  
  zuhir <- c()
  for(i in levI:1)
  {
    for(j in 1:levJ)
    {
      zuhir <- c(zuhir, hyohir[(i-1)*levJ+j] )
    }
  }
  
  zubar <- matrix(c(zuhir), nc=levJ, by=1)
  rownames(zubar) <- rev(rownames(x))
  colnames(zubar) <- colnames(x)
  #zubar <- zubar[nrow(zubar):1,]
  
  par(mar=c(5,6,2,4))
  barplot(t(zubar), hor=1, las=1, xlab="Percentage", col=gray.colors(ncol(x)))
  legend("bottomright", legend=colnames(zubar), fill=gray.colors(ncol(x)))
})


#this plots the chi squared distribution and fill
output$cs_chi_stat <- renderPlot({
  
  user_data <- cs_data()
  
  if(input$cs_yates_correction_box){cs <- chisq.test(user_data, correct=TRUE)}
  else{ cs <- chisq.test(user_data, correct=FALSE)}
  
  alpha = 0.05
  dof = as.numeric(cs$parameter) #degree of freedom
  #dof = 6
  #this function provides the shading for the graph
  funcShaded <- function(x) {
    y <- dchisq(x, df = dof)
    y[x < qchisq(alpha, df = dof, lower.tail = FALSE)] <- NA
    return(y)
  }
  
  p <- ggplot(data.frame(x = c(0, qchisq(0.999, df = dof, lower.tail = TRUE))), aes(x = x)) +
    stat_function(fun = dchisq, args = list(df = dof)) +
    stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
    theme_minimal() +
    geom_vline(xintercept = cs$statistic, color = "steelblue") +
    geom_text(aes(x = cs$statistic, label = paste0("Test statistic = ", round(cs$statistic, 3)), y = 0.025), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("Chi-square distribution (df = ", dof, ")")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x")
  p
})

