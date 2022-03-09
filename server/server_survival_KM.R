
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#SUBTAB: Kaplan Meier Plot

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#updates filter_2 variable choices based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "KM_time_var", 
                    choices = input$checkbox)
})

observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "KM_censor_var", 
                    choices = input$checkbox)
})

#updates selectInput for single var choice in KM plot
observeEvent(input$checkbox, {
  updateSelectInput(session, 
                    "KM_grouping_var", 
                    choices = input$checkbox)
})



#changes xlabel for KM plot to selected input
observeEvent(input$KM_time_var,{
  updateTextInput(session,
                  "KM_xlabel",
                  label="X Label",
                  value=input$KM_time_var)
})

#changes xlabel for KM plot to selected input
observeEvent(input$KM_censor_var,{
  updateTextInput(session,
                  "KM_ylabel",
                  label="Y Label",
                  value=input$KM_censor_var)
})

observeEvent(input$KM_grouping_var, {

  if(typeof(df_to_use()[[input$KM_grouping_var]]) != "character"){
    min_val <- min(df_to_use()[[input$KM_grouping_var]], na.rm=T)
    max_val <- max(df_to_use()[[input$KM_grouping_var]], na.rm=T)
  }else{
    min_val <- 0
    max_val <- 1
  }

  updateSliderInput(session,
                    "KM_continous_cutoff"
                    ,min = min_val
                    ,max = max_val
                    ,step = floor((max_val-min_val)/10)
                    ,value = floor((max_val-min_val)/2)
  )
  
  
})
# #this function will set the value of the theme for the graph
# theme_output_KM <- function(){
#   if (input$KM_theme_choice=='gray'){
#     theme_gray()}
#   else if(input$KM_theme_choice=='bw'){
#     theme_bw()}
#   else if(input$KM_theme_choice=='linedraw'){
#     theme_linedraw()}
#   else if(input$KM_theme_choice=='light'){
#     theme_light()}
#   else if(input$KM_theme_choice=='minimal'){
#     theme_minimal()}
#   else if(input$KM_theme_choice=='classic'){
#     theme_classic()}
#   else if(input$KM_theme_choice=='void'){
#     theme_void()}
#   else if(input$KM_theme_choice=='dark'){
#     theme_dark()}
# }

plotKMInput <- eventReactive(input$KM_plOt_button, {
  data <- df_to_use() #grab imported data into variable
  time <- data[, input$KM_time_var] #extract time data
  censor <- data[, input$KM_censor_var] #extract censor data
  xlabel <- c("Years", "Months", "Days") #set xlabel units
  b <- c(1, 12, 365) #used for 'break by' in ggsurvplot
  for (j in 1:ncol(data)) {
    if (class(data[, j]) %in% c("character")) 
      data[,j] <- as.factor(data[,j])
    else data[,j] = data[, j]
  }

  #create boxplot using ggplot
  if(input$KM_add_grouping_var){ #if user wants to group the data
    xvar <- data[,input$KM_grouping_var]
    dat <- cbind.data.frame(time, censor, xvar)#place extracted data in separate data frame
    #dat <- na.omit(dat)
    if(input$KM_group_conVcat_option=="Categorical"){ #if user want to group by catergorical var
      observe({print("Categorical Triggered")})
      fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ xvar, data = dat) #perform fit using group
      observe({print(fit)})
      ggsurvplot(fit
                 ,data = dat
                 ,font.main = 18
                 ,conf.int = ifelse(input$KM_plot_CI, TRUE, FALSE)
                 ,risk.table = ifelse(input$KM_risk_table, TRUE, FALSE)
                 ,risk.table.y.text.col = ifelse(input$KM_risk_table, TRUE, FALSE)
                 ,pval = input$KM_plot_pval
                 ,palette = ifelse(input$KM_grey_palette, "grey", "default")
                 ,legend.title = input$KM_grouping_var
                 ,legend.labs = levels(xvar)
                 ,font.tickslab = 12
                 #,legend = c(0.2, 0.2)

      )
    }
    else if(input$KM_group_conVcat_option=="Continuous"){#if user wants to group by continous var(sets quartile)
      if(input$KM_continous_options=="Percentile"){
        perc = as.numeric(as.integer(input$KM_continous_percentile))
        dat$new_grouping <- ifelse(dat[, "xvar"] < quantile(dat[, "xvar"], perc/100,  na.rm=TRUE) , "Low", "High")
        observe({print(head(dat))})
        new_grouping <- as.factor(dat$new_grouping)
        fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ new_grouping, data = dat) #perform fit using group
        ggsurvplot(fit 
                   ,data=dat
                   ,conf.int = ifelse(input$KM_plot_CI, TRUE, FALSE)
                   ,risk.table = ifelse(input$KM_risk_table, TRUE, FALSE)
                   ,pval = input$KM_plot_pval
                   ,palette = ifelse(input$KM_grey_palette, "grey", "default")
        )
      }
      else if(input$KM_continous_options=="CV"){
        cutoff_value = as.numeric(input$KM_continous_cutoff)
        dat$new_grouping <- ifelse(dat[, "xvar"] < cutoff_value , "Low", "High")
        observe({print(head(dat))})
        new_grouping <- as.factor(dat$new_grouping)
        fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ new_grouping, data = dat) #perform fit using group
        ggsurvplot(fit 
                   ,data=dat
                   ,conf.int = ifelse(input$KM_plot_CI, TRUE, FALSE)
                   ,risk.table = ifelse(input$KM_risk_table, TRUE, FALSE)
                   ,pval = input$KM_plot_pval
                   ,palette = ifelse(input$KM_grey_palette, "grey", "default")
        )
      
      }
      
    } #end else if input$KM_group_conVcat_option=="Continuous"
  }
  else{
    dat <- cbind.data.frame(time, censor)#place extracted data in separate data frame
    
    fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ 1, data = dat)
    ggsurvplot(fit
               ,data=dat
               #,break.time.by = b[as.numeric(input$KM_time_units)]
               ,font.x = 14
               ,font.y = 14
               ,xlab = xlabel[as.numeric(input$KM_time_units)]
               ,conf.int = input$KM_plot_CI
               ,risk.table = input$KM_risk_table
               )
  }

})

#create KM plot using ggplot
output$KMPlot <- renderPlot({
  # data <- df_to_use() #grab imported data into variable
  # time <- data[, input$KM_time_var] #extract time data
  # censor <- data[, input$KM_censor_var] #extract censor data
  # dat <- cbind.data.frame(time, censor)#place extracted data in separate data frame
  # observe({
  #   print(head(dat))
  # })
  # #fit <- survfit(Surv(as.numeric(time), as.numeric(censor)) ~ 1, data = dat)
  # fit <- survfit(Surv(as.numeric(time), as.numeric(factor(censor))) ~ 1, data = dat )
  # observe({print(fit)})
  #ggsurvplot(fit)
  print(plotKMInput())
  })

#Download Scatter PlotOutput
output$downloadKM <- downloadHandler(
  filename = function(){
    paste(input$KM_download_title, input$KM_download_radiobuttons, sep="")
  },
  content = function(file){
    ggsave(file, plotKMInput())
  }
)









