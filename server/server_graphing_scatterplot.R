#__________________________________________________________________________________________________________

#SUBTAB: SCATTERPLOT
#SCATTER000

#__________________________________________________________________________________________________________
# Function that produces default gg-colours is taken from this discussion:
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette

gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#updates scatterplot x variable choices based on items selected in checkbox select boxes
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "scatter_xcol",
                    choices = input$checkbox)
})

#updates scatterplot y variable choices based on items selected in checkbox select boxes
observeEvent(input$checkbox, {
  #updates with the second element to make sure it doesn't start with a plot of the two same elements
  updateSelectInput(session,
                    "scatter_ycol",
                    choices = input$checkbox)
})

#changes xlabel for scatter plot to selected input
observeEvent(input$scatter_xcol,{
  updateTextInput(session,
                  "scatter_xlabel",
                  label="X Label",
                  value=input$scatter_xcol)
})

#changes xlabel for scatter plot to selected input
observeEvent(input$scatter_ycol,{
  updateTextInput(session,
                  "scatter_ylabel",
                  label="Y Label",
                  value=input$scatter_ycol)
})

#changles strata options to match selected input for scatter plot
observeEvent(input$checkbox, {
  updatePickerInput(session,
                    "scatter_strata_choices",
                    choices = input$checkbox)
})

#renders image for dot key displaying all the dot options in a dropdown button in the scatter plot dot panel
output$scatter_dot_key <- renderImage({
  filename <- normalizePath(file.path("./www/images/dot_key.JPG"))
  
  list(src=filename)
  
}, deleteFile=FALSE)

#This was a pain in the ass (found and edited from stack)
#Renders the color panel for each different stratified categorical variable (each at varied distance color levels)
output$scatter_strata_color_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  req(input$scatter_strata_on)
  lev <- sort(unique(gsub(" ", "_", df_to_use()[[input$scatter_strata_choices]])))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i){
    colourInput(inputId = paste0("col", lev[i]),
                label=paste0("Choose color for ", lev[i]),
                value=cols[i]
    )
  })
})

output$scatter_strata_dot_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  
  req(input$scatter_strata_on)
  lev <- sort(unique(gsub(" ", "_", df_to_use()[[input$scatter_strata_choices]])))
  
  lapply(seq_along(lev), function(i){
    pickerInput(inputId = paste0("dottype", lev[i]),
                label=paste0("Dot style for ", lev[i]),
                choices = c("0", "1", "2", "3", "4", "5", "6", "7", "8","9","10","11","12","13","14","15","16", "17","18"),
                selected="16",
                width="150px",
                options = list(size=5))
  })
})

output$scatter_strata_dotsize_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  req(input$scatter_strata_on)
  lev <- sort(unique(gsub(" ", "_", df_to_use()[[input$scatter_strata_choices]])))
  
  lapply(seq_along(lev), function(i){
    pickerInput(inputId = paste0("dotsize", lev[i]),
                label=paste0("Dot size for ", lev[i]),
                choices = c("1", "2", "3", "4", "5", "6", "7", "8","9","10"),
                selected="3",
                width="150px",
                options = list(size=5))
  })
})

#this function will set the value of the theme for the graph
theme_output_scatter <- function(){
  if (input$scatter_theme_choice=='gray'){
    theme_gray()}
  else if(input$scatter_theme_choice=='bw'){
    theme_bw()}
  else if(input$scatter_theme_choice=='linedraw'){
    theme_linedraw()}
  else if(input$scatter_theme_choice=='light'){
    theme_light()}
  else if(input$scatter_theme_choice=='minimal'){
    theme_minimal()}
  else if(input$scatter_theme_choice=='classic'){
    theme_classic()}
  else if(input$scatter_theme_choice=='void'){
    theme_void()}
  else if(input$scatter_theme_choice=='dark'){
    theme_dark()}
}

#have to make this scatterplot a function and remove it from renderplot otherwise i can't seem to download it.
plotScatterInput <- function(){
  #designs a selectedData dataframe containing the input x,y variables
  selectedData <- reactive({
    selectedData <- df_to_use() %>%
      select(input$scatter_xcol, input$scatter_ycol)
  })
  
  #creates a clustered scatterplot if cluster is turned on
  if(input$scatter_cluster_switch){
    
    #runs kmeans on the two selected data points to make clusters
    clusters <- reactive({
      kmeans(selectedData(), input$scatter_cluster_slider)
    })
    
    #runs ggplot clustering graph
    g_scatter <- ggplot() +
      geom_point(data = selectedData(),
                 mapping = aes(x=selectedData()[[input$scatter_xcol]],
                               y=selectedData()[[input$scatter_ycol]],
                               #as character converts "1,2,3..." to characters so a gradient cluster isn't created
                               colour = as.character(clusters()$cluster))) +
      #places the red circles as centroid
      geom_point(mapping = aes_string(x = clusters()$centers[, input$scatter_xcol],
                                      y = clusters()$centers[, input$scatter_ycol]),
                 color = "red", size = 4)
  }
  
  else if(input$scatter_regression_checkbox && input$scatter_strata_on){
    #creates a list of cols based on the selected ui of the strata choices.
    dot_cols <- paste0("c(", paste0("input$col", sort(gsub(" ","_",df_to_use()[[input$scatter_strata_choices]])), collapse=", "), ")")
    #had to add unique of it made a value for all items in column and not just unique.
    dot_cols <-unique(eval(parse(text=dot_cols)))
    
    #Creates vector that stores dot styles for all strata generated variables
    dot_style <-paste0("c(", paste0("input$dottype", sort(unique(gsub(" ","_",df_to_use()[[input$scatter_strata_choices]]))), collapse=", "), ")")
    dot_style <-as.numeric(eval(parse(text=dot_style)))
    
    #Creates vector that stores dot sizes for all strata generated variables
    dot_size <-paste0("c(", paste0("input$dotsize", sort(unique(gsub(" ","_",df_to_use()[[input$scatter_strata_choices]]))), collapse=", "), ")")
    dot_size <-as.numeric(eval(parse(text=dot_size)))
    
    CI_interval <- ifelse(input$scatter_regression_CI_interval_checkbox, T, F)
    fullrange_option <- ifelse(input$scatter_regression_strata_fullrange, T, F)
    
    g_scatter <- ggplot(df_to_use(),
                        aes(x=df_to_use()[[input$scatter_xcol]],
                            y=df_to_use()[[input$scatter_ycol]],
                            color=df_to_use()[[input$scatter_strata_choices]],
                            shape=df_to_use()[[input$scatter_strata_choices]]))
    
    #this section adds the stratification of color and shape.
    g_scatter <- g_scatter + geom_point(aes(color=df_to_use()[[input$scatter_strata_choices]],
                                            shape=df_to_use()[[input$scatter_strata_choices]],
                                            size=df_to_use()[[input$scatter_strata_choices]])) +
      scale_color_manual(values=dot_cols) + #this line allows the user to have different selected colors for each strata
      scale_shape_manual(values=dot_style) +
      scale_size_manual(values=dot_size)
    
    g_scatter <- g_scatter + geom_smooth(method=lm,
                                         se=CI_interval,
                                         fullrange=fullrange_option,
                                         aes(fill=df_to_use()[[input$scatter_strata_choices]]))
  }
  #this plots the data if the user uses the strata option to separate the data based on a categorical variable
  else if(input$scatter_strata_on){
    
    #creates a list of cols based on the selected ui of the strata choices.
    dot_cols <- paste0("c(", paste0("input$col", sort(gsub(" ","_",df_to_use()[[input$scatter_strata_choices]])), collapse=", "), ")")
    #had to add unique of it made a value for all items in column and not just unique.
    dot_cols <-unique(eval(parse(text=dot_cols)))
    
    #Creates vector that stores dot styles for all strata generated variables
    dot_style <-paste0("c(", paste0("input$dottype", sort(unique(gsub(" ","_",df_to_use()[[input$scatter_strata_choices]]))), collapse=", "), ")")
    dot_style <-as.numeric(eval(parse(text=dot_style)))
    
    #Creates vector that stores dot sizes for all strata generated variables
    dot_size <-paste0("c(", paste0("input$dotsize", sort(unique(gsub(" ","_",df_to_use()[[input$scatter_strata_choices]]))), collapse=", "), ")")
    dot_size <-as.numeric(eval(parse(text=dot_size)))
    
    #stores data for ggplot for stratified data that allows for different inputs of size, shape, and color
    g_scatter <- ggplot(df_to_use(),
                        aes(x=df_to_use()[[input$scatter_xcol]],
                            y=df_to_use()[[input$scatter_ycol]]))
    #this section adds the stratification of color and shape.
    g_scatter <- g_scatter + geom_point(aes(color=df_to_use()[[input$scatter_strata_choices]],
                                            shape=df_to_use()[[input$scatter_strata_choices]],
                                            size=df_to_use()[[input$scatter_strata_choices]])) +
      scale_color_manual(values=dot_cols) + #this line allows the user to have different selected colors for each strata
      scale_shape_manual(values=dot_style) +
      scale_size_manual(values=dot_size)
  }
  else if(input$scatter_regression_checkbox){
    
    CI_interval <- ifelse(input$scatter_regression_CI_interval_checkbox, T, F)
    
    formula <- df_to_use()[[input$scatter_ycol]] ~ df_to_use()[[input$scatter_xcol]]
    
    g_scatter <- ggplot(df_to_use(),
                        aes(x=df_to_use()[[input$scatter_xcol]],
                            y=df_to_use()[[input$scatter_ycol]]
                        )) + geom_point(size=input$scatter_dot_size,
                                        color=input$scatter_dot_color,
                                        shape=as.numeric(input$scatter_dot_shape))
    
    if(input$scatter_regression_LoessMethod){
      g_scatter <- g_scatter + geom_smooth(se=CI_interval,
                                           linetype=input$scatter_regression_lineType,
                                           color=input$scatter_regression_lineColor,
                                           size=input$scatter_regression_linesize)
    }
    else{g_scatter <- g_scatter + geom_smooth(method='lm',
                                              se=CI_interval,
                                              linetype=input$scatter_regression_lineType,
                                              color=input$scatter_regression_lineColor,
                                              size=input$scatter_regression_linesize)
    }
    
    if(input$scatter_regression_equationDisplay){
      
      #this blocks selects the position of the regression equation
      if(input$scatter_regression_customPosition_choice=="Relative"){
        #this option selects options such as top, bottom, right, left
        reg_line_pos_x <- input$scatter_regression_relative_x
        reg_line_pos_y <- input$scatter_regression_relative_y
      }else if(input$scatter_regression_customPosition_choice=="Numerical"){
        #this option selects numerical values between 0-1 that are relative to the plot position
        reg_line_pos_x <- as.numeric(input$scatter_regression_numerical_x)
        reg_line_pos_y <- as.numeric(input$scatter_regression_numerical_y)
      }
      
      #this block chooses the size of the regression equation, currently has 3 options
      if(input$scatter_regression_equation_size=="Small"){
        reg_line_size <- 5
      }else if(input$scatter_regression_equation_size=="Medium"){
        reg_line_size <- 7.5
      }else if(input$scatter_regression_equation_size=="Large"){
        reg_line_size <- 10
      }
      
      #adds regression equation to plot
      g_scatter <- g_scatter + stat_regline_equation(
        aes(label =  paste0("atop(",..eq.label.., ",", ..rr.label.., ")")), #atop is used to add newline/line by line of topics
        formula = formula,
        size=reg_line_size, #sets size of regression equation
        label.y.npc=ifelse(input$scatter_regression_customPosition, reg_line_pos_y, "top"), #sets y position of regression eqn
        label.x.npc=ifelse(input$scatter_regression_customPosition, reg_line_pos_x, "left")) #sets x position of regression eqn
    }
    else{g_scatter <- g_scatter}
  }
  #this runs and creates just the basic input with no swtiches activated
  else{
    g_scatter <- ggplot(df_to_use(),
                        aes(x=df_to_use()[[input$scatter_xcol]],
                            y=df_to_use()[[input$scatter_ycol]]
                        )) + geom_point(size=input$scatter_dot_size,
                                        color=input$scatter_dot_color,
                                        shape=as.numeric(input$scatter_dot_shape))
  }
  
  #this allows the user to change the background of the plot
  if(input$scatter_panel_colorPicker_checkbox){
    g_scatter <- g_scatter + theme(panel.background=element_rect(fill = input$scatter_panel_colorPicker))
  }else{g_scatter <- g_scatter}
  # if(input$scatter_panel_line_checkbox){
  #   g_scatter <- g_scatter + theme(panel.background=element_rect(linetype  = input$scatter_panel_line))
  # }else{g_scatter <- g_scatter}
  
  #this checks if the user has opted to use there own values for axis.  Can probably split into four separte if statements for a more interactive feel
  if(input$scatter_axis_confirm){
    g_scatter <- g_scatter + scale_x_continuous(limits=c(input$scatter_xaxis_min, input$scatter_xaxis_max)) + scale_y_continuous(limits=c(input$scatter_yaxis_min, input$scatter_yaxis_max))
  }else{g_scatter <- g_scatter}
  
  g_scatter <- g_scatter +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(title=input$scatter_title,
         x=input$scatter_xlabel,
         y=input$scatter_ylabel) +
    #hjust is used to center the title, size is used to change the text size of the title
    theme(plot.title = element_text(hjust = 0.5, size=22)) +
    theme_output_scatter()
  
}

#prints scatter plot out
output$ScatterPlot <- renderPlot({
  
  print(plotScatterInput())
  
})

#Download Scatter PlotOutput
output$downloadScatter <- downloadHandler(
  filename = function(){
    paste(input$scatter_download_title, input$scatter_download_radiobuttons, sep="")
  },
  content = function(file){
    ggsave(file, plotScatterInput())
  })

