#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
# SUBTAB: BOXPLOT
# BOXPLOT000
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#updates boxplot variable choices in single option based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "boxplot_single_var",
                    choices = input$checkbox)
})

#updates boxplot variable choices in grouped option based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "boxplot_var",
                    choices = input$checkbox)
})

observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "boxplot_factor_var",
                    choices = input$checkbox)
})

#changles strata options to match selected input for histogram plot
observeEvent(input$checkbox, {
  updatePickerInput(session,
                    "boxplot_strata_choices",
                    choices = input$checkbox)
})

#changes xlabel for violin plot to selected input
observeEvent(input$boxplot_factor_var,{
  updateTextInput(session,
                  "boxplot_xlabel",
                  label="X Label",
                  value=input$boxplot_factor_var)
})

#changes xlabel for violin plot to selected input
observeEvent(input$boxplot_var,{
  updateTextInput(session,
                  "boxplot_ylabel",
                  label="Y Label",
                  value=input$boxplot_var)
})

# #Renders the color panel for each different stratified categorical variable (each at varied distance color levels)
output$boxplot_strata_color_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  lev <- sort(unique(gsub(" ", "_", df_to_use()[[input$boxplot_factor_var]])))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i){
    colourInput(inputId = paste0("cols_boxplot", lev[i]),
                label=paste0("Choose line color for ", lev[i]),
                value="black"
    )
  })
})

output$boxplot_strata_fill_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  lev <- sort(unique(gsub(" ", "_", df_to_use()[[input$boxplot_factor_var]])))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i){
    colourInput(inputId = paste0("cols_boxplot_fill", lev[i]),
                label=paste0("Choose fill color for ", lev[i]),
                value=cols[i]
    )
  })
})

#this function will set the value of the theme for the graph
theme_output_boxplot <- function(){
  if (input$boxplot_theme_choice=='gray'){
    theme_gray()}
  else if(input$boxplot_theme_choice=='bw'){
    theme_bw()}
  else if(input$boxplot_theme_choice=='linedraw'){
    theme_linedraw()}
  else if(input$boxplot_theme_choice=='light'){
    theme_light()}
  else if(input$boxplot_theme_choice=='minimal'){
    theme_minimal()}
  else if(input$boxplot_theme_choice=='classic'){
    theme_classic()}
  else if(input$boxplot_theme_choice=='void'){
    theme_void()}
  else if(input$boxplot_theme_choice=='void'){
    theme_dark()}
}

plotBoxplotInput <- function(){
  
  #create boxplot using ggplot
  if(input$boxplot_single_or_group == "single"){
    g_boxplot <- ggplot(df_to_use(), aes(x="",
                                         y=df_to_use()[[input$boxplot_single_var]])) +
      geom_boxplot(notch=ifelse(input$boxplot_notched, TRUE, FALSE),
                   color = input$box_single_color_outline,
                   fill = input$box_single_color_fill,
                   alpha = input$boxplot_strata_alpha,
                   width = input$boxplot_width)
  }
  else if(input$boxplot_single_or_group == "group"){
    #req(input$cols_boxplot1)
    #sets of color options for multiple boxplot choices
    cols_boxplot <- paste0("c(", paste0("input$cols_boxplot", unique(sort(gsub(" ", "_",df_to_use()[[input$boxplot_factor_var]]))), collapse=", "), ")")
    cols_boxplot <-eval(parse(text=cols_boxplot))
    print(cols_boxplot)
    print(length(cols_boxplot))
    
    cols_boxplot_fill <- paste0("c(", paste0("input$cols_boxplot_fill", sort(gsub(" ", "_",df_to_use()[[input$boxplot_factor_var]])), collapse=", "), ")")
    cols_boxplot_fill <-unique(eval(parse(text=cols_boxplot_fill)))
    print(cols_boxplot_fill)
    print(length(cols_boxplot_fill))
    
    g_boxplot <- ggplot(df_to_use(), aes(x=df_to_use()[[input$boxplot_factor_var]],
                                         y=df_to_use()[[input$boxplot_var]],
                                         color=df_to_use()[[input$boxplot_factor_var]],
                                         fill=df_to_use()[[input$boxplot_factor_var]])) +
      #creates boxplot with ifelse to notch if checkbox selected
      geom_boxplot(notch=ifelse(input$boxplot_notched, TRUE, FALSE),
                   aes(group=df_to_use()[[input$boxplot_factor_var]]),
                   #alpha sets the transperancy value of the boxplots
                   alpha = input$boxplot_strata_alpha,
                   #variable size of boxplots - changes width depending on count in boxplot
                   varwidth = ifelse(input$boxplot_multi_varwidth, TRUE, FALSE),
                   width = input$boxplot_width) +
      #set colors for fill and outline
      scale_color_manual(name=input$boxplot_legend_title,
                         values=cols_boxplot) +
      scale_fill_manual(name=input$boxplot_legend_title,
                        values=cols_boxplot_fill)}
  
  #flips axis if checkbox is selected
  if(input$boxplot_plot_flip){
    g_boxplot <- g_boxplot + coord_flip()
  }else{g_boxplot <- g_boxplot}
  
  #adds a mean point to each box plot if checkbox selected
  if(input$boxplot_plot_mean_points){
    g_boxplot <- g_boxplot + stat_summary(fun.y=mean, geom="point", shape=23, size=4)
  }else{g_boxplot <- g_boxplot}
  
  #adds dot plot to each box plot if checkbox selected
  if(input$boxplot_plot_dotplot){
    g_boxplot <- g_boxplot + geom_dotplot(binaxis='y', stackdir='center', dotsize=input$boxplot_plot_dotplot_size)
  }else{g_boxplot <- g_boxplot}
  
  #adds jitter plot to each box plot if checkbox selected
  if(input$boxplot_plot_jitterplot){
    g_boxplot <- g_boxplot + geom_jitter(shape=16, position=position_jitter(0.2))
  }else{g_boxplot <- g_boxplot}
  
  g_boxplot <- g_boxplot +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(title=input$boxplot_title,
         x=input$boxplot_xlabel,
         y=input$boxplot_ylabel) +
    #hjust is used to center the title, size is used to change the text size of the title
    theme_output_boxplot() +
    theme(plot.title = element_text(hjust = 0.5, size=22),
          #allows user to change position of legend
          legend.position = input$boxplot_legend_position)
}

#create boxplot plot using ggplot
output$BoxPlot <- renderPlot({
  print(plotBoxplotInput())
})

#Download Scatter PlotOutput
output$downloadBoxplot <- downloadHandler(
  filename = function(){
    paste(input$boxplot_download_title, input$boxplot_download_radiobuttons, sep="")
  },
  content = function(file){
    ggsave(file, plotBoxplotInput())
  }
)







