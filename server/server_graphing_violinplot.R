
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#SUBTAB: Violin Plot
#VIOLIN000

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#updates filter_2 variable choices based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updateSelectInput(session, "violin_var", choices = input$checkbox)
})

observeEvent(input$checkbox, {
  updateSelectInput(session, "violin_factor_var", choices = input$checkbox)
})

#updates selectInput for single var choice in violin plot
observeEvent(input$checkbox, {
  updateSelectInput(session, "violin_single_var", choices = input$checkbox)
})

#changles strata options to match selected input for histogram plot
observeEvent(input$checkbox, {
  updatePickerInput(session,
                    "violin_strata_choices",
                    choices = input$checkbox)
})

#changes xlabel for violin plot to selected input
observeEvent(input$violin_factor_var,{
  updateTextInput(session,
                  "violin_xlabel",
                  label="X Label",
                  value=input$violin_factor_var)
})

#changes xlabel for violin plot to selected input
observeEvent(input$violin_var,{
  updateTextInput(session,
                  "violin_ylabel",
                  label="Y Label",
                  value=input$violin_var)
})

#Renders the color panel for each different stratified categorical variable (each at varied distance color levels)
output$violin_strata_color_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  lev <- sort(unique(gsub(" ", "_",df_to_use()[[input$violin_factor_var]])))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i){
    colourInput(inputId = paste0("cols_violin", lev[i]),
                label=paste0("Choose line color for ", lev[i]),
                value="black"
    )
  })
})

output$violin_strata_fill_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  lev <- sort(unique(gsub(" ", "_",df_to_use()[[input$violin_factor_var]])))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i){
    colourInput(inputId = paste0("cols_violin_fill", lev[i]),
                label=paste0("Choose fill color for ", lev[i]),
                value=cols[i]
    )
  })
})

#this function will set the value of the theme for the graph
theme_output_violin <- function(){
  if (input$violin_theme_choice=='gray'){
    theme_gray()}
  else if(input$violin_theme_choice=='bw'){
    theme_bw()}
  else if(input$violin_theme_choice=='linedraw'){
    theme_linedraw()}
  else if(input$violin_theme_choice=='light'){
    theme_light()}
  else if(input$violin_theme_choice=='minimal'){
    theme_minimal()}
  else if(input$violin_theme_choice=='classic'){
    theme_classic()}
  else if(input$violin_theme_choice=='void'){
    theme_void()}
  else if(input$violin_theme_choice=='void'){
    theme_dark()}
}
plotViolinInput <- function(){
  #create boxplot using ggplot
  if(input$violinplot_single_or_group == "single"){
    g_violin <- ggplot(df_to_use(), aes(x="",
                                        y=df_to_use()[[input$violin_single_var]])) +
      geom_violin(trim=input$violin_plot_trim,
                  color = input$violin_single_color_outline,
                  fill = input$violin_single_color_fill,
                  alpha = input$violin_strata_alpha,
                  width = input$violin_width)
  }
  else if(input$violinplot_single_or_group == "group"){
    
    
    cols_violin <- paste0("c(", paste0("input$cols_violin", unique(sort(gsub(" ", "_",df_to_use()[[input$violin_factor_var]]))), collapse=", "), ")")
    cols_violin <-eval(parse(text=cols_violin))
    #print(cols_violin)
    cols_violin_fill <- paste0("c(", paste0("input$cols_violin_fill", sort(gsub(" ", "_",df_to_use()[[input$violin_factor_var]])), collapse=", "), ")")
    cols_violin_fill <-unique(eval(parse(text=cols_violin_fill)))
    #print(cols_violin_fill)
    g_violin <- ggplot(df_to_use(), aes(x=df_to_use()[[input$violin_factor_var]],
                                        y=df_to_use()[[input$violin_var]],
                                        color=df_to_use()[[input$violin_factor_var]],
                                        fill=df_to_use()[[input$violin_factor_var]])) +
      geom_violin(trim=input$violin_plot_trim,
                  alpha=input$violin_strata_alpha,
                  width = input$violin_width) +
      #names here allows the legend title to be applied properly without causing duplicate titles
      scale_color_manual(name =input$violinplot_legend_title,
                         values=cols_violin) +
      scale_fill_manual(name =input$violinplot_legend_title,
                        values=cols_violin_fill)
    # if(input$violin_strata_on){
    #   g_violin <- g_violin + scale_color_manual(values=cols_violin)
    # }else{g_violin <- g_violin}
  }
  
  if(input$violin_plot_mean_points){
    g_violin <- g_violin + stat_summary(fun.y=mean, geom="point", shape=23, size=2)
  }else{g_violin <- g_violin}
  
  if(input$violin_plot_median_points){
    g_violin <- g_violin + stat_summary(fun.y=median, geom="point", shape=24, size=2)
  }else{g_violin <- g_violin}
  
  if(input$violin_plot_boxplot){
    g_violin <- g_violin + geom_boxplot(width=0.1)
  }else{g_violin <- g_violin}
  
  if(input$violin_plot_mean_bar){
    g_violin <- g_violin + stat_summary(fun.data=mean_sdl,
                                        mult=1,
                                        geom="pointrange",
                                        color="red")
  }else{g_violin <- g_violin}
  
  if(input$violin_plot_dotplot){
    g_violin <- g_violin + geom_dotplot(binaxis='y', stackdir='center', dotsize=input$violin_plot_dotplot_size)
  }else{g_violin <- g_violin}
  
  if(input$violin_plot_jitterplot){
    g_violin <- g_violin + geom_jitter(shape=16, position=position_jitter(0.2))
  }else{g_violin <- g_violin}
  
  
  # g_violin <- g_violin + theme_output_violin()
  if(input$violin_panel_colorPicker_checkbox){
    g_violin <- g_violin + theme(panel.background=element_rect(fill = input$violin_panel_colorPicker))
  }else{g_violin <- g_violin}
  
  if(input$violin_plot_flip){
    g_violin <- g_violin + coord_flip()}
  else{g_violin <- g_violin}
  
  g_violin <- g_violin +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(title=input$violin_title,
         x=input$violin_xlabel,
         y=input$violin_ylabel) +
    #hjust is used to center the title, size is used to change the text size of the title
    theme_output_violin() +
    theme(plot.title = element_text(hjust = 0.5, size=22),
          legend.position = input$violinplot_legend_position)
}

#create violin plot using ggplot
output$ViolinPlot <- renderPlot({
  print(plotViolinInput())
})

#Download Scatter PlotOutput
output$downloadViolin <- downloadHandler(
  filename = function(){
    paste(input$violin_download_title, input$violin_download_radiobuttons, sep="")
  },
  content = function(file){
    ggsave(file, plotViolinInput())
  }
)









