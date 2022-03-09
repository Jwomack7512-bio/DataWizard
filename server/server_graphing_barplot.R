#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

# SUBTAB: BARPLOT
# BARPLOT000

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#updates boxplot variable choices in single option based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "barplot_single_var",
                    choices = input$checkbox)
})

#updates boxplot variable choices in grouped option based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "barplot_var",
                    choices = input$checkbox)
})

observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "barplot_factor_var",
                    choices = input$checkbox)
})
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "barplot_multi_facet_options",
                    choices = input$checkbox)
})

observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "barplot_factor_fill",
                    choices = input$checkbox)
})


#changes xlabel for violin plot to selected input
observeEvent(input$boxplot_factor_var,{
  updateTextInput(session,
                  "barplot_xlabel",
                  label="X Label",
                  value=input$boxplot_factor_var)
})

#changes xlabel for violin plot to selected input
observeEvent(input$boxplot_var,{
  updateTextInput(session,
                  "barplot_ylabel",
                  label="Y Label",
                  value=input$boxplot_var)
})


# #Renders the color panel for each different stratified categorical variable (each at varied distance color levels)
output$barplot_strata_color_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  lev <- sort(unique(gsub(" ", "_",df_to_use()[[input$barplot_factor_fill]])))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i){
    colourInput(inputId = paste0("cols_barplot", lev[i]),
                label=paste0("Choose line color for ", lev[i]),
                value="black"
    )
  })
})

output$barplot_strata_fill_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  lev <- sort(unique(gsub(" ", "_",df_to_use()[[input$barplot_factor_fill]])))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i){
    colourInput(inputId = paste0("cols_barplot_fill", lev[i]),
                label=paste0("Choose fill color for ", lev[i]),
                value=cols[i]
    )
  })
})

#this function will set the value of the theme for the graph
theme_output_barplot <- function(){
  if (input$barplot_theme_choice=='gray'){
    theme_gray()}
  else if(input$barplot_theme_choice=='bw'){
    theme_bw()}
  else if(input$barplot_theme_choice=='linedraw'){
    theme_linedraw()}
  else if(input$barplot_theme_choice=='light'){
    theme_light()}
  else if(input$barplot_theme_choice=='minimal'){
    theme_minimal()}
  else if(input$barplot_theme_choice=='classic'){
    theme_classic()}
  else if(input$barplot_theme_choice=='void'){
    theme_void()}
  else if(input$barplot_theme_choice=='void'){
    theme_dark()}
}
#create barchart(s) using ggplot
plotBarplotInput <- function(){
  
  #if multiBarplot option is selected
  if(input$barplot_single_or_group == "Multi Group"){
    
    #sets of color options for multiple barplot choices
    cols_barplot <- paste0("c(", paste0("input$cols_boxplot", unique(sort(gsub(" ", "_",df_to_use()[[input$barplot_factor_fill]]))), collapse=", "), ")")
    cols_barplot <-eval(parse(text=cols_barplot))
    # print(cols_barplot)
    # print(length(cols_barplot))
    
    cols_barplot_fill <- paste0("c(", paste0("input$cols_barplot_fill", sort(gsub(" ", "_",df_to_use()[[input$barplot_factor_fill]])), collapse=", "), ")")
    cols_barplot_fill <-unique(eval(parse(text=cols_barplot_fill)))
    # print(cols_barplot_fill)
    # print(length(cols_barplot_fill))
    
    #Store variable for position of grouped barpot
    if(input$barplot_stacked_or_dodge == "Side-by-Side"){
      barplot_pos = "dodge"
    }else if(input$barplot_stacked_or_dodge == "Stacked"){
      barplot_pos = "stack"
    }else if(input$barplot_stacked_or_dodge == "% Stacked"){
      barplot_pos = "fill"
    }
    #x stores the factor variable (categorical), y is the value variable (numerical), and fill is the multi that splits by groups (I.e the stack)
    g_barplot <- ggplot(df_to_use(), aes(x=df_to_use()[[input$barplot_factor_var]],
                                         y=df_to_use()[[input$barplot_var]],
                                         fill=df_to_use()[[input$barplot_factor_fill]])) +
      #stat is the type of barplot, width controls the width of the bars, and position controls the type of multi plot (whether it is dodge, stacked, or fill)
      geom_bar(stat="identity",
               width=input$barplot_bar_width,
               position=barplot_pos)+
      #set colors for fill and outline of box plots for different split groups
      #name is again used here to control the name of the legend for this plot without spliting the legend into two
      scale_color_manual(name=input$barplot_legend_title,
                         values=cols_barplot) +
      scale_fill_manual(name=input$barplot_legend_title,
                        values=cols_barplot_fill)
    #adds value labels inside bars if checkbox checked and multi option is sidebyside
    if(input$barplot_multi_show_values_inside & input$barplot_stacked_or_dodge == "Side-by-Side"){
      g_barplot <- g_barplot + geom_text(aes(label=round(df_to_use()[[input$barplot_var]])),
                                         vjust=1.6,
                                         color="white",
                                         position = position_dodge(0.9),
                                         size=3.5)
    }
    
    # else if(input$barplot_multi_show_values_inside & input$barplot_stacked_or_dodge == "Stacked"){
    #   g_barplot <- g_barplot + geom_text(aes(y=label_ypos, label=round(df_to_use()[[input$barplot_var]])),
    #                                      vjust=1.6,
    #                                      color="white",
    #                                      size=3.5)
    # }
    else{g_barplot <- g_barplot}
    #this function allows the user to create a facet wrap (subplot) by the categorical variable
    if(input$barplot_multi_facet){
      g_barplot <- g_barplot +facet_wrap(~df_to_use()[[input$barplot_multi_facet_options]])
    }else{g_barplot <- g_barplot}
  }
  #generates plot is only a sinlge group is necessary (still multiple bar charts)
  else if(input$barplot_single_or_group == "Single Group"){
    #x is the categorical variable, and y is the numerical
    g_barplot <- ggplot(df_to_use(), aes(x=df_to_use()[[input$barplot_factor_var]],
                                         y=df_to_use()[[input$barplot_var]])) +
      #color and fill control the color options for the single group barchart
      geom_bar(stat="identity",
               width=input$barplot_bar_width,
               color = input$bar_single_color_outline,
               fill = input$bar_single_color_fill)
    #adds labels to the single groups (by that I mean shows the value of the bar chart)
    if(input$barplot_show_labels){
      #shows the value of bar chart on top of chart
      g_barplot <- g_barplot + geom_text(aes(label=round(df_to_use()[[input$barplot_var]], 2)), vjust=-0.3, size=3.5)
    }else if(input$barplot_show_labels_inside){
      #shows the value of the bar chart inside the chart in white writing
      g_barplot <- g_barplot +   geom_text(aes(label=round(df_to_use()[[input$barplot_var]]), 2), vjust=1.6, color="white", size=3.5)
    }
  }
  #------------
  #These provide overall options to the barplot regardless of type chosen
  #------------
  #flips axis if checkbox is selected
  if(input$barplot_plot_flip){
    g_barplot <- g_barplot + coord_flip()
  }else{g_barplot <- g_barplot}
  
  g_barplot <- g_barplot +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(title=input$barplot_title,
         x=input$barplot_xlabel,
         y=input$barplot_ylabel) +
    #hjust is used to center the title, size is used to change the text size of the title
    theme_output_barplot() +
    theme(plot.title = element_text(hjust = 0.5, size=22),
          #allows user to change position of legend
          legend.position = input$barplot_legend_position)
}

#create barplot plot using ggplot (prints to app)
output$BarPlot <- renderPlot({
  print(plotBarplotInput())
})

#Download barplot option
output$downloadBarplot <- downloadHandler(
  filename = function(){
    paste(input$barplot_download_title, input$barplot_download_radiobuttons, sep="")
  },
  content = function(file){
    ggsave(file, plotBarplotInput())
  }
)

