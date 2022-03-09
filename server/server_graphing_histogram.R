#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

# SUBTAB: HISTOGRAM
# HISTO000

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#updates filter_2 variable choices based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updateSelectInput(session, "histogram_var", choices = input$checkbox, selected=NULL)
})

observeEvent(input$histogram_var, {
  if(is.numeric(df_to_use()[[input$histogram_var]][1])){
    breaks <- pretty(range(df_to_use()[[input$histogram_var]]),
                     n = nclass.FD(df_to_use()[[input$histogram_var]]),
                     min.n = .2)
    bwidth <- breaks[2]-breaks[1]
    
    updateTextInput(session,
                    inputId='histogram_binwidth',
                    value=bwidth)
  }
})

#changles strata options to match selected input for histogram plot
observeEvent(input$checkbox, {
  updatePickerInput(session,
                    "histogram_strata_choices",
                    choices = input$checkbox)
})

#Renders the color panel for each different stratified categorical variable (each at varied distance color levels)
output$histogram_strata_color_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  req(input$histogram_strata_on)
  lev <- sort(unique(gsub(" ", "_", df_to_use()[[input$histogram_strata_choices]])))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i){
    colourInput(inputId = paste0("cols_line_hist", lev[i]),
                label=paste0("Choose line color for ", lev[i]),
                value=cols[i]
    )
  })
})

output$histogram_strata_fill_options_popdown <- renderUI({
  req(input$histogram_strata_on)
  lev <- sort(unique(gsub(" ", "_", df_to_use()[[input$histogram_strata_choices]])))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i){
    colourInput(inputId = paste0("cols_fill_hist", lev[i]),
                label=paste0("Choose fill color for ", lev[i]),
                value=cols[i]
    )
  })
})

#this function will set the value of the theme for the graph
theme_output_histogram <- function(){
  if (input$histogram_theme_choice=='gray'){
    theme_gray()}
  else if(input$histogram_theme_choice=='bw'){
    theme_bw()}
  else if(input$histogram_theme_choice=='linedraw'){
    theme_linedraw()}
  else if(input$histogram_theme_choice=='light'){
    theme_light()}
  else if(input$histogram_theme_choice=='minimal'){
    theme_minimal()}
  else if(input$histogram_theme_choice=='classic'){
    theme_classic()}
  else if(input$histogram_theme_choice=='void'){
    theme_void()}
  else if(input$histogram_theme_choice=='void'){
    theme_dark()}
}
plotHistogramInput <- function(){
  
  if(input$histogram_bin_choice == "Number of Bins"){
    g_histogram <- ggplot(df_to_use(), aes(x=df_to_use()[[input$histogram_var]])) +
      geom_histogram(bins=input$histogram_bins,
                     color=input$histogram_color,
                     fill=input$histogram_fill,
                     linetype=input$histogram_linetype)
  }
  else if(input$histogram_bin_choice == "Numerical Width of Bins"){
    
    g_histogram <- ggplot(df_to_use(), aes(x=df_to_use()[[input$histogram_var]])) +
      geom_histogram(binwidth=as.numeric(input$histogram_binwidth),
                     color=input$histogram_color,
                     fill=input$histogram_fill,
                     linetype=input$histogram_linetype)
  }
  
  #adds mean line
  if(input$histogram_mean_line_on){
    g_histogram <- g_histogram + geom_vline(aes(xintercept=mean(df_to_use()[[input$histogram_var]])),
                                            color="blue", linetype="dashed", size=1)
  }else{g_histogram <- g_histogram}
  # #adds density
  # if(input$histogram_density_overlay){
  #   g_histogram <- g_histogram +
  #     geom_histogram(aes(y=..density..), colour="black", fill="white") +
  #     geom_density(alpha=.2, fill="#FF6666")
  # }else{g_histogram <- g_histogram}
  g_histogram <- g_histogram + theme_output_histogram()
  if(input$histogram_panel_colorPicker_checkbox){
    g_histogram <- g_histogram + theme(panel.background=element_rect(fill = input$histogram_panel_colorPicker))
  }else{g_histogram <- g_histogram}
  
  
}

plotHistogramStrata <- function(){
  #creates a list of cols based on the selected ui of the strata choices.
  cols_line_hist <- paste0("c(", paste0("input$cols_line_hist", sort(gsub(" ", "_", df_to_use()[[input$histogram_strata_choices]])), collapse=", "), ")")
  #had to add unique of it made a value for all items in column and not just unique.
  cols_line_hist <-unique(eval(parse(text=cols_line_hist)))
  
  cols_fill_hist <- paste0("c(", paste0("input$cols_fill_hist", sort(gsub(" ", "_",df_to_use()[[input$histogram_strata_choices]])), collapse=", "), ")")
  cols_fill_hist <-unique(eval(parse(text=cols_fill_hist)))
  
  if(input$histogram_bin_choice == "Number of Bins"){
    g_histogram <- ggplot(df_to_use(), aes(x=df_to_use()[[input$histogram_var]],
                                           color = df_to_use()[[input$histogram_strata_choices]],
                                           fill = df_to_use()[[input$histogram_strata_choices]])) +
      geom_histogram(bins=input$histogram_bins,
                     linetype=input$histogram_linetype,
                     alpha=input$histogram_strate_alpha,
                     position=input$histogram_strata_overlap_position) +
      scale_color_manual(values=cols_line_hist) +
      scale_fill_manual(values=cols_fill_hist)
  }
  else if(input$histogram_bin_choice == "Numerical Width of Bins"){
    
    g_histogram <- ggplot(df_to_use(), aes(x=df_to_use()[[input$histogram_var]],
                                           color = df_to_use()[[input$histogram_strata_choices]],
                                           fill = df_to_use()[[input$histogram_strata_choices]])) +
      geom_histogram(binwidth=as.numeric(input$histogram_binwidth),
                     linetype=input$histogram_linetype,
                     alpha=input$histogram_strate_alpha,
                     position=input$histogram_strata_overlap_position) +
      scale_color_manual(values=cols_line_hist) +
      scale_fill_manual(values=cols_fill_hist)
  }
  g_histogram <- g_histogram + theme_output_histogram()
  if(input$histogram_panel_colorPicker_checkbox){
    g_histogram <- g_histogram + theme(panel.background=element_rect(fill = input$histogram_panel_colorPicker))
  }else{g_histogram <- g_histogram}
  
  
}


#create histogram using ggplot
output$HistogramPlot <- renderPlot({
  if(input$histogram_strata_on){
    print(plotHistogramStrata())
  }else{print(plotHistogramInput())}
  
})

#this function is used to the histogram output information out of ggplot.
get_hist <- function(p) {
  d <- ggplot_build(p)$data[[1]]
  data.frame(bin_center = d$x, bin_edge_1 = d$xmin, bin_edge_2 = d$xmax, count = d$y)
}

#out histogram bin information
output$histogram_results_table = renderTable({
  myhist = get_hist(plotHistogramInput())
  #prints out bin center, bin edges, and counts of bin to output
  
})

#Download Histogram PlotOutput
output$downloadHistogram <- downloadHandler(
  filename = function(){
    paste(input$histogram_download_title, input$histogram_download_radiobuttons, sep="")
  },
  content = function(file){
    if(input$histogram_strata_on){
      ggsave(file, plotHistogramStrata())
    }else{ggsave(file, plotHistogramInput())}
  }
)


