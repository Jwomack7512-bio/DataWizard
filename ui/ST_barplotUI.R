#This tab corresponds to the "BarChart" SubTab (under Graphing Tab) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  September 9, 2020
#  Last Update: September, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------
#__________________________________________________________________________________________________________
#SUBTAB: BARCHART
#__________________________________________________________________________________________________________
SUBTAB_BARPLOT <- tabItem(tabName = "TAB_BARPLOT", 
                          fluidRow(
                            column(width=10,
                                   tags$h2("BarChart")),
#____________________________________
#ToolTip Help
#____________________________________
                            column(width=2,
                                   dropdownButton(
                                     status="info",
                                     icon=icon("info"),
                                     width="1000px",
                                     size="xs",
                                     right = TRUE,
                                     tooltip=tooltipOptions(title = "Click for help",
                                                            placement="left"),
                                     h2('BarChart'),
                                     p("This tab allows for the plotting of a bar plot.  It supports single group barplot and group splitting 
                                       barplots.  As of now the input information must be the following: for single group two columns of data are 
                                       needed.  One column is the categorical identifies while the other is the values is the associated value like
                                       seen below:"),
                                     div(img(src="./images/barplot_single.JPG",
                                             width= "20%",
                                             height="20%",
                                             align=""), style="text-align: center;"),
                                     p("where len is the continous variable and dose is the variable to factor by. Multi Group allows for multiple bar plots
                                       to be plotted for a single factor. An import for multiGroup needs a variable to split by and can be seen in the following:" ),
                                     div(img(src="./images/barplot_multi.JPG",
                                             width= "20%",
                                             height="20%",
                                             align=""), style="text-align: center;"),
                                     p("In multiGroup, each factor var (species) will need a separate bar chart value for each split variable (condition).
                                        The program will then plot a different bar for each condition grouped by the factor variable.")
                                     ), #end dropdownButton
                                   align="right")
                                     ), #end fluidRow
                          hr(),
                          fluidRow(
                            column(width=9,
                                  fluidRow(
                                    column(width=6,
#------------
#Input DropDown Button
#------------
                                           dropdownButton(
                                             label= "Inputs",
                                             icon=icon("sliders"),
                                             circle = FALSE, 
                                             status = "primary",
                                             tags$h3("List of Inputs"),
                                             #choice buttons that allows user to select 'type' of boxplot to show (single or groups)
                                             radioGroupButtons(inputId="barplot_single_or_group",
                                                               label="Select",
                                                               choices=c("Single Group", "Multi Group"),
                                                               selected="Single Group",
                                                               status="primary"),
                                             selectInput(inputId = 'barplot_var',
                                                         label = 'Select Continous Variable',
                                                         choices = character()),
                                             selectInput(inputId = 'barplot_factor_var',
                                                         label = 'Select Variable to Factor By',
                                                         choices = character()),
                                             #adds split group for bar chart (fill value in ggplot geom bar(postion)
                                             conditionalPanel(condition="input.barplot_single_or_group == 'Multi Group'",
                                                              selectInput(inputId = 'barplot_factor_fill',
                                                                          label = 'Variable to Split by',
                                                                          choices =character())))
                                           ),
                                    column(width=6,
     #------------
     #Axis and Download Button
     #------------
                                           #These divs are used to place the buttons inline with eachother in the column.
                                           div(style="display:inline-block; text_align:right;",
                                               #OPTIONS BUTTON 
                                               dropdownButton(label = "Axis Options",icon = icon("gear"),circle = FALSE,status = "primary",right=TRUE,
                                                              textInput(inputId="barplot_title", 
                                                                        label="Title", 
                                                                        value = ""),
                                                              textInput(inputId="barplot_xlabel", 
                                                                        label="X Label", 
                                                                        value = ""),
                                                              textInput(inputId="barplot_ylabel", 
                                                                        label="Y Label", 
                                                                        value = "")
                                               )),
                                           div(style="display:inline-block; text_align:right;",
                                               #DOWNLOAD BUTTON
                                               dropdownButton(label = "Download",icon = icon("download"),circle = FALSE,status = "primary",right=TRUE,
                                                              textInput(
                                                                inputId ="barplot_download_title",
                                                                label=NULL,
                                                                value = "",
                                                                placeholder = "Type Download Title",
                                                                width = NULL
                                                              ),
                                                              radioGroupButtons(
                                                                inputId = "barplot_download_radiobuttons",
                                                                label = NULL,
                                                                choices = c(".jpg", 
                                                                            ".png", ".pdf"),
                                                                individual = TRUE,
                                                                checkIcon = list(
                                                                  yes = tags$i(class = "fa fa-circle", 
                                                                               style = "color: steelblue"),
                                                                  no = tags$i(class = "fa fa-circle-o", 
                                                                              style = "color: steelblue"))
                                                              ),
                                                              downloadBttn(outputId="downloadbarplot",
                                                                           label="Download",
                                                                           style="unite",
                                                                           color="primary",
                                                                           size="sm",
                                                                           block=FALSE,
                                                                           no_outline=FALSE)
                                               )),
                                           align = 'right'
                                    ) #end column
                                    ),#end of fluid Row
                                    #barplot output
                                    jqui_resizable(plotOutput(outputId = 'BarPlot'))
                                  ), #end of column width=9
                            column(width=3,
    #------------
    #Addon Box
    #------------
                                  box(
                                    title="Plot Addons", status="primary", solidHeader=TRUE, collapsible=TRUE, width=NULL,
                                    prettyCheckbox(inputId="barplot_plot_flip",
                                                   label="Flip Axis",
                                                   value=FALSE),
                                    sliderInput(inputId="barplot_bar_width",
                                                label="Bar Width",
                                                min = 0,
                                                max = 1,
                                                value = 0.5,
                                                step=0.1),
                                    conditionalPanel(condition="input.barplot_single_or_group == 'Single Group'",
                                                     prettyCheckbox(inputId="barplot_show_labels",
                                                                    label="Show Values Outside Bars",
                                                                    value=FALSE),
                                                     prettyCheckbox(inputId="barplot_show_labels_inside",
                                                                    label="Show Values Inside Bars",
                                                                    value=FALSE)),
                                    conditionalPanel(condition="input.barplot_single_or_group == 'Multi Group'",
                                                     radioGroupButtons(inputId="barplot_stacked_or_dodge",
                                                                       label="Select",
                                                                       choices=c("Side-by-Side", "Stacked", "% Stacked"),
                                                                       selected="Side-by-Side",
                                                                       status="primary"),
                                                     prettyCheckbox(inputId="barplot_multi_show_values_inside",
                                                                    label="Show Values Inside",
                                                                    value=FALSE),
                                                     prettyCheckbox(inputId="barplot_multi_facet",
                                                                    label="facet",
                                                                    value=FALSE),
                                                     selectInput(inputId="barplot_multi_facet_options",
                                                                 label = "Facet Variable",
                                                                 choices = character())))
                            ) #end column
                          ), #end fluidRow

                            
                          br(),
                          box(
                            #this is a box that holds the import data options.
                            title=NULL, status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                            
                            tabBox(
                              title="Options",
                              width=12,
                              
                              tabPanel("Group Color Options",
                                       fluidRow(
                                         column(width=6,
                                                sliderInput(inputId="barplot_strata_alpha",
                                                            label="Select Level of Transperency",
                                                            min=0,
                                                            max=1,
                                                            step=0.1,
                                                            value=0.5))),
                                       fluidRow(
                                         #checkbox that turns on strata
                                         column(width=6,
                                          conditionalPanel(condition="input.barplot_single_or_group == 'Single Group'",
                                             fluidRow(
                                               column(width=5,
                                                      colourInput(inputId = "bar_single_color_fill",
                                                                  label="Fill Color",
                                                                  value="blue")),
                                               column(width=5,
                                                      colourInput(inputId = "bar_single_color_outline",
                                                                  label="Outline Color",
                                                                  value="white")))),
                                          conditionalPanel(condition="input.barplot_single_or_group == 'Multi Group'",
                                                #this outputs a panel of data that displays a color panel for each categorical variable in the strata'd variable
                                              fluidRow(
                                                column(width=5,
                                                       uiOutput('barplot_strata_color_options_popdown')),
                                                column(width=5,
                                                       uiOutput("barplot_strata_fill_options_popdown"))))
                                         )#end column
                                       ) #end fluidRow

                              ), #end tabPanel
                              
                              tabPanel("Background Options",
                                       
                                       h3("Plot Background Options"),
                                       selectInput(
                                         inputId = "barplot_theme_choice",
                                         label = "Background Theme", 
                                         choices = c("gray", "bw", "linedraw", "light", "minimal", "classic", "void", "dark")
                                       ),
                                       fluidRow(
                                         div(style="display:inline-block; text_align:right;", 
                                             prettyCheckbox(inputId="barplot_panel_colorPicker_checkbox", 
                                                            label=NULL, 
                                                            value = FALSE)),
                                         div(style="display:inline-block; text_align:right;", 
                                             colourInput(inputId="barplot_panel_colorPicker", 
                                                         label="Select Color", 
                                                         value="blue")))
                                       
                              ),#end tabPanel
                              tabPanel("Legend Options",
                                       selectInput(inputId="barplot_legend_position",
                                                   label = "Location of Legend",
                                                   choices = c("left", "right", "top", "bottom", "none"),
                                                   selected = "right"),
                                       textInput(inputId="barplot_legend_title",
                                                 label = "Legend Title",
                                                 value = ""))
                              
                              
                            )#End tabBox
                          ) #End box
                          
) # end tabItem
