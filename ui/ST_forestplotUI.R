#This tab corresponds to the "ForestPlot" SubTab (under Graphing Tab) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  October 23, 2020
#  Last Update: October 23, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------
#__________________________________________________________________________________________________________
#SUBTAB: FORESTPLOT
#__________________________________________________________________________________________________________
SUBTAB_FORESTPLOT <- tabItem(tabName = "TAB_FORESTPLOT", 
                             fluidRow(
                               column(width=10,
                                      tags$h2("Forest Plot")),
                               column(width=2,
                                      dropdownButton(
                                         status="info",
                                         icon=icon("info"),
                                         width="1000px",
                                         size="xs",
                                         right = TRUE,
                                         tooltip=tooltipOptions(title = "Click for help",
                                                                placement="left"),
                                         h2('Forest Plot'),
                                         p("This tab allows for the plotting of a forest plot. Currently data must be entered in a very specific way. First,
                                           if 'OR' option is selected then the data must be in the form below.  In addition, columns must be separated into categorical
                                           and numerical data in 'inputs' options.  In this example, 'Category', 'Contrast', and CI' are selected as categorical"),
                                         div(img(src="./images/forest_plot/OR_table.JPG",
                                                 width= "40%",
                                                 height="40%",
                                                 align=""), style="text-align: center;"),
                                         p("The 'Raw Data' Option under 'Inputs' will run multiple linear regression on the data and then plot
                                           the corresponding plot.  However, this was written before the MLR model under MultiVariate statistics
                                           and is not as sophisticated.  It will run all variables as numerical but does not separate them into factors.
                                           This is something to update in the future.  I think we might be able to get the program to autodetect if the column
                                           is numeric or categorical, but that is a problem for the future.")
                                       ), #end dropdownButton
                                       align="right")
                             ), #end FluidRow
                           
                           br(),
                           fluidRow(
                             column(width=9,
                                    fluidRow(
                                      column(width=6,
                                             dropdown(
                                               label= "Inputs",
                                               icon=icon("sliders"),
                                               circle = FALSE, 
                                               status = "primary",
                                               #choice buttons that allows user to select 'type' of forestplot input to use
                                               radioGroupButtons(inputId="forest_data_load_options",
                                                                 label="Select",
                                                                 choices=c("OR Table" = "OR",
                                                                           "Raw Data" = "RAW"),
                                                                 status="primary"),
                                               #select single variable. One boxplot
                                               conditionalPanel(condition="input.forest_data_load_options == 'OR'",
                                                                pickerInput(inputId = 'forestplot_OR_select_variables',
                                                                            label = 'Select all columns to use',
                                                                            choices = character(),
                                                                            options=list('actions-box'=TRUE), 
                                                                            multiple=TRUE),
                                                                pickerInput(inputId = 'forestplot_OR_select_text',
                                                                            label = 'Select text Columns',
                                                                            choices = character(),
                                                                            options=list('actions-box'=TRUE), 
                                                                            multiple=TRUE),
                                                                pickerInput(inputId = 'forestplot_OR_select_num',
                                                                            label = 'Select Numerical Columns',
                                                                            choices = character(),
                                                                            options=list('actions-box'=TRUE), 
                                                                            multiple=TRUE)),
                                               conditionalPanel(condition="input.forest_data_load_options == 'RAW'",
                                                                selectInput(inputId = 'forestplot_RAW_indepVar',
                                                                            label = 'Select independent Var',
                                                                            choices = character()),
                                                                pickerInput(inputId = "forestplot_RAW_dependentVar",
                                                                            label = "Select Dependent Variables for model",
                                                                            choices = character(),
                                                                            options=list('actions-box'=TRUE), 
                                                                            multiple=TRUE)),
                                               actionButton(inputId ="forest_plot_button",
                                                            label = 'Push2Plot')
                                                                            
                                               
                                             ) #endDropDown
                                      ), #end column width=6
                                      column(width=6,
                                             #____________________________________
                                             #Dropdown Axis Options Button
                                             #____________________________________
                                             #These divs are used to place the buttons inline with eachother in the column.
                                             div(style="display:inline-block; text_align:right;",
                                                 #OPTIONS BUTTON 
                                                 dropdownButton(label = "Axis Options",icon = icon("gear"),circle = FALSE,status = "primary",right=TRUE,
                                                                textInput(inputId="forest_title", 
                                                                          label="Title", 
                                                                          value = ""),
                                                                textInput(inputId="forest_xlabel", 
                                                                          label="X Label", 
                                                                          value = ""),
                                                                textInput(inputId="forest_ylabel", 
                                                                          label="Y Label", 
                                                                          value = "")
                                                 ) #end Dropdown Button
                                             ), #end div
                                             div(style="display:inline-block; text_align:right;",
                                                 #DOWNLOAD BUTTON
                                                 #____________________________________
                                                 #Dropdown Download Button
                                                 #____________________________________
                                                 dropdownButton(label = "Download",icon = icon("download"),circle = FALSE,status = "primary",right=TRUE,
                                                                textInput(
                                                                  inputId ="forest_download_title",
                                                                  label=NULL,
                                                                  value = "",
                                                                  placeholder = "Type Download TItle",
                                                                  width = NULL
                                                                ),
                                                                radioGroupButtons(
                                                                  inputId = "forest_download_radiobuttons",
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
                                                                downloadBttn(outputId="downloadForest",
                                                                             label="Download",
                                                                             style="unite",
                                                                             color="primary",
                                                                             size="sm",
                                                                             block=FALSE,
                                                                             no_outline=FALSE)
                                                 ) #end dropdownButton
                                             ), #end Div
                                             align = 'right'
                                      )#end Column width=6
                                    ),#end FluidRow
                                    #____________________________________
                                    #PlotOutput Line Plot
                                    #____________________________________
                                    verbatimTextOutput("forest_out", placeholder = FALSE),
                                    jqui_resizable(plotOutput(outputId = 'ForestPlot'))
                             ), #end column width=9
                             #____________________________________
                             #Plot Addon Sidebar
                             #____________________________________
                             column(width=3,
                                    box(
                                      title="Plot Addons", status="primary", solidHeader=TRUE, collapsible=TRUE, width=NULL,
                                      sliderInput(inputId="forest_size_options",
                                                  label="Size of Lines",
                                                  min=0,
                                                  max=3,
                                                  step=0.2,
                                                  value=1),
                                      prettyCheckbox(inputId = "forest_show_dots",
                                                     label="Show Points",
                                                     value=FALSE))
                                    
                             ) #endColumn
                           ), #end FluidRow
                           br(),
                           #____________________________________
                           #Options containing Tabs
                           #____________________________________
                           box(
                             #this is a box that holds the import data options.
                             title=NULL, status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                             
                             tabBox(
                               title="Options",
                               width=12,
                               #____________________________________
                               #Color Options
                               #____________________________________
                               tabPanel("Regression Table",
                                        fluidRow(
                                          #add line color options
                                          uiOutput("forest_html_multi_regression_table")
                                        ) #end fluidRow
                               ), #end tabPanel
                               #____________________________________
                               #Background Options
                               #____________________________________                        
                               tabPanel("Background Options",
                                        
                                        fluidRow(
                                          column(width=5,
                                                 selectInput(
                                                   inputId = "theme_output_forest",
                                                   label = "Background Theme", 
                                                   choices = c("gray", "bw", "linedraw", "light", "minimal", "classic", "void", "dark")
                                                 ),
                                                 fluidRow(
                                                   div(style="display:inline-block; text_align:right;", 
                                                       prettyCheckbox(inputId="forest_panel_colorPicker_checkbox", 
                                                                      label=NULL, 
                                                                      value = FALSE)),
                                                   div(style="display:inline-block; text_align:right;", 
                                                       colourInput(inputId="forest_panel_colorPicker", 
                                                                   label="Select Color", 
                                                                   value="grey"))))
                                        )
                                        
                                        
                               ),#end tabPanel
                               #____________________________________
                               #Legend Options
                               #____________________________________
                               tabPanel("Legend Options",
                                        fluidRow(
                                          column(width=5,
                                                 selectInput(inputId="forest_legend_position",
                                                             label = "Location of Legend",
                                                             choices = c("Left" = "left", 
                                                                         "Right" = "right", 
                                                                         "Top" = "top", 
                                                                         "Bottom" = "bottom", 
                                                                         "No Legend" = "none"),
                                                             selected = "right"),
                                                 textInput(inputId="forest_legend_title",
                                                           label = "Legend Title",
                                                           value = "")))
                               ) #end tabPanel
                               
                               
                               
                             )#End tabBox
                           ) #End box
                           
) # end tabItem