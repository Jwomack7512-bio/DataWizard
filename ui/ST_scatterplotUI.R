#This tab corresponds to the "Scatter" SubTab (under Graphing Tab) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  July 28, 2020
#  Last Update: August 15, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

SUBTAB_SCATTER <- tabItem(tabName = "TAB_SCATTER", 
                    tags$h2("ScatterPlots"),
                    br(),
#__________________________________________________________________________________________________________

#Top Button: Inputs

#__________________________________________________________________________________________________________
                    fixedRow(
                      #INPUT DROPDOWN BUTTON
                      column(width=8,
                             dropdownButton(
                               size= "lg",
                               label= "Inputs",
                               icon=icon("sliders"),
                               selectInput(inputId = 'scatter_xcol',
                                           label = 'X Variable',
                                           choices = character()),
                               
                               selectInput(inputId = 'scatter_ycol',
                                           label = 'Y Variable',
                                           choices = character()),
                               
                               
                               circle = FALSE, status = "primary"
                               #icon = icon("gear"), width = "300px",
                               
                               #tooltip = tooltipOptions(title = "Click to select inputs!")
                             )
                      ),#end column
                      column(width=4,
#__________________________________________________________________________________________________________

#Top Right Buttons, "axis Options" and "Download"

#__________________________________________________________________________________________________________
                             #These divs are used to place the buttons inline with eachother in the column.
                             div(style="display:inline-block; text_align:right;",
                                 #OPTIONS BUTTON 
                                 dropdownButton(size= "lg",
                                                label = "Axis Options",
                                                icon = icon("gear"),
                                                circle = FALSE,
                                                status = "primary",right=TRUE,
                                                
                                                radioGroupButtons(inputId="scatter_axis_options",
                                                                  label="Edit",
                                                                  choices=c("Labels", "Axis Range")),
                                                conditionalPanel(condition="input.scatter_axis_options=='Axis Range'",
                                                    fluidRow(
                                                      column(width=4,
                                                             numericInput(inputId="scatter_xaxis_min", 
                                                                 label="x-axis min",
                                                                 value=NULL)),
                                                      column(width=4,
                                                              numericInput(inputId="scatter_xaxis_max", 
                                                                           label="x-axis max", 
                                                                           value=NULL)),
                                                      column(width=4,
                                                             numericInput(inputId="scatter_xstep",
                                                                          label="x step",
                                                                          value=NULL))
                                                      ),#end fluidRow
                                                    fluidRow(
                                                      column(width=4,
                                                            numericInput(inputId="scatter_yaxis_min",
                                                                         label="y-axis min", 
                                                                         value=NULL)),
                                                      column(width=4,
                                                            numericInput(inputId="scatter_yaxis_max", 
                                                                         label="y-axis max", 
                                                                         value=NULL)),
                                                      column(width=4,
                                                             numericInput(inputId="scatter_ystep",
                                                                          label="y step",
                                                                          value=NULL))
                                                    ),#end fluidRow
                                                    switchInput(inputId="scatter_axis_confirm", 
                                                                label="Change Axis", 
                                                                labelWidth='80px')
                                                ),#end ConditionalPanel
                                                conditionalPanel(condition="input.scatter_axis_options=='Labels'",
                                                    textInput(inputId="scatter_title", 
                                                              label="Title", 
                                                              value = ""),
                                                    textInput(inputId="scatter_xlabel", 
                                                              label="X Label", 
                                                              value = ""),
                                                    textInput(inputId="scatter_ylabel", 
                                                              label="Y Label", 
                                                              value = "")
                                                )#end conditionalPanel
                                 )),
                             div(style="display:inline-block; text_align:right;",
                                 #DOWNLOAD BUTTON
                                 dropdownButton(size= "lg",
                                                label = "Download",
                                                icon = icon("download"),
                                                circle = FALSE
                                                ,status = "primary",
                                                right=TRUE,
                                                
                                                textInput(
                                                  inputId ="scatter_download_title",
                                                  label=NULL,
                                                  value = "",
                                                  placeholder = "Type Download TItle",
                                                  width = NULL
                                                ),
                                                radioGroupButtons(
                                                  inputId = "scatter_download_radiobuttons",
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
                                                downloadBttn(outputId="downloadScatter",
                                                             label="Download",
                                                             style="unite",
                                                             color="primary",
                                                             size="lg",
                                                             block=FALSE,
                                                             no_outline=FALSE)
                                 )),
                             align = 'right'
                      )#end Column
                    ),#end FluidRow
#__________________________________________________________________________________________________________

#Plot

#__________________________________________________________________________________________________________
                    jqui_resizable(plotOutput(outputId = 'ScatterPlot')),
                    br(),
                    box(
                      #this is a box that holds the import data options.
                      title=NULL, status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
#__________________________________________________________________________________________________________

#Tabbox Start

#__________________________________________________________________________________________________________
                      tabBox(
                        title = "Advanced Options", width = 12, 
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1",
                        #tab contents values that will affect the 'dots' of the scatter plot
#__________________________________________________________________________________________________________

# TabPanel: Dot Options

#__________________________________________________________________________________________________________
                        tabPanel("Stylistic Dot Options",
                                 fluidRow(
                                   column(width=5,
                                       #slider to change the size of dots.  1.5 seems to be standard size
                                       sliderInput(inputId="scatter_dot_size",
                                                   label="Dot Size:",
                                                   min=0,
                                                   max=10,
                                                   value=1.5,
                                                   step=0.5),
                                       #color selector that allows user to change dot color (starts on black)
                                       colourInput(inputId="scatter_dot_color", 
                                                   label="Select Color", 
                                                   value="black"),
                                 fluidRow(
                                   #dropdown menu that allows user to select dot type. Contains 19 objects,
                                   #These are numbers 0-18 because that is there corresponding id code in r
                                   #and I don't feel like describing 19 dots and writing 19 if statements.
                                   #Although it could probably just use a list and lookup value. But I digress.
                                   div(style="display:inline-block; text_align:right;",
                                       pickerInput(
                                         inputId = "scatter_dot_shape",
                                         label = "Dot type", 
                                         width="400px",
                                         choices = c("0", "1", "2", "3", "4", "5", "6", "7", "8","9","10","11","12","13","14","15","16", "17","18"),
                                         #16 is the solid standard filled dot.  I think thats the standard.
                                         selected = "16",
                                         #only shows 5 options at a time because the whole scroll bar was clunky at the bottom
                                         options = list(size=5))),
                                   #dropdown button that when clicked displays the key to the above numbers showing the dot options
                                   div(style="display:inline-block; text_align:right;",
                                          dropdownButton(
                                            size= "xs",
                                            status="info",
                                            icon=icon("info"),
                                            width="300px",
                                            tooltip=tooltipOptions(title = "Click to see dot key"),
                                            imageOutput("scatter_dot_key")))
                                    ) #End FluidRow
                                   ))
                                 ),#end Tab Panel
                        #This tabPanel sets up strata which allows the grouping of data by categorical values
#__________________________________________________________________________________________________________

# TabPanel: Strata Options

#__________________________________________________________________________________________________________
                        tabPanel("Strata",
                                 fluidRow(
                                   #checkbox that turns on strata
                                   div(style="display:inline-block; text_align:right;",  
                                   prettyCheckbox(inputId="scatter_strata_on",
                                                label="Turn Strata On",
                                                value=FALSE)),
                                   #dropdown that displays options for strata variable
                                   div(style="display:inline-block; text_align:right;",
                                   pickerInput(inputId="scatter_strata_choices",
                                               label="Select Strata Variable",
                                               choices=character()))),
                                   #this outputs a panel of data that displays a color panel for each categorical variable in the strata'd variable
                                   fluidRow(
                                     column(width=2,
                                            uiOutput('scatter_strata_color_options_popdown')),
                                     column(width=2,
                                            uiOutput("scatter_strata_dot_options_popdown")),
                                     column(width=2,
                                            uiOutput("scatter_strata_dotsize_options_popdown"))
                                   ) #end fluidRow
                                 ), #end tabPanel
#__________________________________________________________________________________________________________

# TabPanel: Regression Options

#__________________________________________________________________________________________________________
                        tabPanel("Regression",
                                 prettyCheckbox(inputId="scatter_regression_checkbox", 
                                                label="Turn on Regression", 
                                                value=FALSE),
                                 conditionalPanel(condition="input.scatter_regression_checkbox",
                                                  hr(),
                                                  fluidRow(
                                                    column(width=4,
                                                           box(
                                                             #this is a box that holds the import data options.
                                                             title="Line Options", 
                                                             status="primary", 
                                                             solidHeader=FALSE, 
                                                             collapsible=TRUE, 
                                                             width=NULL,
                                                             
                                                             pickerInput(inputId = "scatter_regression_lineType",
                                                                         label = "Line Type", 
                                                                         choices = c("blank", "solid", "dashed", "dotted", "dotdash", "twodash", 'longdash'),
                                                                         select="solid"),
                                                             
                                                             colourInput(inputId="scatter_regression_lineColor", 
                                                                         label="Select Color", 
                                                                         value="black"),
                                                             
                                                             sliderInput(inputId="scatter_regression_linesize",
                                                                         label="Line Size:",
                                                                         min=0,
                                                                         max=5,
                                                                         value=1,
                                                                         step=0.5)
                                                             
                                                           ) #end box
                                                        ),#end Column
                                                    column(width=4,
                                                           box(
                                                             #this is a box that holds the import data options.
                                                             title="Regression Eqn", 
                                                             status="primary", 
                                                             solidHeader=FALSE, 
                                                             collapsible=TRUE, 
                                                             width=NULL,
                                                             
                                                             prettyCheckbox(inputId="scatter_regression_equationDisplay",
                                                                            label="Show Regression Equation on Graph",
                                                                            value=FALSE),
                                                             conditionalPanel(condition="input.scatter_regression_equationDisplay",
                                                                              pickerInput(inputId="scatter_regression_equation_size",
                                                                                          label = "Size of Equation",
                                                                                          choices= c("Small" = "Small",
                                                                                                     "Medium" = "Medium",
                                                                                                     "Large" = "Large"),
                                                                                          selected="Medium"),
                                                                              prettyCheckbox(inputId="scatter_regression_customPosition",
                                                                                             label="Change Position of Eqn",
                                                                                             value=FALSE),
                                                                              hr(),
                                                                              conditionalPanel(condition="input.scatter_regression_customPosition",
                                                                                               radioButtons(inputId = "scatter_regression_customPosition_choice",
                                                                                                                  label = NULL,
                                                                                                                  choices = c("Relative" = "Relative",
                                                                                                                              "Numerical" = "Numerical"),
                                                                                                                  inline = TRUE),
                                                                                               conditionalPanel(condition = "input.scatter_regression_customPosition_choice=='Numerical'",
                                                                                                               textInput(inputId="scatter_regression_numerical_x",
                                                                                                                         label="Relative x Position (between 0-1)",
                                                                                                                         value=""),
                                                                                                               textInput(inputId="scatter_regression_numerical_y",
                                                                                                                         label="Relative y Position (between 0-1)",
                                                                                                                         value="")
                                                                                                               ), #end conditional Panel for Numerical
                                                                                               conditionalPanel(condition = "input.scatter_regression_customPosition_choice=='Relative'",
                                                                                                              pickerInput(inputId="scatter_regression_relative_x",
                                                                                                                          label="x",
                                                                                                                          choices=c("right" = "right",
                                                                                                                                    "left" = "left")),
                                                                                                              pickerInput(inputId="scatter_regression_relative_y",
                                                                                                                          label="y",
                                                                                                                          choices=c("top" = "top",
                                                                                                                                    "bottom" = "bottom",
                                                                                                                                    "center" = "center"))
                                                                                                              ) #end conditionalPanel for "Relative"
                                                                                               )#end conditional Panel for input.scatter_regression_csutomPosition
                                                                              )#end conditional panel scatter_regression_equationDisplay
                                                           )),
                                                    column(width=4,
                                                           box(
                                                             #this is a box that holds the import data options.
                                                             title="Additional Options", 
                                                             status="primary", 
                                                             solidHeader=FALSE, 
                                                             collapsible=TRUE, 
                                                             width=NULL,
                                                             
                                                             prettyCheckbox(inputId="scatter_regression_CI_interval_checkbox",
                                                                            label="Toggle Confidence Interval on Graph",
                                                                            value=FALSE),
                                                             prettyCheckbox(inputId="scatter_regression_LoessMethod",
                                                                            label="Use Loess Method of Regression",
                                                                            value=FALSE),
                                                             prettyCheckbox(inputId="scatter_regression_strata_fullrange",
                                                                            label="Show full range regression line on stratified sample",
                                                                            value=FALSE)
                                                              )#end box
                                                    )#end Column
                                                  ) #end fluidRow
                                                ) #end conditionalPanel
                                 ), #end tabPanel
#__________________________________________________________________________________________________________

# TabPanel: Clustering Options

#__________________________________________________________________________________________________________
                        tabPanel("Clustering",
                                 materialSwitch(
                                   inputId = "scatter_cluster_switch",
                                   label = "Turn on Clustering", 
                                   value = FALSE,
                                   status = "primary"
                                 ),
                                 sliderInput(inputId = 'scatter_cluster_slider',
                                             label = 'Cluster count',
                                             value = 3,
                                             min = 1,
                                             max = 9)
                        ),#end tabpanel
#__________________________________________________________________________________________________________

# TabPanel: Aesthetics Options

#__________________________________________________________________________________________________________
                        tabPanel("Aesthetics",
                                 fluidRow(
                                   column(width=4,
                                          box(title="Panel Background", status = "primary", soldiheader=TRUE,
                                              selectInput(
                                                inputId = "scatter_theme_choice",
                                                label = "Background Theme", 
                                                choices = c("gray", "bw", "linedraw", "light", "minimal", "classic", "void", "dark")
                                              ),
                                              fluidRow(
                                                div(style="display:inline-block; text_align:right;", 
                                                    prettyCheckbox(inputId="scatter_panel_colorPicker_checkbox", 
                                                                   label=NULL, 
                                                                   value = FALSE)),
                                                div(style="display:inline-block; text_align:right;", 
                                                    colourInput(inputId="scatter_panel_colorPicker", 
                                                                label="Select Color", 
                                                                value="blue")))
                                              
                                          )#endBox
                                   )#End Column
                                 )#end Fluid Row
                        )#end TabPanel
                      )#end tabBox
                    )#end Box
                    
            ) # end tabItem
