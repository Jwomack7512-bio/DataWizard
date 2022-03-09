#This tab corresponds to the "BoxPlot" SubTab (under Graphing Tab) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  July 28, 2020
#  Last Update: August 15, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------
#__________________________________________________________________________________________________________
#SUBTAB: BOXPLOT
#__________________________________________________________________________________________________________
SUBTAB_LINEPLOT <- tabItem(tabName = "TAB_LINEPLOT", 
                          tags$h2("Line Plot"),
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
                                      selectInput(inputId = 'lineplot_xvar',
                                                  label = 'x variable',
                                                  choices = character()),
                                      pickerInput(inputId = 'lineplot_yvar',
                                                  label = 'y variable(s)',
                                                  choices = character(),
                                                  options=list('actions-box'=TRUE), 
                                                  multiple=TRUE)
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
                                                           textInput(inputId="line_title", 
                                                                     label="Title", 
                                                                     value = ""),
                                                           textInput(inputId="line_xlabel", 
                                                                     label="X Label", 
                                                                     value = ""),
                                                           textInput(inputId="line_ylabel", 
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
                                                             inputId ="line_download_title",
                                                             label=NULL,
                                                             value = "",
                                                             placeholder = "Type Download TItle",
                                                             width = NULL
                                                           ),
                                                           radioGroupButtons(
                                                             inputId = "line_download_radiobuttons",
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
                                                           downloadBttn(outputId="downloadLine",
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
                                    verbatimTextOutput("test", placeholder = FALSE),
                                    jqui_resizable(plotOutput(outputId = 'LinePlot'))
                            ), #end column width=9
#____________________________________
#Plot Addon Sidebar
#____________________________________
                            column(width=3,
                                   box(
                                     title="Plot Addons", status="primary", solidHeader=TRUE, collapsible=TRUE, width=NULL,
                                     sliderInput(inputId="line_size_options",
                                                 label="Size of Lines",
                                                 min=0,
                                                 max=3,
                                                 step=0.2,
                                                 value=1),
                                     prettyCheckbox(inputId = "line_show_dots",
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
                            tabPanel("Color Options",
                                     fluidRow(
                                       #add line color options
                                       column(width=12,
                                               fluidRow(
                                                 column(width=3,
                                                        uiOutput("line_color_options_popdown")),
                                                 column(width=3,
                                                        uiOutput("line_type_options_popdown")))
                                       ) #end column
                                     ) #end fluidRow
                            ), #end tabPanel
    #____________________________________
    #Background Options
    #____________________________________                        
                                    tabPanel("Background Options",
                                             
                                             fluidRow(
                                               column(width=5,
                                                      selectInput(
                                                        inputId = "theme_output_line",
                                                        label = "Background Theme", 
                                                        choices = c("gray", "bw", "linedraw", "light", "minimal", "classic", "void", "dark")
                                                      ),
                                                      fluidRow(
                                                        div(style="display:inline-block; text_align:right;", 
                                                            prettyCheckbox(inputId="line_panel_colorPicker_checkbox", 
                                                                           label=NULL, 
                                                                           value = FALSE)),
                                                        div(style="display:inline-block; text_align:right;", 
                                                            colourInput(inputId="line_panel_colorPicker", 
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
                                                        selectInput(inputId="line_legend_position",
                                                                    label = "Location of Legend",
                                                                    choices = c("Left" = "left", 
                                                                                "Right" = "right", 
                                                                                "Top" = "top", 
                                                                                "Bottom" = "bottom", 
                                                                                "No Legend" = "none"),
                                                                    selected = "right"),
                                                        textInput(inputId="line_legend_title",
                                                                  label = "Legend Title",
                                                                  value = "")))
                                      ) #end tabPanel
                                      
                                      
                                      
                                    )#End tabBox
                                  ) #End box
                          
) # end tabItem