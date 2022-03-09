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
SUBTAB_BOXPLOT <- tabItem(tabName = "TAB_BOXPLOT", 
                    tags$h2("Box Plot"),
                    br(),
                    fluidRow(
                      column(width=6,
#____________________________________
#Dropdown Input Button
#____________________________________
                             dropdownButton(
                                label= "Inputs",
                                icon=icon("sliders"),
                                circle = FALSE, 
                                status = "primary",
                                tags$h3("List of Inputs"),
                                #choice buttons that allows user to select 'type' of boxplot to show (single or groups)
                                radioGroupButtons(inputId="boxplot_single_or_group",
                                                  label="Select",
                                                  choices=c("Single Boxplot" = "single",
                                                            "Group Boxplot" = "group"),
                                                  status="primary"),
                                #select single variable. One boxplot
                                conditionalPanel(condition="input.boxplot_single_or_group == 'single'",
                                                 selectInput(inputId = "boxplot_single_var",
                                                             label = "Select Boxplot Variable",
                                                             choices = character())),
                                #set up variable by grouped variable
                                conditionalPanel(condition="input.boxplot_single_or_group == 'group'",
                                                 selectInput(inputId = 'boxplot_var',
                                                             label = 'Select Continous Variable',
                                                             choices = character()),
                                                 selectInput(inputId = 'boxplot_factor_var',
                                                             label = 'Select Variable to Factor By',
                                                             choices = character()))
                                )
                             ),#end Column
                      column(width=3,
#____________________________________
#Dropdown Axis Title Button
#____________________________________                             
                             #These divs are used to place the buttons inline with eachother in the column.
                             div(style="display:inline-block; text_align:right;",
                                 #OPTIONS BUTTON 
                                 dropdownButton(label = "Axis Options",icon = icon("gear"),circle = FALSE,status = "primary",right=TRUE,
                                                textInput(inputId="boxplot_title", 
                                                          label="Title", 
                                                          value = ""),
                                                textInput(inputId="boxplot_xlabel", 
                                                          label="X Label", 
                                                          value = ""),
                                                textInput(inputId="boxplot_ylabel", 
                                                          label="Y Label", 
                                                          value = "")
                                 )),
                             div(style="display:inline-block; text_align:right;",
                                 #DOWNLOAD BUTTON
#____________________________________
#Dropdown Download Button
#____________________________________
                                 dropdownButton(label = "Download",icon = icon("download"),circle = FALSE,status = "primary",right=TRUE,
                                                textInput(
                                                  inputId ="boxplot_download_title",
                                                  label=NULL,
                                                  value = "",
                                                  placeholder = "Type Download TItle",
                                                  width = NULL
                                                ),
                                                radioGroupButtons(
                                                  inputId = "boxplot_download_radiobuttons",
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
                                                downloadBttn(outputId="downloadboxplot",
                                                             label="Download",
                                                             style="unite",
                                                             color="primary",
                                                             size="sm",
                                                             block=FALSE,
                                                             no_outline=FALSE)
                                 )),
                             align = 'right'
                      )#end Column
                    ), #end fluidRow
                    
                    
#____________________________________
#Plot Boxplot
#____________________________________
                  fluidRow(                      
                      column(width=9,
                             jqui_resizable(plotOutput(outputId = 'BoxPlot'))),
#____________________________________
#Plot Addon Sidebar
#____________________________________
                      column(width=3,
                             box(
                               title="Plot Addons", status="primary", solidHeader=TRUE, collapsible=TRUE, width=NULL,
                               sliderInput(inputId = "boxplot_width",
                                           label="Width of Boxplot",
                                           min=0,
                                           max=1,
                                           step=0.1,
                                           value=0.5),
                               prettyCheckbox(inputId="boxplot_plot_flip",
                                              label="Flip Axis",
                                              value=FALSE),
                               prettyCheckbox(inputId="boxplot_notched",
                                              label="Notched Boxplot",
                                              value=FALSE),
                               prettyCheckbox(inputId="boxplot_plot_mean_points",
                                              label="Add Mean Points",
                                              value=FALSE),
                               prettyCheckbox(inputId="boxplot_plot_dotplot",
                                              label="Add DotPlot Inside",
                                              value=FALSE),
                               conditionalPanel(condition = "input.boxplot_plot_dotplot",
                                                fluidRow(column(width=11, offset=1,
                                                                sliderInput(inputId="boxplot_plot_dotplot_size",
                                                                             label = "Dot Size", 
                                                                             value=1, 
                                                                             min = .1,
                                                                             max = 2,
                                                                             step = 0.1)))),
                               prettyCheckbox(inputId="boxplot_plot_jitterplot",
                                              label="Add Jitter DotPlot Inside",
                                              value=FALSE),
                               #condiitonPanel for MultiGroup Options only
                               conditionalPanel(condition="input.boxplot_single_or_group == 'group'",
                                                prettyCheckbox(inputId="boxplot_multi_varwidth",
                                                               label="Size width by var count",
                                                               value = FALSE))
                               
                             ))
                    ), #end fluidRow
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
                           column(width=6,
                                  sliderInput(inputId="boxplot_strata_alpha",
                                              label="Select Level of Transperency",
                                              min=0,
                                              max=1,
                                              step=0.1,
                                              value=1))
                           ),#end FluidRow
                        
                         fluidRow(
                           #checkbox that turns on strata
                           column(width=5,
                                  conditionalPanel(condition="input.boxplot_single_or_group == 'single'",
                                                   fluidRow(
                                                     column(width=5,
                                                            colourInput(inputId = "box_single_color_fill",
                                                                        label="Fill Color",
                                                                        value="white")),
                                                     column(width=5,
                                                            colourInput(inputId = "box_single_color_outline",
                                                                        label="Outline Color",
                                                                        value="black")))),
                            
                                  #this outputs a panel of data that displays a color panel for each categorical variable in the strata'd variable
                                  conditionalPanel(condition="input.boxplot_single_or_group=='group'",
                                                   fluidRow(
                                                     column(width=5,
                                                            uiOutput('boxplot_strata_color_options_popdown')),
                                                     column(width=5,
                                                            uiOutput("boxplot_strata_fill_options_popdown"))))
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
                                            inputId = "boxplot_theme_choice",
                                            label = "Background Theme", 
                                            choices = c("gray", "bw", "linedraw", "light", "minimal", "classic", "void", "dark")
                                          ),
                                          fluidRow(
                                            div(style="display:inline-block; text_align:right;", 
                                                prettyCheckbox(inputId="boxplot_panel_colorPicker_checkbox", 
                                                               label=NULL, 
                                                               value = FALSE)),
                                            div(style="display:inline-block; text_align:right;", 
                                                colourInput(inputId="boxplot_panel_colorPicker", 
                                                            label="Select Color", 
                                                            value="blue"))))
                                 )
                                 
                                 
                        ),#end tabPanel
#____________________________________
#Legend Options
#____________________________________
                        tabPanel("Legend Options",
                                 fluidRow(
                                   column(width=5,
                                          selectInput(inputId="boxplot_legend_position",
                                                      label = "Location of Legend",
                                                      choices = c("Left" = "left", 
                                                                  "Right" = "right", 
                                                                  "Top" = "top", 
                                                                  "Bottom" = "bottom", 
                                                                  "No Legend" = "none"),
                                                      selected = "right"),
                                          textInput(inputId="boxplot_legend_title",
                                                    label = "Legend Title",
                                                    value = "")))
                                 ) #end tabPanel
                                 
                          
                        
                      )#End tabBox
                    ) #End box

            ) # end tabItem