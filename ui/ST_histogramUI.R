#This tab corresponds to the "Histogram" SubTab (under Graphing Tab) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  July 28, 2020
#  Last Update: August 18, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

SUBTAB_HISTOGRAM <- tabItem(tabName = "TAB_HISTOGRAM", 
                          tags$h2("Histogram"),
                          br(),
                          fluidRow(
                            column(width=9,
                                dropdownButton(
                                  label= "Inputs",
                                  icon=icon("sliders"),
                                  circle = FALSE, 
                                  status = "primary",
                                  
                                  selectInput(inputId = 'histogram_var',
                                              label = 'Select Variable',
                                              choices = character()),
                                  radioGroupButtons(inputId="histogram_bin_choice",
                                                    label="Bin Sizing By",
                                                    choices=c("Number of Bins", "Numerical Width of Bins"),
                                                    status="primary"),
                                  conditionalPanel(condition="input.histogram_bin_choice=='Number of Bins'",
                                                   sliderInput(inputId = 'histogram_bins',
                                                                label = 'Number of Bins',
                                                                min = 0,
                                                                max = 100, 
                                                                value = 10)),
                                  conditionalPanel(condition="input.histogram_bin_choice=='Numerical Width of Bins'",
                                                   textInput(inputId='histogram_binwidth',
                                                             label="Choose bin width",
                                                             value="",
                                                             width="200px"))
                                  )),
                                column(width=3,
                                       
                                       #These divs are used to place the buttons inline with eachother in the column.
                                       div(style="display:inline-block; text_align:right;",
                                           #OPTIONS BUTTON 
                                           dropdownButton(label = "PH",icon = icon("gear"),circle = FALSE,status = "primary",right=TRUE,
                                                          h2("PlaceHolder")
                                           )),
                                 div(style="display:inline-block; text_align:right;",
                                     #DOWNLOAD BUTTON
                                     dropdownButton(label = "Download",icon = icon("download"),circle = FALSE,status = "primary",right=TRUE,
                                                    textInput(
                                                      inputId ="histogram_download_title",
                                                      label=NULL,
                                                      value = "",
                                                      placeholder = "Type Download TItle",
                                                      width = NULL
                                                    ),
                                                    radioGroupButtons(
                                                      inputId = "histogram_download_radiobuttons",
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
                                                    downloadBttn(outputId="downloadHistogram",
                                                                 label="Download",
                                                                 style="unite",
                                                                 color="primary",
                                                                 size="sm",
                                                                 block=FALSE,
                                                                 no_outline=FALSE)
                                         )),
                                     align = 'right'
                                )#end Column
                              ),#end FluidRow
                          
                          jqui_resizable(plotOutput(outputId = 'HistogramPlot')),
                          br(),
                          box(
                            #this is a box that holds the import data options.
                            title=NULL, status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                            
                            tabBox(
                            title="Options",
                            width=12,
                            
                                tabPanel("Single Plot",
                                         h3("Histogram Bar Options"),
                                         fluidRow(
                                           column(width=3,
                                                  colourInput(inputId="histogram_color", 
                                                              label="Select Outline Color", 
                                                              value="black")),
                                           column(width=3,
                                                  colourInput(inputId="histogram_fill",
                                                              label="Select fill color",
                                                              value="white")),
                                           column(width=3,
                                                  pickerInput(inputId = "histogram_linetype",
                                                              label = "Line Type", 
                                                              choices = c("blank", "solid", "dashed", "dotted", "dotdash", "twodash", 'longdash'),
                                                              select="solid"))
                                          ), #end fluidRow
                                        
                                          prettyCheckbox(inputId="histogram_mean_line_on",
                                                         label="Toggle Mean Line",
                                                         value=FALSE),
                                         h3("Plot Background Options"),
                                         selectInput(
                                           inputId = "histogram_theme_choice",
                                           label = "Background Theme", 
                                           choices = c("gray", "bw", "linedraw", "light", "minimal", "classic", "void", "dark")
                                         ),
                                         fluidRow(
                                           div(style="display:inline-block; text_align:right;", 
                                               prettyCheckbox(inputId="histogram_panel_colorPicker_checkbox", 
                                                              label=NULL, 
                                                              value = FALSE)),
                                           div(style="display:inline-block; text_align:right;", 
                                               colourInput(inputId="histogram_panel_colorPicker", 
                                                           label="Select Color", 
                                                           value="blue")))
                                         
                                         ), #end tabPanel
                                
                                tabPanel("Strata",
                                         fluidRow(
                                           #checkbox that turns on strata
                                           column(width=6,
                                               div(style="display:inline-block; text_align:right;",  
                                                   prettyCheckbox(inputId="histogram_strata_on",
                                                                  label="Turn Strata On",
                                                                  value=FALSE)),
                                               #dropdown that displays options for strata variable
                                               div(style="display:inline-block; text_align:right;",
                                                   pickerInput(inputId="histogram_strata_choices",
                                                               label="Select Strata Variable",
                                                               choices=character())),
                                               #this outputs a panel of data that displays a color panel for each categorical variable in the strata'd variable
                                               fluidRow(
                                                 column(width=5,
                                                        uiOutput('histogram_strata_color_options_popdown')),
                                                 column(width=5,
                                                        uiOutput("histogram_strata_fill_options_popdown"))
                                                  ) #end fluidRow
                                                ), #end column
                                         
                                           conditionalPanel(condition="input.histogram_strata_on",
                                             column(width=6,
                                                    pickerInput(inputId="histogram_strata_overlap_position",
                                                                label="Select Overlap Mode",
                                                                choices=c("stack", "identity", "dodge"),
                                                                selected="stack"),
                                                    br(),
                                                    sliderInput(inputId="histogram_strate_alpha",
                                                                label="Select Level of Transperency",
                                                                min=0,
                                                                max=1,
                                                                step=0.1,
                                                                value=0.5),
                                                    br(),
                                                    prettyCheckbox(inputId="histogram_strata_mean_lines",
                                                                   label="Show Mean Lines (Does not function as of now)",
                                                                   value=FALSE)))
                                           ) #end fluidRow
                                         
                                         
                                     ), #end tabPanel
                            tabPanel("Output",
                                     tableOutput("histogram_results_table")
                              
                            ) #end tabPanel
                          )#End tabBox
                        ) #End box
    
                  ) # end tabItem
