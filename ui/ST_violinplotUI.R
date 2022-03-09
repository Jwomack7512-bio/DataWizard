#This tab corresponds to the "Violin Plot" SubTab (under Graphing Tab) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  August 18, 2020
#  Last Update: August 18, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

SUBTAB_VIOLIN <- tabItem(tabName = "TAB_VIOLIN", 
                            tags$h2("Violin Plot"),
                            br(),
                         fluidRow(
                           column(width=9,
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
                                       #choice buttons that allows user to select 'type' of violinplot to show (single or groups)
                                       radioGroupButtons(inputId="violinplot_single_or_group",
                                                         label="Select",
                                                         choices=c("Single Violin Plot" = "single",
                                                                   "Group Violin Plot" = "group"),
                                                         status="primary"),
                                       #input options for single variable selection
                                       conditionalPanel(condition="input.violinplot_single_or_group == 'single'",
                                                        selectInput(inputId = "violin_single_var",
                                                                    label = "Select Variable to Plot",
                                                                    choices=character())),
                                       #input options for group variable selection                 
                                       conditionalPanel(condition="input.violinplot_single_or_group == 'group'",
                                                         selectInput(inputId = 'violin_var',
                                                                     label = 'Select Continous Variable',
                                                                     choices = character()),
                                                         selectInput(inputId="violin_factor_var",
                                                                     label = "Variable to factor by",
                                                                     choices=character()))
                                     )#end dropdownButton
                                    ), #end column width=6
                              column(width=6,
#____________________________________
#Dropdown Axis Options Button
#____________________________________
                                     #These divs are used to place the buttons inline with eachother in the column.
                                     div(style="display:inline-block; text_align:right;",
                                         #OPTIONS BUTTON 
                                         dropdownButton(label = "Axis Options",icon = icon("gear"),circle = FALSE,status = "primary",right=TRUE,
                                                        textInput(inputId="violin_title", 
                                                                  label="Title", 
                                                                  value = ""),
                                                        textInput(inputId="violin_xlabel", 
                                                                  label="X Label", 
                                                                  value = ""),
                                                        textInput(inputId="violin_ylabel", 
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
                                                          inputId ="violin_download_title",
                                                          label=NULL,
                                                          value = "",
                                                          placeholder = "Type Download TItle",
                                                          width = NULL
                                                        ),
                                                        radioGroupButtons(
                                                          inputId = "violin_download_radiobuttons",
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
                                                        downloadBttn(outputId="downloadViolin",
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
#PlotOutput Violin Plot
#____________________________________
                              jqui_resizable(plotOutput(outputId = 'ViolinPlot'))
                           ), #end column width=9
#____________________________________
#Plot Addon Sidebar
#____________________________________
                              column(width=3,
                                     box(
                                       title="Plot Addons", status="primary", solidHeader=TRUE, collapsible=TRUE, width=NULL,
                                       sliderInput(inputId = "violin_width",
                                                   label="Width of Violin Plot",
                                                   min=0,
                                                   max=1,
                                                   step=0.1,
                                                   value=0.5),
                                       prettyCheckbox(inputId="violin_plot_flip",
                                                      label="Flip Axis",
                                                      value=FALSE),
                                       prettyCheckbox(inputId="violin_plot_trim",
                                                      label="Trim Boxes",
                                                      value=FALSE),
                                       prettyCheckbox(inputId="violin_plot_mean_points",
                                                      label="Add Mean Points",
                                                      value=FALSE),
                                       prettyCheckbox(inputId="violin_plot_median_points",
                                                      label="Add Median Points",
                                                      value=FALSE),
                                       prettyCheckbox(inputId="violin_plot_boxplot",
                                                      label="Add Boxplot Inside",
                                                      value=FALSE),
                                       prettyCheckbox(inputId="violin_plot_mean_bar",
                                                      label="Add Mean with SD",
                                                      value=FALSE),
                                       prettyCheckbox(inputId="violin_plot_dotplot",
                                                      label="Add DotPlot Inside",
                                                      value=FALSE),
                                       conditionalPanel(condition = "input.violin_plot_dotplot",
                                                        fluidRow(column(width=11, offset=1,
                                                                        sliderInput(inputId="violin_plot_dotplot_size",
                                                                                    label = "Dot Size", 
                                                                                    value=1, 
                                                                                    min = .1,
                                                                                    max = 2,
                                                                                    step = 0.1)))),
                                       prettyCheckbox(inputId="violin_plot_jitterplot",
                                                      label="Add Jitter DotPlot Inside",
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
                                tabPanel("Group Color Options",
                                         fluidRow(
                                           column(width=6,
                                                  sliderInput(inputId="violin_strata_alpha",
                                                              label="Select Level of Transperency",
                                                              min=0,
                                                              max=1,
                                                              step=0.1,
                                                              value=1) )
                                         ), #end FluidRow
                                         fluidRow(
                                           #checkbox that turns on strata
                                           column(width=6,
                                                  conditionalPanel(condition="input.violinplot_single_or_group =='single'",
                                                                   fluidRow(
                                                                     column(width=5,
                                                                            colourInput(inputId = "violin_single_color_fill",
                                                                                        label="Fill Color",
                                                                                        value="white")),
                                                                     column(width=5,
                                                                            colourInput(inputId = "violin_single_color_outline",
                                                                                        label="Outline Color",
                                                                                        value="black")))),
                                                  #conditional panel for colors for multiple violin plots
                                                  conditionalPanel(condition="input.violinplot_single_or_group == 'group'",
                                                    #this outputs a panel of data that displays a color panel for each categorical variable in the strata'd variable
                                                    fluidRow(
                                                      column(width=5,
                                                             uiOutput('violin_strata_color_options_popdown')),
                                                      column(width=5,
                                                             uiOutput("violin_strata_fill_options_popdown"))))
                                           ) #end column
                                         ) #end fluidRow
                                        ), #end tabPanel
#____________________________________
#Background Options
#____________________________________
                                tabPanel("Background Options",
                                         selectInput(
                                           inputId = "violin_theme_choice",
                                           label = "Background Theme", 
                                           choices = c("gray", "bw", "linedraw", "light", "minimal", "classic", "void", "dark")
                                         ),
                                         fluidRow(
                                           div(style="display:inline-block; text_align:right;", 
                                               prettyCheckbox(inputId="violin_panel_colorPicker_checkbox", 
                                                              label=NULL, 
                                                              value = FALSE)),
                                           div(style="display:inline-block; text_align:right;", 
                                               colourInput(inputId="violin_panel_colorPicker", 
                                                           label="Select Color", 
                                                           value="blue")))
                                         
                                ),#end tabPanel
#____________________________________
#Legend Options
#____________________________________
                                tabPanel("Legend Options",
                                         fluidRow(
                                           column(width=5,
                                                  selectInput(inputId="violinplot_legend_position",
                                                              label = "Location of Legend",
                                                              choices = c("Left" = "left", 
                                                                          "Right" = "right", 
                                                                          "Top" = "top", 
                                                                          "Bottom" = "bottom", 
                                                                          "No Legend" = "none"),
                                                              selected = "right"),
                                                  textInput(inputId="violinplot_legend_title",
                                                            label = "Legend Title",
                                                            value = "")))
                                ) #end tabPanel
                                
                              )#End tabBox
                            ) #End box
                            
) # end tabItem
