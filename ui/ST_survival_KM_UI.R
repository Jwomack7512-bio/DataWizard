#This tab corresponds to the "Violin Plot" SubTab (under Graphing Tab) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  August 18, 2020
#  Last Update: August 18, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_KAPLAN_MEIER <- tabItem(tabName = "TAB_KAPLAN_MEIER", 
                         tags$h2("Kaplan Meier"),
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
                                             selectInput(inputId="KM_time_var"
                                                         ,label = "Select Time Variable"
                                                         ,choices = c())
                                             ,selectInput(inputId = "KM_censor_var"
                                                          ,label = "Select Censoring Variable"
                                                          ,choices = c())
                                             
                                                               
                                             
                                             
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
                                                              textInput(inputId="KM_title", 
                                                                        label="Title", 
                                                                        value = ""),
                                                              textInput(inputId="KM_xlabel", 
                                                                        label="X Label", 
                                                                        value = ""),
                                                              textInput(inputId="KM_ylabel", 
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
                                                                inputId ="KM_download_title",
                                                                label=NULL,
                                                                value = "",
                                                                placeholder = "Type Download TItle",
                                                                width = NULL
                                                              ),
                                                              radioGroupButtons(
                                                                inputId = "KM_download_radiobuttons",
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
                                                              downloadBttn(outputId="downloadKM",
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
                                  jqui_resizable(plotOutput(outputId = 'KMPlot'))
                           ), #end column width=9
                           #____________________________________
                           #Plot Addon Sidebar
                           #____________________________________
                           column(width=3,
                                  box(
                                    title="Plot Addons", status="primary", solidHeader=TRUE, collapsible=TRUE, width=NULL
                                    ,selectInput(inputId = "KM_time_units"
                                                 ,label = "Select Time Units"
                                                 ,choices = c("Years" = 1
                                                              ,"Months" = 2
                                                              ,"Days" = 3)
                                                 ,selected = 1)
                                    ,checkboxInput(inputId = "KM_risk_table"
                                                   ,label = "View Risk Table"
                                                   ,value = FALSE)
                                    ,checkboxInput(inputId = "KM_plot_CI"
                                                   ,label = "Show Confidence Intervals"
                                                   ,value = FALSE)
                                    ,conditionalPanel(condition = "input.KM_add_grouping_var"
                                                      ,checkboxInput(inputId = "KM_plot_pval"
                                                                     ,label = "Show Pvalue"
                                                                     ,value = FALSE)
                                    )#end conditional panel
                                    ,actionButton(inputId = "KM_plOt_button"
                                                  ,label = "PLOT")
                                    ) #end box
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
                             tabPanel("Grouping Options"
                                      ,fluidRow(column(width=3
                                                      ,checkboxInput(inputId = "KM_add_grouping_var"
                                                                     ,label = "Select Grouping Variable"
                                                                     ,value = FALSE))
                                                ,column(width = 3
                                                        ,conditionalPanel(condition = "input.KM_add_grouping_var"
                                                                          ,radioGroupButtons(inputId = "KM_group_conVcat_option"
                                                                                             ,label = "Grouping Choice"
                                                                                             ,choices = c("Categorical" = "Categorical"
                                                                                                          ,"Continuous" = "Continuous"))))
                                                ,column(width=3
                                                        ,conditionalPanel(condition = "input.KM_add_grouping_var"
                                                                          ,selectInput(inputId = "KM_grouping_var"
                                                                                       ,label = "Select Variable of Interest as Cohort Group"
                                                                                       ,choices = c()))
                                                )
                                      ) #end fluidrow
                                                
                                      
                                      ,conditionalPanel(condition = "input.KM_add_grouping_var"                  
                                                        ,conditionalPanel(condition = "input.KM_group_conVcat_option == 'Continuous'"
                                                                          ,radioGroupButtons(inputId = "KM_continous_options"
                                                                                             ,label = "Select Numerical Type"
                                                                                             ,choices = c("Percentile" = "Percentile"
                                                                                                          ,"Cutoff Value" = "CV"))
                                                                          ,fluidRow(column(width = 4
                                                                                          ,conditionalPanel(condition = "input.KM_continous_options == 'Percentile'"
                                                                                                            ,sliderInput(inputId = "KM_continous_percentile"
                                                                                                                         ,label = "Select Percentile to Split Group By"
                                                                                                                         ,min = 0
                                                                                                                         ,max = 100 
                                                                                                                         ,step = 5
                                                                                                                         ,value = 25))
                                                                                          ,conditionalPanel(condition = "input.KM_continous_options == 'CV'"
                                                                                                            ,sliderInput(inputId = "KM_continous_cutoff"
                                                                                                                         ,label = "Select Cutoff Value to Split Group By"
                                                                                                                         ,min = 0
                                                                                                                         ,max = 1 
                                                                                                                         ,step = .5
                                                                                                                         ,value = .25))
                                                                                          
                                                                                                            
                                                                                          )#end column
                                                                                   ) #end fluidRow
                                                                          
                                                                          )#end conditionalPanel KM_group_conVat_options == Continous
                                                        ) #end conditionalPanel KM_add_grouping_var
                                      )#end tabPanel
                             #____________________________________
                             #Color Options
                             #____________________________________
                             ,tabPanel("Color Options",

                                      fluidRow(
                                        #checkbox that turns on strata
                                        column(width=6,
                                               checkboxInput(inputId = "KM_grey_palette"
                                                             ,label = "Greyscale Line Color"
                                                             ,value = FALSE)
                                        ) #end column
                                      ) #end fluidRow
                             ), #end tabPanel
                             #____________________________________
                             #Background Options
                             #____________________________________
                             tabPanel("Background Options",
                                      selectInput(
                                        inputId = "KM_theme_choice",
                                        label = "Background Theme",
                                        choices = c("gray", "bw", "linedraw", "light", "minimal", "classic", "void", "dark")
                                      ),
                                      fluidRow(
                                        div(style="display:inline-block; text_align:right;",
                                            prettyCheckbox(inputId="KM_panel_colorPicker_checkbox",
                                                           label=NULL,
                                                           value = FALSE)),
                                        div(style="display:inline-block; text_align:right;",
                                            colourInput(inputId="KM_panel_colorPicker",
                                                        label="Select Color",
                                                        value="blue")))

                             ),#end tabPanel
                             #____________________________________
                             #Legend Options
                             #____________________________________
                             tabPanel("Legend Options",
                                      fluidRow(
                                        column(width=5,
                                               selectInput(inputId="KMplot_legend_position",
                                                           label = "Location of Legend",
                                                           choices = c("Left" = "left",
                                                                       "Right" = "right",
                                                                       "Top" = "top",
                                                                       "Bottom" = "bottom",
                                                                       "No Legend" = "none"),
                                                           selected = "right"),
                                               textInput(inputId="Kmplot_legend_title",
                                                         label = "Legend Title",
                                                         value = "")))
                             ) #end tabPanel

                           )#End tabBox
                         ) #End box
                         
) # end tabItem
