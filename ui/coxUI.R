#This tab corresponds to the "Cox Regression" SubTab (under Graphing Tab) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  August 18, 2020
#  Last Update: August 18, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_COX_SURVIVAL <- tabItem(tabName = "TAB_COX_SURVIVAL",
                            fluidRow(
                              column(width=10,
                                     tags$h2("Cox Survival Model")),
                              column(width=2,
                                     dropdownButton(
                                       status="info",
                                       icon=icon("info"),
                                       width="1000px",
                                       size="xs",
                                       right = TRUE,
                                       tooltip=tooltipOptions(title = "Click for help",
                                                              placement="left"),
                                       h2('This is a test help bar'),
                                       h4("This is where help information goes")
                                     ),
                                     align="right")
                            ),
                            hr(),
                            fluidRow(
                              column(width=6,
                                     fluidRow(
                                       box(
                                         title="Inputs", status="primary", solidHeader=FALSE, collapsible=TRUE, width=12,
                                         #sets up radio group buttons that allows user to select type of wilcoxon test
                                         tabBox(title=NULL, id="Cox_inputs", width=12,
                                                tabPanel("Variables"
                                                         
                                                         ,pickerInput(inputId = "COX_Select_time",
                                                                     label = "Select Time Variable",
                                                                     choices = character())
                                                         
                                                         ,pickerInput(inputId = "COX_Select_censor",
                                                                     label = "Select Censoring Variables",
                                                                     choices = character())
                                                         ,pickerInput(inputId = "COX_Select_covars",
                                                                      label = "Select Censoring Variables",
                                                                      choices = character(),
                                                                      multiple = TRUE) 
                                                ), #end tabpanel COX_TAB1
                                                
                                                tabPanel("Categorical Variables",
                                                         pickerInput(inputId = "COX_Select_CategoricalVar",
                                                                     label = "Select Categorical Variables",
                                                                     choices = character(),
                                                                     multiple = TRUE)
                                                ), #end tablPanel COX_TAB2
                                                tabPanel("Interactions",
                                                         pickerInput(inputId = "COX_joinVar1",
                                                                     label = "Var 1",
                                                                     choices = character()),
                                                         pickerInput(inputId = "COX_joinVar2",
                                                                     label = "Var 2",
                                                                     choices = character()),
                                                         actionBttn(inputId = "COX_join_together_button",
                                                                    label = "Add These Two",
                                                                    style = "pill",
                                                                    color = "danger")
                                                         
                                                ) #end tabPanel multiply
                                         ) #end tabbox
                                       ) #end Box
                                     ), #end FluidRow
                                     fluidRow(
                                       box(
                                         title="CodeOutput", status="primary", solidHeader=FALSE, collapsible=TRUE, width=12,
                                         verbatimTextOutput("COX_code_Out")
                                       ) #end Box
                                     )
                              ), #end Column
                              column(width=6,
                                     
                                     #box to display test results
                                     box(
                                       title="Model Selection", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                       #uiOutput("COX_multiInput_finalVars"),
                                       multiInput(inputId = "COX_multiInput_finalVars",
                                                  label = "Select Final Variables for Model",
                                                  selected = "",
                                                  choices = character(0),
                                                  options = list(
                                                    enable_search = FALSE,
                                                    non_selected_header = "Select To Add To Model:",
                                                    selected_header = "In Model:")),
                                       
                                       actionBttn(inputId = "COX_run_button",
                                                  label = "Perform Regression",
                                                  style = "pill",
                                                  color = "danger")
                                     ) #end box
                              ) #end Column
                            ), #end FluidRow
                            fluidRow(
                              column(width=12,
                                     #box to display the different type of plots available
                                     box(
                                       title="Plots", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                       tabBox(
                                         title=NULL,
                                         width = 12,
                                         height = "800px",
                                         tabPanel("COX Table",
                                                  verbatimTextOutput("COX_summary")),
                                         tabPanel("Summary Plots",
                                                  plotOutput("COX_Plots"))
                                         
                                       ) #end Tabbox
                                       
                                       
                                     ) #end box
                              ) #end column
                            ) #end FluidRow
                            
                            
                            
      ) #end tabItem