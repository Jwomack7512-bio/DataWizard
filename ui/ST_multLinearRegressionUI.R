#This tab corresponds to the "Multiple Linear Regression " SubTab (under Multivariate Statistics) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  November 13, 2020
#  Last Update: November 13, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------
#__________________________________________________________________________________________________________
#SUBTAB: multLR
#__________________________________________________________________________________________________________
SUBTAB_MLR <- tabItem(tabName = "TAB_MLR", 
                         fluidRow(
                           column(width=10,
                                  tags$h2("Multiple Linear Regression")),
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
                                      tabBox(title=NULL, id="MLR_inputs", width=12,
                                             tabPanel("Variables",
                                                      
                                                       pickerInput(inputId = "MLR_Select_D",
                                                                   label = "Select Dependent Variable",
                                                                   choices = character()),
                                                
                                                       pickerInput(inputId = "MLR_Select_multVars",
                                                                   label = "Select Independent Variables",
                                                                   choices = character(),
                                                                   multiple = TRUE)        
                                             ), #end tabpanel MLR_TAB1
                                             
                                             tabPanel("Categorical Variables",
                                                      pickerInput(inputId = "MLR_Select_CategoricalVar",
                                                                  label = "Select Categorical Variables",
                                                                  choices = character(),
                                                                  multiple = TRUE)
                                                ), #end tablPanel MLR_TAB2
                                      tabPanel("Interactions",
                                               pickerInput(inputId = "MLR_joinVar1",
                                                           label = "Var 1",
                                                           choices = character()),
                                               pickerInput(inputId = "MLR_joinVar2",
                                                           label = "Var 2",
                                                           choices = character()),
                                               actionBttn(inputId = "MLR_join_together_button",
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
                                      verbatimTextOutput("MLR_code_Out")
                                    ) #end Box
                                  )
                           ), #end Column
                           column(width=6,
                                  
                                  #box to display test results
                                  box(
                                    title="Model Selection", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                    #uiOutput("MLR_multiInput_finalVars"),
                                    multiInput(inputId = "MLR_multiInput_finalVars",
                                               label = "Select Final Variables for Model",
                                               selected = "",
                                               choices = character(0),
                                               options = list(
                                                 enable_search = FALSE,
                                                 non_selected_header = "Select To Add To Model:",
                                                 selected_header = "In Model:")),
                                    
                                    actionBttn(inputId = "MLR_run_button",
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
                                      tabPanel("MLR Summary",
                                               verbatimTextOutput("MLR_summary")),
                                      tabPanel("Summary Plots",
                                               plotOutput("MLR_Plots")),
                                      tabPanel("Residual",
                                               plotOutput("MLR_ResidualPlot")),
                                      tabPanel("y vs y_fitted",
                                               plotOutput("MLR_fittedPlot")),
                                      tabPanel("Test Tables",
                                               DT::dataTableOutput("MLR_summaryFactorList"),
                                               downloadButton('download_MLR_summaryFactorList_csv',"Download table: csv"),
                                               downloadButton('download_MLR_summaryFactorList_pdf',"Download table: pdf")),
                                      tabPanel("Test Tables 2",
                                               DT::dataTableOutput("MLR_finalfit_LM")),
                                      tabPanel("GT Summary",
                                               gt::gt_output("MLR_GtSummary_table")),
                                      tabPanel("OR Plot",
                                               #uiOutput("plot.ui"))
                                               plotOutput("MLR_OR_Plot"))
                                      
                                    ) #end Tabbox
                                    
                                    
                                  ) #end box
                           ) #end column
                      ) #end FluidRow
                         
                         
                         
) #end tabItem