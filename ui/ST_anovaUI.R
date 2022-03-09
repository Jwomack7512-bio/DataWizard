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
SUBTAB_ANOVA <- tabItem(tabName = "TAB_ANOVA",

                      h2("Anova", align = "center"),
                      h5("Can perform one or two way anova as well as view data in form of boxplots and residual plots.
                                   View Readme tab for more info.",
                         align="center"),
                      br(),
                        column(width=2,
                               offset=10,
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
                               align="right"),
                      hr(),
                      fluidRow(
                        column(width=6,
                               fluidRow(
                                 box(
                                   title="Inputs", status="primary", solidHeader=FALSE, collapsible=TRUE, width=12,
                                   #sets up radio group buttons that allows user to select type of wilcoxon test
                                   tabBox(title=NULL, id="anova_inputs", width = 12,
                                          tabPanel("Variables",
                                                   pickerInput(inputId = "anova_Select_D",
                                                               label = "Select Dependent Variable",
                                                               choices = character()),
                                                   
                                                   pickerInput(inputId = "anova_Select_multVars",
                                                               label = "Select Independent Variables",
                                                               choices = character(),
                                                               multiple = TRUE)        
                                          ), #end tabpanel anova_TAB1
                                          
                                          tabPanel("Categorical Variables",
                                                   pickerInput(inputId = "anova_Select_CategoricalVar",
                                                               label = "Select Categorical Variables",
                                                               choices = character(),
                                                               multiple = TRUE)
                                          ), #end tablPanel anova_TAB2
                                          tabPanel("Interaction",
                                                   pickerInput(inputId = "anova_joinVar1",
                                                               label = "Var 1",
                                                               choices = character()),
                                                   pickerInput(inputId = "anova_joinVar2",
                                                               label = "Var 2",
                                                               choices = character()),
                                                   actionBttn(inputId = "anova_join_together_button",
                                                              label = "Multiply Values",
                                                              style = "pill",
                                                              color = "primary")
                                                   
                                          ) #end tabPanel multiply
                                   ) #end tabbox
                                 ) #end Box
                               ) #end FluidRow
                        ), #end Column
                        column(width=6,
                               
                               #box to display test results
                               box(
                                 title="Model Selection", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                 #uiOutput("anova_multiInput_finalVars"),
                                 multiInput(inputId = "anova_multiInput_finalVars",
                                            label = "Select Final Variables for Model",
                                            selected = "",
                                            choices = character(0),
                                            options = list(
                                              enable_search = FALSE,
                                              non_selected_header = "Select To Add To Model:",
                                              selected_header = "In Model:")),
                                 actionBttn(inputId = "anova_run_button",
                                            label = "Perform Anova",
                                            style = "pill",
                                            color = "primary")
                               ) #end box
                        ) #end Column
                      ), #end FluidRow
                      # fluidRow(
                      #   box(title = "Enter Model", status = "primary", solidHeader=FALSE, collapsible = TRUE, width = NULL,
                      #       column(width=10,
                      #              textInput("anova_formula", label = NULL),
                      #              helpText("Model entered in typical R fashion (Var ~ factor). For more infomation on model input see Readme tab.")),
                      #       column(width=2,
                      #              actionButton("anova_button", "Update", 
                      #                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                      #   )
                      # ),
                      #set up tabbox containing all tabs for the anova information
                    
                        box(
                          title="Outputs",
                          status="primary",
                          solidHeader=FALSE, 
                          collapsible=TRUE, 
                          width=NULL,
                          
                            tabBox(
                              title = NULL, width = 12,
                              # The id lets us use input$tabset1 on the server to find the current tab
                              id = "anova_tabbox", 
                              tabPanel("Show Data",
                                       verbatimTextOutput("anova_dataview")),
                              tabPanel("ANOVA Table",
                                       verbatimTextOutput("anova_table")), 
                              tabPanel("Box Plot",
                                       plotOutput("anova_boxPlot")),
                              tabPanel("Turkey Comparisons",
                                       plotOutput("anova_TurkeyPlot"),
                                       verbatimTextOutput("anova_turkey")),
                              tabPanel("Your Turkey",
                                       plotOutput("anova_TurkeyPlot2"),
                                       verbatimTextOutput("anova_turkey2")),
                              tabPanel("Residual",
                                       plotOutput("anova_residualPlot"))
                            )#end tabBox
                        )#end Box
                  
) #end tabItem