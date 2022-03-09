#This tab corresponds to the "wilcox " SubTab (under Univariate Statistics) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  September 13, 2020
#  Last Update: September 13, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------
#__________________________________________________________________________________________________________
#SUBTAB: wilcox
#__________________________________________________________________________________________________________
SUBTAB_WILCOX <- tabItem(tabName = "TAB_WILCOX", 
                         fluidRow(
                           column(width=10,
                                  tags$h2("Wilcoxon Test")),
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
                                    div(img(src="./images/wilcox_data_help.JPG",
                                            width= "50%",
                                            height="50%",
                                            align=""), style="text-align: center;"),
                                    h4("I willl put help information here and try to put a pp slide here to see how it looks")
                                    ),
                                  align="right")
                         ),
                         hr(),
                         fluidRow(
                           column(width=4,
                                  box(
                                    title="Inputs", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                    #sets up radio group buttons that allows user to select type of wilcoxon test
                                    fluidRow(
                                      column(width=6,
                                             radioGroupButtons(inputId="wilcox_test_options",
                                                               label="Type of Test",
                                                               choices = c("Independent" = "independent", 
                                                                           "Paired" = "paired"))),
                                      column(width=6,
                                             radioGroupButtons(inputId="wilcox_data_import_options",
                                                               label="Type of Data Import",
                                                               choices = c("Numerical" = "num_col",
                                                                           "Grouped" = "group_col")))
                                    ), #end FluidRow
                                  
                                   #set up inputs for condition that numerical data is used (2 cols of numerical)
                                   conditionalPanel(condition="input.wilcox_data_import_options=='num_col'",
                                                    #select inputs for the test           
                                                    selectInput(inputId= "wilcox_input1",
                                                                label = "Select 1st Column Input",
                                                                choices = ""),
                                                    selectInput(inputId="wilcox_input2",
                                                                label="Select 2nd Column Input", 
                                                                choices="")),
                                   #set up conditional Input for grouped data option
                                   conditionalPanel(condition="input.wilcox_data_import_options=='group_col'",
                                                    selectInput(inputId="wilcox_group_numerical",
                                                                label = "Select Column of Numerical Data",
                                                                choices = ""),
                                                    selectInput(inputId="wilcox_group_categorical",
                                                                label="Select Column Containing Groups",
                                                                choices = ""))

                                  ) #end Box
                           ), #end Column
                           column(width=8,
                                  
                                  #box to display test results
                                  box(
                                    title="Results", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                    verbatimTextOutput("wilcox_results")
                                  ), #end box
                                  #box to display the different type of plots available
                                  box(
                                    title="Plots", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                    tabBox(
                                      title=NULL,
                                      width = 12,
                                      tabPanel("Box Plot",
                                               plotOutput("wilcox_boxplot"))
                                      
                                    ) #end Tabbox
                                    
                                    
                                  ) #end box
                           ) #end column
                         )#end FluidRow
                         
                             
) #end tabItem