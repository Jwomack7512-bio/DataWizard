#This tab corresponds to the "chi-square " SubTab (under Univariate Statistics) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  September 12, 2020
#  Last Update: September 12, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------
#__________________________________________________________________________________________________________
#SUBTAB: chisquared
#__________________________________________________________________________________________________________
SUBTAB_CHISQUARED <- tabItem(tabName = "TAB_CHISQUARED", 
                        fluidRow(
                          column(width=10,
                                 tags$h2("chi-squared: Test of Independence")),
                          column(width=2,
                                 dropdownButton(
                                   status="info",
                                   icon=icon("info"),
                                   width="1000px",
                                   size="xs",
                                   right = TRUE,
                                   tooltip=tooltipOptions(title = "Click for help",
                                                          placement="left"),
                                   h2('Chi-Squared: Test of Independence'),
                                   p("This is the tab to perform a chisquared test of independence. Once the data is inputted this tab
                                      will output contingency tables, proportion tables, results of the test, relevant graphs, and a statistical
                                      walkthrough of the test.  The data can be imported in two different form: column and tabular. If the user 
                                     has two columns of categorical data, then column option can be used.  If a table is already made that can easily
                                     be imported using the tabular option."),
                                   div(img(src="./images/chisquared_data_help.JPG",
                                           width= "50%",
                                           height="50%",
                                           align=""), style="text-align: center;"),
                                   p("Currently, if 'Data is a table' is selected the the user must import the new contingency table.
                                      If 'By Columns' is selected then the data imported in the data management tab will be used.  This is
                                     somewhat confusing and can be fixed later on if we want"),
                                   h4("Results"),
                                   p("The results are displayed in the results box.  The p-value can be used to determine if the hypothesis is rejected
                                      or not.  Data visualization inclues a look at the Chi Squared distribution, a mosiac, and stacked box plot.")
                                 ),
                                 align="right")
                        ),
                        hr(),
                        fluidRow(
                          column(width=4,
                                 box(
                                   radioButtons("cs_load_options",
                                                "Choose file upload info:",
                                                choices = c("By Columns" = "cs_col_data", 
                                                            "Data is a table" = "cs_table_data")),
                                   #this is a box that holds the import data options.
                                   title="Inputs", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                   conditionalPanel(condition="input.cs_load_options=='cs_col_data'",
                                                    selectInput(inputId= "chisquared_input1",
                                                                label = "Select 1st Column Input",
                                                                choices = ""),
                                                    selectInput(inputId="chisquared_input2",
                                                                label="Select 2nd Column Input", 
                                                                choices="")),
                                   conditionalPanel(condition="input.cs_load_options=='cs_table_data'",
                                                    fileInput(inputId="cs_table_data", 
                                                              label="Import Table (.csv)")),
                                   
                                   prettyCheckbox(inputId="cs_yates_correction_box",
                                                  label="Apply Yates Correction",
                                                  value=FALSE)
                                 ) #end Box
                          ), #end Column
                          column(width=8,
                                 fluidRow(
                                   column(width=6,
                                          box(
                                            title="Contingency Table", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                            verbatimTextOutput("cs_contingency_table")
                                          )),
                                   column(width=6,
                                          box(
                                            title="Prop Table", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                            br(),
                                            verbatimTextOutput("cs_prop_table")
                                            
                                          ))
                                 ), #end fluidRow
                                 box(
                                   title="Results", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                   verbatimTextOutput("cs_results")
                                 ),
                                 box(
                                   title="Plots", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                   tabBox(
                                     title=NULL,
                                     width = 12,
                                     tabPanel("Chi Table",
                                              plotOutput("cs_chi_stat")),
                                     tabPanel("Mosiac Plot",
                                              plotOutput("cs_mosiac_plot")),
                                     tabPanel("Bar Plot",
                                              plotOutput("cs_barplot"))
                                     
                                   ) #end Tabbox
                                   
                                 ) #end box
                              ) #end column
                        )#end FluidRow
                                   
                      
) #end tabItem