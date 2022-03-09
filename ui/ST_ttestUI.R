#This tab corresponds to the "t-test" SubTab (under Univariate Statistics) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  September 10, 2020
#  Last Update: September 10, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------
#__________________________________________________________________________________________________________
#SUBTAB: tTest
#__________________________________________________________________________________________________________
SUBTAB_TTEST <- tabItem(tabName = "TAB_TTEST", 
                        fluidRow(
                          column(width=10,
                                 tags$h2("t-test")),
#____________________________________
#ToolTip Help
#____________________________________
                          column(width=2,
                                 dropdownButton(
                                   status="info",
                                   icon=icon("info"),
                                   width="1000px",
                                   size="xs",
                                   right = TRUE,
                                   tooltip=tooltipOptions(title = "Click for help",
                                                          placement="left"),
                                   h2('ttest'),
                                   p("This is the tab to perform different t-test with hypothesis testing. It currently supports
                                     one sample ttest, two sample ttest with/without equal variances, and two sample paired ttest.
                                     Data for one sample is simply a selected column of data from the data loaded in the Data Management
                                     tab.  Two sample ttest supports both inputs of two numerical columns or an input of one column of groups and one
                                     column of numerical data."),
                                   div(img(src="./images/ttest.JPG",
                                       width= "50%",
                                       height="50%",
                                       align=""), style="text-align: center;"),
                                   p("After selecting variables the user has the ability to set the null hypothesis, alternative hypothesis, and 
                                      significance level for hypothesis testing."),
                                   h4("Results"),
                                   p("The results displayed in the result box include the method of test, degree of freedom, t-statistic,
                                      p-value, confidence interval, null hypothesis, type of hypothesis testing, significance level, and the 
                                      result of the hypothesis test.  Plotting includes the student t distribution (which can be changed in real
                                      time by changing inputs, a statisitcal walkthrough of how the calculation is done, and a boxplot of the data.")
                                   ),
                                 align="right")
                        ), #end fluidRow
                        hr(),
                        fluidRow(
                          column(width=4,
                                 box(
                                   #this is a box that holds the import data options.
                                   title="Inputs", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                   fluidRow(
                          #choose type of ttest
                                     column(width=6,
                                            radioButtons("sample",
                                                         "Choose type of t-test:",
                                                         choices = c("One sample" = "oneSamp", 
                                                                     "Two sample" = "twoSamp",
                                                                     "Paired" = "pairedSamp"))),
                          #choose if data input is numerical or grouped
                                     column(width=6,
                                           conditionalPanel(condition = "input.sample == 'twoSamp' || input.sample == 'pairedSamp'",
                                           radioButtons("ttest_data_input_options",
                                                        label = "Select Input Data Type",
                                                        choices = c("Numerical" = "num_col",
                                                                    "Grouped" = "group_col"))))
                                   ), #end FluidRow
                                   
                                   hr(style="border-color: black"),
 #____________________________________
 #One Sample ttest
 #____________________________________
                                   conditionalPanel(condition = "input.sample == 'oneSamp'",
                                                    h4("Variable Selection", align="center"),
                                                    hr(style="border-color: black"),
                                  #Variable Input
                                                    selectInput(inputId= "var1", 
                                                                label = "Select Sample Variable",
                                                                choices = ""),
                                                    hr(style="border-color: black"),
                                  #Hypothesis Testing
                                                    h4("Hypothesis Testing", align="center"),
                                                    hr(style="border-color: black"),
                                                    numericInput(inputId="ttest_one_nullHypothesis",
                                                                 label = sprintf("\\( H_0 : \\mu = \\)"),
                                                                 value = 0),
                                                    radioGroupButtons(inputId = "ttest_alternative_hypothesis",
                                                                      label="Alternative",
                                                                      choices = c("\\( \\neq \\)" = "two.sided",
                                                                                  "\\( > \\)" = "greater",
                                                                                  "\\( < \\)" = "less"),
                                                                      individual = TRUE,
                                                                      checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))),
                                                    sliderInput(inputId = "ttest_alpha_value",
                                                                label = "Significance level \\(\\alpha = \\)",
                                                                min = 0.01,
                                                                max = 0.20,
                                                                value = 0.05)
                                   ), #end conditionalPanel
 #____________________________________
 #Two Sample ttest
 #____________________________________
                                   conditionalPanel(condition = "input.sample == 'twoSamp'",
                                                    h4("Variable Selection", align="center"),
                                                    hr(style="border-color: black"),
                                  #Variable Inputs if numerical data
                                                  conditionalPanel(condition="input.ttest_data_input_options=='num_col'",
                                                                   selectInput(inputId= "two_samp_ttest_var1", 
                                                                               label = "Select First Sample",
                                                                               choices = ""),
                                                                   selectInput(inputId= "two_samp_ttest_var2", 
                                                                               label = "Select Second Sample",
                                                                               choices="")),
                                  #Variable Inputs if grouped data
                                                  conditionalPanel(condition="input.ttest_data_input_options=='group_col'",
                                                                   selectInput(inputId= "two_samp_ttest_numerical_group", 
                                                                               label = "Select Numerical Column",
                                                                               choices = ""),
                                                                   selectInput(inputId= "two_samp_ttest_categorical_group", 
                                                                               label = "Select Grouping Column",
                                                                               choices="")),
                                  
                                  #Choose if Variances are equal
                                                    prettyRadioButtons(inputId="two_samp_ttest_varequal",
                                                                 label="Do the Two Samples have Equal Variances?",
                                                                 choices = c("Yes" = "y",
                                                                             "No" = "n"),
                                                                 inline = TRUE,
                                                                 fill=TRUE,
                                                                 status="primary"),
                                                    hr(style="border-color: black"),
                                  #hypothesis Testing
                                                    h4("Hypothesis Testing", align="center"),
                                                    hr(style="border-color: black"),
                                                    numericInput(inputId="two_samp_ttest_nullHypothesis",
                                                                 label = sprintf("\\( H_0 : \\mu_1 - \\mu_2 = \\)"),
                                                                 value = 0),
                                                    radioGroupButtons(inputId = "two_samp_ttest_alternative_hypothesis",
                                                                      label="Alternative",
                                                                      choices = c("\\( \\neq \\)" = "two.sided",
                                                                                  "\\( > \\)" = "greater",
                                                                                  "\\( < \\)" = "less"),
                                                                      individual = TRUE,
                                                                      checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))),
                                                    sliderInput(inputId = "two_samp_ttest_alpha_value",
                                                                label = "Significance level \\(\\alpha = \\)",
                                                                min = 0.01,
                                                                max = 0.20,
                                                                value = 0.05)
                                                    
                                   ), #end conditionalPanel
 #____________________________________
 #Paired Two Sample ttest
 #____________________________________
                                   conditionalPanel(condition="input.sample == 'pairedSamp'",
                                                    h4("Variable Selection", align="center"),
                                                    hr(style="border-color: black"),
                                   #variable inputs if numerical data
                                   conditionalPanel(condition="input.ttest_data_input_options=='num_col'",
                                                    selectInput(inputId= "paired_ttest_var1", 
                                                                label = "Select First Sample",
                                                                choices = ""),
                                                    selectInput(inputId= "paired_ttest_var2", 
                                                                label = "Select Second Sample",
                                                                choices="")),
                                   #Variable Inputs if grouped data
                                   conditionalPanel(condition="input.ttest_data_input_options=='group_col'",
                                                    selectInput(inputId= "paired_ttest_group_numerical", 
                                                                label = "Select Numerical Column",
                                                                choices = ""),
                                                    selectInput(inputId= "paired_ttest_group_categorical", 
                                                                label = "Select Grouping Column",
                                                                choices="")),
                                   #hypothesis Testing
                                                    hr(style="border-color: black"),
                                                    h4("Hypothesis Testing", align="center"),
                                                    hr(style="border-color: black"),
                                                    numericInput(inputId="paired_ttest_nullHypothesis",
                                                                 label = sprintf("\\( H_0 : \\mu_D\\)"),
                                                                 value = 0),
                                                    radioGroupButtons(inputId = "paired_ttest_alternative_hypothesis",
                                                                      label="Alternative",
                                                                      choices = c("\\( \\neq \\)" = "two.sided",
                                                                                  "\\( > \\)" = "greater",
                                                                                  "\\( < \\)" = "less"),
                                                                      individual = TRUE,
                                                                      checkIcon = list(
                                                                        yes = tags$i(class = "fa fa-circle", 
                                                                                     style = "color: steelblue"),
                                                                        no = tags$i(class = "fa fa-circle-o", 
                                                                                    style = "color: steelblue"))),
                                                    sliderInput(inputId = "paired_ttest_alpha_value",
                                                                label = "Significance level \\(\\alpha = \\)",
                                                                min = 0.01,
                                                                max = 0.20,
                                                                value = 0.05))
                                 ) #end box
                          ),#end column
 #____________________________________
 #Results ttest Table
 #____________________________________
                          column(width=8,
                                 box(
                                   title="Results", status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                   conditionalPanel(condition="input.sample == 'oneSamp'",
                                                    tableOutput("ttest_table_out")),
                                   conditionalPanel(condition="input.sample == 'twoSamp'",
                                                    tableOutput("twoSamp_ttest_table_out")),
                                   conditionalPanel(condition="input.sample == 'pairedSamp'",
                                                    tableOutput("paired_ttest_table_out"))
                                 ), #end Box
 #____________________________________
 #Results: Graphical Outputs
 #____________________________________
                                 box(
                                   title=NULL, status="primary", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                                 #statistical Graph with curve
                                   conditionalPanel(condition="input.ttest_graph_or_work=='Graph'",
                                                    conditionalPanel(condition="input.sample == 'oneSamp'",
                                                      plotOutput("ttestSinglePlot")),
                                                    conditionalPanel(condition="input.sample == 'twoSamp'",
                                                      plotOutput("twoSamp_ttest_plot")),
                                                    conditionalPanel(condition="input.sample == 'pairedSamp'",
                                                      plotOutput("paired_ttest_plot"))),
                                 #hypothesis testing walkthrough
                                   conditionalPanel(condition="input.ttest_graph_or_work=='Statistical Walkthrough'",
                                                    conditionalPanel(condition="input.sample == 'oneSamp'",
                                                      uiOutput("t_test_results")),
                                                    conditionalPanel(condition="input.sample == 'twoSamp'",
                                                      uiOutput("two_samp_ttest_results")),
                                                    conditionalPanel(condition="input.sample == 'pairedSamp'",
                                                      uiOutput("paired_samp_test_results"))),
                                 #boxplot output
                                   conditionalPanel(condition="input.ttest_graph_or_work=='Data Visualization'",
                                                    plotOutput("ttest_boxpolot")),
                                   hr(style="border-color: black"),
                                   radioGroupButtons(inputId="ttest_graph_or_work",
                                                     label="View",
                                                     choices = c("Graph", "Statistical Walkthrough", "Data Visualization"),
                                                     checkIcon = list(
                                                       yes = tags$i(class = "fa fa-check-square", 
                                                                    style = "color: steelblue"),
                                                       no = tags$i(class = "fa fa-square-o", 
                                                                   style = "color: steelblue"))))
                                 
                                    
                                
                                 
                                 )#end column
                          )#end Fluid Row
                        
                        
                          
                        
) #end tabItem
