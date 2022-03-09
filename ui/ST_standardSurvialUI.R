#This tab corresponds to the "Standard Survival" under the Survival Analysis Tab
#-------------------------------------------------------------------------
#  Justin Womack
#  September 26, 2020
#  Last Update: September 26, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------
#__________________________________________________________________________________________________________
#SUBTAB: tTest
#__________________________________________________________________________________________________________
SUBTAB_STANDARD_SURVIVAL <- tabItem(tabName = "TAB_STANDARD_SURVIVAL",
                                    # fluidRow(
                                    #   column(width=10,
                                    #          tags$h2("Standard Survival Analysis")),
                                    #   #____________________________________
                                    #   #ToolTip Help
                                    #   #____________________________________
                                    #   column(width=2,
                                    #          dropdownButton(
                                    #            status="info",
                                    #            icon=icon("info"),
                                    #            width="1000px",
                                    #            size="xs",
                                    #            right = TRUE,
                                    #            tooltip=tooltipOptions(title = "Click for help",
                                    #                                   placement="left"),
                                    #            h2('TEST')
                                    #            #uiOutput("ReadMe2")
                                    #            
                                    #          ),
                                    #          align="right")
                                    # ), #end fluidRow
                                    # fluidRow(
                                    #   column(2,
                                    #          wellPanel(
                                    #            h4(strong("Input your file")),
                                    #            selectInput("file2",label= "Select an example ds or upload your own with 'Load my own'", 
                                    #                        choices = c("Example ds File"="Example2", "Load my own data" = "load_my_own2")),
                                    #            conditionalPanel("input.file2 == 'load_my_own2'",
                                    #                             fileInput('file22', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                    #            conditionalPanel("input.file2 == 'Example2'",
                                    #                             downloadButton('downloadEx2', 'Download Example data')
                                    #            )),
                                    #          conditionalPanel("input.cPanels1 == 2",
                                    #                           wellPanel(
                                    #                             h4(strong("KM Analysis")),
                                    #                             selectInput("select21", "Select a Variable of Interest as Cohort Group", 
                                    #                                         choices=c("AGE", "RACE")),
                                    #                             selectInput("binary",label= "The variable of interest is categorical or continuous", 
                                    #                                         choices = c("Categorical Variable" = "categorical", "Continuous Variable"="continuous"), selected= "continuous"),
                                    #                             conditionalPanel("input.binary == 'continuous'",
                                    #                                              radioButtons("cutoff2", "Choose Cutoff Point for Continuous Variable:", list("Optimal Cutoff" = 1,"25th Percentile" = 25,"50th Percentile" = 50, "75th Percentile" = 75), selected = 25)),
                                    #                             selectInput("select22", "Select a Time Variable to Visualize KM Plot", 
                                    #                                         choices=c("os")),
                                    #                             selectInput("select23", "Select a Censoring Variable to Visualize KM Plot", 
                                    #                                         choices=c("os_censor")),
                                    #                             radioButtons("time", "Time Unit:", list("Years" = 1, "Months" = 2, "Days" = 3), selected = 1),
                                    #                             radioButtons("riskt", "Risk Table:", list("Yes" = TRUE, "No" = FALSE), selected = TRUE),
                                    #                             hr()                                
                                    #                           )),
                                    #          conditionalPanel("input.cPanels1 == 3",
                                    #                           wellPanel(
                                    #                             h4(strong("Univariate Association Analysis")),
                                    #                             selectInput("select24", "Select a Time Variable for Survival Analysis", 
                                    #                                         choices=c("os")),
                                    #                             selectInput("select25", "Select a Censoring Variable for Survival Analysis", 
                                    #                                         choices=c("os_censor")),
                                    #                             selectInput("show_vars26", "Select multiple Variables to Generate Univariate Survival Association Table", 
                                    #                                         c("AGE", "RACE"), choices=c("AGE", "RACE"), multiple = TRUE),
                                    #                             radioButtons("assum", "Test for Proportional Hazards Assumption:", list("Yes" = 1,"No" = 0), selected = 0),
                                    #                             hr()                                
                                    #                           )),
                                    #          
                                    #          conditionalPanel("input.cPanels1 == 3",
                                    #                           h4(strong("Downloads")),
                                    #                           wellPanel(
                                    #                             textInput("fname22", "Type the file name you would like to save as", value = "survivaltable"),
                                    #                             downloadButton('x1', 'Download Survival Report')
                                    #                           )),
                                    #          conditionalPanel("input.cPanels1 == 2",
                                    #                           h4(strong("Downloads")),
                                    #                           wellPanel(
                                    #                             textInput("fname21", "Type the file name you would like to save as", value = "kmplot"),
                                    #                             downloadButton('downloadKM', 'Download KM Plot')
                                    #                           )
                                    #          )),
                                    #   column(10,
                                    #          tabsetPanel(
                                    #            tabPanel("Read Me", htmlOutput("ReadMe2"), value =1),
                                    #            tabPanel("KM Analysis and Plot", htmlOutput("pv21"), plotOutput("kmplot", height= 600, width = 800), value =2),
                                    #            tabPanel("Univariate Survival Association (Cox Model)", htmlOutput("pv22"),
                                    #                     DT::dataTableOutput("out2"), value = 3),
                                    #            id = "cPanels1"
                                    #          )                               #uiOutput("out2")
                                    #          
                                    #   ),
                                    #   column(12,
                                    #          tags$head(tags$style(type="text/css", "
                                    #                               #loadmessage {
                                    #                               position: fixed;
                                    #                               bottom: 0px;
                                    #                               right: 0px;
                                    #                               width: 100%;
                                    #                               padding: 5px 0px 5px 0px;
                                    #                               text-align: center;
                                    #                               font-weight: bold;
                                    #                               font-size: 100%;
                                    #                               color: #000000;
                                    #                               background-color: #b8b8b8;
                                    #                               z-index: 105;
                                    #                               }
                                    #                               ")),
                                    #          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                    #                           tags$div("Loading...",id="loadmessage"))
                                    #          
                                    #          )
                                    # )
                                    
                                    navbarPage("CASAS: Cancer Survival Analysis Suite", 
                                               
                                               tabPanel("Standard Survival Analysis",
                                                        fluidRow(
                                                          column(2,
                                                                 wellPanel(
                                                                   h4(strong("Input your file")),
                                                                   selectInput("file2",label= "Select an example ds or upload your own with 'Load my own'", 
                                                                               choices = c("Example ds File"="Example2", "Load my own data" = "load_my_own2")),
                                                                   conditionalPanel("input.file2 == 'load_my_own2'",
                                                                                    fileInput('file22', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                                                   conditionalPanel("input.file2 == 'Example2'",
                                                                                    downloadButton('downloadEx2', 'Download Example data')
                                                                   )),
                                                                 conditionalPanel("input.cPanels1 == 2",
                                                                                  wellPanel(
                                                                                    h4(strong("KM Analysis")),
                                                                                    selectInput("select21", "Select a Variable of Interest as Cohort Group", 
                                                                                                choices=c("AGE", "RACE")),
                                                                                    selectInput("binary",label= "The variable of interest is categorical or continuous", 
                                                                                                choices = c("Categorical Variable" = "categorical", "Continuous Variable"="continuous"), selected= "continuous"),
                                                                                    conditionalPanel("input.binary == 'continuous'",
                                                                                                     radioButtons("cutoff2", "Choose Cutoff Point for Continuous Variable:", list("Optimal Cutoff" = 1,"25th Percentile" = 25,"50th Percentile" = 50, "75th Percentile" = 75), selected = 25)),
                                                                                    selectInput("select22", "Select a Time Variable to Visualize KM Plot", 
                                                                                                choices=c("os")),
                                                                                    selectInput("select23", "Select a Censoring Variable to Visualize KM Plot", 
                                                                                                choices=c("os_censor")),
                                                                                    radioButtons("time", "Time Unit:", list("Years" = 1, "Months" = 2, "Days" = 3), selected = 1),
                                                                                    radioButtons("riskt", "Risk Table:", list("Yes" = TRUE, "No" = FALSE), selected = TRUE),
                                                                                    hr()                                
                                                                                  )),
                                                                 conditionalPanel("input.cPanels1 == 3",
                                                                                  wellPanel(
                                                                                    h4(strong("Univariate Association Analysis")),
                                                                                    selectInput("select24", "Select a Time Variable for Survival Analysis", 
                                                                                                choices=c("os")),
                                                                                    selectInput("select25", "Select a Censoring Variable for Survival Analysis", 
                                                                                                choices=c("os_censor")),
                                                                                    selectInput("show_vars26", "Select multiple Variables to Generate Univariate Survival Association Table", 
                                                                                                c("AGE", "RACE"), choices=c("AGE", "RACE"), multiple = TRUE),
                                                                                    radioButtons("assum", "Test for Proportional Hazards Assumption:", list("Yes" = 1,"No" = 0), selected = 0),
                                                                                    hr()                                
                                                                                  )),
                                                                 
                                                                 conditionalPanel("input.cPanels1 == 3",
                                                                                  h4(strong("Downloads")),
                                                                                  wellPanel(
                                                                                    textInput("fname22", "Type the file name you would like to save as", value = "survivaltable"),
                                                                                    downloadButton('x1', 'Download Survival Report')
                                                                                  )),
                                                                 conditionalPanel("input.cPanels1 == 2",
                                                                                  h4(strong("Downloads")),
                                                                                  wellPanel(
                                                                                    textInput("fname21", "Type the file name you would like to save as", value = "kmplot"),
                                                                                    downloadButton('downloadKM', 'Download KM Plot')
                                                                                  )
                                                                 )),
                                                          column(10,
                                                                 tabsetPanel(
                                                                   tabPanel("Read Me", htmlOutput("ReadMe2"), value =1),
                                                                   tabPanel("KM Analysis and Plot", htmlOutput("pv21"), plotOutput("kmplot", height= 600, width = 800), value =2),
                                                                   tabPanel("Univariate Survival Association (Cox Model)", htmlOutput("pv22"),
                                                                            DT::dataTableOutput("out2"), value = 3),
                                                                   id = "cPanels1"
                                                                 )                               #uiOutput("out2")
                                                                 
                                                          ),
                                                          column(12,
                                                                 tags$head(tags$style(type="text/css", "
                                                                                      #loadmessage {
                                                                                      position: fixed;
                                                                                      bottom: 0px;
                                                                                      right: 0px;
                                                                                      width: 100%;
                                                                                      padding: 5px 0px 5px 0px;
                                                                                      text-align: center;
                                                                                      font-weight: bold;
                                                                                      font-size: 100%;
                                                                                      color: #000000;
                                                                                      background-color: #b8b8b8;
                                                                                      z-index: 105;
                                                                                      }
                                                                                      ")),
                                                                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                                                  tags$div("Loading...",id="loadmessage"))
                                                                 
                                                                 )
                                                          )),
                                               tabPanel("Competing Risk Survival Analysis",
                                                        fluidRow(
                                                          column(2,
                                                                 wellPanel(
                                                                   h4(strong("Input your file")),
                                                                   selectInput("file3",label= "Select an example ds or upload your own with 'Load my own'", 
                                                                               choices = c("Example ds File"="Example3", "Load my own data" = "load_my_own3")),
                                                                   conditionalPanel("input.file3 == 'load_my_own3'",
                                                                                    fileInput('file32', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                                                   conditionalPanel("input.file3 == 'Example3'",
                                                                                    downloadButton('downloadEx3', 'Download Example data')
                                                                   )),
                                                                 conditionalPanel("input.cPanels2 == 2",
                                                                                  wellPanel(
                                                                                    h4(strong("CIF Analysis")),
                                                                                    selectInput("select31", "Select a Variable of Interest as Cohort Group", 
                                                                                                choices=c("AGE", "RACE")),
                                                                                    selectInput("binary2",label= "The variable of interest is categorical or continuous", 
                                                                                                choices = c("Categorical Variable" = "categorical", "Continuous Variable"="continuous")),
                                                                                    conditionalPanel("input.binary2 == 'continuous'",
                                                                                                     radioButtons("cutoff3", "Choose Cutoff Point for Continuous Variable:", list("25th Percentile" = 25,"50th Percentile" = 50, "75th Percentile" = 75), selected = 25)),
                                                                                    selectInput("select32", "Select a Time Variable to Visualize CIF Plot", 
                                                                                                choices=c("os")),
                                                                                    selectInput("select33", "Select a Censoring Variable to Visualize CIF Plot", 
                                                                                                choices=c("os_censor")),
                                                                                    radioButtons("points", "Input Time Points:", list("Yes" = 1, "No" = 0), selected = 0),
                                                                                    textInput("text", label = "Time Points Input", value = "0, 10, 20, 30"),
                                                                                    radioButtons("time2", "Time Unit:", list("Years" = 1, "Months" = 2, "Days" = 3), selected = 2),
                                                                                    radioButtons("event", "Event Code:", list("1" = 1, "2" = 2, "0" = 0), selected = 2),
                                                                                    radioButtons("censor", "Censor Code:", list("1" = 1, "2" = 2, "0" = 0), selected = 0),
                                                                                    hr()                                
                                                                                  )),
                                                                 conditionalPanel("input.cPanels2 == 3",
                                                                                  wellPanel(
                                                                                    h4(strong("Univariate Association Analysis")),
                                                                                    selectInput("select34", "Select a Time Variable for Competing Risk Survival Analysis", 
                                                                                                choices=c("os")),
                                                                                    selectInput("select35", "Select a Censoring Variable for Competing Risk Survival Analysis", 
                                                                                                choices=c("os_censor")),
                                                                                    selectInput("show_vars36", "Select multiple Variables to Generate Univariate Survival Association Table", 
                                                                                                c("AGE", "RACE"), choices=c("AGE", "RACE"), multiple = TRUE),
                                                                                    radioButtons("event2", "Event Code:", list("1" = 1, "2" = 2, "0" = 0), selected = 2),
                                                                                    radioButtons("censor2", "Censor Code:", list("1" = 1, "2" = 2, "0" = 0), selected = 0),
                                                                                    hr()                                
                                                                                  )),
                                                                 conditionalPanel("input.cPanels2 == 3",
                                                                                  h4(strong("Downloads")),
                                                                                  wellPanel(
                                                                                    textInput("fname32", "Type the file name you would like to save as", value = "crrtable"),
                                                                                    downloadButton('x2', 'Download Competing Risk Report')
                                                                                  )),
                                                                 conditionalPanel("input.cPanels2 == 2",
                                                                                  h4(strong("Downloads")),
                                                                                  wellPanel(
                                                                                    textInput("fname31", "Type the file name you would like to save as", value = "cifplot"),
                                                                                    downloadButton('downloadcif', 'Download CIF Plot')
                                                                                  ),
                                                                                  wellPanel(
                                                                                    textInput("fname33", "Type the file name you would like to save as", value = "ciftable"),
                                                                                    downloadButton('downloadciftable', 'Download CIF Report')
                                                                                  )
                                                                 )
                                                          ),
                                                          column(10,
                                                                 tabsetPanel(
                                                                   tabPanel("Read Me", htmlOutput("ReadMe3"), value = 1),
                                                                   tabPanel("CIF Analysis and Plot", htmlOutput("pv31"),
                                                                            plotOutput("cifplot", height= 600, width = 800),
                                                                            DT::dataTableOutput("ciftable"), value = 2),
                                                                   tabPanel("Univariate Survival Association (Fine and Gray Model)", htmlOutput("pv32"),
                                                                            DT::dataTableOutput("out3"), value = 3),
                                                                   id = "cPanels2")                               #uiOutput("out2")
                                                                 
                                                                 #uiOutput("out2")
                                                                 
                                                          ),
                                                          column(12,
                                                                 tags$head(tags$style(type="text/css", "
                                                                                      #loadmessage {
                                                                                      position: fixed;
                                                                                      bottom: 0px;
                                                                                      right: 0px;
                                                                                      width: 100%;
                                                                                      padding: 5px 0px 5px 0px;
                                                                                      text-align: center;
                                                                                      font-weight: bold;
                                                                                      font-size: 100%;
                                                                                      color: #000000;
                                                                                      background-color: #b8b8b8;
                                                                                      z-index: 105;
                                                                                      }
                                                                                      ")),
                                                                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                                                  tags$div("Loading...",id="loadmessage"))
                                                                 
                                                                 )
                                                          )),
                                               tabPanel("Landmark Survival Analysis",
                                                        fluidRow(
                                                          column(2,
                                                                 wellPanel(
                                                                   h4(strong("Input your file")),
                                                                   selectInput("file4",label= "Select an example ds or upload your own with 'Load my own'", 
                                                                               choices = c("Example ds File"="Example4", "Load my own data" = "load_my_own4")),
                                                                   conditionalPanel("input.file4 == 'load_my_own4'",
                                                                                    fileInput('file42', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                                                   conditionalPanel("input.file4 == 'Example4'",
                                                                                    downloadButton('downloadEx4', 'Download Example data')
                                                                   )),
                                                                 wellPanel(
                                                                   h4(strong("KM Analysis")),
                                                                   selectInput("select41", "Select a Variable of Interest as Cohort Group", 
                                                                               choices=c("AGE", "RACE")),
                                                                   selectInput("binary3",label= "The variable of interest is categorical or continuous", 
                                                                               choices = c("Categorical Variable" = "categorical", "Continuous Variable"="continuous")),
                                                                   conditionalPanel("input.binary3 == 'continuous'",
                                                                                    radioButtons("cutoff4", "Choose Cutoff Point for Continuous Variable:", list("Optimal Cutoff" = 1,"25th Percentile" = 25,"50th Percentile" = 50, "75th Percentile" = 75), selected = 25)),
                                                                   selectInput("select42", "Select a Time Variable to Visualize KM/CIF Plot", 
                                                                               choices=c("os")),
                                                                   selectInput("select43", "Select a Censoring Variable to Visualize KM/CIF Plot", 
                                                                               choices=c("os_censor")),
                                                                   selectInput("select44", "Select a Time Dependent Variable to Visualize KM/CIF Plot", 
                                                                               choices=c("wtime")),
                                                                   textInput("text2", label = "Input Time Point for Landmark Analysis", value = "200"),
                                                                   radioButtons("time3", "Time Unit:", list("Years" = 1, "Months" = 2, "Days" = 3), selected = 3),
                                                                   radioButtons("option", "KM or CIF:", list("KM" = TRUE, "CIF" = FALSE), selected = TRUE),
                                                                   radioButtons("riskt2", "Risk Table:", list("Yes" = TRUE, "No" = FALSE), selected = TRUE),
                                                                   hr()                                
                                                                 ),
                                                                 h4(strong("Downloads")),
                                                                 wellPanel(
                                                                   textInput("fname41", "Type the file name you would like to save as", value = "landmarkplot"),
                                                                   downloadButton('downloadlandmarkplot', 'Download Landmark Plot')
                                                                 )
                                                          ),
                                                          column(10,
                                                                 tabsetPanel(
                                                                   tabPanel("Read Me", htmlOutput("ReadMe4") ),
                                                                   tabPanel("Landmark Survival Plot", htmlOutput("pv41"),
                                                                            plotOutput("landmarkplot", height= 600, width = 800))
                                                                 )                               #uiOutput("out2")
                                                                 
                                                                 #uiOutput("out2")
                                                                 
                                                          ),
                                                          column(12,
                                                                 tags$head(tags$style(type="text/css", "
                                                                                      #loadmessage {
                                                                                      position: fixed;
                                                                                      bottom: 0px;
                                                                                      right: 0px;
                                                                                      width: 100%;
                                                                                      padding: 5px 0px 5px 0px;
                                                                                      text-align: center;
                                                                                      font-weight: bold;
                                                                                      font-size: 100%;
                                                                                      color: #000000;
                                                                                      background-color: #b8b8b8;
                                                                                      z-index: 105;
                                                                                      }
                                                                                      ")),
                                                                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                                                  tags$div("Loading...",id="loadmessage"))
                                                                 
                                                                 )
                                                          )),
                                               tabPanel("Quantile Survival Analysis",  
                                                        fluidRow(
                                                          column(2,
                                                                 wellPanel(
                                                                   h4(strong("Input your file")),
                                                                   selectInput("file1",label= "Select an example ds or upload your own with 'Load my own'", 
                                                                               choices = c("Example ds File"="Example", "Load my own data" = "load_my_own")),
                                                                   conditionalPanel("input.file1 == 'load_my_own'",
                                                                                    fileInput('file12', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                                                   conditionalPanel("input.file1 == 'Example'",
                                                                                    downloadButton('downloadEx', 'Download Example data')
                                                                   )),
                                                                 wellPanel(
                                                                   h4(strong("Quantile Survival Analysis")),
                                                                   selectInput("select", "Select a Variable of Interest", 
                                                                               choices=c("AGE", "RACE")),
                                                                   selectInput("binary4",label= "The variable of interest is categorical or continuous", 
                                                                               choices = c("Categorical Variable" = "categorical", "Continuous Variable"="continuous")),
                                                                   conditionalPanel("input.binary4 == 'continuous'",
                                                                                    radioButtons("cutoff", "Choose Cutoff Point:", list("Optimal Cutoff" = 1,"25th Percentile" = 25,"50th Percentile" = 50, "75th Percentile" = 75), selected = 25),
                                                                                    radioButtons("exp_ref", "Choose Reference Level:", list("High" = "High","Low" = "Low"), selected = "High")
                                                                   ),
                                                                   selectInput("select12", "Select a Time Variable for Quantile Analysis", 
                                                                               choices=c("os")),
                                                                   selectInput("select13", "Select a Censoring Variable for Quantile Analysis", 
                                                                               choices=c("os_censor")),
                                                                   radioButtons("time4", "Time Unit:", list("Years" = 1, "Months" = 2, "Days" = 3), selected = 1),
                                                                   actionButton("numb", "Run & generate random numbers"),
                                                                   h5("Hit Run & Generate random number button above EACH time to display output on main panel."),
                                                                   hr()                              # Copy the line below to make a select box 
                                                                 ),
                                                                 h4(strong("Downloads")),
                                                                 wellPanel(
                                                                   textInput("fname11", "Type the file name you would like to save as", value = "QAplot"),
                                                                   downloadButton('downloadQA', 'Download Quantile Survival Plot')),
                                                                 wellPanel(
                                                                   textInput("fname12", "Type the file name you would like to save as", value = "forestplot"),
                                                                   downloadButton('downloadFP', 'Download Forest Plot')),
                                                                 wellPanel(
                                                                   textInput("fname13", "Type the file name you would like to save as", value = "Data for Grid"),
                                                                   downloadButton('downloadTB', 'Download Grid Data')
                                                                 )
                                                          ),
                                                          column(10,
                                                                 tabsetPanel(
                                                                   tabPanel("Read Me", htmlOutput("ReadMe1")),
                                                                   tabPanel("Quantile Survival Plots and Output",                                
                                                                            htmlOutput("pv15"),
                                                                            splitLayout(cellWidths = c("60%", "40%"), htmlOutput("pv11"), htmlOutput("pv12")),
                                                                            splitLayout(cellWidths = c("60%", "40%"), plotOutput("QAplot", height= 600, width = 800),
                                                                                        plotOutput("forestplot", height= 500, width = 600)),
                                                                            htmlOutput("pv13"),
                                                                            DT::dataTableOutput("coxout"),
                                                                            htmlOutput("pv14"),
                                                                            DT::dataTableOutput("CIout")
                                                                   )
                                                                 )                               #uiOutput("out2")
                                                                 #htmlOutput("pv"),
                                                                 #plotOutput("QAplot", height= 600, width = 800),
                                                                 #htmlOutput("pv2"),
                                                                 #plotOutput("forestplot", height= 600, width = 800)
                                                                 #htmlOutput("blurp"))
                                                                 #h5("Descriptive Statistics"),
                                                                 #verbatimTextOutput("out")
                                                                 
                                                          ),
                                                          column(12,
                                                                 tags$head(tags$style(type="text/css", "
                                                                                      #loadmessage {
                                                                                      position: fixed;
                                                                                      bottom: 0px;
                                                                                      right: 0px;
                                                                                      width: 100%;
                                                                                      padding: 5px 0px 5px 0px;
                                                                                      text-align: center;
                                                                                      font-weight: bold;
                                                                                      font-size: 100%;
                                                                                      color: #000000;
                                                                                      background-color: #b8b8b8;
                                                                                      z-index: 105;
                                                                                      }
                                                                                      ")),
                                                                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                                                  tags$div("Loading...",id="loadmessage"))
                                                                 
                                                                 )
                                                          )),
                                               tabPanel("Optimal Cutoff Point Finder",  
                                                        fluidRow(
                                                          column(2,
                                                                 wellPanel(
                                                                   h4(strong("Input your file")),
                                                                   selectInput("file5",label= "Select an example ds or upload your own with 'Load my own'", 
                                                                               choices = c("Example ds File"="Example5", "Load my own data" = "load_my_own5")),
                                                                   conditionalPanel("input.file5 == 'load_my_own5'",
                                                                                    fileInput('file51', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                                                   conditionalPanel("input.file5 == 'Example5'",
                                                                                    downloadButton('downloadEx5', 'Download Example data')
                                                                   )),
                                                                 wellPanel(
                                                                   h4(strong("Choose Variable")),
                                                                   selectInput("select51", "Select Input", 
                                                                               choices=c("AGE", "RACE")),
                                                                   selectInput("select52", "Select a Time Variable", 
                                                                               choices=c("os")),
                                                                   selectInput("select53", "Select a Censoring Variable", 
                                                                               choices=c("os_censor")),
                                                                   hr()                              # Copy the line below to make a select box 
                                                                 ),
                                                                 h4(strong("Downloads")),
                                                                 wellPanel(
                                                                   textInput("fname51", "Type the file name you would like to save as", value = "MRplot"),
                                                                   downloadButton('downloadMR', 'Download Martingale Residual Plot')
                                                                 )
                                                          ),
                                                          column(10,
                                                                 tabsetPanel(
                                                                   tabPanel("Read Me", htmlOutput("ReadMe5")),
                                                                   tabPanel("Optimal Cutpoint Finder Output)", plotOutput("MRplot", height= 600, width = 800),
                                                                            htmlOutput("pv51"),
                                                                            DT::dataTableOutput("Optimal"))
                                                                 )                               #uiOutput("out2")
                                                                 #htmlOutput("pv4"),
                                                                 
                                                                 #htmlOutput("blurp"))
                                                                 #h5("Descriptive Statistics"),
                                                                 #verbatimTextOutput("out")
                                                                 
                                                          ),
                                                          column(12,
                                                                 tags$head(tags$style(type="text/css", "
                                                                                      #loadmessage {
                                                                                      position: fixed;
                                                                                      bottom: 0px;
                                                                                      right: 0px;
                                                                                      width: 100%;
                                                                                      padding: 5px 0px 5px 0px;
                                                                                      text-align: center;
                                                                                      font-weight: bold;
                                                                                      font-size: 100%;
                                                                                      color: #000000;
                                                                                      background-color: #b8b8b8;
                                                                                      z-index: 105;
                                                                                      }
                                                                                      ")),
                                                                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                                                  tags$div("Loading...",id="loadmessage"))
                                                                 
                                                                 )
                                                          )),    
                                               tabPanel("Grid for Summarized Significance",
                                                        fluidRow(
                                                          column(2,
                                                                 wellPanel(
                                                                   h4(strong("Input your file")),
                                                                   selectInput("file6",label= "Select an example ds or upload your own with 'Load my own'", 
                                                                               choices = c("Example ds File"="Example6", "Load my own data" = "load_my_own6")),
                                                                   conditionalPanel("input.file6 == 'load_my_own6'",
                                                                                    fileInput('file61', 'Choose file to upload (maximum size 500 MB).', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                                                                   conditionalPanel("input.file6 == 'Example6'",
                                                                                    downloadButton('downloadEx6', 'Download Example data')
                                                                   )),
                                                                 wellPanel(
                                                                   radioButtons("exp_ref2", "Choose Reference Level:", list("High" = "High","Low" = "Low"), selected = "High")
                                                                 ),
                                                                 h4(strong("Downloads")),
                                                                 wellPanel(
                                                                   textInput("fname61", "Type the file name you would like to save as", value = "Grid"),
                                                                   downloadButton('downloadHM', 'Download Grid Plot')
                                                                 )
                                                          ),
                                                          column(10,
                                                                 tabsetPanel(
                                                                   tabPanel("Read Me", htmlOutput("ReadMe6")),
                                                                   tabPanel("Summarized Significance Grid", htmlOutput("pv61"),
                                                                            plotOutput("heatmapplot", height= 800, width = 800))
                                                                 )                               #uiOutput("out2")
                                                                 
                                                                 #htmlOutput("pv3"), 
                                                                 #htmlOutput("title"),
                                                                 #DT::dataTableOutput("SuperPC")
                                                          ),
                                                          column(12,
                                                                 tags$head(tags$style(type="text/css", "
                                                                                      #loadmessage {
                                                                                      position: fixed;
                                                                                      bottom: 0px;
                                                                                      right: 0px;
                                                                                      width: 100%;
                                                                                      padding: 5px 0px 5px 0px;
                                                                                      text-align: center;
                                                                                      font-weight: bold;
                                                                                      font-size: 100%;
                                                                                      color: #000000;
                                                                                      background-color: #b8b8b8;
                                                                                      z-index: 105;
                                                                                      }
                                                                                      ")),
                                                                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                                                  tags$div("Loading...",id="loadmessage"))
                                                                 
                                                                 )
                                                          )),
                                               tabPanel("Tutorial",
                                                        tags$iframe(src= "CASAS_Tutorial.pdf", width = 1800, height = 1000)),
                                               navbarMenu("About Us",
                                                          tabPanel("How to Cite",
                                                                   fluidRow(
                                                                     column(8, offset = 2,
                                                                            "Rupji M, Zhang X and Kowalski J. CASAS: Cancer Survival Analysis Suite, a web based application [version 1; referees: awaiting peer review]. F1000Research 2017, 6:919 (doi: 10.12688/f1000research.11830.1)",
                                                                            br(),
                                                                            br(),
                                                                            "The National Cancer Institute (NCI) requires that publications acknowledge the Winship Cancer Institute CCSG support, and they are tracking compliance. When using this tool to report results in your publication, please include the following statement in the acknowledgment section of your publication(s):",
                                                                            br(),
                                                                            br(),
                                                                            em("Research reported in this publication was supported in part by the Biostatistics and Bioinformatics Shared Resource of Winship Cancer Institute of Emory University and NIH/NCI under award number P30CA138292. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health.")
                                                                     ))),
                                                          tabPanel("Contact Us",
                                                                   fluidRow(
                                                                     column(8, offset = 2,
                                                                            "This tool was prepared by members of the Winship Biostatistics and Bioinformatics Shared Resource (BBISR) of Emory University.",
                                                                            br(),
                                                                            a(href="https://bbisr.winship.emory.edu/", "https://bbisr.winship.emory.edu/"),
                                                                            br(),
                                                                            br(),
                                                                            "Authors- Manali Rupji, dual M.S., Xinyan (Abby) Zhang, MPH. & Jeanne Kowalski Ph.D.",
                                                                            br(),
                                                                            "Maintainer- Manali Rupji 'manali(dot)rupji(at)emory(dot)edu'")
                                                                   )),
                                                          tabPanel("Feedback",
                                                                   fluidRow(
                                                                     column(8, offset = 2,
                                                                            #br(),
                                                                            "As a Biostatistics and Bioinformatics core, we are actively improving and expanding our NGS analysis services and analysis products. For any questions, comments, or suggestions, please email the developer at manali(dot)rupji(at)emory(dot)edu."
                                                                     )))
                                               ))
                                    
)#end tabItem