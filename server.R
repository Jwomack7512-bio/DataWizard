#This is the server function of the App (Currently contains server for all parts of app)
#-------------------------------------------------------------------------
#  Justin Womack
#  July 22, 2020
#  Last Update: January 2, 2021
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------


#These are function files used in the survival Analysis Tab (copied from source https://github.com/manalirupji/CASAS)
# source(".\\server\\QSE.R")
# source(".\\server\\forestplot.R")
# source(".\\server\\significant.R")
# source(".\\server\\boxcolor.R")
# source(".\\server\\survplot.R")
# source(".\\server\\cutpoint finder.R")
# source(".\\server\\optimalcut_plot.R")
# source(".\\server\\optimalcut_table.R")
# source(".\\server\\CumIncidence.R")
# source(".\\server\\factor2ind.R")

#this source contains functions useful for datamanagement, specifically mutating
source("server/dataManagement.R")

#These vector will store values in the "Multiple Linear Regression" Tab to be multipied together
#values get appended to this vector on button press and then his is loaded into other elements.
values_to_multiply <- vector()
values_to_multiply_LogReg <- vector()
values_to_multiply_anova <- vector()
values_to_multiply_COX <- vector()

server <- shinyServer(function(input, output, session){
  #allows for the import of bigger data frames
  options(shiny.maxRequestSize = 30000*1024^2)
  #options(shiny.sanitize.errors = TRUE)
  
  #render ui if user has bad eyes
  output$css_big_textview <- renderUI({
      tags$head(
        if(input$old_mode){
        tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')}
        #else{tags$link(rel = 'stylesheet', type = 'text/css', href = 'style_normal.css')}
      )
    
  })
  
  #TABITEM: Data Management
  source(file.path("server", "server_datamanagement.R"), local=TRUE)$value
  
  #TABITEM: GRAPHING
  source(file.path("server", "server_graphing_scatterplot.R"), local=TRUE)$value
  source(file.path("server", "server_graphing_histogram.R"), local=TRUE)$value
  source(file.path("server", "server_graphing_violinplot.R"), local=TRUE)$value
  source(file.path("server", "server_graphing_boxplot.R"), local=TRUE)$value
  source(file.path("server", "server_graphing_lineplot.R"), local=TRUE)$value
  source(file.path("server", "server_graphing_barplot.R"), local=TRUE)$value
  source(file.path("server", "server_graphing_forestplot.R"), local=TRUE)$value
  
  #TABITEM: Table Statistics
  source(file.path("server", "server_tableone.R"), local=TRUE)$value
  #TABITEM: InteractivePlot
  source(file.path("server", "server_plotly.R"), local=TRUE)$value

  #TABITEM: Univaraite Statistics
  source(file.path("server", "server_uniVarStat_ttest.R"), local=TRUE)$value
  source(file.path("server", "server_uniVarStat_chisquared.R"), local=TRUE)$value
  source(file.path("server", "server_uniVarStat_wilcox.R"), local=TRUE)$value
  
  #TABITEM: MultiVaraite Statistics
  source(file.path("server", "server_multiVarStat_MLR.R"), local=TRUE)$value
  source(file.path("server", "server_multiVarStat_LogReg.R"), local=TRUE)$value
  source(file.path("server", "server_multiVarStat_anova.R"), local=TRUE)$value
  
  #TABITEM: Survival
  source(file.path("server", "server_survival_KM.R"), local=TRUE)$value
  source(file.path("server", "server_survival_cox.R"), local=TRUE)$value
  
})#end of server