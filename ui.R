#This is the UI of the APP and contains the souce to separate UI files
#-------------------------------------------------------------------------
#  Justin Womack
#  July 22, 2020
#  Last Update: August 15, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------


library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
library(ggplot2)
library(gridExtra)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(tidyverse)
library(dplyr)
library(rhandsontable)
library(data.table)
library(ggpmisc)
library(plotly)
library(colourpicker)
library(shinyBS)
library(tableone)
library(bsplus)
library(readxl)
library(vcd)
library(survival)
library(survMisc)
library(gplots)
library(survminer)
library(cmprsk)
library(dynpred)
library(reshape2)
library(RColorBrewer)
library(forestplot)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(nortest)
library(agricolae)
library(shinyjqui)
library(finalfit)
library(survival)
library(gtsummary)

# #csv2sql needs to be installed with the following: devtools::install_github("kcf-jackson/csv2sql")
# load.lib<-c("shinydashboard", "shinydashboardPlus", "shiny","ggplot2","gridExtra","shinythemes",
#             "shinyWidgets","shinyjs","DT","tidyverse","dplyr","rhandsontable","data.table","ggpmisc",
#             "plotly","colourpicker","shinyBS","tableone","bsplus","readxl","vcd",
#             "survival","survMisc","gplots","survminer","cmprsk","dynpred", 
#             "reshape2","RColorBrewer", "forestplot", "sjPlot", "sjmisc",
#             "sjlabelled", "nortest", "agricolae", "shinyjqui", "finalfit", "survival",
#             "gtsummary")
# 
# 
# install.lib<-load.lib[!load.lib %in% installed.packages()]
# for(lib in install.lib) install.packages(lib,dependencies=TRUE)
# sapply(load.lib,require,character=TRUE)

#load files with UI outputs
source("ui/dataviewUI.R")
source("ui/ST_scatterplotUI.R")
source("ui/ST_histogramUI.R")
source("ui/ST_boxplotUI.R")
source("ui/tableoneUI.R")
source("ui/ST_violinplotUI.R")
source("ui/ST_lineplotUI.R")
source("ui/plotlyUI.R")
source("ui/ST_barplotUI.R")
source("ui/ST_forestplotUI.R")
source("ui/ST_ttestUI.R")
source("ui/ST_chisquaredUI.R")
source("ui/ST_wilcoxUI.R")
source("ui/ST_multLinearRegressionUI.R")
source("ui/ST_anovaUI.R")
source("ui/ST_LogRegUI.R")
source("ui/ST_survival_KM_UI.R")
source("ui/coxUI.R")


ui <- dashboardPage(skin='blue',
                    dashboardHeader(title = "Data Wizard", tags$li(class="dropdown", materialSwitch("old_mode", "Big Text", status="primary", right=TRUE))),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Data Management", tabName = "TAB_DATAVIEW", icon = icon("desktop")),
                        menuItem("Graphing", tabName = "TAB_GRAPHING", icon = icon("chart-area"),
                                 menuSubItem("Scatter Plot", tabName='TAB_SCATTER', icon=icon('braille')),
                                 menuSubItem("Histogram", tabName='TAB_HISTOGRAM', icon=icon("chart-bar")),
                                 menuSubItem("BoxPlot", tabName="TAB_BOXPLOT", icon=icon("box")),
                                 menuSubItem("Violin Plot", tabName = "TAB_VIOLIN"),
                                 menuSubItem("Line Plot", tabName = "TAB_LINEPLOT", icon=icon('line-chart')),
                                 menuSubItem("Bar Chart", tabName = "TAB_BARPLOT"),
                                 menuSubItem("Forest Plot", tabName="TAB_FORESTPLOT")),
                        menuItem("Table Statistics", tabName="TAB_TABLEONE", icon=icon("calendar")),
                        menuItem("Interactive Plot", tabName="TAB_PLOTLY"),
                        menuItem("Univariate Statistics", tabName="TAB_UNIVARSTAT",
                                 menuSubItem("t-test", tabName="TAB_TTEST", icon=NULL),
                                 menuSubItem("Chi-squared", tabName="TAB_CHISQUARED", icon=NULL),
                                 menuSubItem("Wilcox", tabName="TAB_WILCOX", icon=NULL)),
                        menuItem("MultiVariate Statisitcs", tabName="TAB_MULTIVARSTAT",
                                 menuSubItem("Multiple Linear Regression", tabName="TAB_MLR"),
                                 menuSubItem("Logistic Regression", tabName="TAB_LogReg"),
                                 menuSubItem("Anova", tabName = "TAB_ANOVA"))
                      ,menuItem("Survival Analysis", tabName="TAB_SURVIVAL"
                                ,menuSubItem("Kaplan Meier Plot", tabName="TAB_KAPLAN_MEIER")
                                ,menuSubItem("Cox Model", tabName = "TAB_COX_SURVIVAL"))
                      )#end SideBarMenu
                    ), #end dashboardSidebar
                    dashboardBody(
                      #tags$style(js),
                      #activates shiny javascript so that I can play with vanishing and appearing div files
                      useShinyjs(),
                      withMathJax(),
                      tags$script(src="popup.js"),
                      uiOutput("css_big_textview"),
                      
                      #tags$head(
                        #this tag changes the size of the text of all tabbox titles
                        # tags$style(type='text/css', 
                        #            ".nav-tabs {font-size: 20px} ")),
                      #this changes the titles of all values in boxes to a font size
                      # tags$p(tags$style(HTML('.box { font-size: 16px; }'))),
                      #these are the names of the tabitems in the source files (i.e TAB_DATAVIEW <- tabpanel(blah blah blah))
                      tabItems(TAB_DATAVIEW, 
                               SUBTAB_SCATTER, 
                               SUBTAB_HISTOGRAM, 
                               SUBTAB_BOXPLOT,
                               SUBTAB_VIOLIN, 
                               SUBTAB_LINEPLOT,
                               SUBTAB_BARPLOT,
                               SUBTAB_FORESTPLOT,
                               SUBTAB_TABLEONE, 
                               TAB_PLOTLY,
                               SUBTAB_TTEST,
                               SUBTAB_CHISQUARED,
                               SUBTAB_WILCOX,
                               SUBTAB_MLR,
                               SUBTAB_LOGREG,
                               SUBTAB_ANOVA,
                               TAB_KAPLAN_MEIER,
                               TAB_COX_SURVIVAL
                               )
                    ) #end dashboardBody
) #end dashboardPage