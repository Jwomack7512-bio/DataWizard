#This tab corresponds to the "Plotly" SubTab (under Graphing Tab) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  August 24, 2020
#  Last Update: Septempber 27, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_PLOTLY <- tabItem(tabName = "TAB_PLOTLY", 
                          tags$h2("Plotly"),
                          br(),
                          radioGroupButtons(inputId="plotly_plotType",
                                            label='Choose Which Plot to View',
                                            choices = c('Scatter',
                                                        "Histogram",
                                                        "Boxplot",
                                                        "Violin", 
                                                        "Line",
                                                        "Bar"),
                                            status = "primary"),
                          plotlyOutput('plotlyPlot')
)