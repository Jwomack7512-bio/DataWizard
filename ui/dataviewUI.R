#This tab corresponds to the "Data Management" Tab of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  July 22, 2020
#  Last Update: August 15, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_DATAVIEW <- tabItem(tabName = "TAB_DATAVIEW",
        #this row will create a tabbox that will be used to play with the data. 
        #ideal it can select rows, filter, and other such options
        
        fluidRow(
          box(
            #this is a box that holds the import data options.
            title="Data Options", status="primary", solidHeader=TRUE, collapsible=TRUE, width=NULL,
            tabBox(
              title = NULL, width = 12, 
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset1",
#__________________________________________________________________________________________________________


#TABPANEL: IMPORT DATA


#__________________________________________________________________________________________________________
              
              tabPanel("Import Data",
                       fluidRow(column(width=8,
                                       fileInput(inputId="data", 
                                                 label="Import File (.csv, .txt, .xls, .xlsx)")),
                                column(width=3,
                                       radioButtons(
                                               inputId = "dataUI_TABLE_OPTIONS",
                                               label = "Select Table Style",
                                               choices = c("rhandsomeTable", "R Default Table", "Data Table from DT Package")))
                                )#end FluidRow
                       ),#end tablPanel ImportData
#__________________________________________________________________________________________________________


#TABPANEL: SELECT DATA


#__________________________________________________________________________________________________________

              tabPanel("Select Data",
                       #uiOutput("checkbox")),
                       fluidRow(
                         column(width=6,
                                pickerInput(inputId="checkbox", 
                                            label="Select Variables", 
                                            choices=character(), 
                                            options=list('actions-box'=TRUE), 
                                            multiple=TRUE))
                       )),#end tabPanel
#__________________________________________________________________________________________________________

#TABPANEL: FILTER DATA

#__________________________________________________________________________________________________________

              tabPanel("Filter",
                       fluidRow(
                         column(width = 3,
                                radioGroupButtons(
                                  inputId = "FILTER_NUMBER_OPTIONS",
                                  label = "Number of Filters to Apply",
                                  choices = c("One", 
                                              "Two", "Three", "Four"),
                                  justified = TRUE,
                                  checkIcon = list(
                                    yes = icon("ok", 
                                               lib = "glyphicon"))))
                       ), #endFluidRow
                       fluidRow(
                         column(width=1,
                                #This div is used to center checkboxInputs because checkbox inputs want to 
                                #personally annoy me and be out of line with everything else in the FluidRow
                                div(style = "display: inline-block;"),
                                materialSwitch(inputId="filter_on", 
                                               label="Turn On", 
                                               value=FALSE, 
                                               status='primary')), 
                         column(width = 3,
                                selectInput(inputId="filter_var", 
                                            label = "Variable", 
                                            character())),
                         #uiOutput("filter_var_ui")),
                         column(width = 2,
                                pickerInput(inputId="filter_sym", 
                                            label = "Condition", 
                                            choices = list(numerical = c("=", "!=", "<", ">", "between"), 
                                                           categorical = c("equal", 'not equal', "remove NA")))),
                         column(width = 3,
                                #This opens a panel for numerical inputs for the user
                                conditionalPanel(condition="input.filter_sym=='=' || input.filter_sym=='>' || input.filter_sym=='<' || input.filter_sym=='!='" ,
                                                 numericInput(inputId="filter_num",
                                                              label="Numerical Input", 
                                                              value=1)),
                                #this opens a panel for categorical inputs for the user
                                conditionalPanel(condition="input.filter_sym == 'equal' || input.filter_sym == 'not equal'",
                                                 textInput(inputId="filter_text", 
                                                           label="Categorical Input", 
                                                           value = "")),
                                conditionalPanel(condition="input.filter_sym=='between'" ,
                                                 numericInput(inputId="filter_num_min",
                                                              label="Lower Limit", 
                                                              value=1))),
                         column(width = 3,
                                conditionalPanel(condition="input.filter_sym=='between'",
                                                 numericInput(inputId="filter_num_max",
                                                              label="Upper Limit",
                                                              value=2))
                                ) #end Column
                       ), #endFluidROW
                       #this div uses shinyjs to hide this filter row.  This row will be added depending on the value
                       #of FILTER_NUMBER_OPTIONS
                       div(id="filter_div_2",
                           fluidRow(
                             column(width=1,
                                    div(style = "display: inline-block;"), 
                                    materialSwitch(inputId="filter_on_2", 
                                                   label="Turn On",
                                                   value=FALSE, 
                                                   status='primary')), 
                             column(width = 3,
                                    selectInput(inputId="filter_var_2", 
                                                label = "Variable", 
                                                character())),
                             column(width = 2,
                                    pickerInput(inputId="filter_sym_2", 
                                                label = "Condition", 
                                                choices = list(numerical = c("=", "!=", "<", ">", "between"), 
                                                               categorical = c("equal", 'not equal', "remove NA")))),
                             column(width = 3, 
                                    #This opens a panel for numerical inputs for the user
                                    conditionalPanel(condition="input.filter_sym_2=='=' || input.filter_sym_2=='>' || input.filter_sym_2=='<' || input.filter_sym_2=='!='" ,
                                                     numericInput(inputId="filter_num_2",
                                                                  label="Numerical Input", 
                                                                  value=1)),
                                    #this opens a panel for categorical inputs for the user
                                    conditionalPanel(condition="input.filter_sym_2 == 'equal' || input.filter_sym_2 == 'not equal'",
                                                     textInput(inputId="filter_text_2", 
                                                               label="Categorical Input", 
                                                               value = "")),
                                    conditionalPanel(condition="input.filter_sym_2=='between'" ,
                                                     numericInput(inputId="filter_num_min_2",
                                                                  label="Lower Limit", 
                                                                  value=1))),
                             column(width = 3,
                                    conditionalPanel(condition="input.filter_sym_2=='between'",
                                                     numericInput(inputId="filter_num_max_2",
                                                                  label="Upper Limit",
                                                                  value=2))
                             ) #end Column
                           )#end fluidRow
                       ) %>%shinyjs::hidden(),   
                       
                       div(id="filter_div_3",
                           fluidRow(
                             column(width=1,
                                    div(style = "display: inline-block;"), 
                                    materialSwitch(inputId="filter_on_3", 
                                                   label="Turn On", 
                                                   value=FALSE, 
                                                   status='primary')), 
                             column(width = 3,
                                    selectInput(inputId="filter_var_3", 
                                                label = "Variable", 
                                                character())),
                             column(width = 2,
                                    pickerInput(inputId="filter_sym_3", 
                                                label = "Condition", 
                                                choices = list(numerical = c("=", "!=", "<", ">", "between"), 
                                                               categorical = c("equal", 'not equal', "remove NA")))),
                             column(width = 3, 
                                    #This opens a panel for numerical inputs for the user
                                    conditionalPanel(condition="input.filter_sym_3=='=' || input.filter_sym_3=='>' || input.filter_sym_3=='<' || input.filter_sym_3=='!='" ,
                                                     numericInput(inputId="filter_num_3",
                                                                  label="Numerical Input", 
                                                                  value=1)),
                                    #this opens a panel for categorical inputs for the user
                                    conditionalPanel(condition="input.filter_sym_3 == 'equal' || input.filter_sym_3 == 'not equal'",
                                                     textInput(inputId="filter_text_3", 
                                                               label="Categorical Input", 
                                                               value = "")),
                                    conditionalPanel(condition="input.filter_sym_3=='between'" ,
                                                     numericInput(inputId="filter_num_min_3",
                                                                  label="Lower Limit", 
                                                                  value=1))),
                             column(width = 3,
                                    conditionalPanel(condition="input.filter_sym_3=='between'",
                                                     numericInput(inputId="filter_num_max_3",
                                                                  label="Upper Limit",
                                                                  value=2))
                             ) #end Column
                           )#end fluidRow
                       ) %>%shinyjs::hidden(), #end Div
                       
                       div(id="filter_div_4",
                           fluidRow(
                             column(width=1,
                                    div(style = "display: inline-block;"), 
                                    materialSwitch(inputId="filter_on_4", 
                                                   label="Turn On", 
                                                   value=FALSE, 
                                                   status='primary')), 
                             column(width = 3,
                                    selectInput(inputId="filter_var_4", 
                                                label = "Variable", 
                                                character())),
                             column(width = 2,
                                    pickerInput(inputId="filter_sym_4", 
                                                label = "Condition", 
                                                choices = list(numerical = c("=", "!=", "<", ">", "between"),
                                                               categorical = c("equal", 'not equal', "remove NA")))),
                             column(width = 3, 
                                    #This opens a panel for numerical inputs for the user
                                    conditionalPanel(condition="input.filter_sym_4=='=' || input.filter_sym_4=='>' || input.filter_sym_4=='<' || input.filter_sym_4=='!='" ,
                                                     numericInput(inputId="filter_num_4",
                                                                  label="Numerical Input", 
                                                                  value=1)),
                                    #this opens a panel for categorical inputs for the user
                                    conditionalPanel(condition="input.filter_sym_4 == 'equal' || input.filter_sym_4 == 'not equal'",
                                                     textInput(inputId="filter_text_4", 
                                                               label="Categorical Input", 
                                                               value = "")),
                                    conditionalPanel(condition="input.filter_sym_4=='between'" ,
                                                     numericInput(inputId="filter_num_min_4",
                                                                  label="Lower Limit", 
                                                                  value=1))),
                             column(width = 3,
                                    conditionalPanel(condition="input.filter_sym_4=='between'",
                                                     numericInput(inputId="filter_num_max_4",
                                                                  label="Upper Limit",
                                                                  value=2))
                             ) #end Column
                           )#end fluidRow
                       ) %>%shinyjs::hidden() #end Div
              ),#end tabPanel

#__________________________________________________________________________________________________________


#TABPANEL: Mutate DATA


#__________________________________________________________________________________________________________

tabPanel("Mutate Data",
         #uiOutput("checkbox")),
         fluidRow(
           column(width=6,
                  #buttons to switch case for mutate options - transformation,
                  radioGroupButtons(inputId="mutate_radio_options",
                                    label="Choices", 
                                    choices=c("Transformations" = "Transformations",
                                              "Conditional Mutation" = "Conditional_mutate", 
                                              "Multiple Conditions" = "Multiple_conditions"),
                                    status="primary"))),
                  #conditionalPanel for Transformation selection
                  conditionalPanel(condition="input.mutate_radio_options =='Transformations'",
                                   fluidRow(
                                     column(width=3,
                                            #pickerOptions for transformation options
                                            pickerInput(inputId="mutate_transformation_options", 
                                                        label="Select Transformation", 
                                                        choices=c("log(x)" = "log",
                                                                  "log(x+1)" = "logp1",
                                                                  "sqrt(x)" = "sqrt",
                                                                  "ax+b" = "axpb"))
                                            ), #end column
                                    
                                     #conditional panel for log options
                                     conditionalPanel(condition="input.mutate_transformation_options == 'log'",
                                                      
                                                        column(width=3,
                                                             selectInput(inputId="mutate_trans_log",
                                                                         label="Select Column for log(x)",
                                                                         choices = character())),
                                                        column(width=3,
                                                             textInput(inputId="mutate_trans_log_name",
                                                                       label="New Variable Name",
                                                                       value="",
                                                                       placeholder="Type variable name here"))),
                                     #conditional panel for log(x+1)
                                     conditionalPanel(condition="input.mutate_transformation_options == 'logp1'",
                                                      
                                                      column(width=3,
                                                             selectInput(inputId="mutate_trans_logp1",
                                                                         label="Select Column for log(x+1)",
                                                                         choices = character())),
                                                      column(width=3,
                                                             textInput(inputId="mutate_trans_logp1_name",
                                                                       label="New Variable Name",
                                                                       value="",
                                                                       placeholder="Type variable name here"))),
                                     #conditional panel for sqrt(x)
                                     conditionalPanel(condition="input.mutate_transformation_options == 'sqrt'",
                                                      
                                                      column(width=3,
                                                             selectInput(inputId="mutate_trans_sqrt",
                                                                         label="Select Column for sqrt(x)",
                                                                         choices = character())),
                                                      column(width=3,
                                                             textInput(inputId="mutate_trans_sqrt_name",
                                                                       label="New Variable Name",
                                                                       value="",
                                                                       placeholder="Type variable name here"))),
                                     #conditional panel for ax + b
                                     conditionalPanel(condition="input.mutate_transformation_options == 'axpb'",
                                                      
                                                      column(width=3,
                                                             selectInput(inputId="mutate_trans_axpb",
                                                                         label="Select Column for x",
                                                                         choices = character())),
                                                      column(width=3,
                                                             textInput(inputId="mutate_trans_axpb_name",
                                                                       label="New Variable Name",
                                                                       value="",
                                                                       placeholder="Type variable name here")))
                                     
                  
                  
                  
                                  ),#end fluidRow
                                  #this conditional panel displays a and b options for ax+b
                                  #The other panel is based on a fluid row.  This uses additional rows so had to be created outside the initial fluidrow
                                  conditionalPanel(condition="input.mutate_transformation_options == 'axpb'",
                                                   fluidRow(column(width=3,
                                                                   offset = 3,
                                                                   textInput(inputId="mutate_trans_axpb_a",
                                                                             label = "a",
                                                                             value=""))
                                                   ),#end fluidRow
                                                   fluidRow(column(width=3,
                                                                   offset = 3,
                                                                   textInput(inputId="mutate_trans_axpb_b",
                                                                             label = "b",
                                                                             value=""))
                                                   )#end fluidRow
                                  ),#end conditional Panel
                                  conditionalPanel(condition="input.mutate_transformation_options=='logp1'",
                                                   fluidRow(column(width=3,
                                                                   checkboxInput(inputId="logp1_custom_input",
                                                                                 label="Input Custom Value",
                                                                                 value = FALSE)),
                                                            conditionalPanel(condition="input.logp1_custom_input",
                                                                           column(width=3,
                                                                                  textInput(inputId="logp1_custom_input_value",
                                                                                            label = "Custom Value",
                                                                                            value= "")))
                                                            ) #end FluidRow
                                                   )#end conditionalPanel logp1
                                                   
                            ),#end conditionalPanel for transformation selection
         conditionalPanel(condition="input.mutate_radio_options =='Conditional_mutate'",
                          fluidRow(column(width=3,
                                          prettyCheckbox(inputId="mutate_conditMut_isChar",
                                                         label="Condition is non numeric",
                                                         value = FALSE))),
                          
                                           fluidRow(
                                             column(width = 3,
                                                    selectInput(inputId="conditional_mutate_var", 
                                                                label = "Variable", 
                                                                choices=character())),
                                             column(width = 2,
                                                    pickerInput(inputId="conditional_mutate_condition", 
                                                                label = "Condition", 
                                                                choices =  c("=" = "==",
                                                                             "!=" = "!=", 
                                                                             "<" = "<", 
                                                                             ">" = ">"))),
                                             column(width = 3,
                                                    textInput(inputId="conditional_mutate_condition_text", 
                                                              label="Categorical Input", 
                                                              value = "")),
                                             column(width=3,
                                                    textInput(inputId="conditional_mutate_name",
                                                              label="New Variable Name",
                                                              value="",
                                                              placeholder="Type variable name here"))
                                ),#end fluidRow
                          fluidRow(
                                  column(width=3,
                                         offset=8,
                                         textInput(inputId="conditional_mutate_value_condit_true",
                                                   label = "Value if Conditional Statement is True",
                                                   value = ""),
                                  ) #end Column
                          ), #end Fluidrow
                          fluidRow(
                                  column(width=3,
                                         offset=8,
                                         textInput(inputId="conditional_mutate_value_condit_false",
                                                   label="Value if Conditional Statement is False",
                                                   value=""))
                          )#end fluidRow
                      ),#end conditional Panel Conditional Mutate
         conditionalPanel(condition="input.mutate_radio_options =='Multiple_conditions'",
                          fluidRow(column(width=4,
                                          selectInput(inputId="mutate_multicondit_select_column",
                                                      label="Select Column to Perform Mutate On",
                                                      choices=character())),
                                   column(width=4,
                                          div(style="display: inline-block;vertical-align:top;padding-top:20px",
                                                  actionButton(inputId = "push_options_for_multiConditions", 
                                                               label = 'Begin',
                                                               class="btn-primary",
                                                               style="color:#FFF"), alight="left"))
                                   ), #end FluidRow
                          
                          hr(),
                          fluidRow(column(width=4,
                                          uiOutput("multiple_outputs_select_colunms")),
                                          
                                   column(width=4,
                                          uiOutput("mulitple_outputs_categorical"))
                                  )#end fluidRow
         ),#end conditionalPanel for multiple conditions
         
         fluidRow(column(width=12,
                         actionButton(inputId = "mutate_push", 
                                    label = 'Mutate',
                                    class="btn-primary",
                                    style="color:#FFF")),
                  align="right")
                  #) #end column
                #) #end fluidRow
         ),#end tabPanel

#__________________________________________________________________________________________________________


#TABPANEL: EDIT DATA


#__________________________________________________________________________________________________________

            tabPanel("Edit",
                     radioGroupButtons(inputId="edit_radio_options",
                                       label="Choices", 
                                       choices=c("Value Change" = "val_change",
                                                 "numeric/char Conversion" = "num_2_char"),
                                       status="primary"),
                     conditionalPanel(condition="input.edit_radio_options=='val_change'",
                                      fluidRow(column(width=3,
                                                      selectInput(inputId="data_edit_select_column",
                                                                  label="Select Column to change data in",
                                                                  choices = character())),
                                               column(width=3,
                                                      selectInput(inputId="data_edit_select_value",
                                                                  label = "select value to change",
                                                                  choices=character())),
                                               column(width=3,
                                                      textInput(inputId="data_edit_new_value",
                                                                label="Change to:",
                                                                value=""))
                                      ),#end fluidRow
                                      fluidRow(column(width=12,
                                                      actionButton(inputId = "edit_push", 
                                                                   label = 'Edit',
                                                                   class="btn-primary",
                                                                   style="color:#FFF")),
                                               align="right")
                                      ),#end conditionalPanel
                     conditionalPanel(condition="input.edit_radio_options=='num_2_char'",
                                      radioButtons(inputId="edit_numChar_conversion_choice",
                                                   label= "Select Column to Convert from",
                                                   choices=c("Numeric to Character" = "num2char",
                                                             "Character to Numeric" = "char2num")),
                                      fluidRow(column(width=3,
                                                      selectInput(inputId="edit_data_column_to_convert_numChar",
                                                                  label="Select Column to Convert",
                                                                  choices=character())),
                                               column(width=3,
                                                      "Type of Current Var:",
                                                      verbatimTextOutput("edit_typeofVar"))
                                               ),#end fluidRow
                                      
                                      fluidRow(column(width=12,
                                                      actionButton(inputId = "numChar_convert_push", 
                                                                   label = 'Convert',
                                                                   class="btn-primary",
                                                                   style="color:#FFF")),
                                               align="right")
                                      )#end conditionalPanel edit radio options
                    
                     ),#end TabPanel Edit
#__________________________________________________________________________________________________________

#TABPANEL: SORT DATA

#__________________________________________________________________________________________________________
              
              tabPanel("Sort",
                       
                       fluidRow(
                         column(width = 3,
                                radioGroupButtons(
                                  inputId = "ARRANGE_NUMBER_OPTIONS",
                                  label = "Number of Variables to Sort By",
                                  choices = c("One", 
                                              "Two"),
                                  justified = TRUE,
                                  checkIcon = list(
                                    yes = icon("ok", 
                                               lib = "glyphicon"))
                                )
                         )#endCOlumn
                       ), #endFluidRow
                       
                       fluidRow(
                         column(width=1,
                                div(style = "display: inline-block;"), 
                                materialSwitch(inputId="arrange_on_1", 
                                            label='Turn On',
                                            value=FALSE, 
                                            status='primary')),
                         column(width=4,
                                selectInput(inputId="arrange1", 
                                            label="Variable", 
                                            choices = character())),
                          
                         column(width=2,
                                div(style = "display: inline-block;"),
                                checkboxInput(inputId="desc1", 
                                              label="Descending Order"))
                       ), #end Fluid Row
                       
                       div(id="sort_div",
                           fluidRow(
                             column(width=1,
                                    div(style = "display: inline-block;"),
                                    materialSwitch(inputId="arrange_on_2", 
                                                   label='Turn On',
                                                   value=FALSE, 
                                                   status='primary')),
                             column(width=4,
                                    selectInput(inputId="arrange2", 
                                                label="Variable", 
                                                character())),
                             column(width=2,
                                    div(style = "display: inline-block;"),
                                    checkboxInput(inputId="desc2", 
                                                  label="Descending Order"))
                           )#end Fluid Row
                       ) %>%shinyjs::hidden()
                       
              ),#end tabPanel
#__________________________________________________________________________________________________________

#TABPANEL: TABLE SIZE

#__________________________________________________________________________________________________________
              
              #this tab creates the select checkboxs depending on the name of columns in dataframe
              #UI is changed in server function bases on names(df)
              tabPanel("Table Size",
                       #sliders to play with the width and height of rHandsontable since it doesn't take up 
                       #the whole screen.  Lets the user adjust it to their liking
                       fluidRow(
                         column(width=6,
                                sliderInput('rht_width_input', 
                                   label = "Width",
                                   min = 1, 
                                   max = 2160, 
                                   value = 1250),
                       sliderInput('rht_height_input', 
                                   label = "Height",
                                   min = 1, 
                                   max = 1000,
                                   value = 450)
                         )#end Column
                       ) #end FluidRow
                       ),#end TabPanel
#__________________________________________________________________________________________________________

#TABPANEL: DOWNLOAD DATA

#__________________________________________________________________________________________________________
              
              tabPanel("Download Data",
                       textInput(inputId ="dataview_download_title",
                                 label=NULL,
                                 value = "",
                                 placeholder = "Type Download TItle",
                                 width = NULL
                       ),
                       # radioGroupButtons(
                       #   inputId = "dataview_download_radiobuttons",
                       #   label = NULL,
                       #   choices = c(".jpg", 
                       #               ".png", ".pdf"),
                       #   individual = TRUE,
                       #   checkIcon = list(
                       #     yes = tags$i(class = "fa fa-circle", 
                       #                  style = "color: steelblue"),
                       #     no = tags$i(class = "fa fa-circle-o", 
                       #                 style = "color: steelblue"))
                       # ),
                       downloadBttn(outputId="downloadData",
                                    label="Download",
                                    style="unite",
                                    color="primary",
                                    size="sm",
                                    block=FALSE,
                                    no_outline=FALSE)
                       
              )#end tabPanel
              
            )#end tabBox
          )#end box
        ),#end fluidRow
#__________________________________________________________________________________________________________

#Table Output using Conditional Panels

#__________________________________________________________________________________________________________
        
        radioGroupButtons(
                inputId = "dataView_type_of_view",
                label = "Select Data View",
                choices = c("DataView" = "DataView", 
                            "Data Structure" = "data_struc", 
                            "Data Summary" = "data_sum"),
                justified = TRUE,
                checkIcon = list(
                        yes = icon("ok", 
                                   lib = "glyphicon"))),
        conditionalPanel(condition="input.dataView_type_of_view=='DataView'",
                         conditionalPanel(condition="input.dataUI_TABLE_OPTIONS=='rhandsomeTable'",
                                          rHandsontableOutput("output_rhandsometable")),
                         conditionalPanel(condition="input.dataUI_TABLE_OPTIONS=='R Default Table'",
                                          div(style = 'overflow-x: scroll', tableOutput("output_rtable"))),
                         conditionalPanel(condition="input.dataUI_TABLE_OPTIONS=='Data Table from DT Package'",
                                          DT::dataTableOutput("output_DT_table"))),
        conditionalPanel(condition="input.dataView_type_of_view=='data_struc'",
                                          verbatimTextOutput("output_dataTable_structure")),
        conditionalPanel(condition="input.dataView_type_of_view=='data_sum'",
                                          "Numerical",
                                          DT::dataTableOutput("output_data_summary"),
                                          "Categorical",
                                          DT::dataTableOutput("output_dataTable_text"))

                         
                 

                 
)#end tabitem




