#****************************FUNCTIONS***********************************

#This script holds the functions for data mangement tab

#Includes: evaluate_conditional_statements



################# FUNCTION: evaluate_conditional_statements ####################
# Takes tables in database and makes tbls of them so dplyr can be used

# Inputs:
# variable - an array holding all values from a column of data
# condition - condition of evaluation expression (i.e >, =, <)
# check_var - variable that all var in 'variables' are compared against

# Outputs:
# vector of TRUE/FALSE depending on if evaluated expression is true
################################################################################

evaluate_conditional_statements <- function(variable, condition, check_var){
  #paste the conditional statement together
  conditional_statement = paste0(variable, condition, check_var)
  
  #evaluate the conditional statement to return a TRUE or False
  boo <- rep(NA, length(variable)) #initalize vector of NA to store T/F in
  count = 0
  for(i in conditional_statement){ #for each conditional statement
    count = count + 1
    if(eval(parse(text=i))){ #if the evaulated expression is true
      boo[count] <- TRUE #store TRUE to vector
    }else{boo[count] = FALSE} #else store false
  }
  return(boo)
}
  
################# FUNCTION: evaluate_conditional_statements ####################
# Takes tables in database and makes tbls of them so dplyr can be used

# Inputs:
# TF_array - an array holding an array of boolean expressions
# var_ifTrue - vars to put in array if boolean is true
# var_ifFalse - vars to put in array if boolean is false

# Outputs:
# vector of var_ifTrue and var_ifFalse depending on TF_array
################################################################################

create_conditional_column <- function(TF_array, var_ifTrue, var_ifFalse){
  
  output_column <- rep(NA, length(TF_array)) #create array to store condition in
  index = 0
  for(bool in TF_array){#for boolean value in TF_array, store variable in array
    index = index + 1
    if(bool==TRUE){
      output_column[index] = var_ifTrue
    }else(output_column[index] = var_ifFalse)
  }
  return(output_column)
}







