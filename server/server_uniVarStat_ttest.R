#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

#SUBTAB: Ttest
#TTEST000

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________


#**************************************************
#One Sample Ttest

#**************************************************
#updates ttest input variable choices in single option based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "var1",
                    choices = input$checkbox)
})

output$t_test_results <- renderUI({
  test_confint <- t.test(x = df_to_use()[[input$var1]],
                         mu = input$ttest_one_nullHypothesis,
                         alternative = "two.sided",
                         conf.level = 1 - input$ttest_alpha_value)
  
  test <- t.test(x = df_to_use()[[input$var1]],
                 mu = input$ttest_one_nullHypothesis,
                 alternative = input$ttest_alternative_hypothesis,
                 conf.level = 1 - input$ttest_alpha_value)
  withMathJax(
    # paste(c("Your data:", paste(df_to_use()[[input$var1]], collapse = ", ")), collapse = " "),
    # br(),
    tags$b("Basic Data:"),
    br(),
    paste0("\\(n =\\) ", length(df_to_use()[[input$var1]])),
    br(),
    paste0("\\(\\bar{x} =\\) ", round(mean(df_to_use()[[input$var1]]), 3)),
    br(),
    paste0("\\(s =\\) ", round(sd(df_to_use()[[input$var1]]), 3)),
    br(),
    br(),
    tags$b("Confidence interval"),
    br(),
    paste0(
      (1 - input$ttest_alpha_value) * 100, "% CI for \\(\\mu = \\bar{x} \\pm t_{\\alpha/2, n - 1} \\dfrac{s}{\\sqrt{n}} = \\) ",
      "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
    ),
    br(),
    br(),
    tags$b("Hypothesis test"),
    br(),
    paste0("1. \\(H_0 : \\mu = \\) ", test$null.value, " and \\(H_1 : \\mu \\) ", ifelse(input$ttest_alternative_hypothesis == "two.sided", "\\( \\neq \\) ", ifelse(input$ttest_alternative_hypothesis == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
    br(),
    paste0(
      "2. Test statistic : \\(t_{obs} = \\dfrac{\\bar{x} - \\mu_0}{s / \\sqrt{n}} = \\) ",
      "(", round(test$estimate, 3), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", round(sd(df_to_use()[[input$var1]])/sqrt(length(df_to_use()[[input$var1]])), 3), " \\( = \\) ",
      round(test$statistic, 3)
    ),
    br(),
    paste0(
      "3. Critical value :", ifelse(input$ttest_alternative_hypothesis == "two.sided", " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", ifelse(input$ttest_alternative_hypothesis == "greater", " \\( t_{\\alpha, n - 1} = t(\\)", " \\( -t_{\\alpha, n - 1} = -t(\\)")),
      ifelse(input$ttest_alternative_hypothesis == "two.sided", input$ttest_alpha_value / 2, input$ttest_alpha_value), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
      ifelse(input$ttest_alternative_hypothesis == "two.sided", "\\( \\pm \\)", ifelse(input$ttest_alternative_hypothesis == "greater", "", " -")),
      ifelse(input$ttest_alternative_hypothesis == "two.sided", round(qt(input$ttest_alpha_value / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$ttest_alpha_value, df = test$parameter, lower.tail = FALSE), 3))
    ),
    br(),
    paste0("4. Conclusion : ", ifelse(test$p.value < input$ttest_alpha_value, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
    br(),
    br(),
    tags$b("Interpretation"),
    br(),
    paste0("At the ", input$ttest_alpha_value * 100, "% significance level, ", ifelse(test$p.value < input$ttest_alpha_value, "we reject the null hypothesis that the true mean is ", "we do not reject the null hypothesis that the true mean is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
  )
})

output$ttestSinglePlot <- renderPlot({
  
  test <- t.test(x = df_to_use()[[input$var1]],
                 mu = input$ttest_one_nullHypothesis,
                 alternative = input$ttest_alternative_hypothesis,
                 conf.level = 1 - input$ttest_alpha_value)
  
  if (input$ttest_alternative_hypothesis == "two.sided") {
    funcShaded <- function(x) {
      y <- dt(x, df = test$parameter)
      y[x < qt(input$ttest_alpha_value / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$ttest_alpha_value / 2, df = test$parameter) ] <- NA
      return(y)
    }
  } else if (input$ttest_alternative_hypothesis == "greater") {
    funcShaded <- function(x) {
      y <- dt(x, df = test$parameter)
      y[x < qt(input$ttest_alpha_value, df = test$parameter, lower.tail = FALSE) ] <- NA
      return(y)
    }
  } else if (input$ttest_alternative_hypothesis == "less") {
    funcShaded <- function(x) {
      y <- dt(x, df = test$parameter)
      y[x > qt(input$ttest_alpha_value, df = test$parameter, lower.tail = TRUE) ] <- NA
      return(y)
    }
  }
  p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
    stat_function(fun = dt, args = list(df = test$parameter)) +
    stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
    theme_minimal() +
    geom_vline(xintercept = test$statistic, color = "steelblue") +
    geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x")
  p
  
})

#this is used to print out the results of the ttest to the user
output$ttest_table_out = renderTable({
  test <- t.test(x = df_to_use()[[input$var1]],
                 mu = input$ttest_one_nullHypothesis,
                 alternative = input$ttest_alternative_hypothesis,
                 conf.level = 1 - input$ttest_alpha_value)
  tab = matrix(c(test$method,
                 round(test$parameter, 2),
                 round(test$statistic, 2),
                 round(test$p.value, 2),
                 round(test$conf.int[1], 2),
                 round(test$conf.int[2], 2),
                 test$null.value,
                 test$alternative,
                 input$ttest_alpha_value,
                 #this prints out reject or not if the pvlaue is greater than the alpha value
                 ifelse(test$p.value < input$ttest_alpha_value, "Reject", "Not Reject")), nrow=1)
  colnames(tab) = c("method","DF","t-statistic","p-value", "c1", "c2", "H_o", "Type", "Sig. Level", "Result")
  rownames(tab) = "Values"
  tab
})


#**************************************************
#Two Sample Ttest

#**************************************************
#changes input 1 for two sample ttest numerical input
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "two_samp_ttest_var1",
                    choices = input$checkbox)
})

#changes input 2 for two sample ttest numerical input
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "two_samp_ttest_var2",
                    choices = input$checkbox)
})

#changes numerical input for two sample grouped input for ttest
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "two_samp_ttest_numerical_group",
                    choices = input$checkbox)
})

#changes categrocial input for two sample ttest if group input selected
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "two_samp_ttest_categorical_group",
                    choices = input$checkbox)
})

output$two_samp_ttest_results <- renderUI({
  #perform ttests
  if(input$ttest_data_input_options == "group_col"){
    
    #get factors of grouping colum
    my_groups <- unique(df_to_use()[[input$two_samp_ttest_categorical_group]])
    
    #initalize vectors for separation
    x = vector()
    y = vector()
    
    #set indices to count for in separation.
    idx_x = 1;
    idx_y = 1;
    
    #loop to add values from each of the two groups to their respective vectors
    for(i in seq(1:nrow(df_to_use()))){
      if(my_groups[1] == df_to_use()[[input$two_samp_ttest_categorical_group]][i]){
        #adds value to x vector at idx
        x[idx_x] <- df_to_use()[[input$two_samp_ttest_numerical_group]][i]
        idx_x = idx_x + 1
      }
      else if(my_groups[2] == df_to_use()[[input$two_samp_ttest_categorical_group]][i]){
        #adds values to y vector at idx
        y[idx_y] <- df_to_use()[[input$two_samp_ttest_numerical_group]][i]
        idx_y = idx_y + 1
      }
    }
    print(x)
    print(y)
    
  }
  #assign x,y by columns selected if num_col selected
  else if(input$ttest_data_input_options == "num_col"){
    x = df_to_use()[[input$two_samp_ttest_var1]]
    y = df_to_use()[[input$two_samp_ttest_var2]]
  }
  
  if(input$two_samp_ttest_varequal=='y'){
    
    
    test_confint <- t.test(x,y,
                           mu = input$two_samp_ttest_nullHypothesis,
                           alternative = "two.sided",
                           conf.level = 1 - input$two_samp_ttest_alpha_value,
                           paired = FALSE,
                           var.equal = TRUE)
    test <- t.test(x,y,
                   mu = input$two_samp_ttest_nullHypothesis,
                   alternative = input$two_samp_ttest_alternative_hypothesis,
                   conf.level = 1 - input$two_samp_ttest_alpha_value,
                   paired = FALSE,
                   var.equal = TRUE)
    
    s_p <- sqrt(((length(x) - 1) * var(x) + (length(y) - 1) * var(y)) / test_confint$parameter)
    
    
    withMathJax(
      tags$b("Basic Data:"),
      br(),
      paste0("\\(n_1 =\\) ", length(x)),
      br(),
      paste0("\\(n_2 =\\) ", length(y)),
      br(),
      paste0("\\(\\bar{x}_1 =\\) ", round(mean(x), 3)),
      br(),
      paste0("\\(\\bar{x}_2 =\\) ", round(mean(y), 3)),
      br(),
      paste0("\\(s^2_1 =\\) ", round(var(x), 3)),
      br(),
      paste0("\\(s^2_2 =\\) ", round(var(y), 3)),
      br(),
      br(),
      tags$b("Confidence interval"),
      br(),
      paste0((1 - input$two_samp_ttest_alpha_value) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, n_1 + n_2 - 2} (s_p) \\sqrt{\\dfrac{1}{n_1} + \\dfrac{1}{n_2}} \\)"),
      br(),
      paste0("where ", "\\( s_p = \\sqrt{\\dfrac{(n_1 - 1)s^2_1 + (n_2 - 1)s^2_2}{n_1 + n_2 - 2}} = \\) ", round(s_p, 3)),
      br(),
      br(),
      paste0(
        "\\( \\Rightarrow \\)", (1 - input$two_samp_ttest_alpha_value) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\) ", round(test_confint$estimate[1], 3), ifelse(test_confint$estimate[2] >= 0, paste0(" - ", round(test_confint$estimate[2], 3)), paste0(" + ", round(abs(test_confint$estimate[2]), 3))), " \\( \\pm \\) ", "\\( (\\)", round(qt(input$two_samp_ttest_alpha_value / 2, df = test_confint$parameter, lower.tail = FALSE), 3), " * ", round(s_p, 3), " * ", round(sqrt(1 / length(x) + 1 / length(y)), 3), "\\( ) \\) ", "\\( = \\) ",
        "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
      ),
      br(),
      br(),
      tags$b("Hypothesis test"),
      br(),
      paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", test$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$two_samp_ttest_alternative_hypothesis == "two.sided", "\\( \\neq \\) ", ifelse(input$two_samp_ttest_alternative_hypothesis == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
      br(),
      paste0(
        "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{s_p \\sqrt{\\dfrac{1}{n_1} + \\dfrac{1}{n_2}}} = \\) ",
        "(", round(test$estimate[1], 3), ifelse(test$estimate[2] >= 0, paste0(" - ", round(test$estimate[2], 3)), paste0(" + ", round(abs(test$estimate[2]), 3))), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / (", round(s_p, 3), " * ", round(sqrt((1 / length(x)) + (1 / length(y))), 3), ") \\( = \\) ",
        round(test$statistic, 3)
      ),
      br(),
      paste0(
        "3. Critical value :", ifelse(input$two_samp_ttest_alternative_hypothesis == "two.sided", " \\( \\pm t_{\\alpha/2, n_1 + n_2 - 2} = \\pm t(\\)", ifelse(input$two_samp_ttest_alternative_hypothesis == "greater", " \\( t_{\\alpha, n_1 + n_2 - 2} = t(\\)", " \\( -t_{\\alpha, n_1 + n_2 - 2} = -t(\\)")),
        ifelse(input$two_samp_ttest_alternative_hypothesis == "two.sided", input$two_samp_ttest_alpha_value / 2, input$two_samp_ttest_alpha_value), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
        ifelse(input$two_samp_ttest_alternative_hypothesis == "two.sided", "\\( \\pm \\)", ifelse(input$two_samp_ttest_alternative_hypothesis == "greater", "", " -")),
        ifelse(input$two_samp_ttest_alternative_hypothesis == "two.sided", round(qt(input$two_samp_ttest_alpha_value / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$two_samp_ttest_alpha_value, df = test$parameter, lower.tail = FALSE), 3))
      ),
      br(),
      paste0("4. Conclusion : ", ifelse(test$p.value < input$two_samp_ttest_alpha_value, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
      br(),
      br(),
      tags$b("Interpretation"),
      br(),
      paste0("At the ", input$two_samp_ttest_alpha_value * 100, "% significance level, ", ifelse(test$p.value < input$two_samp_ttest_alpha_value, "we reject the null hypothesis that the true difference in means is ", "we do not reject the null hypothesis that the true difference in means is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
    )
  }
  #output if var not equal radio button selected
  else if(input$two_samp_ttest_varequal=='n'){
    
    dat1 = x
    dat2 = y
    test_confint <- t.test(x = dat1,
                           y = dat2,
                           mu = input$two_samp_ttest_nullHypothesis,
                           alternative = "two.sided",
                           conf.level = 1 - input$two_samp_ttest_alpha_value,
                           paired = FALSE,
                           var.equal = FALSE)
    test <- t.test(x = dat1,
                   y = dat2,
                   mu = input$two_samp_ttest_nullHypothesis,
                   alternative = input$two_samp_ttest_alternative_hypothesis,
                   conf.level = 1 - input$two_samp_ttest_alpha_value,
                   paired = FALSE,
                   var.equal = FALSE)
    
    withMathJax(
      tags$b("Basic Data:"),
      br(),
      paste0("\\(n_1 =\\) ", length(dat1)),
      br(),
      paste0("\\(n_2 =\\) ", length(dat2)),
      br(),
      paste0("\\(\\bar{x}_1 =\\) ", round(mean(dat1), 3)),
      br(),
      paste0("\\(\\bar{x}_2 =\\) ", round(mean(dat2), 3)),
      br(),
      paste0("\\(s^2_1 =\\) ", round(var(dat1), 3)),
      br(),
      paste0("\\(s^2_2 =\\) ", round(var(dat2), 3)),
      br(),
      br(),
      tags$b("Confidence interval"),
      br(),
      paste0((1 - input$two_samp_ttest_alpha_value) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, \\nu} \\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}} \\)"),
      br(),
      paste0("where ", "\\( \\nu = \\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}\\Bigg)^2}{\\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1}\\Bigg)^2}{n_1-1} + \\dfrac{\\Bigg(\\dfrac{s^2_2}{n_2}\\Bigg)^2}{n_2-1}} = \\) ", round(test$parameter, 3)),
      br(),
      br(),
      paste0(
        "\\( \\Rightarrow \\)", (1 - input$two_samp_ttest_alpha_value) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\) ",
        "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
      ),
      br(),
      br(),
      tags$b("Hypothesis test"),
      br(),
      paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", test$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", ifelse(input$two_samp_ttest_alpha_value == "two.sided", "\\( \\neq \\) ", ifelse(input$two_samp_ttest_alpha_value == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
      br(),
      paste0(
        "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}}} = \\) ",
        round(test$statistic, 3)
      ),
      br(),
      paste0(
        "3. Critical value :", ifelse(input$two_samp_ttest_alpha_value == "two.sided", " \\( \\pm t_{\\alpha/2, \\nu} = \\pm t(\\)", ifelse(input$two_samp_ttest_alpha_value == "greater", " \\( t_{\\alpha, \\nu} = t(\\)", " \\( -t_{\\alpha, \\nu} = -t(\\)")),
        ifelse(input$two_samp_ttest_alpha_value == "two.sided", input$two_samp_ttest_alpha_value / 2, input$two_samp_ttest_alpha_value), ", ", round(test$parameter, 3), "\\()\\)", " \\( = \\) ",
        ifelse(input$two_samp_ttest_alpha_value == "two.sided", "\\( \\pm \\)", ifelse(input$two_samp_ttest_alpha_value == "greater", "", " -")),
        ifelse(input$two_samp_ttest_alpha_value == "two.sided", round(qt(input$two_samp_ttest_alpha_value / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$two_samp_ttest_alpha_value, df = test$parameter, lower.tail = FALSE), 3))
      ),
      br(),
      paste0("4. Conclusion : ", ifelse(test$p.value < input$two_samp_ttest_alpha_value, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
      br(),
      br(),
      tags$b("Interpretation"),
      br(),
      paste0("At the ", input$two_samp_ttest_alpha_value * 100, "% significance level, ", ifelse(test$p.value < input$two_samp_ttest_alpha_value, "we reject the null hypothesis that the true difference in means is ", "we do not reject the null hypothesis that the true difference in means is "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
    )
  }
})

output$twoSamp_ttest_plot <- renderPlot({
  
  if(input$ttest_data_input_options == "group_col"){
    
    #get factors of grouping colum
    my_groups <- unique(df_to_use()[[input$two_samp_ttest_categorical_group]])
    
    #initalize vectors for separation
    x = vector()
    y = vector()
    
    #set indices to count for in separation.
    idx_x = 1;
    idx_y = 1;
    
    #loop to add values from each of the two groups to their respective vectors
    for(i in seq(1:nrow(df_to_use()))){
      if(my_groups[1] == df_to_use()[[input$two_samp_ttest_categorical_group]][i]){
        #adds value to x vector at idx
        x[idx_x] <- df_to_use()[[input$two_samp_ttest_numerical_group]][i]
        idx_x = idx_x + 1
      }
      else if(my_groups[2] == df_to_use()[[input$two_samp_ttest_categorical_group]][i]){
        #adds values to y vector at idx
        y[idx_y] <- df_to_use()[[input$two_samp_ttest_numerical_group]][i]
        idx_y = idx_y + 1
      }
    }
    
    
    dat1=x
    dat2=y
    
  }
  #assign x,y by columns selected if num_col selected
  else if(input$ttest_data_input_options == "num_col"){
    dat1 = df_to_use()[[input$two_samp_ttest_var1]]
    dat2 = df_to_use()[[input$two_samp_ttest_var2]]
  }
  
  
  if(input$two_samp_ttest_varequal=='y'){
    test <- t.test(x = dat1,
                   y = dat2,
                   mu = input$two_samp_ttest_nullHypothesis,
                   alternative = input$two_samp_ttest_alternative_hypothesis,
                   conf.level = 1 - input$two_samp_ttest_alpha_value,
                   paired = FALSE,
                   var.equal = TRUE)
    
    if (input$two_samp_ttest_alternative_hypothesis == "two.sided") {
      funcShaded <- function(x) {
        y <- dt(x, df = test$parameter)
        y[x < qt(input$two_samp_ttest_alpha_value / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$two_samp_ttest_alpha_value / 2, df = test$parameter) ] <- NA
        return(y)
      }
    } else if (input$two_samp_ttest_alternative_hypothesis == "greater") {
      funcShaded <- function(x) {
        y <- dt(x, df = test$parameter)
        y[x < qt(input$two_samp_ttest_alpha_value, df = test$parameter, lower.tail = FALSE) ] <- NA
        return(y)
      }
    } else if (input$two_samp_ttest_alternative_hypothesis == "less") {
      funcShaded <- function(x) {
        y <- dt(x, df = test$parameter)
        y[x > qt(input$two_samp_ttest_alpha_value, df = test$parameter, lower.tail = TRUE) ] <- NA
        return(y)
      }
    }
    p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dt, args = list(df = test$parameter)) +
      stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
      theme_minimal() +
      geom_vline(xintercept = test$statistic, color = "steelblue") +
      geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
      ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  }
  else if(input$two_samp_ttest_varequal=='n'){
    test <- t.test(x = dat1,
                   y = dat2,
                   mu = input$two_samp_ttest_nullHypothesis,
                   alternative = input$two_samp_ttest_alternative_hypothesis,
                   conf.level = 1 - input$two_samp_ttest_alpha_value,
                   paired = FALSE,
                   var.equal = FALSE)
    
    if (input$two_samp_ttest_alternative_hypothesis == "two.sided") {
      funcShaded <- function(x) {
        y <- dt(x, df = test$parameter)
        y[x < qt(input$two_samp_ttest_alpha_value / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$two_samp_ttest_alpha_value / 2, df = test$parameter) ] <- NA
        return(y)
      }
    } else if (input$two_samp_ttest_alternative_hypothesis == "greater") {
      funcShaded <- function(x) {
        y <- dt(x, df = test$parameter)
        y[x < qt(input$two_samp_ttest_alpha_value, df = test$parameter, lower.tail = FALSE) ] <- NA
        return(y)
      }
    } else if (input$two_samp_ttest_alternative_hypothesis == "less") {
      funcShaded <- function(x) {
        y <- dt(x, df = test$parameter)
        y[x > qt(input$two_samp_ttest_alpha_value, df = test$parameter, lower.tail = TRUE) ] <- NA
        return(y)
      }
    }
    p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
      stat_function(fun = dt, args = list(df = test$parameter)) +
      stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
      theme_minimal() +
      geom_vline(xintercept = test$statistic, color = "steelblue") +
      geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
      ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x")
    p
  }
  
})
#this is used to print out the results of the ttest to the user
output$twoSamp_ttest_table_out = renderTable({
  
  if(input$ttest_data_input_options == "group_col"){
    
    #get factors of grouping colum
    my_groups <- unique(df_to_use()[[input$two_samp_ttest_categorical_group]])
    
    #initalize vectors for separation
    x = vector()
    y = vector()
    
    #set indices to count for in separation.
    idx_x = 1;
    idx_y = 1;
    
    #loop to add values from each of the two groups to their respective vectors
    for(i in seq(1:nrow(df_to_use()))){
      if(my_groups[1] == df_to_use()[[input$two_samp_ttest_categorical_group]][i]){
        #adds value to x vector at idx
        x[idx_x] <- df_to_use()[[input$two_samp_ttest_numerical_group]][i]
        idx_x = idx_x + 1
      }
      else if(my_groups[2] == df_to_use()[[input$two_samp_ttest_categorical_group]][i]){
        #adds values to y vector at idx
        y[idx_y] <- df_to_use()[[input$two_samp_ttest_numerical_group]][i]
        idx_y = idx_y + 1
      }
    }
    
  }
  #assign x,y by columns selected if num_col selected
  else if(input$ttest_data_input_options == "num_col"){
    x = df_to_use()[[input$two_samp_ttest_var1]]
    y = df_to_use()[[input$two_samp_ttest_var2]]
  }
  
  if(input$two_samp_ttest_varequal=='y'){
    test <- t.test(x, y,
                   mu = input$two_samp_ttest_nullHypothesis,
                   alternative = input$two_samp_ttest_alternative_hypothesis,
                   conf.level = 1 - input$two_samp_ttest_alpha_value,
                   paired = FALSE,
                   var.equal = TRUE)
    tab = matrix(c(test$method,
                   round(test$parameter, 2),
                   round(test$statistic, 2),
                   round(test$p.value, 2),
                   round(test$conf.int[1], 2),
                   round(test$conf.int[2], 2),
                   test$null.value,
                   test$alternative,
                   input$ttest_alpha_value,
                   #this prints out reject or not if the pvlaue is greater than the alpha value
                   ifelse(test$p.value < input$two_samp_ttest_alpha_value, "Reject", "Not Reject")), nrow=1)
    colnames(tab) = c("method","DF","t-statistic","p-value", "c1", "c2", "H_o", "Type", "Sig. Level", "Result")
    rownames(tab) = "Values"
    tab
  }
  else if(input$two_samp_ttest_varequal=='n'){
    test <- t.test(x,y,
                   mu = input$two_samp_ttest_nullHypothesis,
                   alternative = input$two_samp_ttest_alternative_hypothesis,
                   conf.level = 1 - input$two_samp_ttest_alpha_value,
                   paired = FALSE,
                   var.equal = FALSE)
    tab = matrix(c(test$method,
                   round(test$parameter, 2),
                   round(test$statistic, 2),
                   round(test$p.value, 2),
                   round(test$conf.int[1], 2),
                   round(test$conf.int[2], 2),
                   test$null.value,
                   test$alternative,
                   input$ttest_alpha_value,
                   #this prints out reject or not if the pvlaue is greater than the alpha value
                   ifelse(test$p.value < input$two_samp_ttest_alpha_value, "Reject", "Not Reject")), nrow=1)
    colnames(tab) = c("method","DF","t-statistic","p-value", "c1", "c2", "H_o", "Type", "Sig. Level", "Result")
    rownames(tab) = "Values"
    tab
  }
})

#**************************************************
#Paired Two Sample Ttest

#**************************************************
observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "paired_ttest_var1",
                    choices = input$checkbox)
})

observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "paired_ttest_var2",
                    choices = input$checkbox)
})

observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "paired_ttest_group_numerical",
                    choices = input$checkbox)
})

observeEvent(input$checkbox, {
  updateSelectInput(session,
                    "paired_ttest_group_categorical",
                    choices = input$checkbox)
})


output$paired_samp_test_results <- renderUI({
  #perform ttests
  if(input$ttest_data_input_options == "group_col"){
    
    #get factors of grouping colum
    my_groups <- unique(df_to_use()[[input$paired_ttest_group_categorical]])
    
    #initalize vectors for separation
    x = vector()
    y = vector()
    
    #set indices to count for in separation.
    idx_x = 1;
    idx_y = 1;
    
    #loop to add values from each of the two groups to their respective vectors
    for(i in seq(1:nrow(df_to_use()))){
      if(my_groups[1] == df_to_use()[[input$paired_ttest_group_categorical]][i]){
        #adds value to x vector at idx
        x[idx_x] <- df_to_use()[[input$paired_ttest_group_numerical]][i]
        idx_x = idx_x + 1
      }
      else if(my_groups[2] == df_to_use()[[input$paired_ttest_group_categorical]][i]){
        #adds values to y vector at idx
        y[idx_y] <- df_to_use()[[input$paired_ttest_group_numerical]][i]
        idx_y = idx_y + 1
      }
    }
    
  }
  #assign x,y by columns selected if num_col selected
  else if(input$ttest_data_input_options == "num_col"){
    x = df_to_use()[[input$paired_ttest_var1]]
    y = df_to_use()[[input$paired_ttest_var2]]
  }
  dat1 = x
  dat2 = y
  
  test_confint <- t.test(x = dat1,
                         y = dat2,
                         mu = input$paired_ttest_nullHypothesis,
                         alternative = "two.sided",
                         conf.level = 1 - input$paired_ttest_alpha_value,
                         paired = TRUE)
  
  test <- t.test(x = dat1,
                 y = dat2,
                 mu = input$paired_ttest_nullHypothesis,
                 alternative = input$paired_ttest_alternative_hypothesis,
                 conf.level = 1 - input$paired_ttest_alpha_value,
                 paired = TRUE)
  
  withMathJax(
    tags$b("Basic Data:"),
    br(),
    paste(c("Difference \\((D) = Sample_2 - Sample_1=\\)", paste(dat2 - dat1, collapse = ", ")), collapse = " "),
    br(),
    paste0("Number of pairs \\(n =\\) ", length(dat1)),
    br(),
    paste0("\\(\\bar{D} =\\) ", round(mean(dat2 - dat1), 3)),
    br(),
    paste0("\\(s^2_D =\\) ", round(var(dat2 - dat1), 3)),
    br(),
    paste0("\\(s_D =\\) ", round(sd(dat2 - dat1), 3)),
    br(),
    br(),
    tags$b("Confidence interval"),
    br(),
    paste0(
      (1 - input$paired_ttest_alpha_value) * 100, "% CI for \\(\\mu_D = \\bar{D} \\pm t_{\\alpha/2, n - 1} \\dfrac{s_D}{\\sqrt{n}} = \\) ",
      "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
    ),
    br(),
    br(),
    tags$b("Hypothesis test"),
    br(),
    paste0("1. \\(H_0 : \\mu_D = \\) ", test$null.value, " and \\(H_1 : \\mu_D \\) ", ifelse(input$paired_ttest_alternative_hypothesis == "two.sided", "\\( \\neq \\) ", ifelse(input$paired_ttest_alternative_hypothesis == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
    br(),
    paste0(
      "2. Test statistic : \\(t_{obs} = \\dfrac{\\bar{D} - \\mu_0}{s_D / \\sqrt{n}} = \\) ",
      round(test$statistic, 3)
    ),
    br(),
    paste0(
      "3. Critical value :", ifelse(input$paired_ttest_alternative_hypothesis == "two.sided", " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", ifelse(input$paired_ttest_alternative_hypothesis == "greater", " \\( t_{\\alpha, n - 1} = t(\\)", " \\( -t_{\\alpha, n - 1} = -t(\\)")),
      ifelse(input$paired_ttest_alternative_hypothesis == "two.sided", input$paired_ttest_alpha_value / 2, input$paired_ttest_alpha_value), ", ", test$parameter, "\\()\\)", " \\( = \\) ",
      ifelse(input$paired_ttest_alternative_hypothesis == "two.sided", "\\( \\pm \\)", ifelse(input$paired_ttest_alternative_hypothesis == "greater", "", " -")),
      ifelse(input$paired_ttest_alternative_hypothesis == "two.sided", round(qt(input$paired_ttest_alpha_value / 2, df = test$parameter, lower.tail = FALSE), 3), round(qt(input$paired_ttest_alpha_value, df = test$parameter, lower.tail = FALSE), 3))
    ),
    br(),
    paste0("4. Conclusion : ", ifelse(test$p.value < input$paired_ttest_alpha_value, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
    br(),
    br(),
    tags$b("Interpretation"),
    br(),
    paste0("At the ", input$paired_ttest_alpha_value * 100, "% significance level, ", ifelse(test$p.value < input$paired_ttest_alpha_value, "we reject the null hypothesis that the true mean of the difference is equal to ", "we do not reject the null hypothesis that the true mean of the difference is equal to "), test$null.value, " \\((p\\)-value ", ifelse(test$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test$p.value, 3))), ")", ".")
  )
  
})

output$paired_ttest_plot <- renderPlot({
  if(input$ttest_data_input_options == "group_col"){
    
    #get factors of grouping colum
    my_groups <- unique(df_to_use()[[input$paired_ttest_group_categorical]])
    
    #initalize vectors for separation
    x = vector()
    y = vector()
    
    #set indices to count for in separation.
    idx_x = 1;
    idx_y = 1;
    
    #loop to add values from each of the two groups to their respective vectors
    for(i in seq(1:nrow(df_to_use()))){
      if(my_groups[1] == df_to_use()[[input$paired_ttest_group_categorical]][i]){
        #adds value to x vector at idx
        x[idx_x] <- df_to_use()[[input$paired_ttest_group_numerical]][i]
        idx_x = idx_x + 1
      }
      else if(my_groups[2] == df_to_use()[[input$paired_ttest_group_categorical]][i]){
        #adds values to y vector at idx
        y[idx_y] <- df_to_use()[[input$paired_ttest_group_numerical]][i]
        idx_y = idx_y + 1
      }
    }
    
  }
  #assign x,y by columns selected if num_col selected
  else if(input$ttest_data_input_options == "num_col"){
    x = df_to_use()[[input$paired_ttest_var1]]
    y = df_to_use()[[input$paired_ttest_var2]]
  }
  
  
  dat1 = x
  dat2 = y
  
  test <- t.test(x = dat1,
                 y = dat2,
                 mu = input$paired_ttest_nullHypothesis,
                 alternative = input$paired_ttest_alternative_hypothesis,
                 conf.level = 1 - input$paired_ttest_alpha_value,
                 paired = TRUE)
  
  if (input$paired_ttest_alternative_hypothesis == "two.sided") {
    funcShaded <- function(x) {
      y <- dt(x, df = test$parameter)
      y[x < qt(input$paired_ttest_alpha_value / 2, df = test$parameter, lower.tail = FALSE) & x > qt(input$paired_ttest_alpha_value / 2, df = test$parameter) ] <- NA
      return(y)
    }
  } else if (input$paired_ttest_alternative_hypothesis == "greater") {
    funcShaded <- function(x) {
      y <- dt(x, df = test$parameter)
      y[x < qt(input$paired_ttest_alpha_value, df = test$parameter, lower.tail = FALSE) ] <- NA
      return(y)
    }
  } else if (input$paired_ttest_alternative_hypothesis == "less") {
    funcShaded <- function(x) {
      y <- dt(x, df = test$parameter)
      y[x > qt(input$paired_ttest_alpha_value, df = test$parameter, lower.tail = TRUE) ] <- NA
      return(y)
    }
  }
  p <- ggplot(data.frame(x = c(qt(0.999, df = test$parameter, lower.tail = FALSE), qt(0.999, df = test$parameter, lower.tail = TRUE))), aes(x = x)) +
    stat_function(fun = dt, args = list(df = test$parameter)) +
    stat_function(fun = funcShaded, geom = "area", alpha = 0.8) +
    theme_minimal() +
    geom_vline(xintercept = test$statistic, color = "steelblue") +
    geom_text(aes(x = test$statistic, label = paste0("Test statistic = ", round(test$statistic, 3)), y = 0.2), colour = "steelblue", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("Student distribution", " t(", round(test$parameter, 3), ")")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x")
  p
  
})
#this is used to print out the results of the ttest to the user
output$paired_ttest_table_out = renderTable({
  if(input$ttest_data_input_options == "group_col"){
    
    #get factors of grouping colum
    my_groups <- unique(df_to_use()[[input$paired_ttest_group_categorical]])
    
    #initalize vectors for separation
    x = vector()
    y = vector()
    
    #set indices to count for in separation.
    idx_x = 1;
    idx_y = 1;
    
    #loop to add values from each of the two groups to their respective vectors
    for(i in seq(1:nrow(df_to_use()))){
      if(my_groups[1] == df_to_use()[[input$paired_ttest_group_categorical]][i]){
        #adds value to x vector at idx
        x[idx_x] <- df_to_use()[[input$paired_ttest_group_numerical]][i]
        idx_x = idx_x + 1
      }
      else if(my_groups[2] == df_to_use()[[input$paired_ttest_group_categorical]][i]){
        #adds values to y vector at idx
        y[idx_y] <- df_to_use()[[input$paired_ttest_group_numerical]][i]
        idx_y = idx_y + 1
      }
    }
    
  }
  #assign x,y by columns selected if num_col selected
  else if(input$ttest_data_input_options == "num_col"){
    x = df_to_use()[[input$paired_ttest_var1]]
    y = df_to_use()[[input$paired_ttest_var2]]
  }
  
  test <- t.test(x,y,
                 mu = input$paired_ttest_nullHypothesis,
                 alternative = input$paired_ttest_alternative_hypothesis,
                 conf.level = 1 - input$paired_ttest_alpha_value,
                 paired = TRUE)
  
  tab = matrix(c(test$method,
                 round(test$parameter, 2),
                 round(test$statistic, 2),
                 round(test$p.value, 2),
                 round(test$conf.int[1], 2),
                 round(test$conf.int[2], 2),
                 test$null.value,
                 test$alternative,
                 input$ttest_alpha_value,
                 #this prints out reject or not if the pvlaue is greater than the alpha value
                 ifelse(test$p.value < input$paired_ttest_alpha_value, "Reject", "Not Reject")), nrow=1)
  colnames(tab) = c("method","DF","t-statistic","p-value", "c1", "c2", "H_o", "Type", "Sig. Level", "Result")
  rownames(tab) = "Values"
  tab
})

output$ttest_boxpolot <- renderPlot({
  if(input$sample == 'oneSamp'){
    var1 <- df_to_use()[[input$var1]]
    boxplot(var1)
  }
  else if(input$sample == 'twoSamp'){
    if(input$ttest_data_input_options == "group_col"){
      
      #get factors of grouping colum
      my_groups <- unique(df_to_use()[[input$two_samp_ttest_categorical_group]])
      
      #initalize vectors for separation
      x = vector()
      y = vector()
      
      #set indices to count for in separation.
      idx_x = 1;
      idx_y = 1;
      
      #loop to add values from each of the two groups to their respective vectors
      for(i in seq(1:nrow(df_to_use()))){
        if(my_groups[1] == df_to_use()[[input$two_samp_ttest_categorical_group]][i]){
          #adds value to x vector at idx
          x[idx_x] <- df_to_use()[[input$two_samp_ttest_numerical_group]][i]
          idx_x = idx_x + 1
        }
        else if(my_groups[2] == df_to_use()[[input$two_samp_ttest_categorical_group]][i]){
          #adds values to y vector at idx
          y[idx_y] <- df_to_use()[[input$two_samp_ttest_numerical_group]][i]
          idx_y = idx_y + 1
        }
      }
      print(x)
      print(y)
      
    }
    #assign x,y by columns selected if num_col selected
    else if(input$ttest_data_input_options == "num_col"){
      x = df_to_use()[[input$two_samp_ttest_var1]]
      y = df_to_use()[[input$two_samp_ttest_var2]]
    }
    
    
    boxplot(x,y)
  }
  else if(input$sample == "pairedSamp"){
    if(input$ttest_data_input_options == "group_col"){
      
      #get factors of grouping colum
      my_groups <- unique(df_to_use()[[input$paired_ttest_group_categorical]])
      
      #initalize vectors for separation
      x = vector()
      y = vector()
      
      #set indices to count for in separation.
      idx_x = 1;
      idx_y = 1;
      
      #loop to add values from each of the two groups to their respective vectors
      for(i in seq(1:nrow(df_to_use()))){
        if(my_groups[1] == df_to_use()[[input$paired_ttest_group_categorical]][i]){
          #adds value to x vector at idx
          x[idx_x] <- df_to_use()[[input$paired_ttest_group_numerical]][i]
          idx_x = idx_x + 1
        }
        else if(my_groups[2] == df_to_use()[[input$paired_ttest_group_categorical]][i]){
          #adds values to y vector at idx
          y[idx_y] <- df_to_use()[[input$paired_ttest_group_numerical]][i]
          idx_y = idx_y + 1
        }
      }
      
    }
    #assign x,y by columns selected if num_col selected
    else if(input$ttest_data_input_options == "num_col"){
      x = df_to_use()[[input$paired_ttest_var1]]
      y = df_to_use()[[input$paired_ttest_var2]]
    }
    boxplot(x,y)
  }
  
  #render help data input file for ttest test
  output$ttest_data_help <- renderImage({
    # filename <- normalizePath(file.path("./images/ttest_help.JPG"))
    #
    # list(src=filename)
    filename <- normalizePath(file.path("./images/ttest.JPG"))
    
    list(src=filename)
  }, deleteFile=FALSE)
  
  # output$chisquared_data_help <- renderImage({
  #   filename <- normalizePath(file.path("./images/chisquared_data_help.JPG"))
  #
  #   list(src=filename)
  #
  # }, deleteFile=FALSE)
  
})

