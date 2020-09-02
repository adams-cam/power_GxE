#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list = ls())

library(shiny)
library(knitr)
library(kableExtra)
library(tidyverse)
source("functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    ####make the navbar pages####
    shinyUI(
        navbarPage("GxE Power and Sample Size ",
                   navbarMenu("Gene: Sample Size",
                              tabPanel("Case-Control",
                                       source("case_control_sample_size.R")$value)
                              #tabPanel("Case-Control Slider",
                              #         source("case_control_ixn_sample_size_slider.R")$value)
                   ),
                   navbarMenu("GxE: Sample Size",
                              tabPanel("Case-Control",
                                       source("case_control_ixn_sample_size.R")$value),
                              tabPanel("Case-only",
                                       source("case_only_ixn_sample_size.R")$value)#,
                              #tabPanel("Case-Control Slider",
                              #         source("case_control_ixn_sample_size_slider.R")$value)
                   ),
                   navbarMenu("GxE: Power",
                              tabPanel("Case-Control",
                                       source("case_control_ixn_power.R")$value),
                              tabPanel("Case-only",
                                       source("case_only_ixn_power.R")$value)
                              ),
                   tabPanel("About", source("about_page.R")$value)
    ) ##close navbarpage
) ##close shinyUI

) ## close fluid page

####server section####

server <- function(input, output, session) {
    
   
    tbl_test <- eventReactive(input$ccn_g_do, {
        
        data.frame(input$ccn_k, input$ccn_prev)
        data.frame(1:10, 1:10)
        
    
        })
        
    
    output$cc_sample_size <- function() {
        df <- tbl_test()
        kable(df, format = "html")
        #kable(tbl_test)
    }

    
    # case only ixn sample size
    output$co_ixn_sample_size <- renderTable({
        
        #input <- list(con_n = "1000",
        #              con_p_e = "0.5", con_p_g = "0.5", con_or_g = "1.1", 
        #con_or_e = "1.1", con_or_ge = "1.6", con_alpha = "0.05", con_beta = "0.8", 
        #con_num_tests = "1")
        
        
        # get var
        con_b3_var <-get_b3_var_co(Pg = as.numeric(input$con_p_g), 
                                   Pe =  as.numeric(input$con_p_e), 
                                   coef = as.numeric(c(input$con_or_g, 
                                                       input$con_or_e, 
                                                       input$con_or_ge)))
        
        # get n
        con_n <- get_b3_sample_size_co(alpha = (as.numeric(input$con_alpha) / 
                                                    as.numeric(input$con_num_tests)), 
                                       beta = as.numeric(input$con_beta), 
                                       var = con_b3_var, 
                                       b3 = log(as.numeric(input$con_or_ge)))
        
        # output
        if (is.na(con_n)) {
            tbl <- NULL }
        else {
            tbl <- data.frame(Params = c("n_case", "Alpha",
                                         "Beta", "# Tests", "Pg", "Pe", 
                                         "ORg", "ORe", "ORge", "Var(ORge)"), 
                              Value = c(con_n,
                                        input$con_alpha,input$con_beta,
                                        input$con_num_tests, input$con_p_g, 
                                        input$con_p_e, input$con_or_g, input$con_or_e, 
                                        input$con_or_ge, 
                                        as.character(round(con_b3_var, 2))))        
        }    
        
        
    })
    
    # case only ixn power
    output$co_ixn_power <- renderTable({
        
        #input <- list(cop_n_case = "1000",
        #              cop_p_e = "0.5", cop_p_g = "0.5", cop_or_g = "1.1", 
        #              cop_or_e = "1.1", cop_num_tests = "1",
        #              cop_or_ge = "1.6", cop_alpha = "0.05", beta = "0.8")
        
        # get var b3
        cop_b3_var <- get_b3_var_co(Pe = as.numeric(input$cop_p_e), 
                                   Pg = as.numeric(input$cop_p_g), 
                                   coef = as.numeric(c(input$cop_or_g, 
                                                       input$cop_or_e, 
                                                       input$cop_or_ge)))
        
        # get power for input
        cop_power <- get_b3_power(alpha = (as.numeric(input$cop_alpha) / 
                                                  as.numeric(input$cop_num_tests)),
                                  var = cop_b3_var, 
                                  b3 = log(as.numeric(input$cop_or_ge)), 
                                  n = as.numeric(input$cop_n_case))
        
        # create output table
        if (is.na(cop_b3_var)) {
            tbl_co_ixn_power <- NULL 
            } else {
            tbl_co_ixn_power <- 
                data.frame(Params = c("n_case", "Alpha", "Beta", "# Tests", 
                                      "Pg", "Pe", "ORg", "ORe", "ORge", "Var(ORge)"), 
                           Value = c(input$cop_n_case, input$cop_alpha,
                                     Beta = round(cop_power, 2), input$cop_num_tests,
                                                 input$cop_p_g, 
                                                 input$cop_p_e, 
                                                 input$cop_or_g, 
                                                 input$cop_or_e, 
                                                 input$cop_or_ge, 
                                                 round(cop_b3_var, 2)))
        }    
        
        #tbl_cc_ixn_power
    }, hover = TRUE, rownames = FALSE, digits = 2)
    
    # case only ixn power plot
    output$plot_co_ixn_power <- renderPlot({
        
        #input <- list(cop_n_case = "1000",
        #              cop_p_e = "0.5", cop_p_g = "0.5", cop_or_g = "1.1", 
        #              cop_or_e = "1.1", cop_num_tests = "1",
        #             cop_or_ge = "1.6", cop_alpha = "0.05", beta = "0.8")
        
        # get var b3
        cop_b3_var <- get_b3_var_co(Pe = as.numeric(input$cop_p_e), 
                                    Pg = as.numeric(input$cop_p_g), 
                                    coef = as.numeric(c(input$cop_or_g, 
                                                        input$cop_or_e, 
                                                        input$cop_or_ge)))
        
        # get power for input
        cop_power <- get_b3_power(alpha = (as.numeric(input$cop_alpha) / 
                                               as.numeric(input$cop_num_tests)),
                                  var = cop_b3_var, 
                                  b3 = log(as.numeric(input$cop_or_ge)), 
                                  n = as.numeric(input$cop_n_case))
        
        # get power for range of n
        cop_n_seq <- seq(100, 10000, 50)
        if(!is.na(cop_b3_var)) {
            power_range <- sapply(cop_n_seq, 
                      function(x) {
                          get_b3_power_cc(n = x, 
                              alpha = (as.numeric(input$cop_alpha) / 
                                           as.numeric(input$cop_num_tests)),
                              var = cop_b3_var, 
                              b3 = log(as.numeric(input$cop_or_ge)))
                          })
            
            index <- (1:length(power_range))[power_range >= 0.8] # get n for power>=0.8
            plot_power(cop_n_seq, power_range, cop_n_seq[index[1]])
        } else{plot.new()}
        
        
    }, height = 400, width = 400)
    
    # case only ixn sample size
    output$co_ixn_sample_size <- renderTable({
        
        #input <- list(con_n = "1000",
        #              con_p_e = "0.5", con_p_g = "0.5", con_or_g = "1.1", 
                      #con_or_e = "1.1", con_or_ge = "1.6", con_alpha = "0.05", con_beta = "0.8", 
                      #con_num_tests = "1")
        
        
        # get var
        con_b3_var <-get_b3_var_co(Pg = as.numeric(input$con_p_g), 
                                   Pe =  as.numeric(input$con_p_e), 
                                   coef = as.numeric(c(input$con_or_g, 
                                                       input$con_or_e, 
                                                       input$con_or_ge)))

        # get n
        con_n <- get_b3_sample_size_co(alpha = (as.numeric(input$con_alpha) / 
                                                    as.numeric(input$con_num_tests)), 
                                       beta = as.numeric(input$con_beta), 
                                       var = con_b3_var, 
                                       b3 = log(as.numeric(input$con_or_ge)))
        
        # output
        if (is.na(con_n)) {
            tbl <- NULL }
        else {
            tbl <- data.frame(Params = c("n_case", "Alpha",
                                         "Beta", "# Tests", "Pg", "Pe", 
                                         "ORg", "ORe", "ORge", "Var(ORge)"), 
                              Value = c(con_n,
                                        input$con_alpha,input$con_beta,
                                        input$con_num_tests, input$con_p_g, 
                                        input$con_p_e, input$con_or_g, input$con_or_e, 
                                        input$con_or_ge, 
                                        as.character(round(con_b3_var, 2))))        
                              }    
        
        
    })
    
    # case only ixn sample size plot
    output$plot_co_ixn_sample_size <- renderPlot({
        
        # get var
        con_b3_var <-get_b3_var_co(Pg = as.numeric(input$con_p_g), 
                                   Pe =  as.numeric(input$con_p_e), 
                                   coef = as.numeric(c(input$con_or_g, 
                                                       input$con_or_e, 
                                                       input$con_or_ge)))
        
        # get n
        con_n <- get_b3_sample_size_co(alpha = (as.numeric(input$con_alpha) / 
                                                    as.numeric(input$con_num_tests)), 
                                       beta = as.numeric(input$con_beta), 
                                       var = con_b3_var, 
                                       b3 = log(as.numeric(input$con_or_ge)))
        
        
        
        # get power for range of n
        con_n_seq <- seq(100, 10000, 50)
        if(!is.na(con_b3_var) | !is.na(con_n)) {
            power_range <- sapply(con_n_seq, 
                                  function(x) {
                                      get_b3_power(n = x, 
                                                  alpha = as.numeric(input$con_alpha) / 
                                                      as.numeric(input$con_num_tests),
                                                  var = con_b3_var, 
                                                  b3 = log(as.numeric(input$con_or_ge)))})
            
            plot_power(con_n_seq, power_range, n_input = con_n)
        } else{}
        
        
    }, 
    height = 400, width = 400)
    
    # case control ixn sample size with sliders
    output$cc_ixn_sample_size_slider <- renderTable({
        
        # get b3 var
        ccns_b3_var <- get_b3_var_cc(k = as.numeric(input$ccns_k), 
                                 p_e = as.numeric(input$ccns_p_e), 
                                 p_g = as.numeric(input$ccns_p_g), 
                                 coef = as.numeric(c(input$ccns_or_g, 
                                                     input$ccns_or_e, 
                                                     input$ccns_or_ge)),
                                 delta = as.numeric(input$ccns_delta))
        
        ccns_n <-  get_b3_sample_size_cc(alpha = (as.numeric(input$ccns_alpha) / 
                                               as.numeric(input$ccns_num_tests)), 
                                  beta = as.numeric(input$ccns_beta),
                                  var = ccns_b3_var, 
                                  b3 = log(as.numeric(input$ccns_or_ge)))

        # create output table
        if (is.na(ccns_b3_var)) {
            tbl <- NULL }
        else {
            tbl <- data.frame(Params = c("n", "n_case", "n_cont", "k", "Alpha",
                                         "Beta", "# Tests", "Pg", "Pe", 
                                         "ORg", "ORe", "ORge", "Var(ORge)", 
                                         "OR(G,E)"), 
                              Value = c(ccns_n,
                                        ceiling(ccns_n*as.numeric(input$ccns_k)), 
                                        ccns_n - ceiling(ccns_n*as.numeric(input$ccns_k)), 
                                        input$ccns_k, input$ccns_alpha,input$ccns_beta,
                                        input$ccns_num_tests, input$ccns_p_g, 
                                        input$ccns_p_e, input$ccns_or_g, input$ccns_or_e, 
                                        input$ccns_or_ge, 
                                        as.character(round(ccns_b3_var, 2)),
                                        input$ccns_delta))
            }    
    }, hover = TRUE, rownames = FALSE, digits = 2)
    
    # case control ixn sample size 
    output$cc_ixn_sample_size <- renderTable({
    
        # get var b3
        ccn_b3_var <- get_b3_var_cc(k = as.numeric(input$ccn_k), 
                             p_e = as.numeric(input$ccn_p_e), 
                             p_g = as.numeric(input$ccn_p_g), 
                             coef = as.numeric(c(input$ccn_or_g, 
                                                 input$ccn_or_e, 
                                                input$ccn_or_ge)),
                             delta = as.numeric(input$ccn_delta))
    

        # get sample size for input
        ccn_n <- get_b3_sample_size_cc(alpha = (as.numeric(input$ccn_alpha) / 
                                             as.numeric(input$ccn_num_tests)), 
                                beta = as.numeric(input$ccn_beta),
                                var = ccn_b3_var, 
                                b3 = log(as.numeric(input$ccn_or_ge)))

        # create output table
        if (is.na(ccn_b3_var)) {
            tbl <- NULL 
            } else {
            tbl <- data.frame(Params = c("n", 
                                         "n_case", 
                                         "n_cont", 
                                         "k", "Alpha", "Beta", 
                                         " Tests", 
                                         "Pg", "Pe", 
                                         "ORg", "ORe", "ORge", 
                                         "Var(ORge)", 
                                         "OR(G,E)"), 
                              Value = c(ccn_n,
                                        ceiling(ccn_n*as.numeric(input$ccn_k)), 
                                        ccn_n - floor(ccn_n*as.numeric(input$ccn_k)), 
                                        input$ccn_k, 
                                        input$ccn_alpha, 
                                        input$ccn_beta,
                                        input$ccn_num_tests, 
                                        input$ccn_p_g, 
                                        input$ccn_p_e, 
                                        input$ccn_or_g, 
                                        input$ccn_or_e, 
                                        input$ccn_or_ge, 
                                        as.character(round(ccn_b3_var, 2)),
                                        input$ccn_delta)
                              )
        }    
                       
        }, hover = TRUE, rownames = FALSE)#, digits = 0)
        
    # case control ixn sample size plot
    output$plot_cc_ixn_sample_size <- renderPlot({
        
        # get var b3
        ccn_b3_var <- get_b3_var_cc(k = as.numeric(input$ccn_k), 
                             p_e = as.numeric(input$ccn_p_e), 
                             p_g = as.numeric(input$ccn_p_g), 
                             coef = as.numeric(c(input$ccn_or_g, 
                                                 input$ccn_or_e, 
                                                 input$ccn_or_ge)),
                             delta = as.numeric(input$ccn_delta))
        
        # get sample size for input
        ccn_n <- get_b3_sample_size_cc(alpha = as.numeric(input$ccn_alpha) / 
                                    as.numeric(input$ccn_num_tests), 
                                beta = as.numeric(input$ccn_beta),
                                var = ccn_b3_var, 
                                b3 = log(as.numeric(input$ccn_or_ge)))
        
        
        # get power for range of n
        ccn_n_seq <- seq(100, 10000, 50)
        if(!is.na(ccn_b3_var) & !is.na(ccn_n)) {
            power_range <- sapply(ccn_n_seq, 
                                  function(x) {
                                      get_b3_power_cc(n = x, 
                                                   alpha = as.numeric(input$ccn_alpha) / 
                                                       as.numeric(input$ccn_num_tests),
                                                   var = ccn_b3_var, 
                                                   b3 = log(as.numeric(input$ccn_or_ge)))})
            
            plot_power(ccn_n_seq, power_range, ccn_n)
        } else{plot.new()}
        
        
    }, 
    height = 400, width = 400)
        
    # case control ixn power 
    output$cc_ixn_power <- renderTable({
        
        #input <- list(n_case = "1000", n_cont = "1000", 
        #             p_e = "0.5", p_g = "0.5", or_g = "1.1", or_e = "1.1", 
        #              or_ge = "1.6", alpha = "0.05", beta = "0.8", delta = "1")
        
        
        n <- as.integer(input$ccp_n_case) + as.integer(input$ccp_n_cont)
        k <- as.integer(input$ccp_n_case) / 
            (as.integer(input$ccp_n_case) + as.integer(input$ccp_n_cont))
    
        # get var b3
        ccp_b3_var<- get_b3_var_cc(k = k, 
                             p_e = as.numeric(input$ccp_p_e), 
                             p_g = as.numeric(input$ccp_p_g), 
                             coef = as.numeric(c(input$ccp_or_g, 
                                                 input$ccp_or_e, 
                                                 input$ccp_or_ge)),
                             delta = as.numeric(input$ccp_delta))
        
        # get power for input
        ccp_power <- get_b3_power_cc(alpha = (as.numeric(input$ccp_alpha) / 
                                           as.numeric(input$ccp_num_tests)),
                              var = ccp_b3_var, 
                              b3 = log(as.numeric(input$ccp_or_ge)), n = n)
        
        
        # create output table
        if (is.na(ccp_b3_var)) {
            tbl_cc_ixn_power <- NULL }
        else {
            tbl_cc_ixn_power <- 
                data.frame(Params = c("n", "n_case", "n_cont", "Alpha",
                                      "Beta", "# Tests", "Pg", "Pe", 
                                      "ORg", "ORe", "ORge", "Var(ORge)", 
                                      "OR(G,E)"), 
                           Value = c(n,                    # n
                                     ceiling(n*k),         # n case
                                     n - ceiling(n*k),     # n control
                                     input$ccp_alpha,      # Alpha
                                     round(ccp_power, 2),  # Beta
                                     input$ccp_num_tests,  # num tests
                                     input$ccp_p_g,        # Pg
                                     input$ccp_p_e,        # Pe
                                     input$ccp_or_g,       # ORg
                                     input$ccp_or_e,       # ORe
                                     input$ccp_or_ge,      # ORge
                                     round(ccp_b3_var, 2), # Var(ORge)
                                     input$ccp_delta))     # OR(G,E)
        }    
        
        #tbl_cc_ixn_power
    }, hover = TRUE, rownames = FALSE, digits = 0)
    
    # case control ixn power plot
    output$plot_cc_ixn_power <- renderPlot({

        #input <- list(ccp_n_case = "1000", ccp_n_cont = "1000", 
        #              ccp_p_e = "0.5", ccp_p_g = "0.5", ccp_or_g = "1.1", ccp_or_e = "1.1", 
        #              ccp_or_ge = "1.6", ccp_alpha = "0.05", ccp_delta = "1", 
        #              ccp_num_tests = "1")
                      
        ##check for N
        n <- as.integer(input$ccp_n_case) + as.integer(input$ccp_n_cont)
        k <- as.integer(input$ccp_n_case) / 
            (as.integer(input$ccp_n_case) + as.integer(input$ccp_n_cont))
        
        #n <- 1000
        #k <- 0.5
        # get var b3
        ccp_b3_var <- get_b3_var_cc(k = k, 
                             p_e = as.numeric(input$ccp_p_e), 
                             p_g = as.numeric(input$ccp_p_g), 
                             coef = as.numeric(c(input$ccp_or_g, input$ccp_or_e, 
                                                 input$ccp_or_ge)),
                             delta = as.numeric(input$ccp_delta))
        
        # get power for range of n
        n_seq <- seq(100, 10000, 50)
        if(!is.na(ccp_b3_var)) {
            power_range <- sapply(n_seq, 
               function(x) {
                   get_b3_power_cc(n = x, 
                                alpha = (as.numeric(input$ccp_alpha) / 
                                             as.numeric(input$ccp_num_tests)),
                                var = ccp_b3_var, 
                                b3 = log(as.numeric(input$ccp_or_ge)))
                   })
            
            index <- (1:length(power_range))[power_range >= 0.8] # get n for power>=0.8
            
            plot_power(n_seq, power_range, n_seq[index[1]])
            } else{plot.new()}
        
        
    }, height = 400, width = 400)

    
}

# Run the application
shinyApp(ui = ui, server = server)

