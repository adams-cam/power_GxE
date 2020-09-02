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
                   navbarMenu("Sample Size",
                              tabPanel("Gene only",
                                       source("case_control_sample_size.R")$value
                                       ),
                              tabPanel("Environment only",
                                       source("case_control_env_sample_size.R")$value
                              ),
                              tabPanel("GxE",
                                       source("case_control_gxe_sample_size.R")$value)
                              ),
                   navbarMenu("Power",
                              tabPanel("Gene only",
                                       source("case_control_power.R")$value),
                              tabPanel("Environment Only",
                                       source("case_control_env_power.R")$value),
                              tabPanel("GxE",
                                       source("case_control_gxe_power.R")$value)
                              ),
                   tabPanel("About",
                            source("about_page.R")$value
                            )
                   ) ##close navbarpage
        ) ##close shinyUI
    ) ## close fluid page


####server section####

server <- function(input, output, session) {
    
    ############################################################################
    # Misc
    ############################################################################    
    
    # ranges for zooming
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    
    ############################################################################
    # Power
    ############################################################################    

    
    # case-control gene only ---------------------------------------------------
    
    # get power
    tbl_cc_gene_power <- eventReactive(input$ccp_g_do, {
        
    
        
        # get coef
        if (isTruthy(input$ccp_g_org_ll) & isTruthy(input$ccp_g_org_ul)) {
            b1 <- log(seq(as.numeric(input$ccp_g_org_ll),
                          as.numeric(input$ccp_g_org_ul), by = 0.05))
        } else {b1 <- log(as.numeric(input$ccp_g_org))}
        
        # P(risk allele) 
        # set p
        #p <- 0.5
        if (isTruthy(input$ccp_g_pg_ll) & isTruthy(input$ccp_g_pg_ul)) {
            p <- seq(as.numeric(input$ccp_g_pg_ll),
                     as.numeric(input$ccp_g_pg_ul), by = 0.05)
        } else {p <- as.numeric(input$ccp_g_pg) }
        
        #p <- 0.5
        q <- 1-p # set q
        if (input$ccp_g_inherit == "Dominant") {
            p_x <- p^2 + 2*p*q
        } else if (input$ccp_g_inherit == "Recessive") {
            p_x <- p^2
        } 
        
        # b0
        #b0 <- log(p_x / (1-p_x))
        #b0 <- plogis(as.numeric(input$ccp_g_prev))
        b0 <- 1
        # calculate Var(b1)
        b1_var <- data.frame(sapply(p_x, function(x) {
            get_b1_var_cc(p_x = x, A = 1, B = exp(b1), 
                          k = as.numeric(input$ccp_g_k))
        })) 
        #colnames(b1_var) <- as.character(p)
        #rownames(b1_var) <- as.character(round(exp(b1), 2))
        
        # get power
        ccp_g_beta <- data.frame(apply(b1_var, 2, function(x) {
            get_power(alpha = as.numeric(input$ccp_g_alpha), 
                      var = x, b = b1, 
                      n = as.numeric(input$ccp_g_n_case) + 
                          as.numeric(input$ccp_g_n_case) * as.numeric(input$ccp_g_k)) 
        }))
        
        if (ncol(ccp_g_beta) == 1) {ccp_g_beta <- data.frame(t(ccp_g_beta))}
        colnames(ccp_g_beta) <- as.character(p)
        rownames(ccp_g_beta) <- as.character(round(exp(b1), 2))
        ccp_g_beta
    
        
    })
    
    # print table output
    output$tbl_cc_gene_power <- function() {
        
        df <- tbl_cc_gene_power()
        kable(df, "html", booktabs = T, align = c("r"), digits = 3,
              caption = "Power") %>%
            kable_styling("striped", full_width = F,
                          position = "left", font_size = 12) %>%
            footnote(general_title = "", 
                     general = c(paste0("N=", input$ccp_g_n_case, " cases, ",
                                 input$ccp_g_k, " controls per case"),
                                 paste0("Inheritence: ", input$ccp_g_inherit),
                                 paste0("alpha=", input$ccp_g_alpha), 
                                 paste0("# of independent tests=", 
                                        input$ccp_g_num_tests))) %>% 
            add_header_above(c("Pg" = ncol(df)+1)) %>% 
            pack_rows( "ORg", 1, nrow(df),
                       bold = T, indent = T, hline_after = F)
    }
    
    # plot output
    output$cc_power_plot <- renderPlot({
        
        #input <- list(ccn_g_k = 1, ccn_g_prev = "0.05", 
        #              ccn_g_inherit = "Dominant",
        #              ccn_g_pg = "0.5", 
        #              #ccn_g_pg_ll = " ", ccn_g_pg_ul = " ",
        #              ccn_g_pg_ll = "0.25", ccn_g_pg_ul = "0.5",
        #              or_g = "1.1", 
        #              #ccn_g_org_ll = " ", ccn_g_org_ul = " ",
        #              ccn_g_org_ll = "1.1", ccn_g_org_ul = "1.5",
        #              ccn_g_alpha = "0.05", ccn_g_beta = "0.8")
        
        # get sample size calcs
        #df <- ccp_g_beta
        #df <- tbl_cc_gene_power() %>% data.frame()
        df <- tbl_cc_gene_power() %>% data.frame() %>% mutate(ORg = rownames(.)) %>% 
            reshape2::melt() %>% rename(Power = value, Pg = variable) %>% 
            mutate(ORg = as.numeric(ORg), Pg = sub("X", "", Pg)) 
        
        # plot
        df %>% ggplot(aes(x = ORg, y = Power, color = Pg)) + 
            #geom_smooth(se = FALSE) + 
            geom_line() + 
            geom_hline(yintercept = 0.8, lty = 2) + 
            labs(y = paste0("Power")) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
            theme_minimal()
    })
    
    # observeEvent for brush zoom
    observeEvent(input$cc_power_plot_click, {
        brush <- input$cc_power_plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    # click for coordinates
    output$cc_power_info <- renderText({
        paste0("Click on plot for data points \n", "ORg=", 
               round(as.numeric(input$cc_power_plot_click$x), 2), "\nPower=", 
               round(as.numeric(input$cc_power_plot_click$y), 2))
    })
    
    
    # case-control environment only --------------------------------------------
    
    # get power
    tbl_cc_env_power <- eventReactive(input$ccp_e_do, {
        
        #input <- list(ccp_e_n_case = "1000", ccp_e_k = "1",
        #              ccp_e_pg = "0.5", 
        #              #ccp_e_pg_ll = " ", ccp_e_pg_ul = " ",
        #              ccp_e_pg_ll = "0.1", ccp_e_pg_ul = "0.5",
        #              ccp_e_org = "1.1", 
        #              #ccp_e_org_ll = " ", ccp_e_org_ul = " ",
        #              ccp_e_org_ll = "1.1", ccp_e_org_ul = "1.5",
        #              ccp_e_alpha = "0.05", ccp_e_beta = "0.8",
        #             ccp_e_num_tests = "1")
        
        
        
        # set b1
        if (isTruthy(input$ccp_e_org_ll) & isTruthy(input$ccp_e_org_ul)) {
            b1 <- log(seq(as.numeric(input$ccp_e_org_ll),
                          as.numeric(input$ccp_e_org_ul), by = 0.05))
        } else {b1 <- log(as.numeric(input$ccp_e_org))}
        
        # set p_x=P(E=1)
        if (isTruthy(input$ccp_e_pg_ll) & isTruthy(input$ccp_e_pg_ul)) {
            p <- seq(as.numeric(input$ccp_e_pg_ll),
                     as.numeric(input$ccp_e_pg_ul), by = 0.05)
        } else {p <- as.numeric(input$ccp_e_pg) }
        p_x <- p
        
        # nusiance paramter     
        b0 <- 1
        
        # calculate Var(b1)
        b1_var <- data.frame(sapply(p_x, function(x) {
            get_b1_var_cc(p_x = x, A = 1, B = exp(b1), 
                          k = as.numeric(input$ccp_e_k))
        })) 

        # get power
        ccp_e_beta <- data.frame(apply(b1_var, 2, function(x) {
            get_power(alpha = as.numeric(input$ccp_e_alpha) / 
                          as.numeric(input$ccp_e_num_tests), 
                      var = x, b = b1, 
                      n = as.numeric(input$ccp_e_n_case) + 
                          as.numeric(input$ccp_e_n_case) * as.numeric(input$ccp_e_k)) 
        }))
        
        if (ncol(ccp_e_beta) == 1) {ccp_e_beta <- data.frame(t(ccp_e_beta))}
        colnames(ccp_e_beta) <- as.character(p)
        rownames(ccp_e_beta) <- as.character(round(exp(b1), 2))
        ccp_e_beta
        
        
    })
    
    # print table output
    output$tbl_cc_env_power <- function() {
        #df <- ccp_e_beta
        df <- tbl_cc_env_power()
        kable(df, "html", booktabs = T, align = c("r"), digits = 3,
              caption = "Power") %>%
            kable_styling("striped", full_width = F,
                          position = "left", font_size = 12) %>%
            footnote(general_title = "", 
                     general = c(paste0("N=", input$ccp_e_n_case, " cases, ",
                                        input$ccp_e_k, " control(s) per case"),
                                 paste0("alpha=", input$ccp_e_alpha), 
                                 paste0("# of independent tests=", 
                                        input$ccp_e_num_tests))) %>% 
            add_header_above(c("Pe" = ncol(df)+1)) %>% 
            pack_rows( "ORe", 1, nrow(df),
                       bold = T, indent = T, hline_after = F)
    }
    
    # plot output
    output$cc_env_power_plot <- renderPlot({
        
        #df <- ccp_g_beta
        df <- tbl_cc_env_power() %>% data.frame() %>% mutate(ORe = rownames(.)) %>% 
            reshape2::melt() %>% rename(Power = value, Pe = variable) %>% 
            mutate(ORe = as.numeric(ORe), Pe = sub("X", "", Pe)) 
        
        # plot
        df %>% ggplot(aes(x = ORe, y = Power, color = Pe)) + 
            #geom_smooth(se = FALSE) + 
            geom_line() + 
            geom_hline(yintercept = 0.8, lty = 2) + 
            labs(y = paste0("Power")) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
            theme_minimal()
    })
    
    # observeEvent for brush zoom
    observeEvent(input$cc_env_power_plot_click, {
        brush <- input$cc_env_power_plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    # click for coordinates
    output$cc_env_power_info <- renderText({
        paste0("Click on plot for data points \n", "ORe=", 
               round(as.numeric(input$cc_env_power_plot_click$x), 2), "\nPower=", 
               round(as.numeric(input$cc_env_power_plot_click$y), 2))
    })
    
    
    # case-control gxe ---------------------------------------------------------
    
    # get power
    tbl_cc_gxe_power <- eventReactive(input$ccp_ge_do, {
        
        # get coef
        b1 <- log(as.numeric(input$ccp_ge_org)) # ORg
        b2 <- log(as.numeric(input$ccp_ge_ore)) # ORe
        
        if (isTruthy(input$ccp_ge_orgxe_ll) & isTruthy(input$ccp_ge_orgxe_ul)) {
            b3 <- log(seq(as.numeric(input$ccp_ge_orgxe_ll),
                          as.numeric(input$ccp_ge_orgxe_ul), by = 0.05))
        } else {b3 <- log(as.numeric(input$ccp_ge_orgxe))}
        
        # P(risk allele) 
        # set p
        if (isTruthy(input$ccp_ge_pg_ll) & isTruthy(input$ccp_ge_pg_ul)) {
            p <- seq(as.numeric(input$ccp_ge_pg_ll),
                     as.numeric(input$ccp_ge_pg_ul), by = 0.05)
        } else {p <- as.numeric(input$ccp_ge_pg) }
        
        q <- 1-p # set q
        if (input$ccp_ge_inherit == "Dominant") {
            p_x <- p^2 + 2*p*q
        } else if (input$ccp_ge_inherit == "Recessive") {
            p_x <- q^2
        } 
        
        # P(E=1)
        pe <- as.numeric(input$ccp_ge_pe)
        
        # calculate Var(be)
        b3_var <- data.frame(sapply(p_x, function(x) {
            get_b3_var_cc(k = 1 / (1 + as.numeric(input$ccp_ge_k)), 
                          p_e = pe, p_g = x, 
                          ORg = exp(b1), ORe = exp(b2), ORgxe = exp(b3))
        }))

        # get sample size
        ccp_ge_beta <- data.frame(apply(b3_var, 2, function(x) {
            get_power(alpha = as.numeric(input$ccp_ge_alpha), 
                      var = x, b = b3, 
                      n = as.numeric(input$ccp_ge_n_case) + 
                          as.numeric(input$ccp_ge_n_case) * 
                          as.numeric(input$ccp_ge_k)) 
        }))
        if (ncol(ccp_ge_beta) == 1) {ccp_ge_beta <- data.frame(t(ccp_ge_beta))}
        colnames(ccp_ge_beta) <- as.character(p)
        rownames(ccp_ge_beta) <- as.character(round(exp(b3), 2))
        ccp_ge_beta
    })    
    
    # print table
    output$cc_gxe_power_tbl <- function() {
        df <- tbl_cc_gxe_power()
        #kable(df, format = "html")
        kable(df, "html", booktabs = T, align = c("r"), digits = 3,
              caption = "Power") %>%
            kable_styling("striped", full_width = F,
                          position = "left", font_size = 12) %>%
            footnote(general_title = "", 
                     general = c(paste0("N=", input$ccp_ge_n_case, " cases, ",
                                        ", M=", input$ccp_ge_k, " controls per case"),
                                 paste0("Inheritence: ", input$ccp_ge_inherit),
                                 paste0("alpha=", input$ccp_ge_alpha),
                                 paste0("# of independent tests=", 
                                        input$ccp_ge_num_tests))) %>% 
            add_header_above(c("Pg" = ncol(df)+1)) %>% 
            pack_rows( "ORg", 1, nrow(df),
                       bold = T, indent = T, hline_after = F)
    }
    
    # plot output
    output$cc_gxe_power_plot <- renderPlot({
        
        #input <- list(ccn_g_k = 1, ccn_g_prev = "0.05", 
        #              ccn_g_inherit = "Dominant",
        #              ccn_g_pg = "0.5", 
        #              #ccn_g_pg_ll = " ", ccn_g_pg_ul = " ",
        #              ccn_g_pg_ll = "0.25", ccn_g_pg_ul = "0.5",
        #              or_g = "1.1", 
        #              #ccn_g_org_ll = " ", ccn_g_org_ul = " ",
        #              ccn_g_org_ll = "1.1", ccn_g_org_ul = "1.5",
        #              ccn_g_alpha = "0.05", ccn_g_beta = "0.8")
        
        # get sample size calcs
        #df <- ccp_g_beta
        #df <- tbl_cc_gene_power() %>% data.frame()
        df <- tbl_cc_gene_power() %>% data.frame() %>% mutate(ORg = rownames(.)) %>% 
            reshape2::melt() %>% rename(Power = value, Pg = variable) %>% 
            mutate(ORg = as.numeric(ORg), Pg = sub("X", "", Pg)) 
        
        # plot
        df %>% ggplot(aes(x = ORg, y = Power, color = Pg)) + 
            #geom_smooth(se = FALSE) + 
            geom_line() + 
            labs(y = "Power", x = "ORgxe") +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
            theme_minimal()
    })
    
    # observeEvent for brush zoom
    observeEvent(input$gxe_power_plot1_dblclick, {
        brush <- input$gxe_power_plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    # click for coordinates
    output$gxe_power_info <- renderText({
        paste0("Click on plot for data points \n", "ORgxe=", 
               round(as.numeric(input$gxe_power_plot_click$x), 2), "\nPower=", 
               round(as.numeric(input$gxe_power_plot_click$y), 2))
    })
    
    
    
    
    ################
    # Sample Size
    ################
    
    # case-control gene only ---------------------------------------------------
    
    # get sample size
    tbl_cc_gene <- eventReactive(input$ccn_g_do, {
    
        # get coef
        if (isTruthy(input$ccn_g_org_ll) & isTruthy(input$ccn_g_org_ul)) {
            b1 <- log(seq(as.numeric(input$ccn_g_org_ll),
                          as.numeric(input$ccn_g_org_ul), by = 0.05))
        } else {b1 <- log(as.numeric(input$ccn_g_org))}
        
        # P(risk allele) 
        # set p
        #p <- 0.5
        if (isTruthy(input$ccn_g_pg_ll) & isTruthy(input$ccn_g_pg_ul)) {
            p <- seq(as.numeric(input$ccn_g_pg_ll),
                     as.numeric(input$ccn_g_pg_ul), by = 0.05)
        } else {p <- as.numeric(input$ccn_g_pg) }
        
        q <- 1-p # set q
        if (input$ccn_g_inherit == "Dominant") {
            p_x <- p^2 + 2*p*q
        } else if (input$ccn_g_inherit == "Recessive") {
            p_x <- p^2
        } 
        
        b0 <- plogis(as.numeric(input$ccn_g_prev))
        #b0 <- log(p_x / (1-p_x))
        #b0 <- as.numeric(input$ccn_g_prev) / (1-as.numeric(input$ccn_g_prev))
        #b0 <- as.numeric(input$ccn_g_prev)
        
        # calculate Var(b1)
        b1_var <- data.frame(sapply(p_x, function(x) {
            get_b1_var_cc(p_x = x, A = 1, B = exp(b1), 
                          k = as.numeric(input$ccn_g_k))
        })) 
        
        # get sample size
        ccn_g_n <- data.frame(apply(b1_var, 2, function(x) {
            get_sample_size_cc(alpha = as.numeric(input$ccn_g_alpha) / 
                                   as.numeric(input$ccn_g_num_tests), 
                               beta = as.numeric(input$ccn_g_beta), 
                               var = x, b = b1) 
        }))
        if (ncol(ccn_g_n) == 1) {ccn_g_n <- data.frame(t(ccn_g_n))}
        colnames(ccn_g_n) <- as.character(p)
        rownames(ccn_g_n) <- as.character(round(exp(b1), 2))
        ccn_g_n <- ceiling(ccn_g_n * (1 / (1 + as.numeric(input$ccn_g_k))))
        ccn_g_n
        
        })
    
    # print table output
    output$cc_sample_size_tbl <- function() {
        df <- tbl_cc_gene()
        #kable(df, format = "html")
        kable(df, "html", booktabs = T, align = c("r"),
              caption = "Sample size N") %>%
            kable_styling("striped", full_width = F,
                          position = "left", font_size = 12) %>%
            footnote(general_title = "", 
                     general = c("N is the number of cases required for input parameters", 
                                 paste0("M = ", input$ccn_g_k, 
                                      " control(s) per case"), 
                                 paste0("Inheritence: ", input$ccn_g_inherit),
                                 paste0("alpha=", input$ccn_g_alpha, 
                                        "; beta=", input$ccn_g_beta),
                                 paste0("# of independent tests=", 
                                        input$ccn_g_num_tests))) %>% 
            add_header_above(c("Pg" = ncol(df)+1)) %>% 
            pack_rows( "ORg", 1, nrow(df),
                       bold = T, indent = T, hline_after = F)
    }
    
    # plot output
    output$cc_sample_size_plot <- renderPlot({
        
        #input <- list(ccn_g_k = 1, ccn_g_prev = "0.05", 
        #              ccn_g_inherit = "Dominant",
        #              ccn_g_pg = "0.5", 
        #              #ccn_g_pg_ll = " ", ccn_g_pg_ul = " ",
        #              ccn_g_pg_ll = "0.25", ccn_g_pg_ul = "0.5",
        #              or_g = "1.1", 
        #              #ccn_g_org_ll = " ", ccn_g_org_ul = " ",
        #              ccn_g_org_ll = "1.1", ccn_g_org_ul = "1.5",
        #              ccn_g_alpha = "0.05", ccn_g_beta = "0.8")
        
        # get sample size calcs
        #df <- tbl_cc_gene()
        df <- tbl_cc_gene() %>% data.frame() %>% mutate(ORg = rownames(.)) %>% 
            reshape2::melt() %>% rename(N = value, Pg = variable) %>% 
            mutate(ORg = as.numeric(ORg), Pg = sub("X", "", Pg)) 
            
        
        # plot
        df %>% ggplot(aes(x = ORg, y = N, color = Pg)) + 
            #geom_smooth(se = FALSE) + 
            geom_line() + 
            labs(y = paste0("N case with ", input$ccn_g_k, " controls per case")) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
            theme_minimal()
        })
    
    # observeEvent for brush zoom
    observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    # click for coordinates
    output$info <- renderText({
        paste0("Click on plot for data points \n", "ORg=", 
               round(as.numeric(input$plot_click$x), 2), "\nN=", 
               round(as.numeric(input$plot_click$y), 2))
    })
    
    
    # case-control environment only --------------------------------------------
    
    # get sample size
    tbl_cc_env <- eventReactive(input$ccn_e_do, {
        
        #input <- list(ccn_e_k = 1, ccn_e_prev = "0.05", 
        #              ccn_e_pg = "0.5", 
        #              #ccn_e_pe_ll = " ", ccn_e_pe_ul = " ",
        #              ccn_e_pe_ll = "0.25", ccn_e_pe_ul = "0.5",
        #              ccn_e_ore = "1.1", 
        #              #ccn_e_ore_ll = " ", ccn_e_ore_ul = " ",
        #              ccn_e_ore_ll = "1.1", ccn_e_ore_ul = "1.5",
        #              ccn_e_alpha = "0.05", ccn_e_beta = "0.8", 
        #              ccn_e_num_tests = "1")
        
        
        # get coef
        if (isTruthy(input$ccn_e_ore_ll) & isTruthy(input$ccn_e_ore_ul)) {
            b1 <- log(seq(as.numeric(input$ccn_e_ore_ll),
                          as.numeric(input$ccn_e_ore_ul), by = 0.05))
        } else {b1 <- log(as.numeric(input$ccn_e_ore))}
        
        # P(risk allele) 
        # set p
        #p <- 0.5
        if (isTruthy(input$ccn_e_pe_ll) & isTruthy(input$ccn_e_pe_ul)) {
            p <- seq(as.numeric(input$ccn_e_pe_ll),
                     as.numeric(input$ccn_e_pe_ul), by = 0.05)
        } else {p <- as.numeric(input$ccn_e_pe) }
        p_x <- p
        
        # calculate Var(b1)
        b1_var <- data.frame(sapply(p_x, function(x) {
            get_b1_var_cc(p_x = x, A = 1, B = exp(b1), 
                          k = as.numeric(input$ccn_e_k))
        })) 
        
        # get sample size
        ccn_e_n <- data.frame(apply(b1_var, 2, function(x) {
            get_sample_size_cc(alpha = as.numeric(input$ccn_e_alpha) / 
                                   as.numeric(input$ccn_e_num_tests), 
                               beta = as.numeric(input$ccn_e_beta), 
                               var = x, b = b1) 
        }))
        if (ncol(ccn_e_n) == 1) {ccn_e_n <- data.frame(t(ccn_e_n))}
        colnames(ccn_e_n) <- as.character(p)
        rownames(ccn_e_n) <- as.character(round(exp(b1), 2))
        ccn_e_n <- ceiling(ccn_e_n * (1 / (1 + as.numeric(input$ccn_e_k))))
        ccn_e_n
        
    })
    
    # print table output
    output$cc_env_sample_size_tbl <- function() {
        df <- tbl_cc_env()
        #kable(df, format = "html")
        kable(df, "html", booktabs = T, align = c("r"),
              caption = "Sample size N") %>%
            kable_styling("striped", full_width = F,
                          position = "left", font_size = 12) %>%
            footnote(general_title = "", 
                     general = c("N is the number of cases required for input parameters", 
                                 paste0("M = ", input$ccn_g_k, 
                                        " control(s) per case"), 
                                 paste0("Inheritence: ", input$ccn_g_inherit),
                                 paste0("alpha=", input$ccn_g_alpha, 
                                        "; beta=", input$ccn_g_beta),
                                 paste0("# of independent tests=", 
                                        input$ccn_g_num_tests))) %>% 
            add_header_above(c("Pe" = ncol(df)+1)) %>% 
            pack_rows( "ORe", 1, nrow(df),
                       bold = T, indent = T, hline_after = F)
    }
    
    # plot output
    output$cc_env_sample_size_plot <- renderPlot({
        
        df <- tbl_cc_env() %>% data.frame() %>% mutate(ORe = rownames(.)) %>% 
            reshape2::melt() %>% rename(N = value, Pe = variable) %>% 
            mutate(ORe = as.numeric(ORe), Pe = sub("X", "", Pe)) 
        
        
        # plot
        df %>% ggplot(aes(x = ORe, y = N, color = Pe)) + 
            geom_line() + 
            labs(y = paste0("N case with ", input$ccn_e_k, " controls per case")) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
            theme_minimal()
    })
    
    # observeEvent for brush zoom
    observeEvent(input$cc_env_sample_size_plot1_dblclick, {
        brush <- input$cc_env_sample_size_plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    # click for coordinates
    output$cc_env_sample_size_info <- renderText({
        paste0("Click on plot for data points \n", "ORg=", 
               round(as.numeric(input$cc_env_sample_size_plot_click$x), 2), "\nN=", 
               round(as.numeric(input$cc_env_sample_size_plot_click$y), 2))
    })
    
    
    # case-control gxe ---------------------------------------------------------
    
    # get sample size
    tbl_cc_gxe <- eventReactive(input$ccn_ge_do, {
        
        #input <- list(ccn_ge_k = 1, ccn_ge_prev = "0.05", 
        #              ccn_ge_inherit = "Dominant",
        #              ccn_ge_pg = "0.5", 
        #              #ccn_ge_pg_ll = " ", ccn_ge_pg_ul = " ",
        #              ccn_ge_pe = "0.5",
        #              ccn_ge_pg_ll = "0.25", ccn_ge_pg_ul = "0.5",
        #              ccn_ge_org = "1.1", ccn_ge_ore = "1.1",
        #              ccn_ge_orgxe = "1.1",
        #              ccn_ge_orgxe_ll = "1.1", ccn_ge_orgxe_ul = "1.5",
        #              ccn_ge_org_ll = "1.1", ccn_ge_org_ul = "1.5",
        #              ccn_ge_alpha = "0.05", ccn_ge_beta = "0.8", 
        #              ccn_ge_num_tests = "1")
        
        
        # controls per case
        #num_controls_per_case <- 1 / as.numeric(input$ccn_g_k) - 1
        num_controls_per_case <- as.numeric(input$ccn_ge_k)
        
        # get coef
        #b0 <- plogis(as.numeric(input$ccn_ge_prev))
        b1 <- log(as.numeric(input$ccn_ge_org)) # ORg
        b2 <- log(as.numeric(input$ccn_ge_ore)) # ORe
        
        if (isTruthy(input$ccn_ge_orgxe_ll) & isTruthy(input$ccn_ge_orgxe_ul)) {
            b3 <- log(seq(as.numeric(input$ccn_ge_orgxe_ll),
                          as.numeric(input$ccn_ge_orgxe_ul), by = 0.05))
        } else {b3 <- log(as.numeric(input$ccn_ge_orgxe))}
        
        # P(risk allele) 
        # set p
        if (isTruthy(input$ccn_ge_pg_ll) & isTruthy(input$ccn_ge_pg_ul)) {
            p <- seq(as.numeric(input$ccn_ge_pg_ll),
                      as.numeric(input$ccn_ge_pg_ul), by = 0.05)
        } else {p <- as.numeric(input$ccn_ge_pg) }
        
        q <- 1-p # set q
        if (input$ccn_ge_inherit == "Dominant") {
            p_x <- p^2 + 2*p*q
        } else if (input$ccn_ge_inherit == "Recessive") {
            p_x <- q^2
        } 
        
        # P(E=1)
        #if (isTruthy(input$ccn_ge_pe_ll) & isTruthy(input$ccn_ge_pe_ul)) {
        #    pe <- seq(as.numeric(input$ccn_ge_pe_ll),
        #              as.numeric(input$ccn_ge_pe_ul), by = 0.05)
        #} else {pe <- as.numeric(input$ccn_ge_pe) }
        pe <- as.numeric(input$ccn_ge_pe)
        
        # calculate Var(b3)
        b3_var <- data.frame(sapply(p_x, function(x) {
            get_b3_var_cc(k = 1 / (1 + as.numeric(input$ccn_ge_k)), 
                          p_e = pe, p_g = x, 
                          ORg = exp(b1), ORe = exp(b2), ORgxe = exp(b3))
        }))

        # get sample size
        ccn_ge_n <- data.frame(apply(b3_var, 2, function(x) {
            get_sample_size_cc(alpha = as.numeric(input$ccn_ge_alpha) / 
                                   as.numeric(input$ccn_g_num_tests), 
                               beta = as.numeric(input$ccn_ge_beta), 
                               var = x, b = b3) 
        }))
        colnames(ccn_ge_n) <- as.character(p)
        rownames(ccn_ge_n) <- as.character(round(exp(b3), 2))
        ccn_ge_n <- ceiling(ccn_ge_n * (1 / (1 + as.numeric(input$ccn_ge_k))))
        ccn_ge_n
    })
    
    # print table
    output$cc_gxe_sample_size_tbl <- function() {
        df <- tbl_cc_gxe()
        #kable(df, format = "html")
        kable(df, "html", booktabs = T, align = c("r"),
              caption = "Sample size N") %>%
            kable_styling("striped", full_width = F,
                          position = "left", font_size = 12) %>%
            footnote(general_title = "", 
                     general = c("N is the number of cases required for input parameters", 
                                 paste0("M = ", input$ccn_g_k, 
                                        " control(s) per case"), 
                                 paste0("Pe=", input$ccn_ge_pe), 
                                 paste0("Inheritence: ", input$ccn_ge_inherit),
                                 paste0("alpha=", input$ccn_ge_alpha, 
                                        "; beta=", input$ccn_ge_beta),
                                 paste0("# of independent tests=", 
                                        input$ccn_ge_num_tests))) %>% 
            add_header_above(c("Pg" = ncol(df)+1)) %>% 
            pack_rows( "ORg", 1, nrow(df),
                       bold = T, indent = T, hline_after = F)
    }
    
    #   # plot output
    output$cc_gxe_sample_size_plot <- renderPlot({
        
        # get sample size calcs
        #df <- tbl_cc_gene()
        df <- tbl_cc_gxe() %>% data.frame() %>% mutate(ORg = rownames(.)) %>% 
            reshape2::melt() %>% rename(N = value, Pg = variable) %>% 
            mutate(ORg = as.numeric(ORg), Pg = sub("X", "", Pg)) 
        
        # plot
        df %>% ggplot(aes(x = ORg, y = N, color = Pg)) + 
            #geom_smooth(se = FALSE) + 
            geom_line() + 
            labs(y = paste0("N case with ", input$ccn_g_k, " controls per case"), 
                 x = "ORgxe") +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
            theme_minimal()
    })
    
    # ranges for zooming
    #ranges <- reactiveValues(x = NULL, y = NULL)
    
    # observeEvent for brush zoom
    observeEvent(input$gxe_plot1_dblclick, {
        brush <- input$gxe_plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    # click for coordinates
    output$gxe_info <- renderText({
        paste0("Click on plot for data points \n", "ORgxe=", 
               round(as.numeric(input$plot_click$x), 2), "\nSample Size=", 
               round(as.numeric(input$plot_click$y), 2))
    })
    
    
    
    
    
    
}

# Run the application
shinyApp(ui = ui, server = server)

