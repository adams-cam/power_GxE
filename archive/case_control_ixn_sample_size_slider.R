 

####create fluid row####
fluidRow(
    
    #### put input area here ####
    column(3,
           style = "background-color: #E8E8E8",
           
            ##change the title here
            div(style="display: inline-block; vertical-align:top; text-align:center; width: 100%;",
               strong("Case-Control")),
            
            br(),
            
            ##put input boxes here
           
            numericInput("ccns_k", "% Case", value = NULL, width = 75, 
                        min = 0.1, max = 0.99, step = 0.01),
           
            splitLayout(cellWidths = c("50%", "50%"), 
                       numericInput("ccns_p_g", "Pg", value = 0.5, width = 75, 
                                   min = 0.1, max = 0.99, step = 0.01),
                       numericInput("ccns_p_e", "Pe", value = 0.5, width = 75, 
                                   min = 0.1, max = 0.99, step = 0.01)),
            
            splitLayout(cellWidths = c("50%", "50%"), 
                       numericInput("ccns_or_g", "ORg", value = 1.2, width = 75, 
                                   min = 0.1, max = 5, step = 0.01),
                       numericInput("ccns_or_e", "ORe", value = 1.5, width = 75, 
                                   min = 0.1, max = 5, step = 0.01)),
           
            sliderInput("ccns_or_ge", "ORgxe", value = 1.5, width = 150, 
                    min = 0.25, max = 5, step = 0.01),
            
            numericInput("ccns_delta", "(G,E)", value = 1, width = 75, 
                    min = 0.5, max = 1.5, step = 0.01),
            
            splitLayout(cellWidths = c("50%", "50%"), 
                    numericInput("ccns_alpha", "Alpha", value = 0.05, width = 75, 
                                 min = 0.1, max = 0.99, step = 0.01),
                    numericInput("ccns_beta", "Beta", value = 0.80, width = 75, 
                                 min = 0.1, max = 0.99, step = 0.01)),
            numericInput("ccns_num_tests", "# Tests", value = 1, width = 100, 
                     min = 1, max = 1e7, step = 1),
           
           br(),
         
           submitButton("Calculate")
           #actionButton("ccns_do", "Action")
           
           
    ), ## close column 1
    
   
    #### put output here ####
    column(9, 
            tabsetPanel(
               tabPanel("Output:",  tableOutput("cc_ixn_sample_size_slider")
                        #splitLayout(cellWidths = c("40%", "60%"), 
                        #           tableOutput("cc_ixn_sample_size_slider"),
                        #            plotOutput("plot_cc_ixn_sample_size_slider"))
                        ),
            tabPanel("Help:", 
                    withMathJax(), 
                            HTML(markdown::markdownToHTML(knit("case_control_ixn_sample_size.Rmd", 
                                                               quiet = T))))
                     )## close tabset panel
           
    ) ## close column
    
) ##close fluid row

