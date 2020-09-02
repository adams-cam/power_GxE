# case_control_ixn_sample_size.R

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
           
           textInput("ccn_k", "% Case", width = 60),

           splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccn_p_g", "Pg", width = 60),          
                       textInput("ccn_p_e", "Pe", width = 60)),
          
           splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccn_or_g", "ORg", width = 60),          
                       textInput("ccn_or_e", "ORe", width = 60)),
           
           splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccn_or_ge", "ORgxe", width = 60),
                       textInput("ccn_delta", "OR(G,E)", width = 60)),          
           
           splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccn_alpha", "Alpha", width = 60),
                       textInput("ccn_beta", "Beta", width = 60)), 
           textInput("ccn_num_tests", "# Tests", width = 100),
           
           br(),
         
           submitButton("Calculate")
           #actionButton("do", "Action")
           
           
    ), ## close column 1
    
   
    #### put output here ####
    column(8, 
            tabsetPanel(
               tabPanel("Output:", 
                        splitLayout(cellWidths = c("33%", "75%"), 
                                    tableOutput("cc_ixn_sample_size"),
                                    plotOutput("plot_cc_ixn_sample_size", 
                                               hover = TRUE))), 
            tabPanel("Help:", 
                    withMathJax(), 
                            HTML(markdown::markdownToHTML(knit("case_control_ixn_sample_size.Rmd", 
                                                               quiet = T))))
                     )## close tabset panel
           
    ) ## close column
    
) ##close fluid row

