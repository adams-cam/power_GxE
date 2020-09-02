 

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
           
           splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccp_n_case", "N case:", width = 60),            
                       textInput("ccp_n_cont", "N control:", width = 60)),
                       
           splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccp_p_g", "Pg:", width = 60),          
                       textInput("ccp_p_e", "Pe:", width = 60)),
          
           splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccp_or_g", "ORg:", width = 60),          
                       textInput("ccp_or_e", "ORe:", width = 60)),
           
           splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccp_or_ge", "ORgxe:", width = 60),
                       textInput("ccp_delta", "OR(G,E):", width = 60)),    
           
           splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccp_alpha", "Alpha:", width = 60),
                       textInput("ccp_num_tests", "# Tests:", width = 60)),
           
           br(),
         
           submitButton("Calculate")
           #actionButton("do", "Action")
           
           
    ), ## close column 1
    
   
    #### put output here ####
    column(8, 
           tabsetPanel(
               tabPanel("Output:", 
                        splitLayout(cellWidths = c("33%", "75%"),
                                    tableOutput("cc_ixn_power"), 
                                    plotOutput("plot_cc_ixn_power", hover = TRUE))
                        ),
               tabPanel("Help:",withMathJax(), 
                        HTML(markdown::markdownToHTML(knit("case_control_ixn_power.Rmd", 
                                                           quiet = T))))
           ) ## close tabset panel
           
    ) ## close column
    
) ##close fluid row

