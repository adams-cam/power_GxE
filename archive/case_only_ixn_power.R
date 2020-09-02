 

####create fluid row####
fluidRow(
    
    #### put input area here ####
    column(3,
           style = "background-color: #E8E8E8",
           
           ##change the title here
           div(style="display: inline-block; vertical-align:top; text-align:center; width: 100%;",
               strong("Case-only")),
           
           br(),
           
           ##put input boxes here
           
           textInput("cop_n_case", "N case:", width = 60),            
           
           splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("cop_p_g", "Pg:", width = 60),          
                       textInput("cop_p_e", "Pe:", width = 60)),
          
           splitLayout(cellWidths = c("33%", "33%", "33%"), 
                       textInput("cop_or_g", "ORg:", width = 60),          
                       textInput("cop_or_e", "ORe:", width = 60),
                       textInput("cop_or_ge", "ORgxe:", width = 60)),

           splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("cop_alpha", "Alpha:", width = 60),
                       textInput("cop_num_tests", "# Tests:", width = 60)),
           
           br(),
         
           submitButton("Calculate")
           #actionButton("do", "Action")
           
           
    ), ## close column 1
    
   
    #### put output here ####
    column(9, 
           tabsetPanel(
               tabPanel("Output:", 
                        splitLayout(cellWidths = c("40%", "60%"),
                                    tableOutput("co_ixn_power"),
                                    plotOutput("plot_co_ixn_power"))), 
               tabPanel("Help:",withMathJax(), 
                        HTML(markdown::markdownToHTML(knit("case_only_ixn_power.Rmd", 
                                                           quiet = T))))
           ) ## close tabset panel
           
    ) ## close column
    
) ##close fluid row

