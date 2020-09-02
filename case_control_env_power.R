

####create fluid row####
fluidRow(
    
    #### put input area here ####
    column(4,
            style = "background-color: #E8E8E8",
           
           ##change the title here
            div(style=paste0("display: inline-block; vertical-align:top; ", 
                            "text-align:center; width: 100%;"),
               strong(h3("Environment Effect"))),
           
            br(),
           
            ##put input boxes here
           
            splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccp_e_n_case", "N case", value = "1000", width = 100),
                       textInput("ccp_e_k", "Controls:Case", value = "1", 
                                 width = 1000)),
           
            splitLayout(cellWidths = c("34%", "33%", "33%"), 
                       textInput("ccp_e_pg", "Pe", value = "0.5", width = 60),
                       textInput("ccp_e_pg_ll", "From:", width = 60), 
                       textInput("ccp_e_pg_ul", "To:", width = 60)),

            splitLayout(cellWidths = c("34%", "33%", "33%"), 
                        textInput("ccp_e_org", "ORe", value = "1.1", width = 60),
                        textInput("ccp_e_org_ll", "From:", width = 60), 
                        textInput("ccp_e_org_ul", "To:", width = 60)),
  
            textInput("ccp_e_alpha", "Alpha", value = "0.05", width = 60),
        
            textInput("ccp_e_num_tests", "# Tests", value = "1", width = 100),
           
            #br(),
         
           #submitButton("Calculate")
           actionButton("ccp_e_do", "Submit")
           
           
    ), ## close column 1
    
   
    #### put output here ####
    column(8, 
            tabsetPanel(
               tabPanel("Table:", 
                       tableOutput("tbl_cc_env_power")),
               tabPanel("Plot:", 
                       plotOutput("cc_env_power_plot", click = "cc_env_power_plot_click", 
                                  dblclick = "cc_env_power_plot1_dblclick",
                                  brush = brushOpts(
                                      id = "cc_env_power_plot1_brush",
                                      resetOnNew = TRUE
                                  )),
                       verbatimTextOutput("cc_env_power_info")),
                tabPanel("Help:", 
                    withMathJax(), 
                    HTML(markdown::markdownToHTML(knit("case_control_env_power.Rmd", 
                                                               quiet = T))))
                     )## close tabset panel
           
    ) ## close column
    
) ##close fluid row

