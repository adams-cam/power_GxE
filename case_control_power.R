# case_control_ixn_sample_size.R

####create fluid row####
fluidRow(
    
    #### put input area here ####
    column(4,
            style = "background-color: #E8E8E8",
           
           ##change the title here
            div(style=paste0("display: inline-block; vertical-align:top; ", 
                            "text-align:center; width: 100%;"),
               strong(h3("Genetic Effect"))),
           
            br(),
           
            ##put input boxes here
           
            splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccp_g_n_case", "N case", value = "1000", width = 100),
                       textInput("ccp_g_k", "Controls:Case", value = "1", 
                                 width = 1000)),
           
            selectInput("ccp_g_inherit", "Inheritance", 
                       c("Dominant", "Recessive"), #, "Additive"), 
                       width = 100,
                       multiple = FALSE, selected = "Dominant", 
                       size = NULL, selectize = TRUE),
           
            splitLayout(cellWidths = c("34%", "33%", "33%"), 
                       textInput("ccp_g_pg", "Pg", value = "0.5", width = 60),
                       textInput("ccp_g_pg_ll", "From:", width = 60), 
                       textInput("ccp_g_pg_ul", "To:", width = 60)),

            splitLayout(cellWidths = c("34%", "33%", "33%"), 
                        textInput("ccp_g_org", "ORg", value = "1.1", width = 60),
                        textInput("ccp_g_org_ll", "From:", width = 60), 
                        textInput("ccp_g_org_ul", "To:", width = 60)),
  
            textInput("ccp_g_alpha", "Alpha", value = "0.05", width = 60),
        
            textInput("ccp_g_num_tests", "# Tests", value = "1", width = 100),
           
            #br(),
         
           #submitButton("Calculate")
           actionButton("ccp_g_do", "Submit")
           
           
    ), ## close column 1
    
   
    #### put output here ####
    column(8, 
            tabsetPanel(
               tabPanel("Table:", 
                       tableOutput("tbl_cc_gene_power")),
               tabPanel("Plot:", 
                       plotOutput("cc_power_plot", click = "cc_power_plot_click", 
                                  dblclick = "cc_power_plot1_dblclick",
                                  brush = brushOpts(
                                      id = "cc_power_plot1_brush",
                                      resetOnNew = TRUE
                                  )),
                       verbatimTextOutput("cc_power_info")),
                tabPanel("Help:", 
                    withMathJax(), 
                    HTML(markdown::markdownToHTML(knit("case_control_power.Rmd", 
                                                               quiet = T))))
                     )## close tabset panel
           
    ) ## close column
    
) ##close fluid row

