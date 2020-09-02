# case_control_gxe_sample_size.R

####create fluid row####
fluidRow(
    
    #### put input area here ####
    column(4,
           style = "background-color: #E8E8E8",
           
             ##change the title here
            div(style=paste0("display: inline-block; vertical-align:top;", 
                "text-align:center; width: 100%;"),
                   strong(h3("GxE"))),
           
            br(),
            
            ##put input boxes here

           splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccp_ge_n_case", "N Case", value = "1000", width = 100),
                       textInput("ccp_ge_k", "Controls:Case", value = "1", width = 100)),

            selectInput("ccp_ge_inherit", "Inheritance", 
                       c("Dominant", "Recessive"), #, "Additive"), 
                       width = 100,
                       multiple = FALSE, selected = "Dominant", 
                       size = NULL, selectize = TRUE),
            
            splitLayout(cellWidths = c("34%", "33%", "33%"), 
                       textInput("ccp_ge_pg", "Pg", value = "0.5", width = 60),
                       textInput("ccp_ge_org", "ORg", value = "1.5", width = 60)),
        
            splitLayout(cellWidths = c("33%", "33%"),
                       textInput("ccp_ge_pg_ll", "From:", width = 60), 
                       textInput("ccp_ge_pg_ul", "To:", width = 60)),
           
            splitLayout(cellWidths = c("34%", "33%"), 
                        textInput("ccp_ge_pe", "Pe", value = "0.5", width = 60),
                        textInput("ccp_ge_ore", "ORe", value = "1.5", width = 60)),
           
                        #textInput("ccp_ge_pe_ll", "From:", width = 60), 
                        #textInput("ccp_ge_pe_ul", "To:", width = 60)
            #hr(),
            splitLayout(cellWidths = c("34%", "33%", "33%"), 
                        textInput("ccp_ge_orgxe", "ORgxe", value = "1.1", width = 60),
                        textInput("ccp_ge_orgxe_ll", "From:", width = 60), 
                        textInput("ccp_ge_orgxe_ul", "To:", width = 60)),
            #hr(),
            splitLayout(cellWidths = c("33%", "33%", "33%"), 
                       textInput("ccp_ge_alpha", "Alpha", value = "0.05", width = 60),
                       textInput("ccp_ge_num_tests", "# Tests", value = "1", width = 100)), 

           #br(),
           actionButton("ccp_ge_do", "Submit"),
           br()
           
           
    ), ## close column 1
    
   
    #### put output here ####
    column(8, 
            tabsetPanel(
               tabPanel("Table:", 
                       tableOutput("cc_gxe_power_tbl")),
               tabPanel("Plot:", 
                        plotOutput("cc_gxe_power_plot", 
                                   click = "gxe_power_plot_click", 
                                   dblclick = "gxe_power_plot1_dblclick",
                                   brush = brushOpts(
                                       id = "gxe_power_plot1_brush",
                                       resetOnNew = TRUE
                                   )),
                        verbatimTextOutput("gxe_power_info")
               ),
                tabPanel("Help:", 
                    withMathJax(), 
                    HTML(markdown::markdownToHTML(knit("case_control_gxe_power.Rmd", 
                                                               quiet = T))))
                     )## close tabset panel
           
    ) ## close column
    
) ##close fluid row

