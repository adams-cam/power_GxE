# case_control_ixn_sample_size.R

####create fluid row####
fluidRow(
    
    #### put input area here ####
    column(4,
           style = "background-color: #E8E8E8",
           
           ##change the title here
           div(style="display: inline-block; vertical-align:top; text-align:center; width: 100%;",
               strong(h3("Genetic Effect"))),
           
           br(),
           
           ##put input boxes here
           
           
            splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccn_e_k", "Controls:Case", value = "1", width = 100)),
                     
            splitLayout(cellWidths = c("34%", "33%", "33%"), 
                       textInput("ccn_e_pe", "Pe", value = "0.5", width = 60),
                       textInput("ccn_e_pe_ll", "From:", width = 60), 
                       textInput("ccn_e_pe_ul", "To:", width = 60)),

            splitLayout(cellWidths = c("34%", "33%", "33%"), 
                        textInput("ccn_e_ore", "ORe", value = "1.1", width = 60),
                        textInput("ccn_e_ore_ll", "From:", width = 60), 
                        textInput("ccn_e_ore_ul", "To:", width = 60)),
  
            splitLayout(cellWidths = c("50%", "50%"), 
                       textInput("ccn_e_alpha", "Alpha", value = "0.05", width = 60),
                       textInput("ccn_e_beta", "Beta", value = "0.80", width = 60)), 
           textInput("ccn_e_num_tests", "# Tests", value = "1", width = 100),
           
           #br(),
         
           #submitButton("Calculate")
           actionButton("ccn_e_do", "Submit")
           
           
    ), ## close column 1
    
   
    #### put output here ####
    column(8, 
            tabsetPanel(
               tabPanel("Sample Size:", 
                       tableOutput("cc_env_sample_size_tbl")),
               tabPanel("Plot:", 
                       plotOutput("cc_env_sample_size_plot", 
                                  click = "cc_env_sample_size_plot_click", 
                                  dblclick = "cc_env_sample_size_plot1_dblclick",
                                  brush = brushOpts(
                                      id = "cc_env_sample_size_plot1_brush",
                                      resetOnNew = TRUE
                                  )),
                       verbatimTextOutput("cc_env_sample_size_info")),
                tabPanel("Help:", 
                    withMathJax(), 
                    HTML(markdown::markdownToHTML(knit("case_control_sample_size.Rmd", 
                                                               quiet = T))))
                     )## close tabset panel
           
    ) ## close column
    
) ##close fluid row

