####create fluid row####
fluidRow(
    
    #### put input area here ####
    column(8, HTML(markdown::markdownToHTML(knit("about_page.Rmd", quiet = T)))
    ) ## close column
    
) ##close fluid row