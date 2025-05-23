
## THIS IS SOME NOTES ON WHAT WOULD ESSENTIALLY BE A VERY VERY BASIC REPORT-BUILDER THAT 
## LETS A USER SPECIFY WHICH SETS OF INDICATORS TO SHOW IN THE REPORT
## AND WHAT TO NAME THAT SUBSECTION


# notes on how to select groups of indicators but with a nice name for each group and name(s) defining members of each group, etc.
# example below shows you can have choices show up in groups,

# *** but still need a way to let the user rename each group ***

# AND still need a way to let the user specify the members of each group or pick some not all etc.

# and then server can evaluate the input as R code for a named list 
# as is needed for the extratable_list_of_sections param of build_community_report()
# but it would be a reactive value not an input if done like this
#################################################################### #

# SECOND EXAMPLE


# doing this way is not using the list actually, each list is 1 element long, quoted R code:

# in global_defaults_*.R :

default_extratable_list_of_sections <- list(
  
  `Breakdown by Race/Ethnicity` = "names_d_subgroups",
  `Language Spoken at Home` = "names_d_language",
  `Language in Limited English Speaking Households` = "names_d_languageli",
  `Breakdown by Sex` = "c('pctmale','pctfemale')",
  `Health` = "names_health",
  `Age` = "c('pctunder5', 'pctunder18', 'pctover64')",
  `Community` = "names_community[!(names_community %in% c( 'pctmale', 'pctfemale', 'pctownedunits_dupe'))]",
  `Poverty` = "names_d_extra" #,  # pctpoor
  # `Site counts and distance` = "names_e_other"
)
################################## # 

shiny::shinyApp(
  ################################## # 
  ui = shiny::fluidPage(
    
    # for advanced tab, a way to specify different choices of indicators to show in extra table:
    uiOutput("extratable_list_of_sections_code_ui"),
    
    # info just for here, to show what the list would look like
    shiny::verbatimTextOutput("result")
  ),
  ################################## # 
  server = function(input, output) {
    
    # the server will call build_community_report() with param  extratable_list_of_sections = extratable_list_of_sections()
    # where extratable_list_of_sections() reactive gets built as a list, from the selections provided by input$extratable_list_of_sections_code
    
    section_choices_allowed <- reactiveVal(default_extratable_list_of_sections)
    section_choices_allowed_labels <- reactiveVal()
    section_choices_allowed_varnames_text <- reactiveVal()
    
    section_choices_picked <- reactiveVal(default_extratable_list_of_sections)
    section_choices_picked_labels <- reactiveVal()
    section_choices_picked_varnames_text <- reactiveVal()
    
    extratable_list_of_sections <- reactive({
      
      mylist <- list(input$extratable_list_of_sections_code)
      names(mylist) <- 
        
        # section_choices_allowed <- 0
        # 
        # section_choices(
        #   section_choices_allowed
        # )
        # section_choices_labels(names(section_choices))
        # section_choices_varnames_text(as.vector(unlist(section_choices)))
        
        
        
    })
    
    
    output$extratable_list_of_sections_code_ui <- renderUI({
      
      shiny::selectizeInput("extratable_list_of_sections_code", "Choose Groups of Indicators:",
                            selections = section_choices_picked(),
                            choices = section_choices_allowed(),
                            multiple = TRUE
      )
    })
    
    output$result <- shiny::renderPrint({
      printable_version <- paste0(
        paste0(
          section_choices_allowed_labels(), " = ",
          input$extratable_list_of_sections_code
        ), 
        collapse = ",\n ")
      
      cat(
        paste("We can turn it into a named list: \n\n selectionslist = list(\n", printable_version, "\n)" ))
    })
    
    
  }
)
