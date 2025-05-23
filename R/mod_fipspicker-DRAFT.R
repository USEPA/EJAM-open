####################################################################### # 
# UI for selecting census units
# e.g., a few cities, all counties in a state, or all states in a Region
####################################################################### # 


##################################################################################### # 
# # DEFAULTS for fipspicker module - see global_defaults_*.R ####
# # 
# fipspicker_fips_type2pick_default <- "Cities or Places" # "Counties"   # default= they want to pick... cities/places?
# # choices = c(
# #   # `EPA Regions` = "EPA Regions", 
# #   States = "States", 
# #   Counties = "Counties", 
# #   `Cities/Places` = "Cities or Places"
# # )
# # Limits height of pulldown list of possible choices, but does NOT limit the number of selections shown in selectize box!
# fipspicker_maxOptions_default_states_picked  <- 255
# fipspicker_maxOptions_default_counties_picked <- 255
# fipspicker_maxOptions_default_cities_picked <- 255
# 
# fipspicker_all_regions_button_defaultchecked         <- TRUE  # Ignored by reset though?
# fipspicker_all_states_button_defaultchecked  <- TRUE  # Ignored by reset though?
# fipspicker_all_counties_button_defaultchecked <- FALSE # at least want this if cities are being picked...  
# 
# fipspicker_all_regions_button_defaultshow  <- FALSE # show at reset/start 
# fipspicker_all_states_button_defaultshow   <- FALSE # show (at reset? or ) if fips_type2pick is this or smaller (states, counties, or places)
# fipspicker_all_counties_button_defaultshow <- FALSE # show (at reset? or ) if fips_type2pick is this or smaller (counties or places)
##################################################################################### # 

############################################### #
# try as a modalDialog ?  ####
# it does not run as a mini shiny app module inside a modal like this, though...
# 
# xyz = shinyApp(
#   ui = basicPage(
#     actionButton("show_fipspicker", "show fipspicker UI in a window")
#   ),
#   server = function(input, output) {
#     fipspicker_modal <- function() {
#       modalDialog( 
# c(
#   fipspicker_module_ui("TESTID"),
#         footer = tagList(
#           modalButton("Cancel"),
#           actionButton("ok", "OK")
#         )
#         )
# )
#     }
#     # Show modal when button is clicked.
#     observe({
#       showModal(fipspicker_modal())
#     }) %>% bindEvent(input$show_fipspicker)
#   }
# )

############################################### #
# .-------------- TESTAPP .-------------- ####

## try modules in a TESTAPP  ####

#    fipspicker_testapp()     # IS A WAY TO LAUNCH THIS TEST APP 

fipspicker_testapp = function(request = "", testing_this_module = TRUE, showtable = TRUE) {
  
  shinyApp(ui = fipspicker_testapp_ui(request = request, showtable = showtable), 
           server = fipspicker_testapp_server, options = list(test.mode = testing_this_module))
}
############################################### #

################################################ 

# .-------------- GADGET .-------------- ####

# fipspicker_gadget() is to try ui/server in a shiny "Gadget" - 

# A gadget lets you show the app in a dialog pane, sort of like a modal dialog

fipspicker_gadget = function(request = "", testing_this_module = TRUE) {
  
  # would need a Done/Cancel button in footer
  
  shiny::runGadget(
    shinyApp(ui = fipspicker_testapp_ui(request = request, showtable = TRUE), 
             server = fipspicker_testapp_server, options = list(test.mode = testing_this_module)),
    viewer = dialogViewer(dialogName = "Select Census Units",
                          height = 1000, width = 1000)
  )
}
############################################### #

## fipspicker TESTAPP UI function 
## (UI of an outer app shell for testing the module)

fipspicker_testapp_ui <- function(request, showtable = TRUE) {
  
  shiny::fluidPage(
    # shiny::h2('Search/Select Places'),
    
    ### THIS WOULD GO IN THE UI OF A PARENT APP: BUT WITHOUT showtable 
    
    fipspicker_module_ui("TESTID", showtable = showtable), 
    
    # shiny::actionButton(inputId =  "testbutton", label = "Done"),
    
    ###NOTE: output of module is a reactive object that can be used in the parent app!
    # h3("Example of a DataTable sortable/filterable table in the parent app"),
    # DT::DTOutput('fipstableout'),
    
    br()
  )
  
}
############################################### #

# fipspicker TESTAPP SERVER function 
# (server of an outer app shell for testing the module)

fipspicker_testapp_server <- function(input, output, session, testing_this_module = NULL) {
  
  if (missing(testing_this_module)) {
    if (is.null(options()$test.mode)) {testing_this_module <- TRUE} else {testing_this_module <- options()$test.mode}
  }
  
  ## Send reactive to module that can use or update it
  ## can pass  a reactive reactive_data1() , but must pass it with with NO parens
  ## then reactive object fipstabledata is output of module, updated when module changes it
  
  
  ### THIS WOULD GO IN THE SERVER OF A PARENT APP: 
  
  fipstabledata <- fipspicker_module_server(id = "TESTID", 
                                            testing_this_module = testing_this_module,
                                            reactdat = reactive("") # can pass reactive param
  )
  output$fipstableout <- DT::renderDT(fipstabledata())
  
} # end of test server
############################################### #



######################################################### #   ######################################################### # 

# .-------------- MODULE UI .-------------- ####

######################################################### #   ######################################################### # 

fipspicker_module_ui <- function(id, showtable = FALSE) {
  
  ns <- NS(id)
  
  # .-------------- UI = fipspicker_ui -------------------  #
  
  pickers <- tagList(
    radioButtons(inputId = ns("fips_type2pick"), label = "What kinds of areas do you want to use?", #  compare or download 
                 selected = EJAM:::global_or_param("fipspicker_fips_type2pick_default"),
                 inline = TRUE,
                 choices = EJAM:::global_or_param("fipspicker_fips_type2pick_choices_default") 
                 #   c(
                 #   # `EPA Regions` = "EPA Regions", 
                 #   States = "States", 
                 #   Counties = "Counties", 
                 #   `Cities/Places` = "Cities or Places"
                 # )
    ),
    actionButton(inputId = ns("reset_button"), label = "Clear all selections/ Reset"),
    
    checkboxInput(inputId = ns("all_regions_button"), "All EPA Regions?", value = EJAM:::global_or_param("fipspicker_all_regions_button_defaultchecked")),
    checkboxGroupInput(inputId = ns("regions_picked"), label = "EPA Region", width = '100%',
                       selected = NULL, # set in server
                       choices = 1:10,  # regions_table is available in server not in UI function # all_regions_choices, # sort(unique(cities_table$eparegion)), 
                       inline = TRUE),
    
    checkboxInput(inputId = ns("all_states_button"), "All States?", value = EJAM:::global_or_param("fipspicker_all_states_button_defaultchecked")),
    selectizeInput(inputId = ns("states_picked"), label = "Select State", 
                   selected = "",
                   choices = NULL, # unique(cities_table$ST),
                   options = list(maxOptions = EJAM:::global_or_param("fipspicker_maxOptions_default_states_picked"), closeAfterSelect = TRUE),
                   multiple = T),
    
    checkboxInput(inputId = ns("all_counties_button"), "All Counties?", value = EJAM:::global_or_param("fipspicker_all_counties_button_defaultchecked")),
    selectizeInput(inputId = ns("counties_picked"), label = "Select Counties", 
                   selected = "",
                   # options = list(maxOptions = 254, closeAfterSelect = TRUE), # no limit by default. TX has the most of any 1 state, 254 Counties. But all in US >3k.
                   options = list(maxOptions = EJAM:::global_or_param("fipspicker_maxOptions_default_counties_picked")), # to avoid displaying so many counties if you click "all in state" just to be able to query all places in state by name typed
                   choices = NULL, # unique(cities_table$countyname_ST),
                   multiple = T),
    
    checkboxInput(inputId = ns("all_cities_button"), "All Cities/Townships/Census Designated Places? [not enabled]", value = FALSE),
    selectizeInput(inputId = ns("cities_picked"), label = "Select Cities/Places",  
                   selected = "", 
                   choices = NULL,       ## (40,000 PLACES !)# loads faster if NULL and then update it via server  . to cities_table$placename,
                   options = list(maxOptions = EJAM:::global_or_param("fipspicker_maxOptions_default_cities_picked"), closeAfterSelect = TRUE),
                   multiple = T)
  )
  #################################### # 
  
  fipspicker_ui <- tagList(
    shinyjs::useShinyjs(),
    titlePanel("Geographic Areas Picker - States, Counties, and Cities/Places"),
    shiny::h5("Select places by name or from a menu"),
    
    sidebarLayout(
      sidebarPanel(
        pickers
      ),
      ##  Show table of selected locations 
      mainPanel(
        conditionalPanel(condition = "showtable == 'TRUE'", 
                         shiny::downloadButton(outputId = ns("download")),
                         tableOutput(outputId = ns("fips_table"))
        ) 
      )
    )
  )
  
  if (showtable) {
    return(fipspicker_ui)
    
  } else {
    return(
      tagList(
        shinyjs::useShinyjs(),
        shiny::h5("Select places by name or from a menu"),
        pickers
      )
    )
  }
}
######################################################### #  

# .-------------- MODULE SERVER .-------------- ####

fipspicker_module_server <- function(id, testing_this_module = FALSE, reactdat, ...) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # .-------------- SERVER = fipspicker_server -------------------  ### #
    
    # observe({
    #   req(reactdat())
    #   ## can do something with value of that parameter that was passed as a reactive
    # })    
    
    ## maybe don't wrap in a function here - it seems to need id to be in its environment, and unsure how to pass that if func is defind elsewhere, and unsure what to do with session if anything 
    
    fipspicker_server <- function(input, output, session, id, testing_this_module = FALSE, ...) {
      
      # ns <- NS(id)
      ################### #
      # do UI functions done within server  need id wrapped in ns() ??
      ### all had been like  show(ns("  or   hide(ns("   or   InputID = ns("
      # ns <- NS(id)  ???
      # ns <- session$ns      # ??? if used in a module?
      # ns <- function(x) {x} # ??? if not used in a module?
      ################### #
      
      # warning( " work in progress")
      if (testing_this_module) {
        options(shiny.testmode = TRUE)
        on.exit(options(shiny.testmode = FALSE))
        cat("Starting data tables setup \n")
      }
      
      # library(shiny); library(data.table)
      
      # DATA TABLES SETUP - Tables of places, counties, states, regions info ####
      
      states_table <- stateinfo2[stateinfo2$ST != "US", c("statename", "FIPS.ST", "ST", "REGION", "is.usa.plus.pr", 
                                                          "is.state", "is.contiguous.us", "is.island.areas")] # etc. etc.
      all_states_choices        <- states_table$ST
      names(all_states_choices) <- states_table$statename
      
      regions_table <- data.frame(
        REGION = 1:10,
        Statecount = as.vector(table(states_table$REGION)), 
        States = sapply(1:10, function(n) paste0(states_table$ST[states_table$REGION %in% n], collapse = ", "))
      )
      all_regions_choices = sort(regions_table$REGION)
      
      cities_table = data.table(censusplaces)[, .(eparegion, ST, countyname, placename, fips)]
      cities_table$countyname_ST <- paste0(cities_table$countyname, ", ", cities_table$ST)
      
      all_cities_withcountyname_choices = cities_table$fips
      names(all_cities_withcountyname_choices) <- paste0(cities_table$placename, ", ", cities_table$countyname_ST, " (", all_cities_withcountyname_choices,")")
      
      counties_table <- unique(cities_table[, .(eparegion, ST, countyname_ST)])
      counties_table$countyfips <- fips_counties_from_countyname(counties_table$countyname_ST)
      
      all_counties_choices <- counties_table$countyfips
      names(all_counties_choices) <- counties_table$countyname_ST
      
      # *** Note cities_table (from censusplaces) and blockgroupstats have slightly 
      # different county name lists,
      # and also lower case "city" is "City" in censusplaces.
      
      # counties_table <- unique(data.frame(
      #   stfips = substr(blockgroupstats$bgfips,1,2),
      #   ST = blockgroupstats$ST,
      #   countyfips = substr(blockgroupstats$bgfips,1,5), 
      #   countyname = gsub(" city$", " City", blockgroupstats$countyname)
      # ))
      # counties_table$countyname_ST = paste0( counties_table$countyname, ", ",  counties_table$ST)
      
      #  counties_table$countyname_ST
      
      ## Note SOME CITIES/PLACES OVERLAP WITH 2+ COUNTIES !!  
      
      if (testing_this_module) {cat("Finished data setup \n")}
      ######################################################### #   ######################################################### # 
      ######################################################### #   ######################################################### # 
      # ~ ####
      ######################################################### #   ######################################################### # 
      # INITIAL view ####
      
      if (testing_this_module) {cat("setting initial view in UI \n")}
      # app should do initial setup because 
      #  UI could but does not here generally specify initial choices and initial selected=
      #  and app launch triggers the reset button upon launch (unless the button was set to ignore initial)
      #  and observers do that too
      #
      ## these get reset by the reset button (which triggers at launch?), but ok to have them here to start with also
      
      if (EJAM:::global_or_param("fipspicker_all_regions_button_defaultchecked")) {
        regions_reset = all_regions_choices
      } else {
        regions_reset = NULL # or this could be some set of initial default selections but not needed
      }
      updateCheckboxGroupInput(session, inputId = ("regions_picked"), inline = TRUE, 
                               choices = all_regions_choices, selected = regions_reset)
      # but then reset amd/or default choice of fips_type2pick updates these 
      shinyjs::hide(("all_regions_button"))
      shinyjs::hide(("regions_picked"))
      shinyjs::hide(("all_states_button"))
      shinyjs::hide(("states_picked"))
      shinyjs::hide(("all_counties_button"))
      shinyjs::hide(("counties_picked"))
      shinyjs::hide(("all_cities_button"))
      shinyjs::hide(("cities_picked"))  
      
      shinyjs::hide(("reset_button"))   ### HIDE TO SIMPLIFY FOR NOW ***
      shinyjs::show(("fips_type2pick")) ### HIDE TO SIMPLIFY FOR NOW ? ***
      
      # TRIGGER RESET BUTTON HOW? tried  bindEvent with ignoreInit = F, ignoreNULL = F
      
      if (testing_this_module) {cat("finished all setup code \n ------------------------------------------ \n")}
      ######################################################### # 
      
      # RESET Button ####
      
      # Reset to default starting view as found in ui and server 
      #      but preserve whatever "fips_type2pick" has been specified, like States, or Counties
      
      observe({ # uses bindEvent so it only updates if reset button is pressed, not when various other inputs change
        if (testing_this_module) {cat("\n\nRESET BUTTON \n")}
        
        ## ---------- REGIONS RESET ####
        
        # show and check/uncheck the "ANY/ALL regions" button
        if (EJAM:::global_or_param("fipspicker_all_regions_button_defaultshow")) {
          shinyjs::show(("all_regions_button"))
        } else {
          shinyjs::hide(("all_regions_button")) # 
        }
        updateCheckboxInput(session = session, inputId = ("all_regions_button"),  
                            value = EJAM:::global_or_param("fipspicker_all_regions_button_defaultchecked"))
        # reset which regions are available (all_regions_choices) and selected
        if (EJAM:::global_or_param("fipspicker_all_regions_button_defaultchecked")) {
          regions_reset = all_regions_choices
        } else {
          regions_reset = NULL
        }
        updateCheckboxGroupInput(session, inputId = ("regions_picked"), 
                                 choices = all_regions_choices, 
                                 selected = regions_reset,
                                 inline = TRUE)  
        if (EJAM:::global_or_param("fipspicker_all_regions_button_defaultchecked")) {
          shinyjs::hide(("regions_picked")) 
        } else {
          if (EJAM:::global_or_param("fipspicker_fips_type2pick_default") == "EPA Regions") {
            shinyjs::show(("regions_picked"))
          } else {
            shinyjs::hide(("regions_picked")) 
          }
        }
        ## ---------- STATES RESET ####
        
        # show and check/uncheck the "ANY/ALL states" box
        if (EJAM:::global_or_param("fipspicker_all_states_button_defaultshow")) {
          shinyjs::show("all_states_button") 
        } else {
          shinyjs::hide("all_states_button")
        }
        # reset which states are available (states_choices) and selected
        inregions <- states_table$REGION %in% regions_reset
        if (length(inregions) == 0 || all(inregions == FALSE)) {
          states_choices <- NULL
        } else {
          states_choices <- all_states_choices[inregions]
        }
        updateCheckboxInput(session = session, inputId = ("all_states_button"),  
                            value = EJAM:::global_or_param("fipspicker_all_states_button_defaultchecked"))        
        if (EJAM:::global_or_param("fipspicker_all_states_button_defaultchecked")) {
          states_reset = as.vector(states_choices) # all available in regions
        } else {
          states_reset = "" # none at all
        }
        updateSelectizeInput(session = session, inputId = ("states_picked"),  server = TRUE, 
                             choices = states_choices,
                             selected = states_reset, 
                             options = list(maxOptions = EJAM:::global_or_param("fipspicker_maxOptions_default_states_picked"), closeAfterSelect = TRUE)) # 
        if (EJAM:::global_or_param("fipspicker_all_states_button_defaultchecked")) {
          shinyjs::hide(("states_picked")) 
        } else {
          if (EJAM:::global_or_param("fipspicker_fips_type2pick_default") == "States") {
            shinyjs::show(("states_picked"))
          } else {
            shinyjs::hide(("states_picked"))
          }
        }
        ## ---------- COUNTIES RESET ####
        
        # show and check/uncheck the "ANY/ALL counties" box
        if (EJAM:::global_or_param("fipspicker_all_counties_button_defaultshow")) {
          shinyjs::show("all_counties_button") 
        } else {
          shinyjs::hide("all_counties_button") 
        }
        updateCheckboxInput(session = session, inputId = ("all_counties_button"), 
                            value = EJAM:::global_or_param("fipspicker_all_counties_button_defaultchecked"))
        # reset which counties are available (counties_choices) and selected
        
        instates  <- counties_table$ST %in% states_reset
        # counties_choices <- counties_table$countyfips[instates]
        # names(counties_choices) <- counties_table$countyname_ST[instates]
        
        counties_choices <- all_counties_choices[instates]
        if (EJAM:::global_or_param("fipspicker_all_counties_button_defaultchecked")) {
          counties_reset = as.vector(counties_choices) # all available 
        } else {
          counties_reset = ""
        }
        updateSelectizeInput(session = session, inputId = ("counties_picked"), server = TRUE, 
                             choices = counties_choices, # probably all
                             selected = counties_reset,  # probably none
                             options = list(maxOptions = EJAM:::global_or_param("fipspicker_maxOptions_default_counties_picked"), closeAfterSelect = TRUE) 
        )
        if (EJAM:::global_or_param("fipspicker_all_counties_button_defaultchecked")) {
          shinyjs::hide(("counties_picked"))
        } else {
          if (EJAM:::global_or_param("fipspicker_fips_type2pick_default") == "Counties") {
            shinyjs::show(("counties_picked"))
          } else {
            shinyjs::hide(("counties_picked"))
          }
        }
        ## ---------- CITIES RESET ####
        
        # check/uncheck the "ANY/ALL cities" box
        shinyjs::hide(("all_cities_button")) # not a useful checkbox - too many places in just 1 county
        updateCheckboxInput(session = session, inputId = ("all_cities_button"), 
                            value = FALSE)
        ## reset which cities/places are available and selected
        # it is complicated to filter cities by county/state/region since FIPS differ in format
        # and it is done in an observer below but does not have to be done here actually for reset button purposes
        updateSelectizeInput(session = session, inputId = ("cities_picked"), server = TRUE, 
                             choices = all_cities_withcountyname_choices,
                             selected = "", 
                             options = list(maxOptions = EJAM:::global_or_param("fipspicker_maxOptions_default_cities_picked"), closeAfterSelect = TRUE) )
        if (EJAM:::global_or_param("fipspicker_fips_type2pick_default") == "Cities or Places") {
          shinyjs::show(("cities_picked"))
        } else { 
          shinyjs::hide(("cities_picked"))
        }
        
        ## ------------- finish reset
        
        # show/hide the pickers? done above but ideally These SHOULD get hidden/shown by the observer of input$fips_type2pick
        ##   maybe hide/show depends on what default you reset to, eg do not hide any if resets back to "Cities or Places"
        shiny::updateActionButton(inputId = "fips_type2pick", disabled = TRUE) # Try to trigger it so observers of it will update things
        shiny::updateActionButton(inputId = "fips_type2pick", disabled = FALSE)
        ## try to trigger the other observers
        req(input$fips_type2pick)
        req(input$all_regions_button)
        req(input$all_states_button)
        req(input$all_counties_button)
        req(input$all_cities_button)
        req(input$regions_picked)
        req(input$states_picked)
        req(input$counties_picked)
        req(input$cities_picked)
        
        # show/hide the "ANY/ALL" buttons when reset (? or just show/hide based on scope of fips_type2pick?)
        # if (fipspicker_all_regions_button_defaultshow)         {shinyjs::show(("all_regions_button"))}         else {shinyjs::hide(("all_regions_button"))}
        # if (fipspicker_all_states_button_defaultshow)  {shinyjs::show(("all_states_button"))}  else {shinyjs::hide(("all_states_button"))} ############## #
        # if (fipspicker_all_counties_button_defaultshow) {shinyjs::show(("all_counties_button"))} else {shinyjs::hide(("all_counties_button"))} ############## #
        # shinyjs::hide(("all_cities_button"))
        cat("...done with RESET \n\n")
      }, priority = 10) |>
        bindEvent(input$reset_button, ignoreInit = TRUE) # not trigger at launch
      ######################################################### # 
      
      # TYPE OF AREA BUTTON - fips_type2pick picked, so SHOW/HIDE pickers for different geo scales/levels ####
      
      ## and values selected would get updated elsewhere... 
      ## but would this trigger those observers? ***
      
      observe({ # radio button was clicked for choice of fips_type2pick
        
        if (testing_this_module) {cat("\nObserved radio button was clicked for choice of fips_type2pick \n")}
        
        ### > basic starting point when change fips_type: ####
        
        ########################################## #
        # regions
        if (EJAM:::global_or_param("fipspicker_all_regions_button_defaultshow")) {
          shinyjs::show(("all_regions_button"))
        } else {
          shinyjs::hide(("all_regions_button"))
        }        
        updateCheckboxInput(session, inputId = "all_regions_button", value = EJAM:::global_or_param("fipspicker_all_regions_button_defaultchecked"))
        updateCheckboxGroupInput(session, inputId = "regions_picked", choices = 1:10, selected = all_regions_choices) # ALL
        shinyjs::hide(("regions_picked"))
        ########################################## #
        # states
        if (EJAM:::global_or_param("fipspicker_all_states_button_defaultshow")) {
          shinyjs::show(("all_states_button"))
        } else {
          shinyjs::hide(("all_states_button"))
        }
        updateCheckboxInput(session, inputId = ("all_states_button"), value = EJAM:::global_or_param("fipspicker_all_states_button_defaultchecked"))
        updateSelectizeInput(session, inputId = "states_picked", server = TRUE,
                             choices = all_states_choices, selected = as.vector(all_states_choices)) # ALL
        shinyjs::hide(("states_picked"))
        ########################################## #
        # counties
        if (EJAM:::global_or_param("fipspicker_all_counties_button_defaultshow")) {
          shinyjs::show(("all_counties_button"))
        } else {
          shinyjs::hide(("all_counties_button"))
        }        
        updateCheckboxInput(session, inputId = ("all_counties_button"), value = EJAM:::global_or_param("fipspicker_all_counties_button_defaultchecked"))
        updateSelectizeInput(session, inputId = "counties_picked", choices = all_counties_choices, 
                             selected = "", server = TRUE) # MIGHT NOT AGREE WITH DEFAULT ABOVE
        shinyjs::hide(("counties_picked"))
        ########################################## #
        # cities
        
        shinyjs::hide(("all_cities_button")) # always hide
        
        updateCheckboxInput(session, inputId = ("all_cities_button"), value = "") # always no
        updateSelectizeInput(session, inputId = "cities_picked", choices = all_cities_withcountyname_choices,   ## MAYBE SLOW
                             selected = "", server = TRUE)
        shinyjs::hide(("cities_picked"))
        ########################################## #
        
        # now modify for specific types:
        
        # ---------------------------------------------------------------- #
        ## > REGIONS ####
        
        if (input$fips_type2pick == "EPA Regions") {   # may disable this option via defaults
          if (testing_this_module) {cat("\nSelecting Regions - updating buttons and boxes\n")}
          
          # pick all for larger units
          # na
          
          # hide larger units??
          # na
          
          # hide smaller units
          shinyjs::hide("all_states_button")
          shinyjs::hide("states_picked")
          shinyjs::hide("all_counties_button")
          shinyjs::hide("counties_picked")
          shinyjs::hide("all_ciies_button")
          shinyjs::hide("cities_picked")
          
          # choices/picks for these units start with none
          updateCheckboxInput(session, inputId = "all_regions_button", value = FALSE)
          updateCheckboxGroupInput(session, inputId = "regions_picked", choices = 1:10, selected = "") # NONE YET
          
          # show/hide these units
          shinyjs::show(("all_regions_button")) # always show if picking this type
          isolate({
            if (input$all_regions_button == TRUE) {
              shinyjs::hide(("regions_picked"))
            } else {
              shinyjs::show(("regions_picked"))
            }
          })
          
        }
        # ---------------------------------------------------------------- #
        ## > STATES ####
        
        if (input$fips_type2pick == "States") {
          if (testing_this_module) {cat("\nSelecting States - updating buttons and boxes\n")}
          
          # pick all for larger units
          updateCheckboxInput(session, inputId = "all_regions_button", value = TRUE)
          updateCheckboxGroupInput(session, inputId = "regions_picked", choices = 1:10, selected = all_regions_choices)  
          
          # show or hide larger units??
          shinyjs::hide(("regions_picked"))
          shinyjs::hide(("all_regions_button"))
          
          # hide smaller units
          shinyjs::hide("all_counties_button")
          shinyjs::hide("counties_picked")
          shinyjs::hide("all_ciies_button")
          shinyjs::hide("cities_picked")
          
          # choices/picks for these units start with none
          updateCheckboxInput(session, inputId = ("all_states_button"), value = FALSE)
          updateSelectizeInput(session, inputId = "states_picked", choices = all_states_choices, selected =  "", server = TRUE)  
          
          # show/hide these units
          shinyjs::show(("all_states_button")) # always show if picking this type
          isolate({
            if (input$all_states_button == TRUE) {
              shinyjs::hide(("states_picked"))
            } else {
              shinyjs::show(("states_picked"))
            }
          })
          
        }
        # ---------------------------------------------------------------- #
        ## > COUNTIES ####
        
        if (input$fips_type2pick == "Counties") {
          if (testing_this_module) {cat("\nSelecting Counties - updating buttons and boxes\n")}
          
          # pick all for larger units
          updateCheckboxInput(session, inputId = "all_regions_button", value = TRUE)
          updateCheckboxGroupInput(session, inputId = "regions_picked", choices = 1:10, selected = all_regions_choices) # ALL
          updateCheckboxInput(session, inputId = ("all_states_button"), value = TRUE)
          updateSelectizeInput(session, inputId = "states_picked", choices = all_states_choices, selected = as.vector(all_states_choices), server = TRUE) # ALL
          
          # show or hide larger units??
          shinyjs::hide(("regions_picked"))
          shinyjs::hide(("all_regions_button"))
          shinyjs::hide(("states_picked"))
          shinyjs::hide(("all_states_button"))
          
          # hide smaller units
          shinyjs::hide("all_cities_button")
          shinyjs::hide("cities_picked")
          
          # choices/picks for these units start with none
          updateCheckboxInput(session, inputId = ("all_counties_button"), value = FALSE)
          updateSelectizeInput(session, inputId = "counties_picked", choices = all_counties_choices, selected = "", server = TRUE)  
          
          # show/hide these units
          if (EJAM:::global_or_param("fipspicker_all_counties_button_defaultshow")) {
            shinyjs::show(("all_counties_button")) ## or DISABLE - UNREALISTIC ?
          } else {
            shinyjs::hide(("all_counties_button"))
          }
          isolate({
            if (input$all_counties_button == TRUE) {
              shinyjs::hide(("counties_picked"))
            } else {
              shinyjs::show(("counties_picked"))
            }
          })
          
        }
        # ---------------------------------------------------------------- #
        ## > CITIES ####
        
        if (input$fips_type2pick == "Cities or Places") {
          if (testing_this_module) {cat("\nSelecting Cities - updating buttons and boxes\n")}
          
          # pick all for larger units
          updateCheckboxInput(session, inputId = "all_regions_button", value = TRUE)
          updateCheckboxGroupInput(session, inputId = "regions_picked", choices = 1:10, selected = all_regions_choices) # ALL
          updateCheckboxInput(session, inputId = ("all_states_button"), value = TRUE)
          updateSelectizeInput(session, inputId = "states_picked", choices = all_states_choices, selected = as.vector(all_states_choices), server = TRUE) # ALL
          updateCheckboxInput(session, inputId = ("all_counties_button"), value = TRUE)
          ## may not need to update the county picks if this makes all cities available regardless of county filter. and other observer might check that anyway?
          # updateSelectizeInput(session, inputId = "counties_picked", choices = all_counties_choices, selected = as.vector(all_counties_choices), server = TRUE) # ALL
          
          # show or hide larger units??
          shinyjs::hide(("regions_picked"))
          shinyjs::hide(("all_regions_button"))
          shinyjs::hide(("states_picked"))
          shinyjs::hide(("all_states_button"))
          shinyjs::hide(("counties_picked"))
          shinyjs::hide(("all_counties_button"))
          
          # hide smaller units
          # na
          
          # choices/picks for these units start with none
          updateCheckboxInput(session, inputId = "all_cities_button", value = FALSE)
          updateSelectizeInput(session, inputId = "cities_picked", server = TRUE,
                               choices = all_cities_withcountyname_choices, selected = "")  
          
          # show/hide these units
          shinyjs::show(("cities_picked"))
          # shinyjs::show(("all_cities_button")) ## DISABLE - UNREALISTIC ? since 40k places in US and rare one would want to pick all places even in 1 county
          
        }
        # ---------------------------------------------------------------- #
        if (testing_this_module) {cat("...Finished main fips_type2pick observer \n---------------------------\n")}
        # ---------------------------------------------------------------- #
      }) %>% bindEvent(input$fips_type2pick, ignoreInit = FALSE)
      ######################################################### #   ######################################################### # 
      
      # ALL/NONE Buttons clicked ####
      
      ########################### #
      ## > REGIONS ####
      observe({ # all regions checkbox click
        
        if (input$all_regions_button == FALSE) {
          if (testing_this_module) {cat("NO regions BUTTON \n")}
          updateCheckboxGroupInput(session, inputId = "regions_picked", 
                                   selected = NULL, inline = TRUE) # clears them
        }
        if (input$fips_type2pick == "EPA Regions") {
          shinyjs::show("all_regions_button")
          shinyjs::show("regions_picked")
        } else {
          shinyjs::hide("all_regions_button")
          shinyjs::hide("regions_picked")
        }
        
        if (input$all_regions_button == TRUE) {
          if (testing_this_module) {cat("ALL regions BUTTON \n")}
          updateCheckboxGroupInput(session, inputId = "regions_picked", 
                                   selected = all_regions_choices, inline = TRUE)
          if (input$fips_type2pick == "EPA Regions") {
            shinyjs::show("all_regions_button")
            shinyjs::hide("regions_picked")
          } else {
            shinyjs::hide("all_regions_button")
            shinyjs::hide("regions_picked")
          }
          
        } 
      }) %>% bindEvent(input$all_regions_button)
      ########################### #
      ## > STATES ####
      observe({ # all states checkbox click
        
        if (input$all_states_button == FALSE) {
          if (testing_this_module) {cat("NO states BUTTON \n")}
          updateSelectizeInput(session, inputId = "states_picked", server = TRUE,
                               selected = NULL,  # clears them
                               choices = all_states_choices)
          if (input$fips_type2pick == "States") {
            shinyjs::show(("all_states_button"))
            shinyjs::show(("states_picked"))
          } else {
            shinyjs::hide(("all_states_button"))
            shinyjs::hide(("states_picked"))
          }
        } else {
          if (testing_this_module) {cat("ALL states BUTTON \n")}
          # pick all states even if not in regions specified, first
          updateSelectizeInput(session, inputId = "states_picked", server = TRUE,
                               selected = as.vector(all_states_choices),  # BUT THIS WOULD PICK ALL WITHOUT FILTERING BY  /REGIONS PICKED EARLIER ***
                               choices = all_states_choices)
          
          if (input$fips_type2pick == "States") {
            if (EJAM:::global_or_param("fipspicker_all_states_button_defaultshow")) {
              shinyjs::show("all_states_button")
              shinyjs::hide("states_picked") #
            } else {
              # shinyjs::hide("all_states_button") # CANNOT HIDE in this case?
              shinyjs::show("all_states_button")
              shinyjs::show("states_picked") # observer overrides this?
            }
          } else {
            shinyjs::hide("all_states_button")
            shinyjs::hide("states_picked")
          }
          
          # hadbeen_picked = input$regions_picked # do not have to isolate since using bindEvent? but we want to get its value and bindEvent could delay that??
          # if (testing_this_module) {cat("", length(hadbeen_picked), "is length of hadbeen_picked regions \n")}
          # updateCheckboxGroupInput(session, inputId = "regions_picked", selected = hadbeen_picked)
          # hide the box of 50 states - too big if all are picked
          shinyjs::hide(("states_picked")) # and you could see the list in the table anyway
        }
      }) %>% bindEvent(input$all_states_button)
      ########################### #
      ## > COUNTIES ####
      observe({  # allcounties checkbox click
        
        if (input$all_counties_button == FALSE) {
          if (testing_this_module) {cat("NO counties BUTTON \n")}
          updateSelectizeInput(session, inputId = "counties_picked", server = TRUE,
                               choices = all_counties_choices,
                               selected = "") # clears them 
          if (input$fips_type2pick == "Counties") {
            if (EJAM:::global_or_param("fipspicker_all_counties_button_defaultshow")) {
              shinyjs::show("all_counties_button")
            } else {
              shinyjs::hide("all_counties_button")
            }
            shinyjs::show("counties_picked") # if selecting counties, ensure button is available
          } else {
            shinyjs::hide("all_counties_button")
            shinyjs::hide("counties_picked") # if selecting regions/states it is irrelevant. if cities, MAYBE want to see counties as a filter?
          }
        }
        if (input$all_counties_button == TRUE) {
          if (testing_this_module) {cat("ALL counties BUTTON \n")}
          updateSelectizeInput(session, inputId = "counties_picked", server = TRUE,
                               choices = all_counties_choices,
                               selected = as.vector(all_counties_choices))  # BUT THIS WOULD PICK ALL WITHOUT FILTERING BY STATES/REGIONS PICKED EARLIER ***
          if (input$fips_type2pick == "Counties") {
            if (EJAM:::global_or_param("fipspicker_all_counties_button_defaultshow")) {
            shinyjs::show("all_counties_button")
            } else {
            shinyjs::hide("counties_picked") # ALL will not be shown? even if you want all within 1 state? do not need to see in the selectize window
            }
          } else {
            shinyjs::hide("all_counties_button")
            shinyjs::hide("counties_picked")
          }
        }
      }) %>% bindEvent(input$all_counties_button )
      ######################################################### # 
      
      # Places were Selected - OPTIONS & PRIOR SELECTIONS UPDATED IF FILTERED SCOPE CHANGES #### 
      
      ######################################### # 
      ##  > REGIONS CHANGED so update STATE options and prior selections to limit it to just region(s) picked ####
      observe({
        req(input$regions_picked)
        if (testing_this_module) {cat("\nObserved change in input$regions_picked, so updating state options -------\n\n")}
        inregions <- states_table$REGION %in% input$regions_picked # cities_table$eparegion %in% input$regions_picked
        if (length(inregions) == 0 || all(inregions == FALSE)) {
          states_choices <- NULL
        } else {
          if (testing_this_module) {cat("", length(all_states_choices[inregions]), " states exist in the selected regions... \n")}
          states_choices <- all_states_choices[inregions]
        }
        isolate({hadbeen_picked <- input$states_picked})
        if (testing_this_module) {
          cat("", length(hadbeen_picked), "hadbeen_picked states, aka input$states_picked, which is "); print(hadbeen_picked)
          cat(" states_choices of states is now length ", length(states_choices), "\n")
        }
        if (!is.null(hadbeen_picked) && length(hadbeen_picked) > 0) {
          new_states_picked <- hadbeen_picked[hadbeen_picked %in% as.vector(states_choices)]
          if (testing_this_module) {cat(" will only show any prior state picks that are in new set of regions, since the regions picks just changed\n")}
        } else {
          new_states_picked <- ""
          if (testing_this_module) {cat(" no prior state picks exist \n")}
        }
        updateSelectizeInput(session, inputId = ("states_picked"), server = TRUE, 
                             selected = new_states_picked, 
                             choices = states_choices,
                             options = list(maxOptions = EJAM:::global_or_param("fipspicker_maxOptions_default_states_picked"), closeAfterSelect = TRUE)
        )
        # updateCheckboxInput(session, inputId = ("all_states_button"),
        #                     value = ifelse(all(as.vector(all_states_choices) %in% new_states_picked), TRUE, FALSE)
        # )
        if (testing_this_module) {
          isolate(cat(" now input$states_picked is ", input$states_picked, "\n"))
          cat(' finished update of states_picked options based on change in selected regions\n')
          cat(" finished observing input$regions_picked\n")
        }
        if (testing_this_module) {cat("...done \n")}
      })
      ######################################### # 
      ##  > STATES CHANGED so update COUNTY options and prior selections to limit it to just state(s) picked ####
      observe({
        req(input$states_picked)
        if (testing_this_module) {cat("\nObserved change in input$states_picked, so updating county options -------\n\n")}
        instates  <- counties_table$ST %in% input$states_picked
        if (length(instates) == 0 || all(instates == FALSE)) {
          counties_choices <- NULL
        } else {
          if (testing_this_module) {cat(" some counties_choices exists, since some of counties_table$ST %in% input$states_picked \n")}
          if (testing_this_module) {cat("", length(all_counties_choices[instates]), "counties exist in the selected states... \n")}
          counties_choices <- all_counties_choices[instates]
        }
        isolate({hadbeen_picked <- input$counties_picked})
        if (testing_this_module) {
          cat("", length(hadbeen_picked), "hadbeen_picked counties, aka input$counties_picked, which is "); print(hadbeen_picked)
          cat(" counties_choices of counties is now length ", length(counties_choices), "\n")
        }
        if (!is.null(hadbeen_picked) && length(hadbeen_picked) > 0) {
          new_counties_picked <- hadbeen_picked[hadbeen_picked %in% as.vector(counties_choices)]
          if (testing_this_module) {cat(" will only show any prior county picks that are in new set of states, since the states picks just changed\n")}
        } else {
          new_counties_picked <- ""
          if (testing_this_module) {cat(" no prior counties picks exist \n")}
        }
        updateSelectizeInput(session, inputId = ("counties_picked"), server = TRUE, 
                             selected = new_counties_picked, 
                             choices = counties_choices,
                             options = list(maxOptions = EJAM:::global_or_param("fipspicker_maxOptions_default_states_picked"), closeAfterSelect = TRUE)
        )
        updateCheckboxInput(session, inputId = ("all_counties_button"),
                            value = ifelse(all(as.vector(all_counties_choices) %in% new_counties_picked), TRUE, FALSE)
        )
        if (testing_this_module) {
          isolate(cat(" now input$counties_picked is ", input$counties_picked, "\n"))
          cat(" finished updating counties_picked options based on changed input$states_picked \n")
          cat(" finished observing input$states_picked \n")
        }
        if (testing_this_module) {cat("...done \n")}
      })
      ######################################### # 
      ##  > COUNTIES CHANGED so update CITY/PLACE options and prior selections to limit it to just county(ies) picked ####
      observe({
        req(input$counties_picked)
        if (testing_this_module) {cat("\nObserved change in input$counties_picked, so updating city options -----\n\n")}
        countyname_ST_picked <- fips2countyname(input$counties_picked, includestate = "ST")
        countyname_ST_picked <- countyname_ST_picked[!is.na(countyname_ST_picked)]
        incounties <- tolower(cities_table$countyname_ST) %in% tolower(countyname_ST_picked)
        
        if (length(incounties) == 0 || all(incounties == FALSE)) {
          cities_choices <- NULL
        } else {
          if (testing_this_module) {cat(" some cities_choices exists, since some counties of full cities_table are in input$counties_picked \n")}
          if (testing_this_module) {cat("", length(all_cities_withcountyname_choices[incounties]), "cities exist in the selected counties... \n")}
          cities_choices <- all_cities_withcountyname_choices[incounties]          
        }
        isolate({hadbeen_picked <- input$cities_picked})
        if (testing_this_module) {
          cat("", length(hadbeen_picked), "hadbeen_picked cities, aka input$cities_picked, which is "); print(hadbeen_picked)
          cat(" cities_choices of cities is now length ", length(cities_choices), "\n")
        }
        if (!is.null(hadbeen_picked) && length(hadbeen_picked) > 0) {
          new_cities_picked <-  hadbeen_picked[hadbeen_picked %in% as.vector(cities_choices)]
          if (testing_this_module) {cat(" will only show any prior city picks that are in new set of counties, since the counties picks just changed\n")}
        } else {
          new_cities_picked <- ""
          if (testing_this_module) {cat(" no prior cities picks exist \n")}
        }
        updateSelectizeInput(session, inputId = ("cities_picked"), server = TRUE, # much faster if server = TRUE for such a long list
                             selected = new_cities_picked,
                             choices = cities_choices,
                             options = list(maxOptions = EJAM:::global_or_param("fipspicker_maxOptions_default_cities_picked"), closeAfterSelect = TRUE)
        )
        updateCheckboxInput(session, inputId = ("all_cities_button"),
                            value = FALSE # NEVER ALLOW ALL CITIES - TOO MANY AND NOT USEFUL # ifelse(all(as.vector(all_cities_withcountyname_choices) %in% new_cities_picked), TRUE, FALSE)
        )
        if (testing_this_module) {
          isolate(cat(" now input$cities_picked is ", input$cities_picked, "\n"))
          cat(' finished update of cities_picked based on change in counties\n')
          cat(" finished observing input$counties_picked\n")
        }
        if (testing_this_module) {cat("...done \n")}
      })
      ######################################################### #   ######################################################### # 
      ######################################################### #   ######################################################### # 
      
      
      # displaytable - UPDATE if PICKS or fips_type CHANGES ####
      
      ######################################################### #
      ##  > REGIONS output table ####
      
      observe({
        
        if (testing_this_module) {cat("input$regions_picked changed, & if fips_type2pick is regions, update output table \n ")}
        yesthistype <- (input$fips_type2pick == "EPA Regions")
        if (yesthistype) {
          if (testing_this_module) {cat(" EPA Regions picks observed to change, so updating output table \n")}
          if (length(input$regions_picked) > 0) {
            if (testing_this_module) {cat(" length(input$regions_picked) > 0 \n")}
            displaytable(
              # show regions selected
              regions_table[regions_table$REGION %in% input$regions_picked, ]
            )
          }
          if (testing_this_module) {cat(" Regions table has", sum(regions_table$REGION %in% input$regions_picked), "rows \n")}
          # bgstable(
          #   # should use cached results of ejamit(radius = 1,3,5,6.2  eg) for 10 regions, not redo
          # )
        }
        if (testing_this_module) {cat("...done \n")}
      }) %>% bindEvent(input$regions_picked, input$fips_type2pick)
      ######################################################### #
      ##  > STATES output table  ####
      
      observe({
        
        # if (input$all_states_button) {
        #   # if (testing_this_module) {cat("all states in region \n")}
        #   inregions <- states_table$REGION %in% input$regions_picked
        #   states_choices <- states_table$ST[inregions]  
        #   names(states_choices) <- states_table$statename[inregions]  
        #   
        #   shiny::updateSelectizeInput(session, inputId = ("states_picked"), server = TRUE,
        #                               choices = states_choices,
        #                               selected = states_table$ST[inregions],
        #                               options = list(maxOptions = fipspicker_maxOptions_default_states_picked, closeAfterSelect = TRUE)
        #   )
        # } else {
        #   ## ? do we want it to reset all when the user UNCHECKS "all in" ? not clear. they could just click reset all.
        #   ## only if they go from all checked to not all, AND still had all selected. 
        #   ##  but what if had checked "all" as way to pick all and then deleted some but still check says all? 
        #   ## if checked "all" regions and you then add or drop a region, you want prior state selections to be filtered not reset.
        #   ## if checked "all" states and you then add or drop a state, you want prior county selections to be filtered not reset.
        #   ## if checked "all" counties and you then add or drop a county, you want prior city selections to be filtered not reset.
        #   ##    do we want to use hadbeen_picked here?
        # }
        # if (testing_this_module) {cat("input$states_picked changed, & if fips_type2pick is states, update output table \n ")}
        
        yesthistype <- (input$fips_type2pick == "States")
        if (yesthistype) {
          if (testing_this_module) {cat(" state picks observed and fips_type2pick is States, so updating output table \n")}
          varnames = c("statename", "FIPS.ST", "ST", "REGION", "is.usa.plus.pr" , "is.state" ,"is.contiguous.us", "is.island.areas")
          if (!all(varnames %in% names(states_table))) {stop("not all varnames are in states_table")}
          displaytable(
            # show states selected
            states_table[states_table$ST %in% input$states_picked, varnames]
          )
          if (testing_this_module) {cat(" States table has", sum(states_table$ST %in% input$states_picked), "rows \n")}
        } 
        # if (testing_this_module) {cat("...done \n")}
      }) %>% bindEvent(input$states_picked, input$fips_type2pick)
      ######################################################### #
      ##  > COUNTIES output table ####
      
      observe({
        
        # if (input$all_counties_button) {
        #   if (testing_this_module) {cat("all counties in state \n")}
        #   instate <- counties_table$ST %in% input$states_picked
        #   counties_choices <- counties_table$countyfips[instate]
        #   names(counties_choices) <- counties_table$countyname_ST[instate]
        #   shiny::updateSelectizeInput(session, inputId = ("counties_picked"), server = TRUE,
        #                               choices = counties_choices,
        #                               selected = counties_choices, 
        #                               # to avoid displaying so many counties if you click "all in state" just to be able to query all places in state by name typed
        #                               options = list(maxOptions = fipspicker_maxOptions_default_counties_picked, closeAfterSelect = TRUE) 
        #   )
        # } else {
        #   ## ? do we want it to reset all when the user UNCHECKS "all in" ? not clear. they could just click reset all.
        #   ## only if they go from all checked to not all, AND still had all selected. 
        #   ##  but what if had checked "all" as way to pick all and then deleted some but still check says all? 
        #   ## if checked "all" regions and you then add or drop a region, you want prior state selections to be filtered not reset.
        #   ## if checked "all" states and you then add or drop a state, you want prior county selections to be filtered not reset.
        #   ## if checked "all" counties and you then add or drop a county, you want prior city selections to be filtered not reset.
        #   ##    do we want to use hadbeen_picked here?
        # }
        
        # if (testing_this_module) {cat("input$counties_picked changed, & if fips_type2pick is counties, update output table \n ")}
        yesthistype <- (input$fips_type2pick == "Counties")
        if (yesthistype) {
          if (testing_this_module) {cat(" * observed change in input$counties_picked, so updating Counties output table \n")
            cat(" input$counties_picked is now ", input$counties_picked, "\n")
            cat(" will now subset table for display... count of counties selected is ", NROW(counties_table[counties_table$countyfips %in% input$counties_picked, ]))
          }
          displaytable(
            counties_table[counties_table$countyfips %in% input$counties_picked, ]
          )
          if (testing_this_module) {cat(" table has", sum(counties_table$countyfips %in% input$counties_picked), "rows \n")}
        }
        # if (testing_this_module) {cat("...done \n")}
      }) %>% bindEvent(input$counties_picked, input$fips_type2pick)
      ######################################################### #
      ##  > CITIES output table ####
      
      observe({
        # if (testing_this_module) {cat("input$cities_picked changed, & if fips_type2pick is cities, update output table \n ")}
        yesthistype <- (input$fips_type2pick == "Cities or Places")
        if (yesthistype) {
          if (testing_this_module) {
            cat(" * observed change in  input$cities_picked, selected Cities, so updating cities output table \n")
            cat(" will now subset cities table for display \n")
          }
          # countyname_ST_picked <- fips2countyname(input$counties_picked, includestate = "ST")
          # incounties <- cities_table$countyname_ST %in% countyname_ST_picked
          # # should never arise:
          # if (input$all_cities_button)  
          
          displaytable(
            cities_table[fips %in% input$cities_picked, ] # data.table
          )
          if (testing_this_module) {cat(" table has", sum(cities_table$fips %in% input$cities_picked), "rows \n")}
        }
        # if (testing_this_module) {cat("...done \n")}
      }) %>% bindEvent(input$cities_picked, input$fips_type2pick)
      ######################################################### #  
      
      # displaytable - renderTable & downloadHandler #############
      
      observe({
        if (input$fips_type2pick == 0 || is.null(input$fips_type2pick)) {
          displaytable(NULL)
        }
      })
      
      displaytable <- reactiveVal() # to show here
      output$fips_table <- renderTable({
        req(displaytable())
        if (NROW(displaytable()) == 0) {req(FALSE)} # should 
        if (testing_this_module) {cat("--- Rendering table --- \n")}
        ## we could conditionally do this render only when showtable == TRUE but it might not matter much
        displaytable()
      })
      
      observe({
        if (NROW(displaytable()) > 0) {
          shinyjs::show(("download")) # show the download button
        } else {
          shinyjs::hide(("download")) # hide the download button
        }
      })
      
      output$download <- downloadHandler(
        filename = function() {
          if (testing_this_module) { print(NROW(displaytable()))}
          isolate({
            req(displaytable())
            x = input$fips_type2pick
            n = NROW(displaytable())
            if ("ST" %in% names(displaytable())) {
              nstates = length(unique(displaytable()$ST))
            } else {
              nstates = sum(displaytable()$Statecount)
            }
          })
          fname = paste0(n, " ", x, 
                         " in ", nstates, " states",
                         ".csv")
        },
        content = function(file) {
          isolate({
            mytable = displaytable()
          })
          write.csv(mytable, file, row.names = FALSE)
        }
      )
      
      # RETURN THE REACTIVE TABLE ####
      # from inner function
      return(reactive(displaytable()))
    }
    # RETURN THE REACTIVE TABLE ####
    # from outer function - returns table as a reactive object
    reactout <- fipspicker_server(input = input, output = output, session = session, id = id, testing_this_module = testing_this_module, ...)
    return(reactout)
  })
}
######################################################### #   ######################################################### #  # end of server function
######################################################### #   ######################################################### #  # end of server function
