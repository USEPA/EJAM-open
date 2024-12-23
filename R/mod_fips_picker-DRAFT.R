####################################################################### # 
# Draft UI for selecting census places
# e.g., a few cities, all counties in a state, or all states in a Region
####################################################################### # 

# TO TRY THIS as a MINI APP #### 

if (1 == 0) {
  
  stop(' you would have to put placespicker_ui here for this mini-app to work.')
  ## Need to use ns() to wrap each id in ui code if its a module, 
  ## but if not actually using this in a module, this step might let the ui still work:
  # ns <- function(x) {x}
  
  shiny::runGadget(list(
    ui =        placespicker_ui, server =        placespicker_server
    # ui = EJAM:::placespicker_ui, server = EJAM:::placespicker_server 
  )
  # ,
  # test.mode = FALSE,   # if using shiny::runApp() not runGadget
  # quiet = TRUE   # if using shiny::runApp() not runGadget
  )
  
  
  
}
################################################ ################################################# #


# TO TRY THIS as a MODULE ####
if (1 == 0) {
  
  
  mod_placespicker_ui <- function(id ) {
    
    ns <- NS(id)
    
    # .-------------- UI ------------------------  ####
    
    placespicker_ui <- fluidPage(
      shinyjs::useShinyjs(),
      titlePanel("Cities and other Census Places"),
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("type2analyze"), label = "What kinds of areas do you want to compare?", 
                      selected = "Cities or Places",  # default to assuming they want to pick cities/places
                      multiple = FALSE,
                      choices = c("Select Regions, States, Counties, or Cities", "EPA Regions", "States", "Counties", "Cities or Places")),
          actionButton(ns("reset"), label = "Clear all selections/ Reset"),
          checkboxGroupInput(ns("regionpicker2"), label = "EPA Region", width = '100%',
                             selected = NULL, # set in server
                             choices = 1:10,  # regiontable is available in server not in UI function # sort(regiontable$REGION), # sort(unique(placetable$eparegion)), 
                             inline = TRUE),
          checkboxInput(ns("allstatesinregion"), "All States in EPA Region(s) above?", value = FALSE),
          selectizeInput(ns("statepicker"), label = "Select State", 
                         selected = "",
                         choices = NULL, # unique(placetable$ST),
                         options = list(closeAfterSelect = TRUE),
                         multiple = T),
          checkboxInput(ns("allcountiesinstate"), "All Counties in State(s) above?", value = FALSE),
          selectizeInput(ns("countypicker"), label = "Select Counties", 
                         selected = "",
                         options = list(maxOptions = 254, closeAfterSelect = TRUE), # no limit by default. TX has the most of any 1 state, 254 Counties. But all in US >3k.
                         # options = list(maxOptions = 10), # to avoid displaying so many counties if you click "all in state" just to be able to query all places in state by name typed
                         choices = NULL, # unique(placetable$countyname_ST),
                         multiple = T),
          checkboxInput(ns("allplacesincounty"), "All Cites/Places in County(ies) above? [not enabled]", value = FALSE),
          selectizeInput(ns("placespicker"), label = "Select Places", 
                         selected = "", 
                         choices = NULL,       ## (40,000 PLACES !)# loads faster if NULL and then update it via server  . to placetable$placename,
                         options = list(closeAfterSelect = TRUE),
                         multiple = T)
        ),
        ##  Show table of selected locations 
        mainPanel(
          shiny::downloadButton(ns("download")),
          tableOutput(ns("placestable"))
        )
      )
    )
    ######################################################### #   ######################################################### # 
    # end of UI
    
    
  }
  ######################################################### #  
  
  mod_placespicker_server <- function(id, ...) {
    shiny::moduleServer(
      id = id,
      function(input, output, session) {
        ns <- session$ns
        
        ### TO BE CONTINUED  NOT DONE
        
        placespicker_server(input = input, output = output, session = session, ...)
        #       but   within server, any UI functions would need id wrapped in ns()
        ### TO BE CONTINUED   NOT DONE
        
        ### example. 
        
        # observeEvent(input$xyz, {
        #   tmp <- (input$xyz) # Update the reactive values for this user-manipulated data to pass back to main environment
        #   reactdat(tmp) #   update the value of reactdat()  based on new value of tmp
        # })
        # return( reactdat ) # no parentheses here - return the reactive object not just its current value
      })
    
  }
  #################################################### # 
  
  #  UI of an outer app shell to use in testing the module
  
  APP_UI_TEST <- function(request) {
    
    shiny::fluidPage(
      shiny::h2('a fips place picker module demo'),
      
      mod_placespicker_ui("TESTID"),
      
      # shiny::actionButton(inputId =  "testbutton", 
      #  label = "Done (may not want this button - just a way to close any modal)"),
      
      h3("Example of a table in the parent app"),
      'fipstableout' ,
      
      h3("Example of a live view of the data, as seen in the parent app"),
      DT::DTOutput(outputId =  "testinfo" ),
      br()
    )}
  
  #  SERVER of an outer app shell to use in testing the module
  
  APP_SERVER_TEST <- function(input, output, session) {
    
    # Send table template as initial value 
    #reactive_data1 <-  reactiveVal(0)
    
    fipstableout <- mod_placespicker_server( id = "TESTID") #,  reactdat = reactive_data1 )
    fipstableout
    
    # can pass  a reactive reactive_data1() , but must pass it with with NO parens
    
    # to view actual table in rendered form to be ready to display it in app UI
    # observe({
    #   tmp <- reactive_data1() # reactiveVal(out())
    #   output$testinfo <- DT::renderDT(DT::datatable(  tmp  ))
    # })  # %>%  bindEvent(input$testbutton)   # (when the "Done entering points" button is pressed? but that is inside the module)
    
  } # end of test server
  ########################################### #
  
  ## Run the outer app shell to test the module
  
  shinyApp(ui = APP_UI_TEST, server = APP_SERVER_TEST)
  
} # end of module test
################################################ ################################################# #
# ~ ### #


# .-------------- SERVER -------------------  ####

placespicker_server <- function(input, output, session) {
  
  warning( " work in progress")
  
  # library(shiny); library(data.table)
  
  ns <- session$ns      # ??? if used in a module?
  ns <- function(x) {x} # ??? if not used in a module?
  
  # SET UP Tables of places, counties, states, regions info ####
  
  statetable <- stateinfo2[stateinfo2$ST != "US", c("statename", "FIPS.ST", "ST", "REGION", "is.usa.plus.pr", 
                                                    "is.state", "is.contiguous.us", "is.island.areas")] # etc. etc.
  
  regiontable <- data.frame(
    REGION = 1:10,
    Statecount = as.vector(table(statetable$REGION)), 
    States = sapply(1:10, function(n) paste0(statetable$ST[statetable$REGION %in% n], collapse = ", "))
  )
  
  placetable = data.table(censusplaces)[, .(eparegion, ST, countyname, placename, fips)]
  placetable$countyname_ST <- paste0(placetable$countyname, ", ", placetable$ST)
  
  countytable <- unique(placetable[, .(eparegion, ST, countyname_ST)])
  countytable$countyfips <- fips_counties_from_countyname(countytable$countyname_ST)
  
  # *** Note placetable (from censusplaces) and blockgroupstats have slightly 
  # different county name lists,
  # and also lower case "city" is "City" in censusplaces.
  
  # countytable <- unique(data.frame(
  #   stfips = substr(blockgroupstats$bgfips,1,2),
  #   ST = blockgroupstats$ST,
  #   countyfips = substr(blockgroupstats$bgfips,1,5), 
  #   countyname = gsub(" city$", " City", blockgroupstats$countyname)
  # ))
  # countytable$countyname_ST = paste0( countytable$countyname, ", ",  countytable$ST)
  
  #  countytable$countyname_ST
  
  ## Note SOME CITIES/PLACES OVERLAP WITH 2+ COUNTIES !!  
  
  ######################################################### #   ######################################################### # 
  ######################################################### #   ######################################################### # 
  
  
  ######################################################### #   ######################################################### # 
  # EMPTY start view ####
  
  allstatesinregion_button_defaultshow = TRUE # means always show if type2analyze is this or smaller (states, counties, or places)
  allcountiesinstate_button_defaultshow = TRUE # means always show if type2analyze is this or smaller (counties or places)
  
  updateCheckboxGroupInput(inputId = ns("regionpicker2"), choices = sort(regiontable$REGION), selected = sort(regiontable$REGION), inline = TRUE)
  shinyjs::hide(ns("regionpicker2"))  # but then default choice of type2analyze updates these 
  # if (allstatesinregion_button_defaultshow) {shinyjs::show(ns("allstatesinregion"))} else {shinyjs::hide(ns("allstatesinregion"))} ############## #
  shinyjs::hide(ns("allstatesinregion"))
  shinyjs::hide(ns("statepicker"))
  # if (allcountiesinstate_button_defaultshow) {shinyjs::show(ns("allcountiesinstate"))} else {shinyjs::hide(ns("allcountiesinstate"))} ############## # 
  shinyjs::hide(ns("allcountiesinstate"))
  shinyjs::hide(ns("countypicker"))
  shinyjs::hide(ns("allplacesincounty"))
  shinyjs::hide(ns("placespicker"))
  ######################################################### # 
  
  # RESET Button view ####
  
  observe({
    cat("RESET\n")
    # reset to default starting view as found in ui and server
    
    updateSelectizeInput(session, inputId = ns("type2analyze"), label = "What kinds of areas do you want to compare?", 
                         selected = "Cities or Places",  # maybe do not reset back to default. leave geoscale as currently selected? makes resetting selections more complicated.  
                         choices = c("Select Regions, States, Counties, or Cities", "EPA Regions", "States", "Counties", "Cities or Places")
    )
    
    updateCheckboxInput(session = session, inputId = ns("allstatesinregion"),  value = FALSE)
    updateCheckboxInput(session = session, inputId = ns("allcountiesinstate"), value = FALSE)
    updateCheckboxInput(session = session, inputId = ns("allplacesincounty"),  value = FALSE)
    
    ## this assumes selected after reset is county or place level:
    if (allstatesinregion_button_defaultshow) {shinyjs::show(ns("allstatesinregion"))} else {shinyjs::hide(ns("allstatesinregion"))} ############## #
    if (allcountiesinstate_button_defaultshow) {shinyjs::show(ns("allcountiesinstate"))} else {shinyjs::hide(ns("allcountiesinstate"))} ############## #
    # shinyjs::show(ns("allstatesinregion")) ############## #
    # shinyjs::hide(ns("allcountiesinstate")) ############## #
    shinyjs::hide(ns("allplacesincounty"))
    
    updateCheckboxGroupInput(      session, inputId = ns("regionpicker2"), choices = sort(regiontable$REGION), selected = sort(regiontable$REGION), inline = TRUE)
    
    inregions <- statetable$REGION %in% input$regionpicker2 
    mychoices <- statetable$ST[inregions]  # unique(placetable$ST[inregions])
    names(mychoices) <- statetable$statename[inregions] # fips2statename(fips_state_from_state_abbrev(mychoices))
    updateSelectizeInput(session = session, inputId = ns("statepicker"),  server = TRUE, selected = "", choices = mychoices) # 
    
    # instates  <- countytable$ST %in% input$statepicker
    # mychoices <- countytable$countyfips[instates] 
    # names(mychoices) <- countytable$countyname_ST[instates]
    mychoices <- NULL
    updateSelectizeInput(session = session, inputId = ns("countypicker"), server = TRUE, selected = "", 
                         options = list(maxOptions = 254, closeAfterSelect = TRUE), # no cap by default
                         choices = mychoices) # , choices = NULL
    
    mychoices <- NULL
    updateSelectizeInput(session = session, inputId = ns("placespicker"), server = TRUE, selected = "", choices = mychoices) # , choices = NULL
    
    # # these should get hidden/shown by the observer of input$type2analyze but reset button does not quite work right if not done here
    ## and hide/show depends on what default you reset to. do not hide any if resets back to "Cities or Places"
    # shinyjs::hide(ns("regionpicker2"))
    # shinyjs::hide(ns("statepicker"))
    # shinyjs::hide(ns("countypicker"))
    # shinyjs::hide(ns("placespicker"))
    
  }) |>
    bindEvent(input$reset) 
  ######################################################### # 
  
  
  # SHOW/HIDE pickers for different geo scales/levels ####
  
  observe({
    
    if (input$type2analyze ==  "Select Regions, States, Counties, or Cities") {
      shinyjs::hide(ns("regionpicker2"))
      shinyjs::hide(ns("allstatesinregion")) ############## #
      shinyjs::hide(ns("statepicker"))
      shinyjs::hide(ns("allcountiesinstate")) ############## #
      shinyjs::hide(ns("countypicker"))
      shinyjs::hide(ns("allplacesincounty"))
      shinyjs::hide(ns("placespicker"))
    }
    if (input$type2analyze == "EPA Regions") {
      shinyjs::show(ns("regionpicker2"))
      shinyjs::hide(ns("allstatesinregion")) ############## #
      shinyjs::hide(ns("statepicker"))
      shinyjs::hide(ns("allcountiesinstate")) ############## #
      shinyjs::hide(ns("countypicker"))
      shinyjs::hide(ns("allplacesincounty"))
      shinyjs::hide(ns("placespicker"))
    }
    if (input$type2analyze == "States") {
      shinyjs::show(ns("regionpicker2"))
      shinyjs::show(ns("allstatesinregion")) ############## #
      shinyjs::show(ns("statepicker"))
      shinyjs::hide(ns("allcountiesinstate")) ############## # ok
      shinyjs::hide(ns("countypicker"))
      shinyjs::hide(ns("allplacesincounty"))
      shinyjs::hide(ns("placespicker"))
    }
    if (input$type2analyze == "Counties") {
      shinyjs::show(ns("regionpicker2"))
      # shinyjs::hide(ns("allstatesinregion")) ############## #
      if (allstatesinregion_button_defaultshow) {
        updateCheckboxInput(session, inputId = ns("allstatesinregion"), value = FALSE)
        shinyjs::show(ns("allstatesinregion"))
      } else {
        shinyjs::hide(ns("allstatesinregion"))
      } ############## #
      if (allcountiesinstate_button_defaultshow) {
        updateCheckboxInput(session, inputId = ns("allcountiesinstate"), value = FALSE)
        shinyjs::show(ns("allcountiesinstate"))
      } else {
        shinyjs::hide(ns("allcountiesinstate"))
      } ############## #
      shinyjs::show(ns("statepicker"))
      shinyjs::show(ns("countypicker"))
      shinyjs::hide(ns("allplacesincounty"))
      shinyjs::hide(ns("placespicker"))
    }
    if (input$type2analyze == "Cities or Places") {
      
      shinyjs::show(ns("regionpicker2"))
      
      # to allow a search of places to show ALL states, all counties(by default? or always?):
      ## but note will need to help distinguish between places of same name in multiple counties of same state
      
      shinyjs::show(ns("statepicker"))
      if (allstatesinregion_button_defaultshow) {
        updateCheckboxInput(session, inputId = ns("allstatesinregion"), value = FALSE)
        shinyjs::show(ns("allstatesinregion"))
      } else {
        shinyjs::hide(ns("allstatesinregion"))
      } ############## #
      # shinyjs::show(ns("allstatesinregion"))
      
      shinyjs::show(ns("countypicker"))
      # shinyjs::show(ns("allcountiesinstate"))
      if (allcountiesinstate_button_defaultshow) {
        updateCheckboxInput(session, inputId = ns("allcountiesinstate"), value = FALSE)
        shinyjs::show(ns("allcountiesinstate"))
      } else {
        shinyjs::hide(ns("allcountiesinstate"))
      } ############## #
      
      shinyjs::show(ns("placespicker"))
      # shinyjs::show(ns("allplacesincounty")) # disallowed since 40k places in US and rare one would want to pick all places even in 1 county
    }
  })
  ######################################################### #   ######################################################### # 
  
  
  # OPTIONS & PRIOR SELECTIONS UPDATED IF SCOPE CHANGES #### 
  
  
  
  ##  > STATE options and prior selections updated based on region(s) picked ####
  observe({
    req(input$regionpicker2)
    # print("observed change in input$regionpicker2")
    inregions <- statetable$REGION %in% input$regionpicker2 # placetable$eparegion %in% input$regionpicker2
    mychoices <- statetable$ST[inregions]  # unique(placetable$ST[inregions])
    names(mychoices) <- statetable$statename[inregions] # fips2statename(fips_state_from_state_abbrev(mychoices))
    isolate({hadbeenselected <- input$statepicker})
    
    # print(names(mychoices)); print("are names of mychoices for states in regions here")    
    # print("hadbeenselected is "); print(hadbeenselected)
    
    updateSelectizeInput(session, inputId = ns("statepicker"), server = TRUE, 
                         choices = mychoices
    )
    if (length(hadbeenselected) > 0) {
      updateSelectizeInput(session, inputId = ns("statepicker"), server = TRUE, 
                           # only show prior picks that are in new set of regions, since the regions picks just changed
                           selected = hadbeenselected[hadbeenselected %in% mychoices], 
                           choices = mychoices
      )
    }
  })
  
  ##  > COUNTY options and prior selections updated based on state(s) picked ####
  observe({
    req(input$statepicker)
    # print("observed change in input$statepicker")
    instates  <- countytable$ST %in% input$statepicker
    mychoices <- countytable$countyfips[instates] 
    names(mychoices) <- countytable$countyname_ST[instates]
    # print("length of county choices is"); print(length(mychoices))
    
    isolate({hadbeenselected <- input$countypicker})    
    updateSelectizeInput(session, inputId = ns("countypicker"), server = TRUE, 
                         # do not update   options = list(maxOptions = 254, closeAfterSelect = TRUE) 
                         choices = mychoices
    )
    if (length(hadbeenselected) > 0) {
      updateSelectizeInput(session, inputId = ns("countypicker"), server = TRUE, 
                           # do not update   options = list(maxOptions = 254, closeAfterSelect = TRUE) 
                           # only show prior picks that are in new set of states, since the state picks just changed
                           selected = hadbeenselected[hadbeenselected %in% mychoices], 
                           choices = mychoices
      )
    }
  })
  
  ##  > CITY/PLACE options and prior selections updated based on county(ies) picked ####
  observe({
    req(input$countypicker)
    # cat("\n\nstarted an update of placespicker options due to change in input$countypicker  filter ------------- \n\n")
    if (length(input$countypicker) == 0)  {
      mychoices <- ""
    } else {
      # changed county pick(s), so update choices allowed to places in county(ies)
      countyname_ST_picked <- fips2countyname(input$countypicker, includestate = "ST")
      countyname_ST_picked <- countyname_ST_picked[!is.na(countyname_ST_picked)]
      incounties <- tolower(placetable$countyname_ST) %in% tolower(countyname_ST_picked)
      mychoices <- placetable$fips[incounties]
      names(mychoices) <- paste0(placetable$placename[incounties], ", ", placetable$countyname_ST[incounties], " (", mychoices,")")
      # isolate(
      #   cat("input$countypicker is ", input$countypicker, '\n\n')
      # )
      # cat("countyname_ST_picked is", countyname_ST_picked, '\n\n')
      # cat("mychoices is", mychoices, "\n\n")
      # cat("names(mychoices) is", names(mychoices), '\n\n')
    }
    
    isolate({hadbeenselected <- input$placespicker})
    updateSelectizeInput(session, inputId = ns("placespicker"), server = TRUE, # much faster if server = TRUE for such a long list
                         choices = mychoices
    )
    # cat("hadbeenselected", hadbeenselected, '\n\n')
    # cat("supposedly now selected after filter hadbeenselected to those allowed: hadbeenselected[hadbeenselected %in% mychoices] ", hadbeenselected[hadbeenselected %in% mychoices], '\n\n')
    
    if (length(hadbeenselected) > 0) {
      updateSelectizeInput(session, inputId = ns("placespicker"), server = TRUE,
                           selected = hadbeenselected[hadbeenselected %in% mychoices],
                           choices = mychoices
                           # only show prior picks that are in new set of states, since the state picks just changed
      )
    }
    # isolate(
    #     cat("now input$placespicker is ", input$placespicker, "\n")
    #  )
    # cat('finished update of placespicker ---------------------- \n\n')
  })
  
  
  ######################################################### #   ######################################################### # 
  ######################################################### #   ######################################################### # 
  
  ######################################################### #   ######################################################### # 
  
  # OUTPUT TABLE UPDATED - if PICKS CHANGE, OR "ALL" BUTTON hit, OR LEVEL CHANGES ####
  
  ##  > REGIONS ################### 
  
  observe({
    if (input$type2analyze == "EPA Regions") {
      
      # note there is no button for input$allregionsinusa, unlike for states or counties filters.
      
      displaytable(
        # show regions selected
        regiontable[regiontable$REGION %in% input$regionpicker2, ]
      )
      # bgstable(
      #   # should use cached results of ejamit(radius = 1,3,5,6.2  eg) for 10 regions, not redo
      # )
    }
  })
  
  
  ##  > STATES / All States (in given Region(s)) ################### 
  
  observe({
    
    if (input$allstatesinregion) {
      inregions <- statetable$REGION %in% input$regionpicker2
      mychoices <- statetable$ST[inregions]  
      names(mychoices) <- statetable$statename[inregions]  
      
      shiny::updateSelectizeInput(session, inputId = ns("statepicker"),
                                  choices = mychoices,
                                  selected = statetable$ST[inregions]
      )
    } else {
      ## ? do we want it to reset all when the user UNCHECKS "all in" ? not clear. they could just click reset all.
      ## only if they go from all checked to not all, AND still had all selected. 
      ##  but what if had checked "all" as way to pick all and then deleted some but still check says all? 
      ## if checked "all" regions and you then add or drop a region, you want prior state selections to be filtered not reset.
      ## if checked "all" states and you then add or drop a state, you want prior county selections to be filtered not reset.
      ## if checked "all" counties and you then add or drop a county, you want prior city selections to be filtered not reset.
      ##    do we want to use hadbeenselected here?
    }
    
    
    if (input$type2analyze == "States") {
      
      varnames = c("statename", "FIPS.ST", "ST", "REGION", "is.usa.plus.pr" , "is.state" ,"is.contiguous.us", "is.island.areas")
      if (!all(varnames %in% names(statetable))) {stop("not all varnames are in statetable")}
      displaytable(
        # show states selected
        statetable[statetable$ST %in% input$statepicker, varnames]
      )
      
    }
  })
  
  
  ##  > COUNTIES / All Counties (in given State(s)) ###################
  
  observe({
    
    if (input$allcountiesinstate) {
      instate <- countytable$ST %in% input$statepicker
      mychoices <- countytable$countyfips[instate]
      names(mychoices) <- countytable$countyname_ST[instate]
      shiny::updateSelectizeInput(session, inputId = ns("countypicker"),
                                  choices = mychoices,
                                  selected = mychoices, 
                                  
                                  # to avoid displaying so many counties if you click "all in state" just to be able to query all places in state by name typed
                                  options = list(maxOptions = 254, closeAfterSelect = TRUE) 
      )
    } else {
      ## ? do we want it to reset all when the user UNCHECKS "all in" ? not clear. they could just click reset all.
      ## only if they go from all checked to not all, AND still had all selected. 
      ##  but what if had checked "all" as way to pick all and then deleted some but still check says all? 
      ## if checked "all" regions and you then add or drop a region, you want prior state selections to be filtered not reset.
      ## if checked "all" states and you then add or drop a state, you want prior county selections to be filtered not reset.
      ## if checked "all" counties and you then add or drop a county, you want prior city selections to be filtered not reset.
      ##    do we want to use hadbeenselected here?
    }
    
    if (input$type2analyze == "Counties") {
      
      displaytable(
        countytable[countytable$countyfips %in% input$countypicker, ]
      )
      
    }
  })
  
  
  ##  > CITIES / All Cities/Places (in given counties) ###################
  
  observe({
    if (input$type2analyze == "Cities or Places") {
      
      # cat("started update of placespicker displayed table ---------------------------- \n\n")
      
      # countyname_ST_picked <- fips2countyname(input$countypicker, includestate = "ST")
      # incounties <- placetable$countyname_ST %in% countyname_ST_picked
      
      # # should never arise:
      # if (input$allplacesincounty)  
      
      displaytable(
        placetable[placetable$fips %in% input$placespicker, ]
      )
    }
  })
  ######################################################### #  
  
  observe({
    if (input$type2analyze == 0 || input$type2analyze == "Select Regions, States, Counties, or Cities") {
      displaytable(NULL)
    }
  })
  
  
  # OUTPUT TABLE DOWNLOAD - renderTable & download ############# 
  displaytable <- reactiveVal() # to show here
  output$placestable <- renderTable({
    displaytable()
  })
  
  observe({
    if (NROW(displaytable()) > 0) {
      shinyjs::show(ns("download"))
    } else {
      shinyjs::hide(ns("download"))
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      # print(NROW(displaytable()))
      # c("Select Regions, States, Counties, or Cities", "EPA Regions", "States", "Counties", "Cities or Places")
      isolate({
        req(displaytable())
        x = input$type2analyze
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
  
  # fipstableout <- shiny::rea
  
  
}
######################################################### #   ######################################################### # 
######################################################### #   ######################################################### #  # end of server function



######################################################### #   ######################################################### # 
