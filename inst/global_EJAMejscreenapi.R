
# global_EJAMejscreenapi.R defines variables needed in global environment?? should remain in module namespace, actually

warning( " work in progress - this global.... file must be sourced only inside a function like a module to avoid conflicts with globalenv() ?")

# ------------------------ ____ Get packages, functions ---------------------------------  ####
# require(shiny)
# require(data.table) # used in ejscreenapi1.R, ejscreenapi.R
# require(tidyverse) # NOT SURE THIS ALL IS NEEDED !!
# require(magrittr) # already part of tidyr?  for the pipe used with leaflet in server.R,   %>%  
# require(DT)    # used in ui.R and in server.R for displaying tabular data
# require(readr) # used in server.R
# require(leaflet) # ; require(mapview) # for mapping. leaflet is used in ui.R and in ejscreenapi.R
# require(leaflet.extras2) # for addEasyprint() to add print button on a map
# # require(sf)  # geospatial data, distances etc.
# require(jsonlite)  # mostly for using API 
# require(httr) # used by ejscreenRESTbroker() for httr ::GET()
# require(htmltools) # probably not needed... or imported by shiny? provides tags like h5() etc
# # require(urltools) # probably want to switch to using this nice package for working with URL-encoded parameters, encoding, parsing, etc. URLs
# require(viridis) # color palettes
# # require(hrbrthemes) # fonts that are better for graphics - COULD USE THIS IN boxplots_ratio.R but need install fonts
######################################################## # 

# ------------------------ ____ SET DEFAULTS / OPTIONS for app ------------------------  ####

# * Note each time a user session is started, the application-level option set is duplicated, for that session. 
# * If the options are set from inside the server function, then they will be scoped to the session.
#     LET ADVANCED USERS ADJUST THESE, as INPUTS ON ADVANCED SETTINGS TAB

######################################################## # 
## >Options in general ####
# 
## ------------------------ enable bookmarking? ####
# bookmarking_allowed <- TRUE  # https://mastering-shiny.org/action-bookmark.html
# if (bookmarking_allowed) {enableBookmarking(store = "url")}
# 
# default_hide_advanced_settings <- FALSE
# default_testing        <- TRUE
# default_shiny.testmode <- TRUE  # If TRUE, then various features for testing Shiny applications are enabled.
# default_print_uploaded_points_to_log <- TRUE
# 
# ## Raise Memory Limit on file upload to 100Mb  
# options(shiny.maxRequestSize = 100*1024^2) 
# 
# ## Loading/wait spinners (color, type) ####
# ## note: was set at type = 1, but this caused screen to "bounce"
# options(spinner.color = "#005ea2", spinner.type = 4)

## ------------------------ app title ####
apptitle <- "EJAM's ejscreenapi tool v2.2"

######################################################## # 
## ------------------------ IP address ####
ips <- c('10.147.194.116', 'awsgeopub.epa.gov', '204.47.252.51', 'ejscreen.epa.gov')
whichip <- ips[4]

######################################################## # 
# > Options for site point uploads, radius  ####

## ------------------------ limits on # of points ####





# input$max_pts_upload
default_max_pts_upload  <-   5 * 1000 
maxmax_pts_upload  <-  10 * 1000 #   cap uploaded points 

# input$max_pts_map
default_max_pts_map       <- 1 * 1000
maxmax_pts_map       <- 5 * 1000 # max we will show on map 

# input$max_pts_showtable
default_max_pts_showtable <- 1 * 1000 # max that will be shown in the interactive viewer. It drops the rest.
maxmax_pts_showtable <- 5 * 1000 # 10k is extremely slow. check server side vs client side 

# input$max_pts_run
default_max_pts_run      <-  1 * 1000 # initial cap but can adjust in advanced tab
maxmax_pts_run      <- 15 * 1000 # absolute max you can analyze here, even with advanced tab 

#  The app may timeout anyway?  Assume 1 minute per 100 sites if 3 mile radius, so 500 sites is a few minutes and 5k sites takes hours
# 13k cap would handle all RMP sites but could take 2 hours!

## ------------------------ Options for Radius  #####

# input$default_miles
default_default_miles <- 1
max_default_miles <- 50 * 1000 / meters_per_mile # 50 km
# input$max_miles
default_max_miles     <- 10
maxmax_miles     <- 50 * 1000 / meters_per_mile # 50 km
#   radius miles for slider input where user specifies radius. Note 5 km is 3.1 miles, 10 km is 6.2 miles ; and 10 miles is 16 kilometers (10 * meters_per_mile/1000). 50 km is too much/ too slow.
minradius  <- 0.25 # miles
stepradius  <- 0.05 # miles

######################################################## # 











######################################################## # 

# >Options for calculations & what stats to output ####

## ------------------------ calculate and/or include in downloaded outputs 

default_calculate_ratios <- TRUE   # probably need to calculate even if not shown in excel download, since plots and short summary report rely on them/
default_include_averages <- TRUE
default_include_extraindicators <- TRUE
# very few for API. see EJAM.





######################################################## # 

# >Options for viewing results  ####



## ------------------------ map colors, weights, opacity ####

default_circleweight <- 4

opacitymin   <- 0 
opacitymax   <- 0.5
opacitystep  <- 0.025
opacitystart <- 0.5
opacityratio <- 2 / 5
base_color_default      <- "blue"  ;
cluster_color_default   <- "red"   ;
highlight_color_default <- 'orange';
# default_popup_maxHeight <- 400
# default_popup_maxWidth <- 850
#for use in... popupOptions = list(maxHeight = default_popup_maxHeight, maxWidth = default_popup_maxWidth),

## ------------------------ predict time to complete ####
perhourslow  <- 3000  # to give an estimate of how long it will take
perhourguess <- 6000  # seeing 8k if 1 mile, 4.7k if 5 miles, roughly. 207 ECHO run was 2 . 1  minutes, 5.9k/hr.
perhourfast <- 12000  # approx 12k RMP sites would take almost 2 hours (1 to 2 hours, or even 4?).
report_every_n_default <- 100

## ------------------------ download as excel vs csv ####
asExcel <- TRUE # WHETHER TO DOWNLOAD RESULTS AS EXCEL OR CSV

######################################################## # 

### Excel formatting options   --------------------- #


# heatmap column names - defaults could be set here and made flexible in advanced tab


# heatmap cutoffs for bins - defaults could be set here and made flexible in advanced tab


# heatmap colors for bins - defaults could be set here and made flexible in advanced tab


# not used:
default_ok2plot <- FALSE # the plots to put in excel tabs via ejam2excel() and xls_formatting2() and the plot functions


############################################################################## # # # 



## ------------------------ text message for tips on using table ####
tabletips_message <- shiny::span(  # imported from htmltools package
  'Click column name (header) to sort. Click again to reverse order. Click row(s) to highlight selected points on map. ', # tags$br(),
  # 'Type in Search box to find limit rows to matches (searches in all columns).',   # placeholder - will add search filters
  shiny::tags$br(),
  'Download using button at upper left of table.'
)
# ------------------------ text message about ECHO facility search ####
echo_url <-  'https://echo.epa.gov/facilities/facility-search' # used in server.R and in message below
echo_message <- paste0('To use the ECHO website to search for and specify a list of regulated facilities, 
                       1) go to ', echo_url, ' and 2) under Facility Characteristics Results View select data table, 
                       click Search, then 3) click Customize Columns, use checkboxes to include Latitude and Longitude, 
                       then 4) click Download Data, then 5) return to this app to upload that ECHO site list.\n')






























