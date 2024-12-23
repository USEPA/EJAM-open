
#' Get EJScreen results near each site, compiled as a table, with map and plot
#' 
#' @description This is the main high-level function that does all the work involved in 
#'   using the EJScreen API outside of the web app to get results for 
#'   multiple locations compiled in one table.
#'   
#' @details 
#'   This function provides EJScreen indicator results near each of a list of sites, 
#'   all in one function, for those who want to do so in RStudio (for interactive analysis) 
#'   or via their own software/apps (programmatically)
#'   rather than through the web app interface. It is an alternative to EJAM, as
#'   explained below.
#'   
#'   Based on a set of places, `ejscreenit()` requests an EJScreen standard report for each place
#'   and assembles all those results as a single spreadsheet. The indicators include
#'   demographic and environmental indicators as well as EJ Indexes that combine them,
#'   and some geospatial information about each site.
#'   
#'   `ejscreenit()` is the basic, high-level tool that enables easy access to the EJScreen API
#'   for a list of sites, outside the context of a web app. It accepts lat/lon values
#'   flexibly and interactively in the R console, 
#'   and then uses the helper functions below to return a list of 3 objects:
#'   - a table
#'   - an interactive map (html widget with popups in R console or saved as html file)
#'   - plot of some indicators over the range of sites
#'   
#'   The table it provides uses plain-English column headers, such as
#'   "Traffic Proximity and Volume (daily traffic count/distance to road)"
#'   
#'   
#'   # EJAM vs EJScreen
#'   
#'   To obtain *overall summary* statistics (and faster results) use ejamit() 
#'   from the EJAM package instead of ejscreenit(). 
#'   The two tools, ejamit() and ejscreenit(), are different, and have pros and cons: 
#'   
#'   The key advantages of EJAM are 
#'   1. *ejscreenit / EJScreen API is much, much slower than EJAM*
#'   2. *ejscreenit cannot provide an overall summary across sites, while EJAM can*
#'   
#'   An "overall summary" here means, for example, the overall percent low income 
#'   among all unique residents near any one or more of the sites.
#'   This inability to provide an overall summary aggregating across sites
#'   is due to limitations of the EJScreen API 
#'   -- it does not provide all data needed to correctly aggregate across sites
#'   (see detailed explanation in the ejscreenapi About document).
#'   
#'   
#'   # Inputs and Outputs
#'   
#'   The ejscreenit() function accepts a set of points (locations) and a radius, and then
#'   accesses EJScreen (the server via API) to assemble a list of results
#'   similar to the EJScreen standard report but for each of many sites. 
#'   
#'   Points can be specified in several ways:
#'     
#'    - as a table (columns lat, lon that are the geographic coordinates) 
#'    
#'    - in a spreadsheet file with that info (named, or pointed to interactively in RStudio)
#'    
#'   The function returns a list of items: table, map, plot   
#'   
#'   - a table of scores that are like the EJScreen standard buffer report, but with one row per site (input point)
#'   
#'      - and as a set of columns in that table, a set of ratios, which are the 
#'   indicator value nearby a site divided by the indicator value in the US overall 
#'   (which puts all indicators in common units, facilitating comparisons, 
#'   offering a summary statistic that is complementary to the percentile but may be more useful in some cases)
#'   
#'   - a map with interactivity and popups of results at each point
#'   
#'   - a plot/graphic visualization of the range of sites
#'   
#'   
#'   # Details of helper functions used
#'   
#'   This function works by requesting each site report from EPA servers 
#'   via the [EJScreen API](https://ejscreen.epa.gov/mapper/ejscreenapi1.html) 
#'   using various helper functions. 
#'   It uses functions including the following: 
#'   
#'   - [ejscreenit()] is the basic, high-level tool that enables access to the EJScreen API
#'   for a list of sites, outside the context of a web app. It accepts lat/lon values flexibly and interactively in the R console, 
#'   and then uses the helper functions below to return a list of 3 objects: a table, interactive map, and plot.
#'   The table it provides uses plain-English column headers, such as
#'   "Traffic Proximity and Volume (daily traffic count/distance to road)"
#'     - [ejscreenapi_plus()] can accept a file of point data, uses [ejscreenapi()] to get EJScreen stats,
#'       and then prepends input table and renames columns as R variables like "traffic.score" 
#'       - [ejscreenapi()]  gets EJ stats for many points as a data.table of many rows.
#'       - [ejscreenapi1()] gets EJ stats for 1 point via API, as data.frame of 1 row.
#'       - [ejscreenRESTbroker()] gets EJ stats for one point as JSON.
#'       
#'   Also used are 
#'   [ejscreenapi_plus()], [ejscreenapi()], [urls_clusters_and_sort_cols()],
#'   [fixcolnames()], [url_linkify()], [popup_from_ejscreen()], [calc_ratios_to_avg()], [boxplots_ratios()]
#'
#' @param x Point locations may be specified in several ways: 
#' 
#'   - x can be a vector of longitudes (decimal degrees), 
#'   in which case y also must be provided as latitudes; or 
#'   
#'   - x can be the path/filename to a file in .xlsx or .csv format with lat, lon header names
#'   
#'   - x can be a data.frame, data.table, or matrix (or list) with lat,lon as column names (or names of list elements)
#'   
#'   - x can be omitted if one is using RStudio interactively, in which case the user is 
#'   prompted to select a file from a local drive.
#'   
#' @param y latitudes, or ignored if x was a file or table with lat,lon info.
#' @param radius in miles - gets data on all residents within that distance from each point
#' @param maxradiusmiles optional in case you want to use a radius of more than default cap
#' @param nicenames whether to use long plain english headers in table or R variable names,
#'   e.g.,  "Particulate Matter (PM 2.5 in ug/m3)" not "pm"
#' @param fips if used instead of x,y it can specify fips codes of counties, tracts, or blockgroups
#' @param shapefile not implemented
#' @param namestr optional text
#' @param nosave   logical, if TRUE, sets as FALSE and overrides save_map, save_plot, save_table. Ignored if FALSE.
#' @param nosee    logical, if TRUE, sets as FALSE and overrides see_map, see_plot, see_table. Ignored if FALSE.
#' @param save_map   logical, whether to save png image file locally
#' @param see_map    logical, whether to display interactive map
#' @param save_plot  logical, whether to save png image file locally
#' @param see_plot   logical, whether to display plot (boxplots)
#' @param save_table logical, whether to save table of data in a file locally
#' @param see_table  logical, whether to see datatable in viewer
#' @param folder full path of directory in which to save files like map, plot, table 
#'   Default is working directory. 
#' @param interactiveprompt logical, whether to prompt for upload of file and ask to hit key to continue after map shown, etc.
#'   If interactive() is TRUE, this actually defaults to TRUE.
#' @param see_table  logical, whether to display interactive table
#' @param calculate_ratios whether to add columns with ratio of raw score to the US or State average
#' @param getstatefromplacename set to FALSE if you need the exact output of API and
#'   TRUE if you want to try to extract ST abbrev and statename from the placename field,
#'   which is more likely to be correct than the stateAbbr and stateName fields in the API output.
#' @param ... passed to [ejscreenapi_plus()]
#'
#' @return Returns a list with map, boxplot, table 
#'   - map of sites with popups of EJ stats, as returned plot, 
#'     viewed interactive map and .png
#'   - graphic boxplot of some demographics as ratios to average, as returned plot, 
#'     viewed noninteractive plot and .png
#'   - table of results as a returned data.frame, 
#'     viewed interactive datatable, and .xlsx and .csv
#'     Which includes us.ratios and state.ratios via one row per site 
#'     and one column per indicator as ratios to US and to State average.
#'     
#'     It also now drops redundant columns where the same numbers had been returned from API
#'     using the normal name and a synonym name. 
#'   
#' @seealso [ejscreenapi_plus()]
#' @import leaflet
#'
#' @examples \dontrun{
#'  pts <- testpoints_50[1:3, ] # sample data from package
#'  mapfast(pts)
#'  #pts <- system.file("testdata/latlon/testpoints_10.xlsx", package="EJAM") 
#'  if (interactive)
#'  
#'  x <- ejscreenit(pts, 
#'    save_map = F, save_plot = F, save_table = F, folder = "~", see_table = T)
#'  
#'  myradius <- 1 # in miles
#'  # myradius <- 5000 / meters_per_mile # 5 kilometer radius, approx 3.1 miles
#'  # Get results from server by using API 
#'  x <- ejscreenit(
#'    pts=pts, radius = myradius,
#'    save_table = FALSE, save_map = FALSE, save_plot = FALSE)
#'   
#'  # see format of output results
#'  names(x) # [1] "table" "map"   "plot"
#'  
#'  y = ejscreenapi_plus(pts, radius = myradius)
#'  all.equal(x$table, y)
#'  
#'  # For this table view, remove map and pdf URL columns 
#'  y <- x$table[ , !grepl("EJScreen", names(x$table))] 
#'  t(y[1, ])  # see one column of results
#'  DT::datatable(y)  # see interactive data table view in viewer of RStudio
#'  
#'  # View links in RStudio Viewer window rather than shiny app or Excel 
#'  html_print(HTML(paste(paste(x$table[,"EJScreen Report"], collapse="<br>"),
#'     "<p></p>", paste(x$table[,"EJScreenMAP"], collapse="<br>") )  ))
#'  
#'  # View the boxplots of results
#'  x$plot
#'  
#'  # Save the plot as a file:
#'  
#'  # You can save it while running ejscreenit()
#'  # by including the parameters save_map = T, nosave = F, folder = "."
#'  # or afterwords like this:
#'  
#'  png(filename = file.path(folder,"Boxplot of EJ stats.png"), 
#'    width = 1200, height = 600)
#'  x$plot
#'  dev.off()
#'  
#'  # Save the map as a file:
#'  
#'  # You can save the map while running ejscreenit()
#'  # by including the parameters save_plot = T, nosave = F, folder = "."
#'  # or afterwords save as an interactive HTML webpage, like this:
#'  
#'  # htmltools::save_html(x$map, "mapfile.html")
#'  
#'  # or make it printable as a static file like this:
#'  x$map %>% leaflet.extras2::easyprintMap(
#'    filename = file.path(folder, "map.png"), exportOnly = TRUE)
#' }
#' 
#' @export
#' 
ejscreenit <- function(x, y=NULL, radius = 3, maxradiusmiles=10,
                       fips = NULL,
                       shapefile = NULL,
                       namestr = '',
                       nosave = TRUE, nosee = TRUE,
                       save_map    =TRUE, see_map  =TRUE,
                       save_plot   =TRUE, see_plot =TRUE,
                       save_table  =TRUE, see_table=FALSE,
                       interactiveprompt = FALSE, # but defaults to TRUE if interactive() !
                       calculate_ratios = TRUE,
                       nicenames = TRUE, # e.g.,  "Particulate Matter (PM 2.5 in ug/m3)" not "pm"
                       folder=".",
                       # codefilesourced='./global.R', codefoldersourced='./R',
                       getstatefromplacename = TRUE,
                       ...) {
  
  if (!is.null(shapefile)) {warning('shapefile not implemented yet')}
  
  ################################################### #  ################################################### #
  if (nosave) {save_map <- FALSE; save_plot <- FALSE; save_table <- FALSE}
  if (nosee ) {see_map  <- FALSE;  see_plot <- FALSE;  see_table <- FALSE}
  ################################################### #  ################################################### #
  if (interactive() & missing(interactiveprompt)) interactiveprompt <- TRUE
  if (any(is.null(fips))) {
    
    ## Confusingly, parameters order differs between ejscreenapi_plus() and other functions like latlon_from_anything(),
    ## where most functions in ejam use lat,lon order but ejscreenapi_plus() used x,y meaning lon,lat !!
    ## That is further complicated by the fact that the functions are flexible in allowing the first param to be a file/ table of lat,lon if the 2d is missing.
    ## So this code figures out what is what and interprets each param as appropriate:
    if (missing(y) | is.null(y)) {
      pts <- latlon_from_anything(anything = x, interactiveprompt = interactiveprompt)
    } else {
      pts <- latlon_from_anything(anything = y, lon_if_used = x, interactiveprompt = interactiveprompt)
    }
   
  } else {
    pts <- NULL
    radius <- 0
    see_map <- FALSE # Since not yet implemented
    save_map <- FALSE # Since not yet implemented
  }
  ################################################### #
  
  if (radius > maxradiusmiles) {stop(paste0('radius cannot be > ', maxradiusmiles, ' miles.'))}
  
  ################################################### #
  # MAP INPUT POINTS BEFORE BUFFERING ####
  if (see_map) {
    if (!missing(shapefile)) {
      print(mapfast(shapefile, column_names = 'all'))
    } else {
    print(mapfast(pts, column_names = 'all'))
    }
    if (interactiveprompt) {
      junk <- readline('Press any key to go on after viewing this map of input places')
    }
    # mapfast(testoutput_ejscreenapi_plus_5) is ok, but ejscreenit(pts) fails here ***
  }
  
  ################################################### #
  # GET RESULTS via API####
  # ejscreenapi_plus() to GET BUFFER RESULTS AND ENHANCE THE TABLE A BIT, in 1 step  
  if (nicenames) {
    usewhichnames <- "long"
  } else {
    usewhichnames <- "rname"
  }
  out <- ejscreenapi_plus(pts, radius = radius, mapping_for_names = map_headernames, 
                          usewhichnames = usewhichnames,
                          fips = fips,
                          shapefile = shapefile,
                          namestr = namestr,
                          # verbose = FALSE, # ALREADY THE DEFAULT IN ejscreenapi_plus() and putting it here causes problems if user tries to specify a value for it in ejscreenit()
                          calculate_ratios = calculate_ratios,
                          getstatefromplacename = TRUE,
                          ...)
  
  errorhappened <- TRUE # gets changed once all is finished without problems
  on.exit(if (errorhappened) return(out))
  
  ### prepending the input points is done in ejscreenapi_plus() now.
  # out <- cbind(pts, out) 
  
  ################################################### #
  if (0 == 1) {
    ## ejscreenapi_plus() combines these steps: ####
    ## use API to get results for each site ####
    out2 <- ejscreenapi(lon = pts$lon, lat = pts$lat, radius = radius, 
                        shapefile = shapefile, 
                        drop_redundant_indicators = TRUE,
                        getstatefromplacename = TRUE)    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  
    ## add weblinks #### 
    out2 <- urls_clusters_and_sort_cols(out2)
    # and ejscreenapi_plus() now adds ratios as well
    # ...
    ## Column headers made nicer and sorted -- but does it really do that??? #### 
    names(out2) <- fixcolnames(names(out2), "api", 'r', mapping_for_names = map_headernames)
    ## Add commas to numbers for population counts? ####
    # if ('totalPop' %in% names(out2)) out2$totalPop <- prettyNum(out2$totalPop, big.mark = ',')
    # if ('pop'      %in% names(out2)) out2$pop      <- prettyNum(out2$pop,      big.mark = ',')   #   ****
    
    ## Put user input table at left of results table? ####
    out2 <- cbind(pts, out2)
    
    # and adds placeholders if supplementary demog missing
    # and tries to add ratio to us and state average 
    
    #dim(out2) # APPROX 200 COLUMNS IN out2
    # t(out2[1,-grep('pdfurl|URL', names(out2))])
    # rm(out2)}
    ################################################### #
  }
  
  ################################################### #
  # MAPS  ####
  
  # MAP OUTPUT RESULTS - many variables
  
  ################################################### #   
  ## Make map that can be saved as png ####
  
  names(out) <- fixcolnames(names(out), usewhichnames, 'r') # and later to do  names(out) <- fixcolnames(names(out), 'r', usewhichnames)
  
  if (any(!is.null(fips))) {
    outmap <- NA
  } else {
  
  #   longer popup text (all Demog and all Envt indicators)
  mypopup_full_results <- popup_from_ejscreen(out)
  #  popup_print(mypopup_full_results)
  outmap  <- leaflet::leaflet(out) %>% leaflet::addTiles() %>% leaflet::clearShapes() %>%
    leaflet::addCircles(radius = out$radius.miles[1] * meters_per_mile ,   #  units in meters for addCircles, and pixels for addCircleMarkers
                        popupOptions = list(maxHeight = 400, maxWidth = 850),
                        popup = mypopup_full_results,
                        lat = ~lat, lng = ~lon)
  
  ################################################### #   
  ## Save map as png file ####
  
  if (save_map) {
    # outmap %>%
    if (interactive()) {
      # cat("Saving Map\n")
      leaflet.extras2::easyprintMap(outmap, filename = rstudioapi::selectFile(caption = "Save Map", path = folder, filter = "*.png", existing = FALSE)) # file.choose())
    } else {
      leaflet.extras2::easyprintMap(outmap, filename = file.path(folder, 'map.png'))
    }
  }
  
  ## Add save button on map ####
  leaflet.extras2::addEasyprint(map = outmap, options = leaflet.extras2::easyprintOptions(
    exportOnly = TRUE, filename = file.path(folder, "map.png"), # this specifies it would be saved as a png file
    title = 'Save Map Snapshot')) # but this does not show up in x$map result of this overall ejscreenit() ??
  
  ## Show map in RStudio viewer ####
  if (see_map) {
    # print(mapfast(out, radius = out$radius.miles[1], column_names = 'ej'))  # not sure why this line was here in addition to print(outmap)
    print(outmap)
  }
  }
  # stop('pause here in script')
  
  if (calculate_ratios) {
    
    ################################################### #   
    # BOXPLOT RATIOS TO US AVG ####
    #
    # NOTE THAT these ratios are already available as columns in the out table, now, via updated ejscreenapi_plus()  :
    
    names_e_FOR_RATIOS <- names_e
    names_d_FOR_RATIOS <- c(names_d, names_d_subgroups) # c(names_d, names_d_subgroups) # map_headernames$newnames_ejscreenapi[ map_headernames$varlist == "names_d"]
    names_d_FOR_RATIOS <- intersect(names_d_FOR_RATIOS, names(out)) # in case subgroups missing for ratios since average was needed to calculate that
    
    us.ratios <- list(
      ratios_e = out[ , intersect( paste0('ratio.to.avg.', names_e_FOR_RATIOS), names(out)) ], 
      ratios_d = out[ , intersect( paste0('ratio.to.avg.', names_d_FOR_RATIOS), names(out)) ]          #  ]
    )  
    # st.ratios <- list(
    #   ratios_e = out[ , intersect( paste0('ratio.to.state.avg.', names_e_FOR_RATIOS) , names(out)) ], 
    #   ratios_d = out[ , intersect( paste0('ratio.to.state.avg.', names_d_FOR_RATIOS) , names(out)) ]
    # )  
    
    ## boxplots_ratios(us.ratios$ratios_d)
    outplot <- boxplots_ratios(us.ratios$ratios_d, 'ratio.to.avg.pctlowinc', '% low income', wheretext = out$radius.miles[1])
    # outplot <- boxplots_ratios(st.ratios$ratios_d, 'ratio.to.state.avg.pctlowinc', '% low income', wheretext = out$radius.miles[1])
    if (see_plot) {print(outplot)}
    if (save_plot) {
      if (interactive()) {
        png(filename = rstudioapi::selectFile(caption = "Save Plot", path = folder, filter = "*.png", existing = FALSE)) # file.choose())  
        # boxplots_ratios(us.ratios$ratios_d, 'pctlowinc', '% low income')
        print(outplot)
        # outplot
        dev.off()
      } else {
        png(filename = file.path(folder, 'outplot.png'))  
        # boxplots_ratios(us.ratios$ratios_d, 'pctlowinc', '% low income')
        print(outplot)
        # outplot
        dev.off()
      }
    }
  }
  
  names(out) <- fixcolnames(names(out), 'r', usewhichnames)
  
  ################################################### #
  # SAVE as csv OR EXCEL SPREADSHEET ####
  if (save_table) {
    if (interactive()) {
      openxlsx::write.xlsx(out, file = rstudioapi::selectFile(caption = "Save Spreadsheet", path = folder, filter = "*.xlsx", existing = FALSE)) # file.choose())  
    } else {
      openxlsx::write.xlsx(out, file = file.path(folder, 'ejscreenapi_testout.xlsx'))
    }
  }
  # if (nicenames) {
  #   names(out) <- fixcolnames(names(out), 'r', 'long')
  # }
  
  ################################################### #
  # INTERACTIVE TABLE OF RESULTS ####
  if (interactiveprompt) {
    if (interactiveprompt & !nosee & (see_plot | see_map) & see_table) {
      junk <- readline('Press any key to go on to see data table, after viewing map or plot')
    }
    if (see_table) {DT::datatable(out, escape = F)}
  }
  ################################################### #
  errorhappened <- FALSE 
  if (!exists("outplot")) {outplot <- NULL} # if calculate_ratios = FALSE then plot not created
  return(list(table = out, map = outmap, plot = outplot))
}
