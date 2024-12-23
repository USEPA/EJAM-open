
#' Given a set of points (lat lon), get table of EJScreen API results near each
#' 
#' @description Using EJScreen API without Shiny app interface 
#' @details See [ejscreenit()] for more details on this.
#'    [ejscreenit()] uses functions below, but returns a list with table, map, plot, etc.
#'      [ejscreenapi_plus()] accepts file or table or vectors of point data, 
#'        uses [ejscreenapi()] to get EJScreen stats,
#'        and then prepends input table and renames columns, to return a table.
#'        [ejscreenapi()]    gets EJ stats for many points as a data.table of many rows.
#'          [ejscreenapi1()] gets EJ stats for 1 point via API, as data.frame of 1 row.
#'          [ejscreenRESTbroker()] gets EJ stats for one point as JSON.
#'          
#'   It also now drops redundant columns where the same numbers had been returned from API
#'   using the normal name and a synonym name, as with TOTALPOP and "totalPop" 
#'   
#' @param x longitudes; or path/filename to xlsx or csv with lat, lon;
#'    or data.frame or data.table with lat,lon
#' @param y latitudes, or ignored if x was a file or table with lat,lon info.
#' @param radius circular buffer radius (in miles by default, unless unit changed)
#' @param unit default is miles
#' @param wkid do not use. https://epsg.io/4326
#' 
#' @param fips if used instead of lon,lat it should be a character FIPS code vector
#'   (counties, tracts, or blockgroups)
#' @param shapefile not implemented
#' @param namestr optional text
#' @param report_every_n default is to provide an update every so often
#' @param save_when_report default is FALSE but if TRUE it saves work in progress every so often
#' @param format_report_or_json do not use
#' @param on_server_so_dont_save_files default is FALSE, but set to TRUE if this is run on a server
#' @param ipurl change only if different URL has to be used for the EJScreen API
#' @param mapping_for_names a table that translates between original (as on FTP site), 
#'   short friendly (useful in coding or analysis), 
#'   and long complete variable names (for clearer Excel headers).
#'   This can be read from a csv file or from data in a package. 
#'   Not documented here, as format may change. 
#' @param usewhichnames default is to use the short friendly ones?
#' @param calculate_ratios whether to add columns with ratio of raw score to the US or State average
#' @param verbose whether to print to console / viewer / plot
#' @param getstatefromplacename set to FALSE if you need the exact output of API and
#'   TRUE if you want to try to extract ST abbrev and statename from the placename field,
#'   which is more likely to be correct than the stateAbbr and stateName fields in the API output.
#'
#' @return Returns a data.frame of results, one row per buffer (site), one column per indicator,
#'   with over 300 columns. 
#'   [ejscreenit()] returns that as one element of a list that also has a map and plot.
#' @seealso [ejscreenit()] which also demonstrates a map and a plot, 
#'   and accepts filename as input pts. see [ejscreenapi()] that 
#'   uses [ejscreenapi1()] and [ejscreenRESTbroker()]
#'
#' @examples \dontrun{ # see [ejscreenit()] for examples
#'   pts <- data.frame(
#'    siteid = 1:2,
#'    sitename = c("site A", "site B"),
#'    lon =      c(-91.132107, -91.09),
#'    lat =      c(30.494982,   30.45)
#'   )
#'   # pts <- testpoints_5
#'   
#'   myradius <- 1
#'   
#'   x <- testoutput_ejscreenapi_plus_5; names(x) <- fixcolnames(names(x), "r", "long")
#'   # x <- ejscreenapi_plus(pts,              radius = myradius, usewhichnames = "long")
#'   # x <- ejscreenapi_plus(x = pts$lon, y = pts$lat, radius = myradius, usewhichnames = "long")
#'   # x <- ejscreenapi_plus(pts,              radius = myradius, usewhichnames = "long")
#'   
#'   ## view results
#'   t(x[1, 3:ncol(x)])
#'   DT::datatable(x , escape = FALSE)
#'   
#'   names(x) <- fixcolnames(names(x), "long", "r")
#'   boxplots_ratios((calc_ratios_to_avg(x))$ratios_d, wheretext = myradius)
#'   
#'  }
#'  
#' @export
#' @keywords internal
#'  
ejscreenapi_plus <- function(x, y=NULL, radius = 3, unit ='miles', wkid=4326,
                             fips = NULL,
                             shapefile = NULL, # pending
                             namestr = '',
                             report_every_n=100, save_when_report=FALSE, 
                             format_report_or_json='pjson', on_server_so_dont_save_files=FALSE, ipurl='ejscreen.epa.gov',
                             mapping_for_names = NULL, 
                             usewhichnames='r',
                             calculate_ratios=TRUE,
                             verbose=FALSE,
                             getstatefromplacename = TRUE) {
  
  if (!is.null(shapefile)) {warning('shapefile not implemented yet')}
  
  if (is.null(mapping_for_names)) {mapping_for_names <- map_headernames} # should be available as data via package or loaded via global.R
  
  if (any(!is.null(fips))) {
    radius <- 0
    lat = rep(NA, length(fips))
    lon = rep(NA, length(fips))
    pts <- data.table(fips = fips, lat = lat, lon = lon)
  } else {
    
  # For convenience, if x is a path and filename (and y is missing), read the file named by x, but 
  # and if x is a data.frame with lat,lon columns, just use it.
    ## Confusingly, parameters order differs between ejscreenapi_plus() and other functions like latlon_from_anything(),
    ## where most functions in ejam use lat,lon order but ejscreenapi_plus() used x,y meaning lon,lat !!
    ## That is further complicated by the fact that the functions are flexible in allowing the first param to be a file/ table of lat,lon if the 2d is missing.
    ## So this code figures out what is what and interprets each param as appropriate:
  if (missing(y) | is.null(y)) {
    pts <- latlon_from_anything(anything = x)
  } else {
    pts <- latlon_from_anything(anything = y, lon_if_used = x)
  }
  lon <- pts$lon; lat <- pts$lat
  }
  # ***use EJScreen API*** ####
  
  batchtableout <- ejscreenapi(
    lon = lon, lat = lat,
    radius = radius, unit = unit, wkid = wkid, 
    fips = fips,
    shapefile = shapefile, 
    namestr = namestr,
    format_report_or_json = format_report_or_json, ipurl = ipurl,
    report_every_n = report_every_n, save_when_report = save_when_report, 
    on_server_so_dont_save_files = on_server_so_dont_save_files,
    drop_redundant_indicators = TRUE,
    verbose = verbose,
    getstatefromplacename = getstatefromplacename
  ) # possibly could add progress bar as a widget here 
  
  ############################################################## #
  
  # THIS PART IS nearly IDENTICAL TO WHAT app_server.R DOES: ####
  #
  ## Add more columns to basic batch results ####
  ### Combine input (from user) + output (from EJ stat buffering) ####
  #      (e.g., that user input uploaded may have site name, address, etc.) 
  #      & return combined table: 1 row per site (buffer), 1 col per indicator (variable).
  ### Add links to EJScreen ####
  ### Flag sites near others ####
  ### Put best cols 1st #### 
  results_table <- cbind(pts, batchtableout) # needed here to allow links to be made . Since pts was a data.table, results_table here will be too!
  results_table <- urls_clusters_and_sort_cols(results_table)
  
  # results_table <- results_table[, names(results_table) != 'mapurl']   # drop this column that was only useful while viewing uploaded points but is redundant in final results here
  
  ############################################################## #
  
  # table is renamed here so code will be identical to code in server.R
  table_as_displayed <- results_table
  setDF(table_as_displayed) # code below assumed it was df not dt
  
  # colnames in results table are always api version of names
  #   name the columns using the Rfieldnames style  - also ensures any calc_ratios_to_avg() will work right
  
  names(table_as_displayed) <- fixcolnames(
    namesnow = names(table_as_displayed), 
    oldtype = 'api', # oldtype = "original",
    newtype = 'r',  
    mapping_for_names = mapping_for_names
  )
  # remove duplicate columns since pts had lat,lon and renamed geometry.x,.y become second versions of lat,lon
  if (anyDuplicated(names(table_as_displayed))) {
    table_as_displayed <- table_as_displayed[, !duplicated(names(table_as_displayed))]
  }
  # maybe obsolete but had also been needed:
  if (('lat' %in% names(table_as_displayed)) & ('lat.1' %in% names(table_as_displayed)) ) {
    if (all.equal(table_as_displayed$lat, table_as_displayed$lat.1)) {
      table_as_displayed$lat.1 <- NULL
    }
  }
  if (('lon' %in% names(table_as_displayed)) & ('lon.1' %in% names(table_as_displayed))  ) {
    if (all.equal(table_as_displayed$lon, table_as_displayed$lon.1)) {
      table_as_displayed$lon.1 <- NULL
    }
  }
  ############################################################# #
  
  # note: ejscreenapi_plus() vs app_server_EJAMejscreenapi vs MODULE ############################################################# #
  
  ### Add Ratios to us or state averages ####
  
  if (calculate_ratios) {
    table_as_displayed <- calc_ratios_to_avg_for_ejscreenapi(table_as_displayed = table_as_displayed)
  } # end of ratio calculations 
  
  ############################################################## #
  
  
  
  
  
  #-_Commas for pop count ####
  # if ('totalPop' %in% names(table_as_displayed)) table_as_displayed$totalPop <- prettyNum(table_as_displayed$totalPop, big.mark = ',') 
  # if ('pop'      %in% names(table_as_displayed)) table_as_displayed$pop      <- prettyNum(table_as_displayed$pop,      big.mark = ',') 
  # # if ('total population (ACS 5yr file)' %in% names(table_as_displayed)) table_as_displayed[,`total population (ACS 5yr file)`] <- prettyNum(table_as_displayed[,`total population (ACS 5yr file)`], big.mark = ',') 
  
  if (interactive() & verbose) {
    # note that  ejscreenit()  already 
    #  prints to console and viewer but only if you send its output to console, not just capture it in a variable.
    # so just use that if interactive, like ejscreenit() or x=ejscreenit(); x 
    x <- table_as_displayed
    # names(x) <- fixnames_to_type(names(x), oldtype = "r", newtype = "r")
    #  Add commas to numbers for population counts? ####  
    if ('totalPop' %in% names(x)) x$totalPop <- prettyNum(x$totalPop, big.mark = ',')
    if ('pop'      %in% names(x)) x$pop      <- prettyNum(x$pop,      big.mark = ',')
    # if ('total population (ACS 5yr file)' %in% names(x)) x[,`total population (ACS 5yr file)`] <- prettyNum(x[,`total population (ACS 5yr file)`], big.mark = ',')
    print( 
      t(x[1,4:ncol(x)])  # columns 1:3 are very long URLs that mess up the console view here.
    )
    # print(DT::datatable(t(x[ , 4:ncol(x)]),options=list(paging=FALSE), colnames = x$`Point ID`))
    
    # boxplots_ratios(calc_ratios_to_avg(x), wheretext = "nearby")
  }
  
  names(table_as_displayed) <- fixcolnames(
    namesnow = names(table_as_displayed), 
    oldtype = 'r', # original
    newtype = usewhichnames, # towhichnames = usewhichnames, 
    mapping_for_names = mapping_for_names
  )
  # for some reason it does not fix ratio column names the first time??
  names(table_as_displayed) <- fixcolnames(
    namesnow = names(table_as_displayed), 
    oldtype = 'r', # original
    newtype = usewhichnames, # towhichnames = usewhichnames, 
    mapping_for_names = mapping_for_names
  )
  setDF(table_as_displayed)
  
  return(table_as_displayed)
}
