
#' Use EJScreen API to get stats on one or more circular buffers
#' 
#' Get a data.table of EJScreen report results for one or multiple circular buffers.
#' 
#' @details 
#' Specify a radius and vector of latitude longitude points,
#' and get for a buffer the population weighted mean value of each raw indicator
#' like percent low-income, and total population count, and percentiles for those
#' raw indicator scores, all from EJScreen, as in an EJScreen standard report. 
#' 
#' The functions [ejscreenapi_plus()] and [ejscreenit()] are higher-level functions
#' that provide renamed variables in their outputs.
#' Column names returned by ejscreenapi() are those provided by the EJScreen API, 
#' e.g., RAW_D_INCOME, not the renamed variables like pctlowinc etc. or Percent Low Income
#'
#'   [ejscreenRESTbroker()] is the lowest level function here for access to the API.
#'   
#'   [ejscreenRESTbroker2table()] converts that to a table.
#'   
#'   `ejscreenRESTbroker2table(ejscreenRESTbroker())`
#'   returns the same 1-row data.frame as ejscreenapi1() 
#'   except the latter drops the percent signs and makes those values numeric, 
#'   converting text like 45% to the number 45.
#'   
#'   This also drops redundant columns where the same numbers had been returned from API
#'   using the normal name and a synonym name, as with TOTALPOP and "totalPop" 
#'   
#' To compare API variable names and renamed versions:
#'  ```
#'  x <- ejscreenapi(-100, 40, 1)
#'  cbind(names(x), fixnames(names(x)))
#'  ```
#'  Note this and related functions could be recoded using httr2 and some 
#'  best practices, as described here: <https://httr.r-lib.org/articles/api-packages.html>.
#'   
#'   This relies on [ejscreenapi1()] to request URL of pdf report on each site
#'   via the API, and does some error checking, but like [ejscreenapi1()] it does a GET
#'   request via API and then parses the JSON results from the GET request, cleans it up,
#'   adds URLs as links, compiles it as a data.table, 
#'   enables a progress bar, etc.
#'   
#' Note that this API is fairly slow, so it is fine for 10 sites, but not large numbers.
#' It varies, but can run about 1k to 10k sites per hour, 
#' for circular buffers of 1 or 3 mile radius.
#' It sometimes fails to obtain results, 
#' which may be caused by unreliable results from the API
#' rather than the code requesting results via the API.
#' 
#'    See  <https://www.epa.gov/ejscreen/ejscreen-api>
#' 
#' @param lon Longitude numeric vector
#' @param lat Latitude numeric vector
#' @param radius radius of circular buffer (uses a default if none specified)
#' @param unit "miles" (default) or "kilometers"
#' @param wkid optional spatial reference code, if default is not what is needed
#' 
#' @param fips if used instead of lon,lat it should be a character FIPS code vector
#'   (counties, tracts, or blockgroups)
#' @param shapefile not implemented
#' @param namestr optional text
#' @param report_every_n Should it report ETA snd possibly save interim file after every n points
#' @param save_when_report optional, write .rdata file to working directory 
#'   with results so far, after ever n points, to have most results even if it crashes
#' @param format_report_or_json  default is pjson but could modify to allow it to be report to get just a pdf URL
#'   but that also can be gotten via [url_ejscreen_report()]
#' @param on_server_so_dont_save_files FALSE by default, but TRUE prevents saving any progress or crash-related files
#' @param ipurl which URL or IP to try
#' @param updateProgress Used to create progress bar in Shiny app
#' @param drop_redundant_indicators Set to FALSE if you do not want to exclude from results the indicators that appear twice 
#'   just under 2 different names, like RAW_D_LIFEEXP and RAW_HI_LIFEEXPPCT which are identical.
#' @param nicenames Set it to TRUE if you want to have it rename headers as long friendly plain English not R variable names
#'   but note downstream functions mostly expect rname format
#'    that uses [ejscreenapi1()] and [ejscreenRESTbroker()]  and [ejscreenRESTbroker2table()]
#' @param verbose whether to print to console / viewer / plot
#' @param getstatefromplacename set to FALSE if you need the exact output of API and
#'   TRUE if you want to try to extract ST abbrev and statename from the placename field,
#'   which is more likely to be correct than the stateAbbr and stateName fields in the API output.
#' @seealso [ejscreenit()] [ejscreenapi_plus()] [ejscreenapi()]
#' @examples  
#'  \dontrun{
#'  # Specify size of buffer circle and pick random points as example data
#'  myradius <- 1
#'  pts2 <- data.frame(lon = c(-111.9040233, -73.7917865), lat = c(33.5604162, 41.0613821))
#'  pts5 <- testpoints_5
#'  
#'  out <- ejscreenapi(lon = pts2$lon, lat = pts2$lat, radius = myradius)
#'  head(t(out))
#'  outnice <- ejscreenapi(lon = pts2$lon, lat = pts2$lat, radius = myradius, nicenames = TRUE)
#'  head(t(outnice), 24)
#'  }
#'  
#' @export
#' @keywords internal
#'  
ejscreenapi <- function(lon, lat, radius = 3, unit = 'miles', wkid = 4326 ,
                        fips = NULL,
                        shapefile = NULL,
                        namestr = '',
                        report_every_n=1000, save_when_report=FALSE, 
                        format_report_or_json='pjson', on_server_so_dont_save_files=FALSE, 
                        ipurl='ejscreen.epa.gov', updateProgress=NULL, drop_redundant_indicators=TRUE, 
                        nicenames=FALSE,
                        verbose=TRUE,
                        getstatefromplacename = TRUE
) {
  
  offline_warning(); offline_cat()
  
  if (!is.null(shapefile)) {warning('shapefile not implemented yet for ejscreenapi()')}
  
  if (any(!is.null(fips))) {
    
    radius <- rep(0, length(fips))
    lat = rep(NA, length(fips))
    lon = rep(NA, length(fips))
    
  } else {
    
    # Could possibly allow lat lon to be specified in a csv or xlsx file here, too.
    #################################################################################### #
    # warn if invalid lat lon values ####
    ok_point <- latlon_is.valid(lat = lat, lon = lon)
    if (!all(ok_point)) {
      warning(paste0(sum(!ok_point), ' lat lon values look invalid'))
      lat[!ok_point] <- NA; lon[!ok_point] <- NA
      # rather than drop those rows, try to return NA values
    }
    if (any(is.na(radius)) | any(radius <= 0) | any(radius > 100)) {stop('radius outside allowed range')}
  }
  
  if (!(unit %in% c('miles', 'kilometers'))) {stop('unit must be miles or kilometers')}
  unitcode = switch(unit,
                    'miles' = 9035,
                    'kilometers' = 9036
  )
  # SAME AS ejscreenapi1() above here ########### #
  #################################################################################### #
  
  if (!(format_report_or_json %in% 'pjson')) {
    if (format_report_or_json == "report") {
      return(
        ejscreenapi1( # vectorized at least for this use
          lon = lon, 
          lat = lat, 
          radius = radius, 
          unit = unit, wkid = wkid,
          fips = fips,
          namestr = namestr,
          shapefile = shapefile, # something for shapefiles would require POST not GET
          format_report_or_json = "report")
      )
    } else {
      stop('format_report_or_json must be pjson or report')
    }
    stop('format_report_or_json must be pjson')
  }
  # if (!(format_report_or_json %in% c('pjson', 'report'))) {stop('format_report_or_json must be pjson or report')}
  
  #################################################################################### #
  
  pts <- data.frame(lon = lon, lat = lat)
  n <- NROW(pts)

  if (verbose) {
  cat("\n Using ejscreenapi() to analyze", n, 'sites',
      ## 'for residents living within a radius of', radius[1], unit, 'from each site.', 
      '\n', file = stderr())
  if (n > 10) {
    cat(
      speedmessage(n = n, perhourslow = 900, perhourfast = 1800, perhourguess = 1400), '\n', file = stderr()
    )
  }
  }
  # be ready to save progress so far if it crashes during a very time consuming long run #### 
  # if (save_when_report & !on_server_so_dont_save_files){
  tdir = tempdir()
  # }
  finished_without_crashing <- FALSE
  on.exit({if (!finished_without_crashing) {
    if (!on_server_so_dont_save_files) {
      save(outlist, file = file.path(tdir, 'saved_this_before_crash.rdata'))
      cat("see ", file.path(tdir, 'saved_this_before_crash.rdata'), "\n")
    }
  } })
  
  # emptyresults will let us return empty row if no valid result near a point ####
  # could have used ejscreenapi1() but this should work too:
  emptyresults <- ejscreenRESTbroker2table_na_filler
  
  # prepare to store results , pre-allocate space ####
  outlist <- vector(mode = 'list', length = n)
  pdfurl  <- vector(mode = 'list', length = n)
  noresults_count <- 0
  
  #################################################  #################################################
  ########################## LOOP OVER POINTS ###############  #################################################
  #################################################  #################################################
  benchmark.start <- Sys.time()


  if (length(radius) == 1) {radius <- rep(radius, n)}
  if (length(fips) == 1) {fips <- rep(fips, n)}
  # if (length(namestr) == 1) {namestr <- rep(namestr, n)}
  
  for (i in 1:n) {
    
    if (NROW(shapefile) == 0) {shaperow <- NULL} else {shaperow <- shapefile[i, ]}
    
    ############################################################################### # 
    ## use API to get buffer result from EJScreen server ** ####
    
    ej.data <- try(ejscreenRESTbroker(
      lon = pts$lon[i], 
      lat = pts$lat[i], 
      radius = radius[i], 
      unit = unitcode, wkid = wkid,
      fips = fips[i],
      namestr = namestr[i], ### #
      shapefile = shaperow, 
      ipurl = ipurl,
      f = format_report_or_json
    ))
    
    if (inherits(ej.data, 'try-error')) {
      failed <- TRUE; warning('API not accessible or failed')
      ej.data <- emptyresults
    } else {failed <- FALSE}
    
    if (!failed) {
      
      # use API to get URL of report ####
      # or perhaps directly use    url_ejscreen_report()

      pdfurl[i] <- try(ejscreenapi1(
        lon = pts$lon[i], 
        lat = pts$lat[i], 
        radius = radius[i], 
        unit = unit, wkid = wkid, 
        fips = fips[i],
        namestr = namestr[i],
        shapefile = shaperow,
        ipurl = ipurl,
        format_report_or_json = 'report'
      ))
      
      if (inherits(pdfurl[i], 'try-error')) {failed <- TRUE; warning('API not accessible or failed')}
      
      # parse response into data.frame format, at 1 buffer ####
      # note confusingly fromJSON() also is the name of functions in RJSONIO and in rjson packages.
      # ej.data <- try(data.table::as.data.table(jsonlite::fromJSON(
      #   rawToChar(ej.data$content)
      # )))
      ej.data <- try(ejscreenRESTbroker2table(ej.data, getstatefromplacename = getstatefromplacename)) # now it is one data.frame
      
      if (failed | inherits(ej.data, 'try-error')) {
        failed <- TRUE; warning('error in parsing JSON returned by API for point number ', i, ' - Returning no result for that point.')
        ej.data <- emptyresults
      }
    }
    
    # if null results if area too small or sparsely populated ####
    # BLANK RESULTS THAT API RETURNS DUE TO SMALL BUFFER OR SPARSELY POPULATED AREA
    # Note: api does not return values for coords in highly nonpopulated areas.
    # it just returns this as ej.data:
    #    message                                                messageType
    # 1: Input area too small or sparsely populated for output  underLimit    
    if (NCOL(ej.data) == 2) {
      failed <- TRUE
      warning('no results, probably since area is too small')
      cat(' No results - (probably because) area too small or sparsely populated for output \n', file = stderr())
    }
    
    if (NCOL(ej.data) < 100) {
      failed <- TRUE
      warning('Unexpected format returned - check API')
      cat(    'Unexpected format returned - check API ', file = stderr())
    }
    
    if (failed) {
      outlist[[i]] <- emptyresults
      noresults_count <- noresults_count + 1
      if ('message' %in% names(ej.data)) {outlist[[i]]$msg <- ej.data$message}
    } else {
      # obsolete:
      # OK. save all but geometry column since got valid results back, i.e. got more than 100 columns instead of an error message      
      # ej.data$geometry <- NULL  
      # lat lon are stored in a second and third row but the rest of those rows is just duplicated info so drop them
      # lon is in   ej.data[2,'geometry']  and also  is  pts$lon[i]
      outlist[[i]] <-  (ej.data)
      # outlist[[i]]$msg <- ''
    }
    ############################################################################### # 
    
    # Report/save progress so far in loop ####
    hourly <- speedreport(benchmark.start, Sys.time(), i)
    remaining <- n - i
    hrsleft <- remaining / hourly
    exact_minutes_to_go <- hrsleft * 60
    minutes_to_go <- round(exact_minutes_to_go, 0)
    if (i == 10 & n > 19) {
      if (verbose | n > 19) {
        cat('    Estimated', minutes_to_go, 'minutes remaining for ejscreenapi() at the rate so far.\n', file = stderr())
      }
    }
    # CAN SAVE INTERIM RESULTS IF NEEDED, in case it fails after a large number were done.
    
    if (i %% report_every_n == 0 | (i > report_every_n & i == n)) {
      # this is end of a group of report_every_n points or is the last incomplete chunk assuming we had a big enough n to bother chunking at all
      chunknum <- ceiling(i/report_every_n)
      chunkstart <- ((chunknum - 1) * report_every_n) + 1  #( i + 1 - report_every_n)
      chunkend <- i
      if (save_when_report & (!on_server_so_dont_save_files) ) {
        x <- outlist[chunkstart:chunkend]
        save(x, file = file.path(tdir, paste('temp_ejscreenapibatch_outlist_chunk', chunknum, '.rdata', sep = '')))
      }
      # Report speed so far in loop####
      if (verbose) {
        estimated_endtime <- Sys.time() +  (minutes_to_go * 60)   
        cat('\n', file = stderr())
        cat(paste0(  i, " of ", n, " done so far\n\n"))
        cat(as.character(format((benchmark.start), format =   "%r")), "is the time this batch started.\n", file = stderr())
        cat(as.character(format(Sys.time(),        format =   "%r")),  "is the time now.\n", file = stderr())
        cat(as.character(format(estimated_endtime, format =   "%r")), "is the time this batch is projected to be finished.\n", file = stderr())
        cat('\n', minutes_to_go, 'minutes estimated remaining \n\n', file = stderr())
      }
      if (noresults_count > 0)  {
        cat('Results were unavailable for ', noresults_count, ' out of these ', n, ' sites so far. \n\n', file = stderr())
      }
    }
    if (is.function(updateProgress)) {
      if (exact_minutes_to_go > 2) {
        timeleft <- paste0(minutes_to_go, " minutes")
      } else {
        timeleft <- paste0(round(exact_minutes_to_go * 60, 0), " seconds")
      }
      boldtext  <- paste0(i, " of ", n, " done")
      smalltext <- paste0("Approx. ", timeleft, " to go.")
      updateProgress(message_main = boldtext, message_detail = smalltext, value = i)
    }
  }
  #################################################  #################################################
  # LOOP OVER POINTS ENDS (outlist is ready) ###############  #################################################
  #################################################  #################################################
  
  # Save to local file the interim results if still need to  ####
  if (n < report_every_n & save_when_report & (!on_server_so_dont_save_files) ) {
    # we did not save any chunks, so save whole, in case rbind fails
    save(outlist, file = file.path(tdir, 'temp_ejscreenapibatch_outlist_full.rdata'))
    if (verbose) {
      cat("see ", file.path(tdir, 'temp_ejscreenapibatch_outlist_full.rdata'), "\n")
    }}
  
  # *** Compile results in a data.table *** (like a data.frame) ####
  results <- data.table::rbindlist(outlist, fill = T, idcol = 'id')
  ## may want to change it from 'id' to 'ejscreen_uniq_id' similar to 'ejam_uniq_id' ***
  # results <- do.call(rbind, outlist) # would be the way to do this if data.frame not data.table
  if (drop_redundant_indicators) {
    # Remove redundant columns ###
    # (API provided several indicators twice using different names,
    #  once in data$main and once in data$demographics) and maybe in data$extras
    # Find colnames in results that are an api_synonym but not an apiname, whose data is also present in the form of corresponding apiname
    # [1] "RAW_D_PEOPCOLOR"  "RAW_D_INCOME"     "RAW_D_LESSHS"     "RAW_D_LING"       "RAW_D_UNDER5"     "RAW_D_OVER64"    
    # [7] "RAW_D_UNEMPLOYED" "RAW_D_LIFEEXP"    "S_D_LIFEEXP"      "N_D_LIFEEXP"      "N_D_LIFEEXP_PER"  "geometry.x"      
    # [13] "geometry.y"
    synhere <- names(results)[!(names(results) %in% map_headernames$apiname) & (names(results) %in% map_headernames$api_synonym)]
    cnames_redundant <- synhere[fixcolnames(synhere, 'api_synonym', 'api') %in% names(results)]
    if (length(cnames_redundant) > 0) {
      colnumkeep <- which(!(names(results) %in% cnames_redundant))
      results <- results[ , colnumkeep, with = FALSE]
    }
  }
  # # Fix percent signs and N/A ####
  # ## Percentage signs in some indicators may cause problems in calculations or sorting so remove those
  results <- makenumericdfFORSHINY(results)
  
  
  ################################################################## #
  # DO NOT NEED TO GET URL FOR REPORT HERE.
  # no longer will here Add URL for pdf-like report, in results table ####
  # results$pdfurl <- unlist(pdfurl)
  #   but server code uses  urls_clusters_and_sort_cols( )
  #   and  ejscreenapi_plus() does too :   results_table <- urls_clusters_and_sort_cols(results_table) # and see  url_4table() in EJAM pkg
  ################################################################## #
  
  # Done. Report to console how long it took. ####
  if (verbose) {
    cat('Finished  \n', file = stderr())
  }
  # speedreport(benchmark.start, Sys.time(), n)
  finished_without_crashing <- TRUE
  
  if (noresults_count > 0) {
    warnmessage <- paste0('Results were unavailable for ', noresults_count,' out of these ', n, ' sites.')
    if (verbose) {
      cat(warnmessage,  "\n", file = stderr())
    }
    warning(warnmessage)
  }
  # print(noresults_count); print(NROW(results))
  attr(results, 'noresults') <- (noresults_count == NROW(results))
  
  # if (drop_redundant_indicators) { # already done above
  #   results <- results[, !(names(results) %in% map_headernames$api_synonym)] # this was removing P_HISP with no copy by another name!
  # }
  # drop this synonym still here
  if (all(c("RAW_D_LIFEEXP", "RAW_HI_LIFEEXP")  %in% names(results))) {results$RAW_HI_LIFEEXP <- NULL}
  if (nicenames) {
    names(results) <- fixcolnames(names(results) , "api", 'long') # but downstream functions mostly expect rname format
  }
  if (anyDuplicated(names(results))) {
    warning("These column names are duplicates - possibly an error renaming them:", 
            paste0(sort(names(results)[names(results) %in% names(results)[duplicated(names(results))]]), collapse = ","))
  }
  # Return results invisibly ####
  invisible(results)
  
  ########################################################################### # 
  
  ### # 7/2023 version2.2 has these variables as output if query is on 1 point with radius: 
  ### # also in   apifull_example_v2.2$apiname2.2 
  ### 
  # api_output_names <- c(
  ### 
  ### raw Demog and Envt --------------------------------------- #
  #   "RAW_D_PEOPCOLOR", "RAW_D_INCOME",  "RAW_D_LESSHS",  "RAW_D_LING", "RAW_D_UNDER5",  "RAW_D_OVER64","RAW_D_UNEMPLOYED",  "RAW_D_LIFEEXP",   "RAW_D_DEMOGIDX2",   "RAW_D_DEMOGIDX5", 
  #   "RAW_E_LEAD", "RAW_E_DIESEL","RAW_E_CANCER","RAW_E_RESP","RAW_E_TRAFFIC",     "RAW_E_NPDES","RAW_E_NPL","RAW_E_RMP","RAW_E_TSDF",          "RAW_E_O3","RAW_E_PM25",   "RAW_E_UST",    "RAW_E_RSEI_AIR",
  ###
  ### State percentiles --------------------------------------- # --------------------------------------- #
  #   "S_D_PEOPCOLOR",    "S_D_INCOME",    "S_D_LESSHS",     "S_D_LING",   "S_D_UNDER5",    "S_D_OVER64",    "S_D_UNEMPLOYED",   "S_D_LIFEEXP",    "S_D_DEMOGIDX2",     "S_D_DEMOGIDX5", 
  #   "S_E_LEAD",     "S_E_DIESEL",   "S_E_CANCER",   "S_E_RESP",  "S_E_TRAFFIC",     "S_E_NPDES","S_E_NPL",    "S_E_RMP",  "S_E_TSDF",           "S_E_O3",  "S_E_PM25",      "S_E_UST",      "S_E_RSEI_AIR", 
  ### State averages
  #   "S_D_PEOPCOLOR_PER","S_D_INCOME_PER","S_D_LESSHS_PER","S_D_LING_PER","S_D_UNDER5_PER","S_D_OVER64_PER","S_D_UNEMPLOYED_PER","S_D_LIFEEXP_PER","S_D_DEMOGIDX2_PER","S_D_DEMOGIDX5_PER", 
  #   "S_E_LEAD_PER", "S_E_DIESEL_PER","S_E_CANCER_PER","S_E_RESP_PER","S_E_TRAFFIC_PER","S_E_NPDES_PER","S_E_NPL_PER","S_E_RMP_PER","S_E_TSDF_PER","S_E_O3_PER","S_E_PM25_PER","S_E_UST_PER","S_E_RSEI_AIR_PER",
  ### State percentiles EJ index - 2-factor and then supplemental 5-factor versions
  #   "S_P2_LEAD",   "S_P2_DIESEL", "S_P2_CANCER", "S_P2_RESP", "S_P2_TRAFFIC",      "S_P2_NPDES", "S_P2_NPL", "S_P2_RMP", "S_P2_TSDF",           "S_P2_O3",  "S_P2_PM25",   "S_P2_UST",     "S_P2_RSEI_AIR",  
  #   "S_P5_LEAD",   "S_P5_DIESEL", "S_P5_CANCER", "S_P5_RESP", "S_P5_TRAFFIC",      "S_P5_NPDES", "S_P5_NPL", "S_P5_RMP", "S_P5_TSDF",           "S_P5_O3",  "S_P5_PM25",   "S_P5_UST",     "S_P5_RSEI_AIR",
  ###
  ### US averages --------------------------------------- # --------------------------------------- #
  #   "N_D_PEOPCOLOR", "N_D_INCOME",  "N_D_LESSHS", "N_D_LING", "N_D_UNDER5", "N_D_OVER64", "N_D_UNEMPLOYED", "N_D_LIFEEXP", "N_D_DEMOGIDX2", "N_D_DEMOGIDX5", 
  #   "N_E_LEAD",  "N_E_DIESEL", "N_E_CANCER", "N_E_RESP", "N_E_TRAFFIC", "N_E_NPDES", "N_E_NPL", "N_E_RMP", "N_E_TSDF", "N_E_O3", "N_E_PM25", "N_E_UST",   "N_E_RSEI_AIR",
  ### US percentiles
  #   "N_D_PEOPCOLOR_PER", "N_D_INCOME_PER", "N_D_LESSHS_PER", "N_D_LING_PER", "N_D_UNDER5_PER", "N_D_OVER64_PER", "N_D_UNEMPLOYED_PER",   "N_D_LIFEEXP_PER", "N_D_DEMOGIDX2_PER", "N_D_DEMOGIDX5_PER", 
  #   "N_E_LEAD_PER",      "N_E_DIESEL_PER", "N_E_CANCER_PER", "N_E_RESP_PER",   "N_E_TRAFFIC_PER", "N_E_NPDES_PER", "N_E_NPL_PER", "N_E_RMP_PER",   "N_E_TSDF_PER", "N_E_O3_PER", "N_E_PM25_PER", "N_E_UST_PER",   "N_E_RSEI_AIR_PER",
  ### US percentiles EJ index - 2 then 5-factor (suppl)
  #   "N_P2_LEAD", "N_P2_DIESEL", "N_P2_CANCER", "N_P2_RESP", "N_P2_TRAFFIC", "N_P2_NPDES", "N_P2_NPL", "N_P2_RMP", "N_P2_TSDF", "N_P2_O3", "N_P2_PM25", "N_P2_UST", "N_P2_RSEI_AIR", 
  #   "N_P5_LEAD", "N_P5_DIESEL", "N_P5_CANCER", "N_P5_RESP", "N_P5_TRAFFIC", "N_P5_NPDES", "N_P5_NPL", "N_P5_RMP", "N_P5_TSDF", "N_P5_O3", "N_P5_PM25", "N_P5_UST", "N_P5_RSEI_AIR", 
  ### 
  ### misc / general info --------------------------------------- # --------------------------------------- #
  #   "stateAbbr", "stateName", "epaRegion", "totalPop", 
  #   "NUM_NPL", "NUM_TSDF", "NUM_WATERDIS",  "NUM_AIRPOLL", "NUM_BROWNFIELD", "NUM_TRI", "NUM_SCHOOL", "NUM_HOSPITAL",  "NUM_CHURCH", "YESNO_TRIBAL", "YESNO_CEJSTDIS", "YESNO_IRADIS", 
  #   "YESNO_AIRNONATT", "YESNO_IMPWATERS", "YESNO_HOUSEBURDEN", "YESNO_TRANSDIS", "YESNO_FOODDESERT", 
  #   "centroidX", "centroidY",
  #   "geometry.spatialReference.wkid", "geometry.x"  , "geometry.y",    ### if not removed and not a blockgroup query on fips, e.g. 
  ### maybe instead    "areaid", "areatype",  ### if relevant since did query of blockgroup(s) on fips, e.g.    
  #   "statLayerCount",  "statLayerZeroPopCount", "weightLayerCount", "timeSeconds", 
  #   "distance",  "unit", 
  #   "statlevel", "inputAreaMiles", "placename", 
  ### 
  ### language, race ethnicity subgroups, other demographics including counts --------------------------------------- #
  #   "P_ENGLISH", "P_SPANISH", "P_FRENCH", "P_RUS_POL_SLAV", "P_OTHER_IE", "P_VIETNAMESE", "P_OTHER_ASIAN", "P_ARABIC", "P_OTHER", "P_NON_ENGLISH", 
  #  "P_WHITE", "P_BLACK", "P_ASIAN", "P_HISP", "P_AMERIND", "P_HAWPAC", "P_OTHER_RACE", "P_TWOMORE", 
  #  "P_AGE_LT5", "P_AGE_LT18", "P_AGE_GT17", "P_AGE_GT64", 
  #   "P_HLI_SPANISH_LI", "P_HLI_IE_LI", "P_HLI_API_LI", "P_HLI_OTHER_LI",
  # "P_LOWINC", "PCT_MINORITY",  "P_EDU_LTHS", "P_LIMITED_ENG_HH",  "P_EMP_STAT_UNEMPLOYED", "P_DISABILITY", "P_MALES", "P_FEMALES",
  # "LIFEEXP", "PER_CAP_INC", "HSHOLDS", "P_OWN_OCCUPIED", 
  #
  ### Health indicators, Critical service Gaps, and Climate Indicators (RAW, State avg and pctiles, National avg and pctiles) --------------------------------------- #
  #
  # "RAW_HI_LIFEEXP", "RAW_HI_LIFEEXPPCT",          "RAW_HI_HEARTDISEASE",     "RAW_HI_ASTHMA",       "RAW_HI_CANCER", "RAW_HI_DISABILITYPCT",  
  # "RAW_CG_LIMITEDBBPCT", "RAW_CG_NOHINCPCT",           "RAW_CI_FLOOD",    "RAW_CI_FLOOD30",        "RAW_CI_FIRE",  "RAW_CI_FIRE30",
  #
  # "S_HI_LIFEEXP_AVG", "S_HI_LIFEEXPPCT_AVG",        "S_HI_HEARTDISEASE_AVG",    "S_HI_ASTHMA_AVG",    "S_HI_CANCER_AVG", "S_HI_DISABILITYPCT_AVG",  
  # "S_CG_LIMITEDBBPCT_AVG", "S_CG_NOHINCPCT_AVG",         "S_CI_FLOOD_AVG",  "S_CI_FLOOD30_AVG",      "S_CI_FIRE_AVG", "S_CI_FIRE30_AVG",
  # 
  # "S_HI_LIFEEXP_PCTILE",  "S_HI_LIFEEXPPCT_PCTILE", "S_HI_HEARTDISEASE_PCTILE", "S_HI_ASTHMA_PCTILE", "S_HI_CANCER_PCTILE", "S_HI_DISABILITYPCT_PCTILE",  
  # "S_CG_LIMITEDBBPCT_PCTILE",  "S_CG_NOHINCPCT_PCTILE",  "S_CI_FLOOD_PCTILE", "S_CI_FLOOD30_PCTILE", "S_CI_FIRE_PCTILE", "S_CI_FIRE30_PCTILE",
  #
  # "N_HI_LIFEEXP_AVG",  "N_HI_LIFEEXPPCT_AVG",       "N_HI_HEARTDISEASE_AVG",    "N_HI_ASTHMA_AVG",    "N_HI_CANCER_AVG", "N_HI_DISABILITYPCT_AVG", 
  # "N_CG_LIMITEDBBPCT_AVG", "N_CG_NOHINCPCT_AVG",         "N_CI_FLOOD_AVG",    "N_CI_FLOOD30_AVG",    "N_CI_FIRE_AVG",  "N_CI_FIRE30_AVG", 
  # 
  # "N_HI_LIFEEXP_PCTILE", "N_HI_LIFEEXPPCT_PCTILE", "N_HI_HEARTDISEASE_PCTILE",  "N_HI_ASTHMA_PCTILE", "N_HI_CANCER_PCTILE", "N_HI_DISABILITYPCT_PCTILE",  
  # "N_CG_LIMITEDBBPCT_PCTILE", "N_CG_NOHINCPCT_PCTILE",   "N_CI_FLOOD_PCTILE", "N_CI_FLOOD30_PCTILE", "N_CI_FIRE_PCTILE", "N_CI_FIRE30_PCTILE"
  # )
  ###
  # emptyresults <- data.frame(matrix(NA, ncol = length(api_output_names)))
  # names(emptyresults) <- api_output_names
  
  
  
  
  ##  # note: below was the version 2.1 sort order returned by the API as of 6/15/2023, just before the EJScreen July update to v2.2:  ####
  #
  # latestnames <- c(
  # "RAW_D_MINOR", "RAW_D_INCOME", "RAW_D_LESSHS", "RAW_D_LING", "RAW_D_UNDER5", "RAW_D_OVER64", "RAW_D_UNEMPLOYED", "RAW_D_INDEX", 
  # "RAW_E_LEAD", "RAW_E_DIESEL", "RAW_E_CANCER", "RAW_E_RESP", "RAW_E_TRAFFIC",   "RAW_E_NPDES", "RAW_E_NPL", "RAW_E_RMP", "RAW_E_TSDF", "RAW_E_O3",   "RAW_E_PM25", "RAW_E_UST",  
  # 
  # "S_D_MINOR", "S_D_INCOME", "S_D_LESSHS",  "S_D_LING", "S_D_UNDER5", "S_D_OVER64", "S_D_UNEMPLOYED", "S_D_INDEX", 
  # "S_E_LEAD", "S_E_DIESEL", "S_E_CANCER", "S_E_RESP", "S_E_TRAFFIC",   "S_E_NPDES", "S_E_NPL", "S_E_RMP", "S_E_TSDF", "S_E_O3", "S_E_PM25",   "S_E_UST",
  # "S_D_MINOR_PER", "S_D_INCOME_PER", "S_D_LESSHS_PER",   "S_D_LING_PER", "S_D_UNDER5_PER", "S_D_OVER64_PER", "S_D_UNEMPLOYED_PER",   "S_D_INDEX_PER",
  # "S_E_LEAD_PER", "S_E_DIESEL_PER", "S_E_CANCER_PER",  "S_E_RESP_PER", "S_E_TRAFFIC_PER", "S_E_NPDES_PER", "S_E_NPL_PER",   "S_E_RMP_PER", "S_E_TSDF_PER", "S_E_O3_PER", "S_E_PM25_PER",  "S_E_UST_PER", 
  # "S_P_LEAD", "S_P_DIESEL", "S_P_CANCER", "S_P_RESP",   "S_P_TRAFFIC", "S_P_NPDES", "S_P_NPL", "S_P_RMP", "S_P_TSDF",   "S_P_O3", "S_P_PM25", "S_P_UST",
  # 
  # "R_D_MINOR", "R_D_INCOME", "R_D_LESSHS",  "R_D_LING", "R_D_UNDER5", "R_D_OVER64", "R_D_UNEMPLOYED", "R_D_INDEX",
  # "R_E_LEAD", "R_E_DIESEL", "R_E_CANCER", "R_E_RESP", "R_E_TRAFFIC",   "R_E_NPDES", "R_E_NPL", "R_E_RMP", "R_E_TSDF", "R_E_O3", "R_E_PM25",    "R_E_UST", 
  # "R_D_MINOR_PER", "R_D_INCOME_PER", "R_D_LESSHS_PER", "R_D_LING_PER", "R_D_UNDER5_PER", "R_D_OVER64_PER", "R_D_UNEMPLOYED_PER",    "R_D_INDEX_PER",
  # "R_E_LEAD_PER", "R_E_DIESEL_PER", "R_E_CANCER_PER",   "R_E_RESP_PER", "R_E_TRAFFIC_PER", "R_E_NPDES_PER", "R_E_NPL_PER", "R_E_RMP_PER", "R_E_TSDF_PER", "R_E_O3_PER", "R_E_PM25_PER",    "R_E_UST_PER",
  # "R_P_LEAD", "R_P_DIESEL", "R_P_CANCER", "R_P_RESP",   "R_P_TRAFFIC", "R_P_NPDES", "R_P_NPL", "R_P_RMP", "R_P_TSDF",    "R_P_O3", "R_P_PM25", "R_P_UST", 
  # 
  # "N_D_MINOR", "N_D_INCOME", "N_D_LESSHS",   "N_D_LING", "N_D_UNDER5", "N_D_OVER64", "N_D_UNEMPLOYED", "N_D_INDEX", 
  # "N_E_LEAD", "N_E_DIESEL", "N_E_CANCER", "N_E_RESP", "N_E_TRAFFIC",    "N_E_NPDES", "N_E_NPL", "N_E_RMP", "N_E_TSDF", "N_E_O3", "N_E_PM25",    "N_E_UST", 
  # "N_D_MINOR_PER", "N_D_INCOME_PER", "N_D_LESSHS_PER",  "N_D_LING_PER", "N_D_UNDER5_PER", "N_D_OVER64_PER", "N_D_UNEMPLOYED_PER",    "N_D_INDEX_PER",
  # "N_E_LEAD_PER", "N_E_DIESEL_PER", "N_E_CANCER_PER",   "N_E_RESP_PER", "N_E_TRAFFIC_PER", "N_E_NPDES_PER", "N_E_NPL_PER",   "N_E_RMP_PER", "N_E_TSDF_PER", "N_E_O3_PER", "N_E_PM25_PER",  "N_E_UST_PER", 
  # "N_P_LEAD", "N_P_DIESEL", "N_P_CANCER", "N_P_RESP",    "N_P_TRAFFIC", "N_P_NPDES", "N_P_NPL", "N_P_RMP", "N_P_TSDF",   "N_P_O3", "N_P_PM25", "N_P_UST",     
  # 
  # "stateAbbr", "stateName", "epaRegion",    "totalPop", "NUM_NPL", "NUM_TSDF", "statLayerCount", "statLayerZeroPopCount",   "weightLayerCount", "timeSeconds", "distance", "unit", "statlevel",   "inputAreaMiles"
  # )
  
  
  ################################################################################### #
  
}
