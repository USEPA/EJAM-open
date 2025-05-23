

#' Get an EJ analysis (residential population and environmental indicators) in or near a list of locations
#'
#' @description This is the main function in EJAM that runs the analysis.
#'   It does essentially what the web app does, to analyze/summarize near a set of points,
#'   or in a set of polygons from a shapefile, or in a list of Census Units like Counties.
#' @details See examples in vignettes/ articles at `r EJAM:::repo_from_desc('github.io', get_full_url = T)`
#'
#' @param sitepoints data.table with columns lat, lon giving point locations of sites or facilities around which are circular buffers
#' @param radius in miles, defining circular buffer around a site point
#' @param radius_donut_lower_edge radius of lower edge of donut ring if analyzing a ring not circle
#' @param maxradius miles distance (max distance to check if not even 1 block point is within radius)
#' @param avoidorphans logical If TRUE, then where not even 1 BLOCK internal point is within radius of a SITE,
#'   it keeps looking past radius, up to maxradius, to find nearest 1 BLOCK.
#'   What EJScreen does in that case is report NA, right?
#'   So, does EJAM really need to report stats on residents presumed to be within radius,
#'    if no block centroid is within radius?
#'    Best estimate might be to report indicators from nearest block centroid
#'    which is probably almost always the one your site is sitting inside of,
#'    but ideally would adjust total count to be a fraction of blockwt
#'    based on what is area of circular buffer as fraction of area of block it is apparently inside of.
#'    Setting this to TRUE can produce unexpected results, which will not match EJScreen numbers.
#'    Note that if creating a proximity score, by contrast, you
#'    instead want to find nearest 1 SITE if none within radius of this BLOCK.
#' @param quadtree (a pointer to the large quadtree object) created using [indexblocks()] which uses the SearchTree package.
#'   Takes about 2-5 seconds to create this each time it is needed.
#'   It can be automatically created when the package is attached via the .onAttach() function
#' @param fips optional FIPS code vector to provide if using FIPS instead of sitepoints to specify places to analyze,
#'   such as a list of US Counties or tracts. Passed to [getblocksnearby_from_fips()]
#' @param shapefile optional. A sf shapefile object or path to .zip, .gdb, .json, .kml, etc., or folder that has a shapefiles, to analyze polygons.
#'   e.g., `out = ejamit(shapefile = testdata("portland.json", quiet = T), radius = 0)`
#'   If in RStudio you want it to interactively prompt you to pick a file,
#'   use shapefile=1 (otherwise it assumes you want to pick a latlon file).
#' @param countcols character vector of names of variables to aggregate within a buffer using a sum of counts,
#'   like, for example, the number of people for whom a poverty ratio is known,
#'   the count of which is the exact denominator needed to correctly calculate percent low income.
#' @param wtdmeancols character vector of names of variables to aggregate within a buffer using population-weighted or other-weighted mean.
#' @param calculatedcols character vector of names of variables to aggregate within a buffer using formulas that have to be specified.
#' @param calctype_maxbg character vector of names of variables to aggregate within a buffer
#'   using max() of all blockgroup-level values.
#' @param calctype_minbg character vector of names of variables to aggregate within a buffer
#'   using min() of all blockgroup-level values.
#' @param subgroups_type Optional (uses default). Set this to "nh" for non-hispanic race subgroups
#'   as in Non-Hispanic White Alone, nhwa and others in names_d_subgroups_nh;
#'   "alone" for race subgroups like White Alone, wa and others in names_d_subgroups_alone;
#'   "both" for both versions. Possibly another option is "original" or "default"
#'   Alone means single race.
#' @param include_ejindexes whether to try to include Summary Indexes (assuming dataset is available) - passed to [doaggregate()]
#' @param calculate_ratios whether to calculate and return ratio of each indicator to US and State overall averages - passed to [doaggregate()]
#' @param extra_demog if should include more indicators from v2.2 report on language etc.
#' @param need_proximityscore whether to calculate proximity scores
#' @param infer_sitepoints set to TRUE to try to infer the lat,lon of each site around which the blocks in sites2blocks were found.
#'   lat,lon of each site will be approximated as average of nearby blocks,
#'   although a more accurate slower way would be to use reported distance of each of 3 of the furthest block points and triangulate
#' @param need_blockwt if fips parameter is used, passed to [getblocksnearby_from_fips()]
#'
#' @param thresholds list of percentiles like list(80,90) passed to
#'   batch.summarize(), to be
#'   counted to report how many of each set of indicators exceed thresholds
#'   at each site. (see default)
#' @param threshnames list of groups of variable names (see default)
#' @param threshgroups list of text names of the groups (see default)
#' @param progress_all progress bar from app in R shiny to run 
#' @param updateProgress progress bar function passed to [doaggregate()] in shiny app
#' @param updateProgress_getblocks progress bar function passed to [getblocksnearby()] in shiny app
#' @param in_shiny if fips parameter is used, passed to [getblocksnearby_from_fips()]
#' @param quiet Optional. passed to getblocksnearby() and batch.summarize(). set to TRUE to avoid message about using [getblocks_diagnostics()],
#'   which is relevant only if a user saved the output of this function.
#' @param silentinteractive   to prevent long output showing in console in RStudio when in interactive mode,
#'   passed to [doaggregate()] also. app server sets this to TRUE when calling doaggregate() but
#'   [ejamit()] default is to set this to FALSE when calling [doaggregate()].
#' @param called_by_ejamit Set to TRUE by [ejamit()] to suppress some outputs even if ejamit(silentinteractive=F)
#' @param testing used while testing this function, passed to doaggregate()
#' @param showdrinkingwater T/F whether to include drinking water indicator values or display as NA. Defaults to TRUE.
#' @param showpctowned T/f whether to include percent owner-occupied units indicator values or display as NA. Defaults to TRUE.
#' @param ... passed to [getblocksnearby()] etc. such as  report_progress_every_n = 0
#' @param download_fips_bounds_to_calc_areas if set to TRUE, it is slower because it downloads bounds of each unit to calculate area in square miles
#' @return This returns a named list of results.
#' ```
#' # To see the structure of the outputs of ejamit()
#' structure.of.output.list(testoutput_ejamit_10pts_1miles)
#' dim(testoutput_ejamit_10pts_1miles$results_summarized$keystats)
#' dim(testoutput_ejamit_10pts_1miles$results_summarized$rows)
#' dim(testoutput_ejamit_10pts_1miles$results_summarized$cols)
#' dim(testoutput_ejamit_10pts_1miles$results_summarized$keyindicators)
#' ```
#'
#'   * **results_overall**  a data.table with one row that provides the summary across all sites, the aggregated results for all unique residents.
#'
#'   * **results_bysite**   results for individual sites (buffers) - a data.table of results,
#'     one row per ejam_uniq_id (i.e., each site analyzed), one column per indicator
#'
#'   * **results_bybg_people**  results for each block group, to allow for showing the distribution of each
#'      indicator across everyone, including the distribution within a single residential population group, for example.
#'
#'   * **longnames**  descriptive long names for the indicators in the above outputs
#'
#'   * **count_of_blocks_near_multiple_sites**  additional detail
#'
#'   * **sitetype** indicates if analysis used latlon, fips, or shp
#'
#'   * **results_summarized** named list with "rows", "cols", "keystats", "keyindicators",
#'     each providing additional summary stats.
#'     Each is a data.frame except x$results_summarized$keystats is a matrix/array.
#'
#'     - x$results_summarized$cols provides, at each site,
#'     the count of Summary Indexes at or above a threshold like the 80th percentile.
#'
#'     - x$results_summarized$keyindicators provides summary stats for a handful of indicators.
#'
#'     - x$results_summarized$keystats provides, for each indicator, the average
#'     across all sites and average across all (unique) residents, one row per indicator (a "tall" format).
#'
#'     - x$results_summarized$rows provides the same, but as one column per indicator,
#'     corresponding to the format used in results_bysite or results_overall.
#'
#'   * **formatted** another tall format showing averages for all indicators
#'
#'   * **sitetype** the type of analysis done: "latlon", "shp", "fips", etc.
#'
#' @examples
#'
#' # See examples in vignettes/ articles
#'
#'  # All in one step, using functions not shiny app:
#'  out <- ejamit(testpoints_100_dt, 2)
#'
#'  \donttest{
#'  # Do not specify sitepoints and it will prompt you for a file,
#'  # if in RStudio in interactive mode!
#'  out <- ejamit(radius = 3)
#'
#'   # Specify facilities or sites as points for test data,
#'   # use 1000 test facility points from the R package
#'   testsites <- testpoints_1000
#'   # use facility points in an excel or csv file
#'    testsites <- latlon_from_anything(
#'      system.file(paste0("testdata/latlon/",
#'       "testpoints_10.xlsx"),
#'     package = "EJAM")
#'     )
#'    # head(testsites)
#'   # use facility points from a random sample of EPA-regulated facilities
#'   testsites <- testpoints_n(1e3)
#'
#'   # Specify max distance from sites to look at (residents within X miles of site point)
#'   radius <- 3.1 # miles
#'
#'   # Get summaries of all indicators near a set of points
#'   out <- ejamit(testsites, radius)
#'   # out <- ejamit("myfile.xlsx", 3.1)
#'
#'   # Shapefile examples
#'   out2 = ejamit(shapefile = testshapes_2, radius = 0)
#'   out3 = ejamit(shapefile = testdata("portland.json", quiet = T), radius = 0)
#'   
#'   # FIPS examples
#'   out4 = ejamit(fips = testinput_fips_cities)
#'   out5 = ejamit(fips = fips_counties_from_state_abbrev("DE"), radius = 0)
#'   
#'   # View results overall
#'   round(t(out$results_overall), 3.1)
#'
#'   # View plots
#'    plot_distance_by_group(results_bybg_people = out$results_bybg_people)
#'    distance_by_group(out$results_bybg_people)
#'
#'   # View maps
#'   mapfast(out$results_bysite, radius = 3.1)
#'
#'   # view results at a single site
#'   mapfast(out$results_bysite, radius = 3.1)
#'   # all the raw numbers at one site
#'   t(out$results_bysite[1, ])
#'
#'   # if doing just 1st step of ejamit()
#'   #  get distance between each site and every nearby Census block
#'
#'   s2b <- testoutput_getblocksnearby_100pts_1miles
#'   getblocks_diagnostics(s2b)
#'
#'   testsites <- testpoints_10[2,]
#'   s2b <- getblocksnearby(testsites, radius = 3.1)
#'   getblocks_diagnostics(s2b)
#'   plotblocksnearby(s2b)
#'
#'   # if doing just 2d step of ejamit()
#'   #  get summaries of all indicators based on table of distances
#'   out <- doaggregate(s2b, testsites) # this works now and is simpler
#'
#' }
#' @seealso  [getblocksnearby()] [doaggregate()]
#'
#' @export
#'
ejamit <- function(sitepoints = NULL,
                   radius = 3,
                   radius_donut_lower_edge = 0,
                   maxradius = 31.07,
                   avoidorphans = FALSE,
                   quadtree = NULL,
                   fips = NULL,  # namestr = '', ?
                   shapefile = NULL,
                   countcols = NULL,
                   wtdmeancols = NULL,
                   calculatedcols = NULL,
                   calctype_maxbg = NULL,
                   calctype_minbg = NULL,
                   subgroups_type = "nh",
                   include_ejindexes = TRUE,
                   calculate_ratios = TRUE,
                   extra_demog = TRUE,
                   need_proximityscore = FALSE,
                   infer_sitepoints = FALSE,
                   need_blockwt = TRUE,
                   
                   thresholds = list(80, # in server/global default
                                     80 # in server/global default
                   ),
                   threshnames = list(c(names_ej_pctile, names_ej_state_pctile),
                                      c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
                   threshgroups = list("EJ-US-or-ST", "Supp-US-or-ST"),
                   
                   updateProgress = NULL,
                   updateProgress_getblocks = NULL,
                   progress_all = NULL,
                   in_shiny = FALSE,
                   quiet = TRUE,
                   silentinteractive = FALSE,
                   called_by_ejamit = TRUE,
                   testing = FALSE,
                   showdrinkingwater = TRUE,
                   showpctowned = TRUE,
                   download_fips_bounds_to_calc_areas = FALSE, # SLOWS IT DOWN IF TRUE... testing
                   ...
) {
  
  #################### #
  # if sitepoints, fips, and shapefile are all missing or NULL,
  # allow interactive selection of any type of file (not just latlon csv/xlsx/xls that is already allowed below)
  if (
    (missing(sitepoints) || is.null(sitepoints)) &&
    (missing(fips) || is.null(fips)) &&
    (missing(shapefile) || is.null(shapefile))
  ) {
    
    selected_pathfile <- select_valid_file(silentinteractive = silentinteractive)
    # now use appropriate function(s) to examine selected_pathfile and assign that filename to the appropriate input parameter so it can be read later by the right reading function
    # filetype <- sitetype_from_filepath(selected_pathfile)
    # sitetype <- filetype
    
    inparams <- sites_from_file(selected_pathfile) # just returns list of params for ejamit()
    sitepoints = inparams$sitepoints
    fips = inparams$fips
    shapefile = inparams$shapefile
  }
  
  sitetype <- ejamit_sitetype_from_input(sitepoints = sitepoints, fips = fips, shapefile = shapefile)
  
  if (interactive() && !silentinteractive) {cat("Type of sites provided:", sitetype, '\n')}
  #################### #
  
  # * POLYGONS / SHAPEFILES ####
  
  if (sitetype == "shp") {
    
    ## . check shp ####
    
    # something like this could replace similar code in server: ***
    shp <- shapefile_from_any(shapefile, cleanit = FALSE)
    # shp <- cbind(ejam_uniq_id = 1:nrow(shp), shp) # assign id to ALL even empty or invalid inputs
    shp  <- shapefile_clean(shp) # reassigns ejam_uniq_id to all, then drops invalid but not empty!! uses default crs = 4269;  drops invalid rows or return NULL if none valid  # shp <- sf::st_transform(shp, crs = 4269) # done by shapefile_clean()
    shp$valid <- !(!sf::st_is_valid(shp) | sf::st_is_empty(shp) ) # but clean dropped invalid ones already
    # ***  retain  empty but not invalid rows for analysis? ***
    if (is.null(shp)) {stop('No valid shapes found in shapefile')}
    class(shp) <- c(class(shp), 'data.table')
    
    # now it does shapefix()
    
    # Here we retain all rows, columns include ejam_uniq_id, valid, invalid_msg
    data_uploaded <- sf::st_drop_geometry(shp)#[, unique(c(1:2, which(names(shp) == "ejam_uniq_id")))] # any 1 or 2 plus id to make sure it isn't just 1 col and drop=T happens
    
    # Here are just valid ones
    shp_valid <- shp[shp$valid, ] # valid ones only
    #rm(shp)
    ##################################### #
    
    # . radius (buffer) for polygons ####
    if (missing(radius)) {
      if (interactive() && !silentinteractive && !in_shiny && rstudioapi::isAvailable()) {
        radius <- askradius(default = 0, message = "To add a buffer around each polygon, enter distance in miles. For none, use 0.")
      } else {
        radius = 0 # default is no buffer around polygon
      }
    }
    if (!is.null(radius[1]) && radius[1] > 0) {
      if (!silentinteractive) {cat('Adding buffer around each polygon.\n')}
      shp_valid <- shape_buffered_from_shapefile(shapefile = shp_valid, radius.miles = radius) # default crs ok here? ***
    }
    ##################################### #
    if (!silentinteractive) {cat('Finding blocks whose internal points are inside each polygon.\n')}
    
    # . get_blockpoints_in_shape() ####
    
    mysites2blocks <- get_blockpoints_in_shape(shp_valid)$pts
    
    # . doaggregate shp ####
    
    if (!silentinteractive) {cat('Aggregating at each polygon and overall.\n')}
    
    sites2states_or_latlon <- mysites2blocks # ??? # data.frame(ejam_uniq_id = shp_valid$ejam_uniq_id, ST = NA)  # check this
    #Initialize progress bar and function to track doaggregate
    if (!is.null(progress_all)) {
      progress_all$inc(1/3,
                       message = 'Step 2 of 3',
                       detail  = 'Aggregating')
      
      progress_doagg <- shiny::Progress$new(min = 0, max = 1)
      on.exit(progress_doagg$close(), add = TRUE)
      progress_doagg$set(value   = 0,
                         message = 'Initiating aggregation')
      nrows_blocks_value            <- nrow(mysites2blocks)
      predicted_doaggregate_runtime <- predict_doaggregate_runtime(nrows_blocks_value)
      upper_bound_value             <- predicted_doaggregate_runtime[, "fit"]
      updateProgress_doagg <- function(value = NULL,
                                       message_detail = NULL,
                                       message_main   = NULL) {
        if (!is.null(value)) {
          progress_fraction <- min(1, value / upper_bound_value)
        } else {
          current_value     <- progress_doagg$getValue()
          progress_fraction <- current_value +
            (progress_doagg$getMax() - current_value) / 5
        }
        progress_doagg$set(value   = progress_fraction,
                           message = message_main,
                           detail  = message_detail)
      }
      
    }else{
      updateProgress_doagg <- NULL
    }
    out <- suppressWarnings(
      
      doaggregate(
        
        sites2blocks = mysites2blocks,
        sites2states_or_latlon = sites2states_or_latlon,  # in server, this seems to be same as mysites2blocks ???
        radius = radius,  #
        countcols = countcols,
        wtdmeancols = wtdmeancols,
        calculatedcols = calculatedcols,
        calctype_maxbg = calctype_maxbg,
        calctype_minbg = calctype_minbg,
        subgroups_type = subgroups_type,
        include_ejindexes = include_ejindexes,
        calculate_ratios = calculate_ratios,
        extra_demog = extra_demog,
        need_proximityscore = need_proximityscore,
        infer_sitepoints = FALSE,
        called_by_ejamit = called_by_ejamit,
        updateProgress = updateProgress_doagg,
        silentinteractive = silentinteractive,
        testing = testing,
        showdrinkingwater = showdrinkingwater,
        showpctowned = showpctowned
      )
    )
    
  } # end shp type
  ######################## #
  
  # * FIPS  ####
  
  if (sitetype == "fips") {
    
    
    
    # * FIPS  ####
    
    ## . check fips ####
    
    # getblocksnearby_from_fips() should include doing something like fips_lead_zero() ?
    # but also want to know what type each fips is (probably all should be same like all are tracts or all are county fips)
    
    #### *** should confirm this is needed ####
    # Here we retain all rows, columns include ejam_uniq_id, valid, invalid_msg
    data_uploaded = data.frame(fips = fips, ejam_uniq_id = fips, n = 1:length(fips), valid = fips_valid(fips))
    data_uploaded$invalid_msg = ifelse(data_uploaded$valid, "",  "invalid FIPS")
    data_uploaded$ejam_uniq_id = as.character(data_uploaded$ejam_uniq_id) # for merge or join below to work, must match class (integer vs character) of output of doaggregate() and before that output of getblocksnearby_from_fips(fips_counties_from_state_abbrev('DE'))  #  1:length(fips))
    
    # Here we only keep valid ones (vector not data.frame)
    fips <- fips#[data_uploaded$valid]
    #### *** should confirm this is needed ####
    
    ## . radius is ignored for fips ####
    radius <- 999 # use this value when analyzing by fips not by circular buffers, as input to doaggregate(),
    # then in output of doaggregate()$results_bysite$radius.miles is returned as 0 for every fips, as in _overall.
    if (!missing(radius) && radius != 0 && radius != 999) {warning("radius was specified, but will be ignored as irrelevant when analyzing fips")}
    
    ## . getblocksnearby_from_fips() ####
    
    if (!silentinteractive) {cat('Finding blocks in each FIPS Census unit.\n')}
    
    mysites2blocks <- getblocksnearby_from_fips(
      
      fips = fips,  # these get retained as ejam_uniq_id for the fips case.
      inshiny = inshiny,
      
      need_blockwt = need_blockwt
    )
    if (nrow(mysites2blocks) == 0) {
      return(NULL)
    }
    # this should have site = each FIPS code (such as each countyfips),
    # no lat,lon columns,
    # and otherwise same outputs as getblocksnearby()
    
    # . doaggregate  fips ####
    
    if (!silentinteractive) {cat('Aggregating at each FIPS Census unit and overall.\n')}
    # sites2states_or_latlon
    # fipshere <- fips # ?? in case any got dropped during getblocks..., but cant create state pctiles for those anyway.
    fipshere <- unique(mysites2blocks$ejam_uniq_id)
    sites2states_or_latlon <- data.table(ejam_uniq_id = fipshere, ST = fips2state_abbrev(fipshere))
    setkey(sites2states_or_latlon, ejam_uniq_id)
    
    #Initialize progress bar and function to track doaggregate
    if (!is.null(progress_all)) {
      progress_all$inc(1/3,
                       message = 'Step 2 of 3',
                       detail  = 'Aggregating')
      
      progress_doagg <- shiny::Progress$new(min = 0, max = 1)
      on.exit(progress_doagg$close(), add = TRUE)
      progress_doagg$set(value   = 0,
                         message = 'Initiating aggregation')
      nrows_blocks_value            <- nrow(mysites2blocks)
      predicted_doaggregate_runtime <- predict_doaggregate_runtime(nrows_blocks_value)
      upper_bound_value             <- predicted_doaggregate_runtime[, "fit"]
      updateProgress_doagg <- function(value = NULL,
                                       message_detail = NULL,
                                       message_main   = NULL) {
        if (!is.null(value)) {
          progress_fraction <- min(1, value / upper_bound_value)
        } else {
          current_value     <- progress_doagg$getValue()
          progress_fraction <- current_value +
            (progress_doagg$getMax() - current_value) / 5
        }
        progress_doagg$set(value   = progress_fraction,
                           message = message_main,
                           detail  = message_detail)
      }
      
    }else{
      updateProgress_doagg <- NULL
    }
    
    
    
    
    
    out <- suppressWarnings(
      
      doaggregate(
        
        sites2blocks = mysites2blocks,
        sites2states_or_latlon = sites2states_or_latlon,
        radius = radius,  # use artificially large value when analyzing by fips
        countcols = countcols, 
        wtdmeancols = wtdmeancols,
        calculatedcols = calculatedcols,
        calctype_maxbg = calctype_maxbg,
        calctype_minbg = calctype_minbg,
        subgroups_type = subgroups_type,
        include_ejindexes = include_ejindexes,
        calculate_ratios = calculate_ratios,
        extra_demog = extra_demog,
        need_proximityscore = need_proximityscore,
        infer_sitepoints = FALSE,
        called_by_ejamit = called_by_ejamit,
        updateProgress = updateProgress_doagg,
        silentinteractive = silentinteractive,
        testing = testing,
        showdrinkingwater = showdrinkingwater,
        showpctowned = showpctowned
      )
      
    )
    #close doagg progress bar
    if (exists("progress_doagg")) {
      progress_doagg$close()
    }
  } # end fips type
  ######################## #
  
  # * LAT/LON POINTS ####
  
  if (sitetype == "latlon") {
    
    
    
    ## . getblocksnearby() ####
    
    ################################################################################## #
    # note this overlaps or duplicates code in app_server.R   for data_up_latlon()  and data_up_frs()
    
    ## . check pts ####
    
    # Get pts, if user entered a table, path to a file (csv, xlsx), or whatever, then read it to get the lat lon values from there
    # adds ejam_uniq_id column if it is missing. if present and not 1:N, warns but leaves it that way so ejamit_compare_types_of_places() can work.
    sitepoints <- sitepoints_from_anything(anything = sitepoints,
                                           invalid_msg_table = TRUE,
                                           set_invalid_to_na = FALSE)
    stopifnot(is.data.frame(sitepoints), "lat" %in% colnames(sitepoints), "lon" %in% colnames(sitepoints), NROW(sitepoints) >= 1, is.numeric(sitepoints$lat))
    
    # Here are preserved ALL rows (pts) including invalid ones
    # print(sitepoints)
    data_uploaded <- sitepoints[, c("ejam_uniq_id", "lat", "lon", "valid", "invalid_msg" )] # invalids here were not passed to getblock.. but some valids here might not return from getblock.. so this distinguishes where it was dropped
    data_uploaded <- data.frame(data_uploaded) # not data.table
    
    # Here are only valid points
    sitepoints <- sitepoints[sitepoints$valid, ] # only valid points go through doaggregate() but we preserved full list as data_uploaded, like data_uploaded() in server
    ###   *** should we drop all columns other than lat,lon,ejam_uniq_id ? ST? user might have provided a huge number of columns/ waste of memory.
    
    ##################################### #
    ## . radius ####
    if (missing(radius)) {
      if (interactive() && !silentinteractive && !in_shiny && rstudioapi::isAvailable()) {
        radius <- askradius(default = radius)
      } else {
        if (shiny::isRunning()) {
          # this should never arise - should have been set by app_server.R and ejamit was not being used except in fips case by server anyway
          warning(paste0("Using default radius of ", radius, " miles because not provided as parameter."))
          radius <- radius
        } else {
          # notshiny.  noninteractive or silent so don't prompt.
          warning(paste0("Using default radius of ", radius, " miles because not provided as parameter."))
          radius <- radius
        }
      }
    }
    ##################################### #
    
    # Get runtime of ejamit in seconds
    ejamit_runtime_prediction <- predict_ejamit_runtime(nrow(sitepoints), radius)
    predicted_time <- ejamit_runtime_prediction[, "upr"]
    
    ## print runtime if predicted time > 2 minutes / 120 seconds
    ## note: models may over-estimate runtime for small analyses
    if (predicted_time > 120) {
      print(paste("Ejamit is predicted to take", round(predicted_time, 0), "seconds"))
    }
    ## . getblocksnearby() ####
    
    if (!missing(quadtree)) {warning("quadtree should not be provided to ejamit() - that is handled by getblocksnearby() ")}
    
    ################################################################################## #
    if (!silentinteractive) {cat('Finding blocks nearby.\n')}
    
    mysites2blocks <- getblocksnearby(
      
      sitepoints = sitepoints, # invalid latlon were already dropped
      radius = radius,
      radius_donut_lower_edge = radius_donut_lower_edge,
      maxradius = maxradius,
      avoidorphans = avoidorphans,
      # quadtree = localtree,
      quiet = quiet,
      updateProgress = updateProgress_getblocks,
      ...  #  could provide report_progress_every_n = 500  # would be passed through to getblocksnearbyviaQuadTree()
    )
    ################################################################################## #
    
    # . doaggregate pts ####
    
    
    #Initialize progress bar and function to track doaggregate
    if (!is.null(progress_all)) {
      progress_all$inc(1/3,
                       message = 'Step 2 of 3',
                       detail  = 'Aggregating')
      
      progress_doagg <- shiny::Progress$new(min = 0, max = 1)
      on.exit(progress_doagg$close(), add = TRUE)
      progress_doagg$set(value   = 0,
                         message = 'Initiating aggregation')
      nrows_blocks_value            <- nrow(mysites2blocks)
      predicted_doaggregate_runtime <- predict_doaggregate_runtime(nrows_blocks_value)
      upper_bound_value             <- predicted_doaggregate_runtime[, "fit"]
      updateProgress_doagg <- function(value = NULL,
                                       message_detail = NULL,
                                       message_main   = NULL) {
        if (!is.null(value)) {
          progress_fraction <- min(1, value / upper_bound_value)
        } else {
          current_value     <- progress_doagg$getValue()
          progress_fraction <- current_value +
            (progress_doagg$getMax() - current_value) / 5
        }
        progress_doagg$set(value   = progress_fraction,
                           message = message_main,
                           detail  = message_detail)
      }
      
    }else{
      updateProgress_doagg <- NULL
    }
    
    if (!silentinteractive) {cat('Aggregating at each site and overall.\n')}
    doaggregate_runtime_prediction <- predict_doaggregate_runtime(nrow(mysites2blocks))
    predicted_time <- doaggregate_runtime_prediction[, "fit"]
    
    cat(paste("doaggregate is predicted to take", round(predicted_time, 0), "seconds \n"))
    out <- suppressWarnings(
      
      doaggregate(
        
        sites2blocks = mysites2blocks,
        sites2states_or_latlon = sitepoints, # sites2states_or_latlon = unique(x[ , .(ejam_uniq_id, lat, lon)]))
        radius = radius,
        countcols = countcols,
        wtdmeancols = wtdmeancols,
        calculatedcols = calculatedcols,
        calctype_maxbg = calctype_maxbg,
        calctype_minbg = calctype_minbg,
        subgroups_type = subgroups_type,
        include_ejindexes = include_ejindexes,
        calculate_ratios = calculate_ratios,
        extra_demog = extra_demog,
        need_proximityscore = need_proximityscore,
        infer_sitepoints = infer_sitepoints,
        called_by_ejamit = called_by_ejamit,
        updateProgress = updateProgress_doagg,
        silentinteractive = silentinteractive,
        testing = testing,
        showdrinkingwater = showdrinkingwater,
        showpctowned = showpctowned
      )
    )
    #close doagg progress bar
    #progress_doagg$close()
  } # end latlon type
  ################################################################ #
  
  # * AREA CALCULATION ####
  
  # see if already provided by doaggregate() ?
  if ("area_sqmi" %in% names(out$results_bysite)) {
    # done
  } else {
    if (sitetype %in% "fips") {
      ## Getting bounds here would be slow!
      if (download_fips_bounds_to_calc_areas) {
        areas <- area_sqmi(fips = fipshere) # uses  shapes_from_fips(fipshere)
      } else {
        # *** ASSUMES NO BUFFER ADDED TO FIPS UNITS !
        areas <- rep(NA, length(fipshere))
      }
    }
    if (sitetype %in% "shp") {
      areas <- area_sqmi(shp = shp_valid) # includes any BUFFER ADDED
    }
    if (sitetype %in% "latlon") {
      # CHECK IF DONUT OPTION WAS USED !       radius_donut_lower_edge
      if (all(radius_donut_lower_edge == 0)) {
        areas <- area_sqmi(df = out$results_bysite)
      } else {
        areas <- area_sqmi(df = out$results_bysite) - area_sqmi(radius.miles = radius_donut_lower_edge)
      }
    }
    out$results_bysite$area_sqmi <- areas
    out$results_overall$area_sqmi <- sum(areas, na.rm = TRUE)
    out$results_bybg_people$area_sqmi <- NA
  }
  # end of lat lon vs FIPS vs shapefile
  ################################################################ #
  # ~ ####
  # Handle sites dropped during getblocksnearby or doaggregate steps or with no data (zero pop)
  # * valid, invalid_msg   ####
  
  #     latlon, shp, fips handled the same way here
  
  # Add rows with invalid sites that were dropped before getblocks..
  # Flag as invalid other cases (dropped by getblocks.. or by doaggregate, or had no results)
  # Say why, in invalid_msg column
  
  # data_uploaded is a data.frame, all rows including invalid, cols = ejam_uniq_id, valid, invalid_msg, done for latlon, fips, and shp cases.
  # "valid" and "invalid_msg"   already created for shp by shapefix() in shapefile_from_any()
  # "valid" and "invalid_msg"   already created for latlon in sitepoints_from_any()
  # "valid" and "invalid_msg"   already created for fips by fips_valid()
  
  dropped_before_getblocks <- !data_uploaded$valid
  site_in_blocksfound  <- data_uploaded$ejam_uniq_id %in% mysites2blocks$ejam_uniq_id
  site_in_results      <- data_uploaded$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id
  site_in_results_pop0 <- data_uploaded$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id[out$results_bysite$pop == 0]
  
  if (sitetype == "latlon") {
    dup <- data.frame(sitepoints) # latlon. already has ejam_uniq_id
  }
  if (sitetype == "fips") {
    dup <- data.frame(fips = fips, ejam_uniq_id = as.character(fips)) # for merge or join below to work, must match class (integer vs character) of output of doaggregate() and before that output of getblocksnearby_from_fips(fips_counties_from_state_abbrev('DE'))  #  1:length(fips))
  }
  if (sitetype == "shp") {
    dup <- sf::st_drop_geometry(shp)#[, intersect(names(shp), c('ejam_uniq_id', 'valid', colnames(shp)[1:2], 'NAME'))]
    # dup <- data.frame(dup, ejam_uniq_id = 1:NROW(dup)) # shp already had ejam_uniq_id
  }
  
  
  data_uploaded$valid[site_in_results_pop0 | !site_in_results] <- FALSE  # NOTE this also says invalid if zero population
  
  data_uploaded$invalid_msg[!dropped_before_getblocks & !site_in_blocksfound] <- "no block centroids (area too small for low pop density)"
  data_uploaded$invalid_msg[site_in_blocksfound & !site_in_results] <- "blocks with residents found but unable to aggregate" # why?
  data_uploaded$invalid_msg[site_in_results_pop0]   <- "blocks found but zero residents" # msg differs from server version
  
  # Merge invalid and valid sites and msg, so results_bysite has ALL sites originally provided for analysis.
  setDT(data_uploaded)
  out$results_bysite <- merge(data_uploaded[, .(ejam_uniq_id, valid, invalid_msg)],
                              out$results_bysite,
                              by = 'ejam_uniq_id', all = T)
  setorder(out$results_bysite, ejam_uniq_id)
  
  out$results_overall$valid <- TRUE # needs to be TRUE for some functions like ejam2report() ? or  sum(out$results_bysite$valid, na.rm = T)
  out$results_overall$invalid_msg <- ""
  if (!setequal(names(out$results_overall), names(out$results_bysite))) {stop('column names in bysite and overall do not match')}
  setcolorder(out$results_overall, names(out$results_bysite))
  
  out$longnames <- fixcolnames(names(out$results_bysite), 'r', 'long') # or try to sort c(fixcolnames(c("valid", "invalid_msg"), 'r', 'long'), out$longnames)
  
  ## see out$results_summarized$keystats
  ## see structure.of.output.list(out$results_summarized)
  ## see results_bybg_people  has only a subset so already does not match colnames
  ################################################################ #
  
  # * Hyperlinks ####
  
  ##  _>>> should use url_4table() ! *** in server & ejamit ####
  # duplicated almost exactly in app_server (near line 1217) but uses reactives there. *** except this has been updated here to handle FIPS not just latlon analysis.
  # #  Do maybe something like this:
  # links <- url_4table(lat=out$results_bysite$lat, lon=out$results_bysite$lon, radius = radius,
  #    regid=ifelse("REGISTRY_ID" %in% names(out$results_bysite), out$results_bysite$REGISTRY_ID, NULL))
  # out$results_bysite[ , `:=`(links$results_bysite)] # would that work??? how to avoid big cbind step to add the new columns?
  # out$results_overall <- cbind(out$results_overall, links$results_overall) #
  # setcolorder(out$results_bysite, neworder = links$newcolnames)
  # setcolorder(out$results_overall, neworder = links$newcolnames)
  # out$longnames <- c(newcolnames, links$longnames)
  
  if ("REGISTRY_ID" %in% names(out$results_bysite)) {
    echolink = url_echo_facility_webpage(REGISTRY_ID, as_html = T)
  } else {
    echolink = rep(NA, NROW(out$results_bysite))
  }
  
  ### disable ejscreen report links while the site is down
  
  if ("ejscreen_is_down" == "ejscreen_is_down") {
    out$results_bysite[ , `:=`(
      `EJScreen Report` = NA,
      `EJScreen Map`    = NA,
      `ACS Report`      = NA,
      `ECHO Report` = echolink
    )]
  } else {
    
    if (sitetype == "fips") {
      
      # analyzing by FIPS not lat lon values
      areatype <- fipstype(fips)
      if (!(all(areatype %in% c("blockgroup", "tract", "city", "county", 'state')))) {warning("FIPS must be one of 'blockgroup', 'tract', 'city', 'county' 'state' for the EJScreen API")}
      out$results_bysite[ , `:=`(
        `EJScreen Report` = NA,#url_ejscreen_report(   areaid   = fips, areatype = areatype, as_html = T), #  namestr=my text not implemented here
        `EJScreen Map`    = NA,#url_ejscreenmap(       wherestr = fips2name(fips), as_html = T),  # this needs a name not FIPS
        `ACS Report`      = NA,
        `ECHO Report` = echolink
      )]
    } else {
      out$results_bysite[ , `:=`(
        `EJScreen Report` = NA,#url_ejscreen_report(    lat = out$results_bysite$lat, lon = out$results_bysite$lon, radius = radius, as_html = T),
        `EJScreen Map`    = NA,#url_ejscreenmap(        lat = out$results_bysite$lat, lon = out$results_bysite$lon,                  as_html = T),
        `ACS Report`      = NA,
        `ECHO Report` = echolink
      )]
    }
  }
  if (NROW(out$results_bysite) == 1) {
    # If we analyzed only 1 place then overall is same as 1 site per row!
    out$results_overall[ , `:=`(
      `EJScreen Report` = out$results_bysite$`EJScreen Report`,
      `EJScreen Map`    = out$results_bysite$`EJScreen Map`,
      `ACS Report`      = out$results_bysite$`ACS Report`,
      `ECHO Report`     = out$results_bysite$`ECHO Report`
    )]
  } else {
    out$results_overall[ , `:=`(
      `EJScreen Report` = NA,
      `EJScreen Map`    = NA,
      `ACS Report`      = NA,
      `ECHO Report`     = NA
    )]
  }
  # placeholders just so colnames are more consistent with other tables
  #  (but still missing pctiles, ratios, and averages)
  out$results_bybg_people[ , `:=`(
    `EJScreen Report` = NA,
    `EJScreen Map`    = NA,
    `ACS Report`      = NA,
    `ECHO Report`     = NA
  )]
  newcolnames <- c(
    "EJScreen Report",
    "EJScreen Map",
    "ACS Report",
    "ECHO Report")
  # put those up front as first columns
  data.table::setcolorder(out$results_bysite,  neworder = newcolnames)
  data.table::setcolorder(out$results_overall, neworder = newcolnames)
  data.table::setcolorder(out$results_bybg_people, neworder = newcolnames)
  out$longnames <- c(newcolnames, out$longnames)
  ################################################################ #
  
  if (sitetype == "fips") {
    # Analyzed by FIPS so reporting a radius does not make sense here.
    radius <- NA
  }
  # ( doaggregate already provided this but ok to do again)
  out$results_bysite[      , radius.miles := radius]
  out$results_overall[     , radius.miles := radius]
  out$results_bybg_people[ , radius.miles := radius]
  ################################################################ #
  
  # * batch.summarize()   ####
  
  # For each indicator, calc AVG and PCTILES, across all SITES and all PEOPLE
  
  out$results_summarized <- batch.summarize(
    sitestats = data.frame(out$results_bysite),
    # popstats =  data.frame(out$results_bysite), # now does not have to get passed twice
    quiet = quiet,
    ## user-selected quantiles to use
    #probs = as.numeric(input$an_list_pctiles), # probs = c(0, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99, 1)
    thresholds = thresholds, # list(90, 90),
    threshnames = threshnames, # list(c(names_ej_pctile, names_ej_state_pctile), c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
    threshgroups = threshgroups # list("EJ-US-or-ST", "Supp-US-or-ST")
  )
  ################################################################ #
  
  # * table_tall_from_overall() ####
  
  out$formatted <- table_tall_from_overall(out$results_overall, fixcolnames(names(out$results_bysite), 'r', 'long')) # out$longnames)
  
  ###################################### #
  ## report the sitetype ####
  
  out$sitetype <- sitetype
  
  ###################################### #
  if (interactive() & !silentinteractive & !in_shiny) {
    
    #* show summary in RStudio ####
    # and sites table in viewer pane
    if (nrow(out$results_bysite) > 1000) {message("> 1,000 rows may be too much for client-side DataTables - only showing some rows here")}
    # ejam2tableviewer(out)
    ###################################### #
    print(structure.of.output.list(out, objectname = "Output of ejamit()"))
    cat("\nWays to view or save results of  'x <- ejamit()'   \n Overall results: ejam2barplot(x); ejam2ratios(x); ejam2table_tall(x); ejam2report(x)\n Results by site: ejam2map(x); ejam2barplot_sites(x); ejam2tableviewer(x); ejam2excel(x); ejam2shapefile(x) \n\n")
  }
  ################################################################ #
  
  invisible(out)
}


