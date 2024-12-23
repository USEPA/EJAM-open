## example
# out <- ejamit_compare_types_of_places(testpoints_10[1:4, ], typeofsite = c("A", "B", "B", "C"))

### Note that it takes roughly the same amount of time to do the getblocksnearby() for all points at once
### versus doing it group by group, so it 
### would not be much more efficient to do that part of ejamit() all at once:
# system.time({x = list() ; ng = 10 ; for (i in 1:ng) x[[i]] = getblocksnearby(testpoints_1000[(1 + (i-1) * (1000/ng)):(i * (1000/ng)),])})
# system.time({x= getblocksnearby(testpoints_1000)})


#' Compare subsets (types) of places that are all from one list
#' @description *** DRAFT - May change but works as currently drafted.
#'  e.g., change output formats of results_bytype vs results_overall
#' 
#' @param sitepoints see [ejamit()]
#' @param typeofsite   vector of length same as NROW(sitepoints), where 
#'   each unique value defines a group of sites
#' @param silentinteractive passed to [ejamit()]
#' @param shapefile  see [ejamit()]
#' @param fips  see [ejamit()]
#' @param ...  see [ejamit()]
#'
#' @return similar to ejamit output but results_overall has one row per unique typeofsite
#' 
#' @export
#'
#' @examples
#'   out <- ejamit_compare_types_of_places(testpoints_10[1:4, ], 
#'     typeofsite = c("A", "B", "B", "C"))
#'   cbind(Rows_or_length = sapply(out, NROW))
#'   
#'   ejam2barplot_sitegroups(out, names_these_ratio_to_avg[1], topn = 3)
#'   
#'   ejam2barplot_sitegroups(out, "sitecount_unique", topn=3, sortby = F)
#'   
#'   ejam2barplot_sitegroups(out, "pop", topn = 3, sortby = F)
#'   
#'   # use calculated variable not in original table
#'   df <- out$results_bytype
#'   df$share <- df$pop / sum(df$pop)
#'   df$pop_per_site <- df$pop / df$sitecount_unique
#'   
#'   plot_barplot_sites(df,
#'     "share", ylab = "Share of Total Population",
#'     topn = 3, names.arg = out$types , sortby = F)
#'     
#'   plot_barplot_sites(df,
#'     "pop_per_site", ylab = "Pop. at Avg. Site in Group",
#'     topn = 3, main = "Nearby Residents per Site, by Site Type",
#'     names.arg = out$types , sortby = F)
#'   
#'   \dontrun{
#'     
#'   # Analyze by EPA Region
#'   
#'   pts <- data.frame(testpoints_1000)
#'   
#'   # Get State and EPA Region of each point from lat/lon
#'   
#'    x <- state_from_latlon(lat = pts$lat, lon = pts$lon)
#'    pts <- data.frame(pts, x)
#'    
#'    out_byregion <- ejamit_compare_types_of_places(
#'      pts, typeofsite = pts$REGION)
#'    
#'    dvarname <- names_d[3]
#'    ejam2barplot_sitegroups(out_byregion, dvarname)
#'    abline(h = usastats_means(dvarname))
#'    
#'    ejam2barplot_sitegroups(out_byregion, "ratio.to.avg.pctmin",
#'       main = "By EPA Region", ylim = c(0, 2))
#'    abline(h = 1)
#'      
#'    # Analyze by State (slow)
#'    
#'    out_bystate <- ejamit_compare_types_of_places(pts, typeofsite = pts$ST)
#'    
#'    ejam2barplot_sitegroups(out_bystate, "sitecount_unique", 
#'      names.arg = out_bystate$types, topn = 52, cex.names = 0.5,
#'      main = "Sites by State")
#'
#'   }
#'   
ejamit_compare_types_of_places <- function(sitepoints, typeofsite = NULL, 
                                           shapefile = NULL, fips = NULL,
                                           silentinteractive = TRUE,  ...) {
  
  ########################################################## # 
  # note this means latlon vs fips vs shp, not type in the sense of which group (subset) that is specified via typeofsite param
  sitetype <- ejamit_sitetype_from_input(sitepoints = sitepoints, fips = fips, shapefile = shapefile)
  ########################################################## # 
  
  if (sitetype == 'fips') {
    
    if (is.null(typeofsite) || length(typeofsite) != NROW(fips)) {
      stop("typeofsite must be a vector as long as fips, each unique value defining a group of sites")
    }
  } else {
    fips <- NULL
  }
  ########################################################## # 
  
  if (sitetype == "latlon") {
    
    # sitepoints_from_any() will 
    # accept sitepoints interactively or from filepath or from object, 
    # infer lat/lon cols,
    
    # Note sitepoints_from_any() is able to check if sitepoints was missing 
    #  in expected params of ejamit_compare_types_of_places()
    sitepoints <- sitepoints_from_any(sitepoints)   #############  this adds ejam_uniq_id only if not there yet
    sitepoints <- data.frame(sitepoints)
    # sitepoints$typeofsite <- typeofsite # that would not get used
    #################### # 
    if (is.null(typeofsite) || length(typeofsite) != NROW(sitepoints)) {
      stop("typeofsite must be a vector as long as NROW(sitepoints), each unique value defining a group of sites")
    }
  } else {
    sitepoints <- NULL
  }
  ########################################################## # 
  
  if (sitetype == "shp") {
    # shapefile_from_any()  now does shapefix() which creates unique ids only if not already there, etc.
    shp <- shapefile_from_any(shapefile, cleanit = FALSE)
    if (is.null(typeofsite) || length(typeofsite) != NROW(shp)) {
      stop("typeofsite must be a vector as long as NROW(shapefile), (before empty or invalid ones are removed), each unique value defining a group of sites")
    }
    
    shp$valid <- shp$valid <- (sf::st_is_valid(shp) &  !sf::st_is_empty(shp))
    shp[shp$valid, ] <- shapefile_clean(shp) # uses default crs = 4269;  drops invalid rows or return NULL if none valid  # shp <- sf::st_transform(shp, crs = 4269) # done by shapefile_clean()
    # *** is it ok to retain invalid rows for analysis or should they be dropped? can ejamit() handle those?? ***
    if (is.null(shp)) {stop('No valid shapes found in shapefile')}
    class(shp) <- unique(c(class(shp), 'data.table'))
    if (!shiny::isRunning() && !silentinteractive) {
      cat('Checking for valid polygons.\n')
      message(paste0("Assigned unique IDs to but dropped ", sum(!shp$valid), " invalid or empty rows from shapefile."))
    }
    
  } else {
    shp <- shapefile # NULL
  }
  
  ########################################################## # 
  #  Revamp ejamit_compare_types_of_places()
  # 
  # We should do ejamit(sitepoints) once for the results_overall, results_bysite, results_bybg_people (ie bybgbysite),
  # and then to get only the results_bytype (ie by group of sites) separately in a streamlined way...
  # So the way to do all this could be the following:
  #  *** - modify ejamit() to use new params to ejamit(  groupby = )  -- seems like the best approach...
  #     that would as usual run getblocksnearby() only once, and call doaggregate() once, but 
  #     *** A) modify doaggregate() to use new params doaggregate( groupby = )
  #       it would do the usual summaries 
  #      PLUS (maybe call a new function that would..) 
  #       return a new "results_bytype" table .... that would   
  #       calculate a  results_overall  1-row table on each group (each site type),
  #       and assemble those as a special results_bytype table. 
  #       Where/ how to best fit that into doaggregate() ?
  #      AND modify  related functions like ejam2report, ejam2excel, other excel functions, and server/shiny
  #      to 1st just save and then be able to display the new by type info.
  # 
  #  ejam2report(sitenumber = ) works but would add also ejam2report(sitetype = x), rather than typenumber =x
  #     or B) a separate doaggregate_bygroup() that ONLY calculates and returns results_bytype
  #       (but NOT calculate or return a results_overall that ignores groups/types, etc.)
  # ### But, note it would be much more efficient if each group could be summarized based on just results_bysite 
  #  instead of needing to redo the getblocksnearby() and redo the join of blockgroupstats 
  #  and could simply use wtd mean or sum over sites in a group,
  #  ***which should be OK IF THOSE SITES DO NOT OVERLAP AT ALL WITHIN A GROUP !!*** Explore and confirm this.
  #  We could take note of which sites overlap any others in a new site-specific column created while aggregating sites2blocks,
  #  and for all the ones that do not overlap, aggregate by group from that 1 full results_bysite done once over the whole set of groups.
  #  and for the other ones that might overlap within a group, could you go back to using something like.... 
  #  results_bysite_bybg? since that tells you for each site what fraction of each bg to count.... 
  #  no, you need to go back to sites2blocks table!! 
  # The problem is still to count each person only once you have to know which blocks are near which site, 
  # for the blockgroups where parts of the bg are near one site and parts are near the other site.
  
  ########################################################## # 
  # Find a way to make ejamit by group (by site type) work well with the ejam2xyz() functions.
  # 
  # ejamit_compare_types_of_places() as a 1st draft, for now,  
  # does not report out$results_bybg_people, to save time/space,
  # and
  # does not report a typical out$results_overall, since that would require run of all groups at once
  # and
  # calculates a  results_bytype  which has 1 row per type,
  # which is what you want to see in excel,
  # and initial draft compromise was to report a 
  # table actually containing "results_bytype", but named "results_overall"
  # But, then you have to do some things to prep output of ejamit_... for use in ejam2excel() 
  # since that expects  "results_overall" and "results_bysite" and "results_bybg_people"
  # It would be useful to have a way that ejamit_compare_types_of_places() or 
  # some bygroup version of ejamit could work easily with the ejam2xyz() functions.
  ########################################################## # 
  
  
  types <- unique(typeofsite)
  
  results_overall     <- list()
  results_bysite      <- list()
  # results_bybg_people <- list() # unused for now
  # results_summarized  <- list() # unused for now (see comment below on complicated format)
  longnames           <- list()
  typeofsite_list     <- list()
  sitecount_bytype    <- list()
  ########################################################## # 
  
  began <- Sys.time()
  ndone <- 0
  
  # loop over types ####
  
  for (i in seq_along(types)) {
    
    cat("Type", i, "of", length(types), "=", types[i], " -- ")
    
    if (sitetype == "latlon") {sitepoints_subset = sitepoints[typeofsite == types[i], ,drop = F]} else {sitepoints_subset = NULL}
    if (sitetype == "fips")   {fips_subset       = fips[      typeofsite == types[i]  ]} else {fips_subset       = NULL}
    if (sitetype == "shp")    {shapefile_subset  = shp[       typeofsite == types[i], ]} else {shapefile_subset  = NULL}
    
    out <- suppressWarnings({
      
      # *** make sure ejamit() does not reASSIGN ejam_uniq_id 1:N within each group of sites 
      #  we want 1:N for the full set of sites only  ***
      ## ejamit() ####
      
      ejamit(
        sitepoints = sitepoints_subset, 
        fips       = fips_subset,
        shapefile  = shapefile_subset,
        ..., quiet = TRUE, silentinteractive = TRUE)
    }) 
    
    results_overall[[i]] <- out$results_overall   # one row, for this group of places (this type) 
    results_bysite[[i]]  <- out$results_bysite    # one row per site, in this group
    
    # results_bybg_people[[i]] <- out$results_bybg_people  # 1 row/blockgroup near this group of sites
    
    # results_summarized[[i]]  <- out$results_summarized   # but this itself is a named list:
    #
    #   This would be made by batch.summarize()
    #   cols is a table for this group of sites, 1 row/site in this group, 
    #      with "Max.of.EJ.US.or.ST", "Number.of.EJ.US.or.ST.at.above.threshold.of.90" etc.
    #   rows is a table of 1 col/indicator, with rownames being 
    #    "Average site","Average person", "Median site","Median person","Min","Max","Sum"
    
    longnames[[i]] <- out$longnames
    
    # 1 item, the name of this type of site:
    typeofsite_list[[i]] <- typeofsite[typeofsite == types[i]]
    
    # 1 number, the count of unique sites in group, as a site Should be here only once/group:
    sitecount_bytype[[i]] <- NROW(results_bysite[[i]]) 
    
    ndone <- ndone + sitecount_bytype[[i]]
    cat("Finished", ndone, "of", sum(unlist(sitecount_bytype)), "sites. ") # check this 
    junk <- speedreport(began, Sys.time(), ndone)
  }
  cat("\n\n")
  
  ## Be careful about how many were submitted in original list,
  ## vs  how many valid and included in results_bysite, etc.
  #    sapply(outall, NROW)
  # typeofsite       604629 all submitted to ejamit()
  # ejam_uniq_id     604629 same
  # results_bysite   603858  missing some results, some ejam_uniq_id are not there - not valid/no results
  # types                99
  # sitecount_bytype     99
  # results_bytype       99
  # results_overall      99
  # longnames           394
  # > max(outall$results_bysite$ejam_uniq_id)  # [1] 604629
  # > length(outall$results_bysite$ejam_uniq_id)  # [1] 603858
  # > length(outall$results_bysite$ejam_uniq_id) - max(outall$results_bysite$ejam_uniq_id)  # [1] -771
  # So the actual vector of site type with results reported is
  # not outall$typeofsite (which has all submitted)
  # but is only 
  # outall$typeofsite[outall$results_bysite$ejam_uniq_id]
  # (which is only those appearing in results_bysite)
  
  # out <- list() ####
  
  out <- list(
    
    types = types,
    sitecount_bytype = unlist(sitecount_bytype),
    results_bytype = data.table::rbindlist(results_overall), 
    # will be N rows instead of usual 1 row (where N is the number of types of site, ie groups)
    results_overall = NA, # will be replaced but this sets the order of the list
    # results_overall = ejamit(sitepoints = sitepoints, ...)$results_overall,
    ## This seems inefficient to run them all AGAIN but as a whole instead of by group:
    
    ejam_uniq_id =  sitepoints$ejam_uniq_id,      # FIX ***
    typeofsite = unlist(typeofsite_list),
    results_bysite = data.table::rbindlist(results_bysite),
    # Does not have a column with typeofsite, because we want it to be the
    #  same shape as normal ejamit()$results_bysite, so out$typeofsite has that info.
    
    # results_bybg_people  would be needed once per group to show x as func of distance within a typeofsite,
    # but cannot rbind them since they overlap where 2 groups of sites share some blockgroups.
    ## not sure how/if to report this... *** tbd
    
    # results_summarized <- 0, # format is different. ## not sure how/if to report this... *** tbd
    
    longnames = longnames[[1]]
  )
  
  # print(data.frame(sitecount = as.vector(out$sitecount_bytype), out$results_overall[ , c("typeofsite", "pop")]))
  
  out$results_overall <- out$results_bytype # overall is the name needed in ejam2excel() etc.
  out$results_overall$ejam_uniq_id <- out$types # NOTE: this for the "overall" table 1 row per type 
  #   is the code of each type, not typical ejam_uniq_id of 1:N
  names(out$results_overall) <- gsub("ejam_uniq_id", "typeofsite", names(out$results_overall)) # make it more clear by naming it this way
  
  # create some summary stats across the site types
  
  out$results_bysite$typeofsite <- out$typeofsite # add temp column to simplify next line
  x = out$results_bysite[, .(validresults = sum(valid)), by = 'typeofsite'] # normally by= refers to a column within the DT[] so typeofsite was made a col here temporarily 
  out$results_bysite[, typeofsite := NULL] # remove temp col
  names(x) <- c("type", "valid")
  x$sitecount = out$sitecount_bytype
  x$pctvalid = round(100 * x$valid / x$sitecount, 0)
  x$pop = round(out$results_bytype$pop, 0 )
  x$pop_persite = round(x$pop / x$sitecount, 0)
  x$pctofallpop = round(100 * x$pop / sum(x$pop), 0)
  x$pctofallsitecount = round(100 * x$sitecount / sum(x$sitecount), 0)
  
  ratiostats = table_round(
    out$results_bytype[,
                       names_d_ratio_to_state_avg,
                       ## or else
                       # c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg),
                       with = FALSE,],  var = names_d_ratio_to_state_avg
  )
  out$validstats <- data.frame(x)
  out$ratiostats = ratiostats
  
  # put some key stats into the results_overall or results_bytype table
  out$results_overall$valid <- x$valid
  names(out$results_overall) <- gsub("invalid_msg", "sitecount", names(out$results_overall))
  out$results_overall$sitecount <- out$sitecount_bytype
  #
  out$results_bytype$valid <- x$valid
  names(out$results_bytype) <- gsub("invalid_msg", "sitecount", names(out$results_bytype))
  out$results_bytype$sitecount <- out$sitecount_bytype
  
  # print some results ####
  print(  out$validstats )
  print(  out$ratiostats )
  
  cat("Use  ejam2excel(out)  to view results, and see the types of sites compared, one row each, in the Overall tab\n")
  cat("Use ejam2barplot_sitegroups() to plot results.\n\n")
  ended <- Sys.time()
  cat(paste0("\n ",
             sum(out$sitecount_bytype),   # FIX/check *** should also equal NROW(results_bysite)
             " sites in ", length(unique(typeofsite)), " groups (types of sites).\n"))
  speedreport(began, ended, 
              sum(out$sitecount_bytype)   # FIX/check ***  should also equal NROW(results_bysite)
  )
  
  return(out)
}
#################################################################### #

