#' plotblocksnearby - Map view of Census blocks (their centroids) near one or more sites
#' Utility to quickly view one or more facility points on map with the blocks found nearby
#' @details Uses [getblocksnearby()] if lat,lon points provided as sitepoints,
#'  but skips it if looks like user passed output of getblocksnearby(),
#'  and then displays a map of those blocks near the specified point.
#' @param sitepoints table of points with lat, lon in decimal degrees (data.frame or data.table),
#'   but also could just be the output of getblocksnearby() if that has already been done.
#' @param radius optional. in miles (Max allowed is 32 miles, or just over 50 kilometers since 31.06856 miles is 50 * 1000 / meters_per_mile).
#' @param sites2blocks optional. If provided, used as sites2blocks like [testoutput_getblocksnearby_10pts_1miles]
#'    If neither sites2blocks nor sitepoints is provided it cannot plot and returns error.
#'    If sites2blocks and sitepoints are both provided, it uses them both to plot blocks and sites (centers of circles).
#'    If sites2blocks not provided, but sitepoints alone is provided, checks if sitepoints is actually sites2blocks, and uses as such.
#'    If sites2blocks not provided, but sitepoints alone is provided, and sitepoints is really sitepoints, it runs getblocksnearby() to create sites2blocks.
#'    If sites2blocks is provided, but sitepoints is not, it could only do a bad approximation of sitepoints so it will not draw the circle or site at center of the circle.
#' @param siteidvarname optional. specifies the column name in sites2blocks that is the unique site id, the values of which should 
#'   also be the row numbers of the corresponding sites in sitepoints, with a site appearing once in sitepoints, 
#'   and in sites2blocks appearing once per block that is near that site.
#' @param usemapfast optional. simpler plot if FALSE
#' @param returnmap optional. if set TRUE, returns the leaflet map object instead of tabular info.
#'   That  is needed to pass results to map_blockgroups_over_blocks() for example. 
#' @param overlay_blockgroups optional. if set TRUE, also plots overlay of blockgroup boundaries.
#' @param maxradius optional. see [getblocksnearby()]
#' @param avoidorphans optional. see [getblocksnearby()]
#' @param ... optional. passed to mapfast() or plot() depending on usemapfast, 
#'   like column_names = "ej" for better map popups on block points
#'
#' @return invisibly returns sites2blocks like getblocksnearby() does
#' @export
#'
#' @examples 
#'   #  see all Census Blocks within 1 mile of 1 site, if already had run getblocksnearby()
#'   getblocks_output <- copy(testoutput_getblocksnearby_10pts_1miles)
#'   if ("siteid" %in% names(getblocks_output)) {
#'   siteidvarname <- "siteid" # the old default
#'   eg <- getblocks_output[siteid == 1,]
#'   eg2 <- getblocks_output[siteid %in% c(4,10),]
#'   } else {
#'   siteidvarname <- "ejam_uniq_id"
#'    eg <- getblocks_output[ejam_uniq_id == 1,]
#'    eg2 <- getblocks_output[ejam_uniq_id %in% c(4,10),]
#'   }
#'    z <-  plotblocksnearby(sitepoints = testpoints_10, 
#'      sites2blocks = eg, radius = 1)
#'    # see two sites if already had run getblocksnearby()
#'    z2 <-  plotblocksnearby(sitepoints = testpoints_10[c(4,10),], 
#'      sites2blocks = eg2, radius = 1)
#'   \dontrun{
#'   # See one randomly selected regulated facility from FRS and all Census Blocks within 2 miles:
#'     plotblocksnearby(testpoints_n(1), 2) 
#'   # See two sites and all Census Blocks within 5 kilometers
#'     plotblocksnearby(testpoints_2, radius = convert_units(5, from = "km", towhat = "miles"))
#'   # See 100 sites and all blocks within 1 mile of each - 
#'   # Note you have to specify radius here or it uses default that may not match intent 
#'   # - and this is a bit slow
#'   plotblocksnearby(testoutput_ejamit_100pts_1miles$results_bysite[, c(siteidvarname, "lat", "lon"), with=FALSE],
#'      radius = 1)
#'   }
plotblocksnearby <- function(sitepoints, radius=3, sites2blocks, siteidvarname = "ejam_uniq_id", 
                             usemapfast=TRUE, returnmap=FALSE, overlay_blockgroups=FALSE,
                             maxradius = 31.07, avoidorphans = FALSE, ...) {
  if (radius > 32) {radius <- 32; warning("Cannot use radius above 32 miles (almost 51 km) here - Returning results for 32 miles!")}
  if (missing(sitepoints) &  missing(sites2blocks)) {
    warning('must provide either sitepoints or sites2blocks or both')
    return(NULL)
  }
  if (missing(sitepoints) & !missing(sites2blocks)) {
    warning('sitepoints missing so will not try to guess site at center of circle - drawing surrounding blocks only')
  }
  if (!missing(sitepoints) & !missing(sites2blocks)) {
    # good
  }
  
  if ( all(c("blockid", "distance", siteidvarname) %in% names(sitepoints))) {
    really_sitepoints <- FALSE
    # instead of a list of lat,lon points, the user has just provided the output of getblocksnearby() already run, aka sites2blocks
    # so skip getblocksnearby() ... do not have to do it again if already done.
    sites2blocks <- sitepoints # which ignores what they might have also passed as sites2blocks = xyz redundantly
    if (NROW(unique(sites2blocks[ , ..siteidvarname])) > 100) {
      warning("this mapping function is not intended for so many sites - showing only the first 100")
      sites2blocks <- sites2blocks[get(siteidvarname) %in% sort(unique(get(siteidvarname)))[1:100], ]
    }
    # but will need to approximate the lat,lon of each site, below, after we get lat lon of each block.
    # and the approximation done here is pretty bad so the circle drawn around that bad approximation of sitepoints is not covering the right blocks !
  } else {
    really_sitepoints <- TRUE
    if (NROW(sitepoints) > 100) {
      warning("this mapping function is not intended for so many sites - showing only the first 100")
      sitepoints <- sitepoints[1:100, ]
    }
    if (missing(sites2blocks)) {
      # Use getblocksnearby() since   user only provided a list of lat,lon points.
      
      # # and it now automatically creates (in the output) a column ejam_uniq_id that is 1:N to number the list of input points
      # if ('ejam_uniq_id' %in% colnames(sitepoints)) {
      #   if (!identical(sitepoints$ejam_uniq_id, 1:NROW(sitepoints))) {
      #     # problem... input file has ejam_uniq_id but not as numbering sites 1:N, 
      #     # and getblocksnearby() is about to create a column by that name using 1:N 
      #     # but now handles that sort of, by creating a new ejam_uniq_id and saving the one submitted...
      #   }
      # }
      sites2blocks <- getblocksnearby(sitepoints = sitepoints, radius = radius, maxradius = maxradius, avoidorphans = avoidorphans)
      if ("ejam_uniq_id_as_submitted_to_getblocks" %in% names(sites2blocks)) {
        # try to make siteidvarname hold the original information that was submitted by caller as sitepoints$ejam_uniq_id and might not be 1:NROW
        sites2blocks[ , ejam_uniq_id := ejam_uniq_id_as_submitted_to_getblocks]
        ## *** what if ejam_uniq_id was not the siteidvarname ??
      }
    }
  }
  
  if (NROW(sites2blocks) == 0) {
    warning('No block internal points are near the specified sitepoints')
    zeronearby <- TRUE
    setDT(sitepoints) 
    x <- copy(sitepoints)
    
    
  } else {
    zeronearby <- FALSE
    
    if (!(siteidvarname %in% names(sites2blocks))) {
      
      if ('siteid' %in% names(sites2blocks)) {
        warning(paste0("problem - siteidvarname ('", siteidvarname,"') is not a colname of sites2blocks so using 'siteid' column instead"))
        siteidvarname <- "siteid" # assume it was supposed to be this
      } else {
        if ('ejam_uniq_id' %in% names(sites2blocks)) {
          warning(paste0("problem - siteidvarname ('", siteidvarname,"') is not a colname of sites2blocks so using 'ejam_uniq_id' column instead"))
          siteidvarname <- "ejam_uniq_id" # assume it was supposed to be this
        }
      }
    }
    
    # Get the lat,lon of each block so we can map them  ####
    bl <- latlon_join_on_blockid(sites2blocks) # now checks 1st to see if lat lon already there.
    setDT(bl) # redundant but fast
    
    # in the scenario where we got only output of getblocksnearby() not actual sitepoints,
    # we could use latlon_from_s2b()
    if (!really_sitepoints) {
      # infer radius approximately??? ####
      if (missing(radius)) {radius <- radius_inferred(bl)} # round(max(bl$distance, na.rm = TRUE), 1)}
      #  roughly infer sitepoints lat,lon of sites from info in sites2blocks table... just for a rough map...  
      # USING   latlon_from_s2b()  
      # sitepoints <- bl[ , list(lat = mean(blocklat), lon = mean(blocklon)), by = siteidvarname]
      # create dummy empty info for now
      sitepoints <- latlon_from_s2b(bl)  ### did not bother handling siteidvarname vs ejam_uniq_id here... probably never use that option anyway
      # sitepoints <- data.frame(willrename = -999); colnames(sitepoints) <- siteidvarname
    }
    setnames(bl, 'lat', 'blocklat')
    setnames(bl, 'lon', 'blocklon')    
    
    # Put site point(s) (which have lat,lon) and surrounding block points (which have blocklat,blocklon) into one table
    setDT(sitepoints) # not sure this is needed
    x <- copy(sitepoints)
    if ("blockid" %in% names(x)) {
      setnames(x, "blockid", "blockid.of.site")
    } else {
      # x$blockid.of.site <- NA
    }
    if (siteidvarname %in% names(x)) {
      if (!all(as.vector(unlist(x[ , ..siteidvarname])) == 1:NROW(x))) {
        warning(paste0("siteidvarname specified ('", siteidvarname,"') is already a colname of sitepoints, but note it is not equal to rownumbers as assumed by getblocksnearby()"))
        # bl$joinon <- bl[ , ..siteidvarname]
        # x$joinon <- 1:NROW(sitepoints)
        # x <- merge(x, bl, by = "joinon", all.y = TRUE) # all.y is true to keep all blocks in case dummy sitepoints was used and has no matching siteidvarname rows
        # x[ , joinon := NULL] # do not keep?
        # result will have siteidvarname from both x and y?
        
        x <- merge(x, bl, by = siteidvarname, all.y = TRUE)
      } else {
        
        x <- merge(x, bl, by = siteidvarname, all.y = TRUE)
      }
    } else {
      x$joinon <- 1:NROW(sitepoints)
      setnames(x, "joinon", siteidvarname)
      
      x <- merge(x, bl, by = siteidvarname, all.y = TRUE) 
    }
  }
  
  if (usemapfast) {
    
    if (!zeronearby) {
      # BLOCKS SURROUNDING A SITE
      xb <- copy(x)
      xb[ , blockcount_near_site := .N, by = siteidvarname]
      if ("blockid.of.site" %in% names(xb)) {xb$blockid.of.site <- NULL}
      xb[, lat := blocklat]
      xb[, lon := blocklon]
      xb <- unique(xb)
      
    }
    if (really_sitepoints) {
      
      # SITE AT CENTER OF A CIRCLE
      xpt <- copy(x)
      xpt[ , blockcount_near_site := .N, by = siteidvarname]
      xpt <- xpt[ !duplicated(xpt[ , .(lat,lon)]),] # only one point per siteidvarname, since the block coordinates in x had been named blocklat, blocklon, so the lat,lon here were just the site coordinates
      xpt$distance <- 0
      xpt$bgid    <- NA
      xpt$blockwt <- NA
      if ("blockid.of.site" %in% names(xpt)) {
        xpt$blockid  <- xpt$blockid.of.site
        xpt$blockid.of.site <- NULL
      } else {
        xpt$blockid <- NA
      }
      xpt$blocklat <- NA; xpt$blocklon <- NA
      if (zeronearby) {
        mapinfo <- xpt
      } else {
        mapinfo <- rbind(xpt, xb)
      }
    } else {
      mapinfo <- xb
    }
    if ("blockid" %in% names(mapinfo)) {
      if (!exists('blockid2fips')) {
        dataload_from_pins("blockid2fips")
        if (!exists('blockid2fips')) {
          warning("cannot find blockid2fips")
          blockid2fips <- data.table(blockid = NA, blockfips = NA)
        }
      }
      mapinfo[blockid2fips, blockfips := blockfips, on = "blockid"]
      mapinfo[ , bgfips := substr(blockfips, 1, 12)]
      mapinfo[blockgroupstats, bgpop := pop, on = 'bgid']
      mapinfo[ , blockpop := round(blockwt * bgpop, 1)]
      mapinfo[ , pop_nearby := sum(blockpop, na.rm = TRUE), by = siteidvarname] # ?
    }
    
    z <- mapfast(mapinfo, radius = 0.005, ...) #  %>% 
    
    if (really_sitepoints) {
      # overall circle centered on each site, to show radius of search   (units in meters for circles, and pixels for circle markers)
      z <- leaflet::addCircles(z, lat = xpt$lat, lng = xpt$lon, radius = meters_per_mile *  as.vector(radius), color = "gray",
                               fillOpacity = 0.06, fillColor = "gray", opacity = 0.7  ) #  %>% # overall circle
      # site point in center of each circle
      z <- leaflet::addCircleMarkers( z, lat = xpt$lat, lng = xpt$lon, radius = 10, color = "red", opacity = 0.75) # %>%  # in pixels for center of circle=point 
    }
    # Map popup info for each site (if available) and blocks surrounding the site
    vnames <- c('blockfips', 'blockid', 'blocklat', 'blocklon', 
                'distance', 'distance_unadjusted', 'radius.miles', # 'block_radius_miles',
                'blockwt', 'blockpop', 'pop_nearby',
                'bgpop', 'bgfips', 'bgid',   #, names_d, names_d_subgroups, names_e,   # for blockgroup
                'ejam_uniq_id', 'blockcount_near_site'       # for site
    )
    vnames <- intersect(vnames, colnames(mapinfo))
    z <- leaflet::addCircles(z, lat = mapinfo$lat, lng = mapinfo$lon, fillOpacity = 0.1, 
                             popup = popup_from_df(setDF(mapinfo), column_names = vnames), radius = 10)
    
    if (overlay_blockgroups) {
      z <- map_blockgroups_over_blocks(z)
    }
    print(z)
    
  } else {
    
    bplot <- function(x,   ... ) {
      if (!zeronearby) {
        plot(x = x$blocklon, y = x$blocklat , ... )
      }
      if (really_sitepoints) {
        points(x = x$lon, y = x$lat  , col = "red")
      }
    }
    # bplot(x, sample(1:nrow(sitepoints), 1)) # plots a random site surrounded by nearby block points
    bplot(x, main = "Site and surrounding block centroids", xlab = "", ylab = "", ...)
    
  }
  if (returnmap) {
    return(z)
  } else {
    invisible(sites2blocks)
  }
}

