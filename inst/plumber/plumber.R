####################################################### #
#
# Set up API for access to EJAM functionality, using the plumber package.
#
# also see    EJAM file "plumber/test_the_api.R"
############################# #
#* @apiTitle EJAM API
#* 
#* @apiDescription Provides EJAM/EJScreen batch analysis summary results.
#* See the EJAM package for technical documentation on functions powering the API, at <https://usepa.github.io/EJAM/index.html>

# future::plan("multisession")  # did not seem to work

############################# #

library(EJAM)

############################# #

# MULTIPLE POINTS or Shapefile WILL NEED TO BE PASSED USING POST AND REQUEST BODY
#  SO IT IS A BIT MORE COMPLICATED - NOT DONE YET

############################# #


####################################################### #
#  DEFINE API ENDPOINTS ####
####################################################### #


# ejamit ####

#* json table of EJAM analysis summary results for all residents within X miles of a single point or in a polygon
#*
#* @param lat Latitude decimal degrees  (single point only, for now)
#* @param lon Longitude decimal degrees (single point only, for now)
#* @param radius Radius in miles
#* @param shapefile shapefile (ignores lat,lon,radius if this is provided). NOT YET IMPLEMENTED. 
#* @param names "long" returns plain-English name of each indicator. Any other setting returns short variable names like "pctlowinc"
#* @param test "true" or "false" If true, returns a pre-calculated result (ignoring lat, lon, radius)
#* 
#* Like EJAM::ejamit()$results_overall (but with friendlier column names for indicators).
#* 
#* Calling from R for example: 
#* url2 <- "http://urlgoeshere/ejamit?lon=-101&lat=36&radius=1&test=true"; 
#* results_overall <- httr2::request(url2) |> httr2::req_perform() |>  
#* httr2::resp_body_json() |> jsonlite::toJSON() |> jsonlite::fromJSON()
#* 
#* @get /ejamit
#* 
function(lat = 40.81417, lon = -96.69963, radius = 1, shapefile = 0, names = "long", test = "false") {
  
  lat <- as.numeric(lat); lon <- as.numeric(lon); radius <- as.numeric(radius)
  if (length(lat) != 1 | length(lon) != 1) {lat <- 40.81417; lon <- -96.69963}
  if (length(radius) != 1) {radius <- 1}
  
  if (test == "true") {
    out <- as.data.frame(EJAM::testoutput_ejamit_10pts_1miles$results_overall)
  } else {
    
    # promises::future_promise({  # did not seem to work
      
       if (!all(0 == shapefile)) {
         
         return("not working yet for shapefile inputs")
         
         out <- ejamit(
           shapefile = shapefile,
           radius = radius
           )$results_overall
         
       } else {
         
      out <- ejamit(
        sitepoints = data.frame(lat = lat, lon = lon),
        radius = radius
      )$results_overall
      
      }
    # })
    
  }
  
  if (names == "long") {
    names(out) <- fixcolnames(names(out), 'r', 'long')
  }
  
  # if (attachment == "true") {
  # plumber::as_attachment(
  #   value = as.data.frame(out),
  #   filename = "EJAM_results.csv"
  # )
  # } else {
  out
  # }
}
####################################################### #

# ejamit_csv ####
#
#* csv table of EJAM analysis summary results for all residents within X miles of a single point defined by latitude and longitude. 
#*
#* @param lat Latitude decimal degrees (single point only, for now)
#* @param lon Longitude decimal degrees (single point only, for now)
#* @param radius Radius in miles
#* @param names "long" returns plain-English name of each indicator. Any other setting returns short variable names like "pctlowinc"
#* @param test "true" or "false" If true, returns a pre-calculated result (ignoring lat, lon, radius)
#* 
#* Like EJAM::ejamit()$results_overall (but with friendlier column names for indicators). 
#* 
#* @serializer csv
#* @get /ejamit_csv
#* 
function(lat = 40.81417, lon = -96.69963, radius = 1, names = "long", test = "false") {
  
  lat <- as.numeric(lat); lon <- as.numeric(lon); radius <- as.numeric(radius)
  if (length(lat) != 1 | length(lon) != 1) {lat <- 40.81417; lon <- -96.69963}
  if (length(radius) != 1) {radius <- 1}
  
  if (test == "true") {
    out <- as.data.frame(EJAM::testoutput_ejamit_10pts_1miles$results_overall)
  } else {
    # promises::future_promise({ # did not seem to work 
      out <- ejamit(
        sitepoints = data.frame(lat = lat, lon = lon),
        radius = radius
      )$results_overall
    # }) # did not seem to work
  }
  
  if (names == "long") {
    names(out) <- fixcolnames(names(out), 'r', 'long')
  }
  
  # if (attachment == "true") {
  plumber::as_attachment(
    value = as.data.frame(out),
    filename = "EJAM_results.csv"
  )
  # } else {
  #   out
  #   }
}
####################################################### #

# getblocksnearby ####
#
#* json table of distances to all Census blocks near given point.
#* 
#* @param lat decimal degrees (single point only, for now)
#* @param lon decimal degrees (single point only, for now)
#* @param radius Radius of circular area in miles. 
#* 
#* Finds all Census blocks whose internal point is within radius of site point.
#* 
#* @get /getblocksnearby
#* 
function(lat, lon, radius) {
  
  lat <- as.numeric(lat); lon <- as.numeric(lon); radius <- as.numeric(radius)
  if (length(lat) != 1 | length(lon) != 1) {lat <- 40.81417; lon <- -96.69963}
  if (length(radius) != 1) {radius <- 1}
  
  # require(EJAM)
  # if (!exists("blockwts"))  dataload_from_pins()
  # if (!exists("localtree")) indexblocks()
  
  # promises::future_promise({  # 
    
    out <- EJAM::getblocksnearby(
      data.frame(
        lat = lat,
        lon = lon
      ),
      radius = as.numeric(radius)  # , quadtree = localtree
    )
  # })
  out
}
####################################################### #

# get_blockpoints_in_shape ####
#
#* json table of Census blocks in each polygon
#* 
#* @param polys Spatial data that is polygons as from sf::st_as_sf()
#* @param addedbuffermiles width of optional buffering to add to the points (or edges), in miles
#* @param dissolved If TRUE, use sf::st_union(polys) to find unique blocks inside any one or more of polys
#* @param safety_margin_ratio  multiplied by addedbuffermiles, how far to search for blocks nearby using EJAM::getblocksnearby(), before using those found to do the intersection 
#* @param crs coordinate reference system used in st_as_sf() and st_transform() and shape_buffered_from_shapefile_points(), crs = 4269 or Geodetic CRS NAD83
#* @get /get_blockpoints_in_shape
#* 
function(polys,
         addedbuffermiles = 0,
         dissolved = FALSE,
         safety_margin_ratio = 1.10,
         crs = 4269
) {
  
  return("not working yet for shapefile inputs")
  
  # require(EJAM)
  # if (!exists("blockwts"))  dataload_from_pins()
  # if (!exists("localtree")) indexblocks()
  
  # promises::future_promise({  # })
    
    out <- EJAM::get_blockpoints_in_shape(
      polys = polys,
      addedbuffermiles = addedbuffermiles,
      dissolved = dissolved,
      safety_margin_ratio = safety_margin_ratio,
      crs = crs
    )
  # })
  out
}
####################################################### #

# # doaggregate ####
# #
#* List of tables and other info summarizing demog and envt based on sites2blocks table
#* 
#* @param sites2blocks see [doaggregate()]
#* @param sites2states_or_latlon see [doaggregate()]
#* @param countcols see [doaggregate()]
#* @param popmeancols see [doaggregate()]
#* @param calculatedcols see [doaggregate()]
#* @param ... passed to [doaggregate()]
#* @get /doaggregate
#* 
function(sites2blocks, sites2states_or_latlon, countcols, popmeancols, calculatedcols, ...) {
  # promises::future_promise({  
    require(EJAM)
    if (!exists("blockwts"))  dataload_from_pins()
    if (!exists("localtree")) indexblocks()
    EJAM::doaggregate(sites2blocks = sites2blocks,
                      sites2states_or_latlon = sites2states_or_latlon,
                      countcols = countcols, popmeancols = popmeancols, calculatedcols = calculatedcols, ... )
  # })
}
# ####################################################### #

# echo ####
#
#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
#* 
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}
####################################################### #

# if (format == "excel") {
#   # NOT WORKING YET - THIS WOULD NOT RETURN A SPREADSHEET IF save_now=FALSE... IT JUST WOULD CREATE A WORKBOOK IN openxlsx::  format.
# promises::future_promise({  # }) 
#   # out <- table_xls_from_ejam(ejamit(sitepoints = sitepoints, radius = radius), launchexcel = F, save_now = FALSE)
# }) 

# ##promises::future_promise({  # }) 
#   out <- as.data.frame(as.data.frame(EJAM::ejamit(sitepoints = sitepoints, radius = radius)[["results_overall"]]))
# ##}) 
# }
#

####################################################### #
####################################################### #
