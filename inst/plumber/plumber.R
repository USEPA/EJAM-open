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

## JUST A DRAFT - NOT TESTED AT ALL 

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
#* url2 <- "https://urlgoeshere/ejamit?lon=-101&lat=36&radius=1&test=true";
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

## JUST A DRAFT - NOT TESTED AT ALL 

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

# ejam2report ####

## JUST A DRAFT - NOT TESTED AT ALL 


##  We probably also want an API endpoint that combines ejamit() and ejam2report(),
##  so it is essentially doing  ejam2report(ejamit(  ))
##  so inputs are point(s) or polygon(s) or fip(s), and output is html summary report.


#* like ejam2report(), returns html EJAM summary report, analysis results, given the list that is the output of ejamit()
#*
#* @param ejamitout the output of ejamit(), and if omitted, a sample report is returned
#* @param ... other parameters passed to ejam2report(), but these are preset and cannot be changed:
#*   launch_browser = FALSE, fileextension = "html", return_html = FALSE, filename = "EJAM_results.html"
#*
#* Like EJAM::ejam2report()
#*
#* @serializer write_file
#* @get /ejam2report
#*
function(ejamitout = testoutput_ejamit_10pts_1miles, ...) {
  
  # ejamitout = testoutput_ejamit_10pts_1miles,
  # sitenumber = NULL,
  # analysis_title = 'Summary of Analysis',
  # submitted_upload_method = c("latlon", "SHP", "FIPS")[1],
  # shp = NULL,
  # return_html = FALSE,
  # fileextension = c("html", "pdf")[1],
  # filename = NULL,
  # launch_browser = TRUE,
  # show_ratios_in_report = TRUE,
  # extratable_show_ratios_in_report = TRUE,
  # extratable_title = '', #'Additional Information',
  # extratable_title_top_row = 'ADDITIONAL INFORMATION',
  # extratable_list_of_sections = list(
  #   # see build_community_report defaults and see global_defaults_*.R
  #   `Breakdown by Population Group` = names_d_subgroups,
  #   `Language Spoken at Home` = names_d_language,
  #   `Language in Limited English Speaking Households` = names_d_languageli,
  #   `Breakdown by Sex` = c('pctmale','pctfemale'),
  #   `Health` = names_health,
  #   `Age` = c('pctunder5', 'pctunder18', 'pctover64'),
  #   `Community` = names_community[!(names_community %in% c( 'pctmale', 'pctfemale', 'pctownedunits_dupe'))],
  #   `Poverty` = names_d_extra,
  #   `Features and Location Information` = c(
  #     names_e_other,
  #     names_sitesinarea,
  #     names_featuresinarea,
  #     names_flag
  #   ),
  #   `Climate` = names_climate,
  #   `Critical Services` = names_criticalservice,
  #   `Other` = names_d_other_count
  #   # , `Count above threshold` = names_countabove  # need to fix map_headernames longname and calctype and weight and drop 2 of the 6
  # ),
  # ## all the indicators that are in extratable_list_of_sections:
  # extratable_hide_missing_rows_for = as.vector(unlist(extratable_list_of_sections))
  
  fname = "EJAM_results.html"

  out <- ejam2report(out, launch_browser = FALSE, fileextension = "html", 
                     return_html = FALSE, ## ???
                     filename = fname, ## or NULL ?
                     ...)
  
  # if (attachment == "true") {
  plumber::as_attachment(
    value = out,
    filename = fname
  )
  # } else {
  #   out
  #   }
}
####################################################### #

# ejam2excel ####

## JUST A DRAFT - NOT TESTED AT ALL 

#* like ejam2excel(), returns xlsx file of EJAM analysis results for all residents within X miles of a single point defined by latitude and longitude.
#*
#* @param lat Latitude decimal degrees (single point only, for now)
#* @param lon Longitude decimal degrees (single point only, for now)
#* @param radius Radius in miles
#* @param names "long" returns plain-English name of each indicator. Any other setting returns short variable names like "pctlowinc"
#* @param test "true" or "false" If true, returns a pre-calculated result (ignoring lat, lon, radius)
#* @param ... other parameters passed to ejam2excel()
#*
#* Like EJAM::ejam2excel()
#*
#* @serializer write_file
#* @get /ejam2excel
#*
function(lat = 40.81417, lon = -96.69963, radius = 1, test = "false", ...) {

  fname = "EJAM_results.xlsx"

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

  out <- ejam2excel(out, ...)

  # if (attachment == "true") {
  plumber::as_attachment(
    value = out,
    filename = fname
  )
  # } else {
  #   out
  #   }
}
####################################################### #

# getblocksnearby ####

## JUST A DRAFT - NOT TESTED AT ALL 

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

## JUST A DRAFT - NOT TESTED AT ALL 

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

# doaggregate ####

## JUST A DRAFT - NOT TESTED AT ALL 

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
  if (!exists("blockgroupstats")) {library(EJAM)} # to use installed version only if not already attached
  # library(EJAM)
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
