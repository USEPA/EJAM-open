
############################################################################# #
#  shapes_from_fips  ####
############################################################################# #


#' Download shapefiles based on FIPS codes of States, Counties, Cities/CDPs, Tracts, or Blockgroups
#'
#' @param fips vector of one or more Census FIPS codes such as from [name2fips()]
#' @return spatial data.frame with one row per fips (assuming any fips are valid)
#' @examples
#'  fipslist = list(
#'   statefips = name2fips(c('DE', 'RI')),
#'   countyfips = fips_counties_from_state_abbrev(c('DE')),
#'   cityfips = name2fips(c('chelsea,MA', 'st. john the baptist parish, LA')),
#'   tractfips = substr(blockgroupstats$bgfips[300:301], 1, 12),
#'   bgfips = blockgroupstats$bgfips[300:301]
#'   )
#'   shp <- list()
#'   \dontrun{
#'    for (i in seq_along(fipslist)) {
#'     shp[[i]] <- shapes_from_fips(fipslist[[i]])
#'     print(shp[[i]])
#'     # mapfast(shp[[i]])
#'    }
#'   }
#'   
#' 
#' @export
#'
shapes_from_fips <- function(fips) {
  
  suppressWarnings({
    ftype <- fipstype(fips)
  })
  shp <- NULL

  if (all(ftype %in% 'blockgroup')) {
    shp <- shapes_blockgroups_from_bgfips(fips)
  }
  
  if (all(ftype %in% 'tract')) {
    shp <- shapes_tract_from_tractfips(fips)
  }

  if (all(ftype %in% 'city')) {
    shp <- shapes_places_from_placefips(fips)
  }
  
  if (all(ftype %in% 'county')) {
    shp <- shapes_counties_from_countyfips(fips)
  }
  
  if (all(ftype %in% 'state')) {
    shp <- shapes_state_from_statefips(fips)
  }
  
  return(shp)
}
########################### # ########################### # ########################### # ########################### #
# states ####


#' Get boundaries of State(s) for mapping
#'
#' @param fips vector of one or more State FIPS codes
#' @seealso [shapes_from_fips()]
#' @return spatial data.frame of boundaries
#' @keywords internal
#'
shapes_state_from_statefips <- function(fips) {
  
  expectedtype = 'state'
  
  ftype = fipstype(fips)
  if (all(is.na(ftype))) {
    warning('no valid fips')
    return(NULL)
  }
  if (!all(ftype[!is.na(ftype)] %in% expectedtype)) {
    stop("expected all valid fips to be for", expectedtype)
  }
  fips = fips_lead_zero(fips)
  fips = fips[fips_valid(fips)]
  if (length(fips) == 0) {stop('no valid fips')}
  
  shp = states_shapefile[match(fips, states_shapefile$GEOID), ]
  return(shp)  
}
########################### # ########################### # ########################### # ########################### #

# counties ####


#' Get Counties boundaries via API, to map them
#'
#' @details Used [sf::read_sf()], which is an alias for [sf::st_read()]
#'   but with some modified default arguments.
#'   read_sf is quiet by default/ does not print info about data source, and
#'   read_sf returns an sf-tibble rather than an sf-data.frame
#'   
#'   Also note the tidycensus and tigris R packages.
#' @seealso [shapes_from_fips()]
#' @param countyfips FIPS codes as 5-character strings (or numbers) in a vector
#'   as from fips_counties_from_state_abbrev("DE")
#' @param outFields can be "*" for all, or can be
#'   just some variables like SQMI, POPULATION_2020, etc., or none
#' @param myservice URL of feature service to get shapes from.
#'   Only default was tested
#'
#' @return spatial object via [sf::st_read()]
#'
#' @keywords internal
#'
shapes_counties_from_countyfips <- function(countyfips = '10001', outFields = c("NAME", "FIPS", "STATE_ABBR", "STATE_NAME", "POP_SQMI"), # "",
                                            myservice = c(
                                              "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/2/query",
                                              "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_and_States_with_PR/FeatureServer/0/query",
                                              "https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/EJScreen_2_22_US_Percentiles_Tracts/FeatureServer/query")[1]
) {
  # for a vector of  FIPS, use arcgis API to obtain map boundaries of just those census units
  fips = countyfips
  
  expectedtype = 'county'
  
  ftype = fipstype(fips)
  if (all(is.na(ftype))) {
    warning('no valid fips')
    return(NULL)
  }
  if (!all(ftype[!is.na(ftype)] %in% expectedtype)) {
    stop("expected all valid fips to be for", expectedtype)
  }
  fips = fips_lead_zero(fips)
  fips = fips[fips_valid(fips)]
  if (length(fips) == 0) {stop('no valid fips')}
  
  if (length(outFields) > 1) {
    outFields <- paste0(outFields, collapse = ",")
  }
  # outFields values: 
  # from   https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/2
  # OBJECTID (type: esriFieldTypeOID, alias: OBJECTID, SQL Type: sqlTypeOther, length: 0, nullable: false, editable: false)
  # NAME (type: esriFieldTypeString, alias: County Name, SQL Type: sqlTypeOther, length: 50, nullable: true, editable: true)
  # STATE_NAME (type: esriFieldTypeString, alias: State Name, SQL Type: sqlTypeOther, length: 20, nullable: true, editable: true)
  # STATE_ABBR (type: esriFieldTypeString, alias: State Abbreviation, SQL Type: sqlTypeOther, length: 2, nullable: true, editable: true)
  # STATE_FIPS (type: esriFieldTypeString, alias: State FIPS, SQL Type: sqlTypeOther, length: 2, nullable: true, editable: true)
  # COUNTY_FIPS (type: esriFieldTypeString, alias: County FIPS, SQL Type: sqlTypeOther, length: 3, nullable: true, editable: true)
  # FIPS (type: esriFieldTypeString, alias: FIPS Code, SQL Type: sqlTypeOther, length: 5, nullable: true, editable: true)
  # POPULATION (type: esriFieldTypeInteger, alias: 2022 Total Population, SQL Type: sqlTypeOther, nullable: true, editable: true)
  # POP_SQMI (type: esriFieldTypeDouble, alias: 2022 Population per square mile, SQL Type: sqlTypeOther, nullable: true, editable: true)
  # SQMI (type: esriFieldTypeDouble, alias: Area in square miles, SQL Type: sqlTypeOther, nullable: true, editable: true)
  # POPULATION_2020 (type: esriFieldTypeInteger, alias: 2020 Total Population, SQL Type: sqlTypeOther, nullable: true, editable: true)
  # POP20_SQMI (type: esriFieldTypeDouble, alias: 2020 Population per square mile, SQL Type: sqlTypeOther, nullable: true, editable: true)
  # Shape__Area (type: esriFieldTypeDouble, alias: Shape__Area, SQL Type: sqlTypeDouble, nullable: true, editable: false)
  # Shape__Length (type: esriFieldTypeDouble, alias: Shape__Length, SQL Type: sqlTypeDouble, nullable: true, editable: false)
  
  if (length(fips) > 50) {
    # The API does let you get >50 at once but instead of figuring out that syntax, this function works well enough
    batchsize <- 50
    batches <- 1 + (length(fips) %/% batchsize)
    # ***  add code here to handle 50 at a time and assemble them
    out <- list()
    for (i in 1:batches) {
      first <- 1 + ((i - 1) * batchsize)
      last <- min(first + batchsize - 1, length(fips))
      out[[i]] <- shapes_counties_from_countyfips(fips[first:last], outFields = outFields, myservice = myservice)
    }
    out <- do.call(rbind, out)
    return(out)
  }
  
  if (grepl("ejscreen", myservice, ignore.case = TRUE)) {FIPSVARNAME <- "ID"} else {FIPSVARNAME <- "FIPS"}
  myurl <- httr2::url_parse(myservice)
  myurl$query <- list(
    where = paste0(paste0(FIPSVARNAME, "='", fips, "'"), collapse = " OR "),  ########################### #
    outFields = outFields,
    returnGeometry = "true",
    f = "geojson")
  request <- httr2::url_build(myurl)
  mymapdata <- sf::st_read(request) # st_read returns data.frame, read_sf returns tibble
  return(mymapdata)
}
########################### # ########################### # ########################### # ########################### #

# tracts  ####

#' Get tract boundaries, via API, to map them
#'
#' @details This is useful mostly for small numbers of tracts.
#'   The EJScreen map services provide other ways to map tracts and see EJScreen data.
#' @param fips one or more FIPS codes as 11-character strings in a vector
#' @param outFields can be "*" for all, or can be
#'   just a vector of variables that particular service provides, like FIPS, SQMI, POPULATION_2020, etc.
#' @param myservice URL of feature service to get shapes from.
#' @seealso [shapes_from_fips()]
#' @return spatial object via [sf::st_read()] # sf-data.frame, not sf-tibble like [sf::read_sf()]
#'
#' @keywords internal
#'
shapes_tract_from_tractfips <- function(fips, outFields = c("FIPS", "STATE_ABBR", "SQMI"),
                                        myservice = "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/4/query") {
  
  outFields <- paste0(outFields, collapse = ',')
  
  expectedtype = 'tract'
  
  ftype = fipstype(fips)
  if (all(is.na(ftype))) {
    warning('no valid fips')
    return(NULL)
  }
  if (!all(ftype[!is.na(ftype)] %in% expectedtype)) {
    stop("expected all valid fips to be for", expectedtype)
  }
  fips = fips_lead_zero(fips)
  fips = fips[fips_valid(fips)]
  if (length(fips) == 0) {stop('no valid fips')}
  
  shp <- shapes_blockgroups_from_bgfips(fips, outFields = outFields, myservice = myservice)
  return(shp)
}
########################### # ########################### # ########################### # ########################### #

# blockgroups ####


#' Get blockgroups boundaries, via API, to map them
#'
#' @details This is useful mostly for small numbers of blockgroups.
#'   The EJScreen map services provide other ways to map blockgroups and see EJScreen data.
#' @param bgfips one or more block group FIPS codes as 12-character strings in a vector
#' @param outFields can be "*" for all, or can be
#'   just a vector of variables that particular service provides, like FIPS, SQMI, POPULATION_2020, etc.
#' @param myservice URL of feature service to get shapes from.
#'
#'   "https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/
#'   EJScreen_2_21_US_Percentiles_Block_Groups/FeatureServer/0/query"
#'
#'   for example provides EJScreen indicator values, NPL_CNT, TSDF_CNT, EXCEED_COUNT_90, etc.
#' @seealso [shapes_from_fips()]
#' @return spatial object via [sf::st_read()] # sf-data.frame, not sf-tibble like [sf::read_sf()]
#'
#' @keywords internal
#'
shapes_blockgroups_from_bgfips <- function(bgfips = '010890029222', outFields = c("FIPS", "STATE_ABBR", "SQMI"),
                                           myservice = c(
                                             "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/5/query",
                                             "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Block_Groups/FeatureServer/0/query",
                                             "https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/EJScreen_2_21_US_Percentiles_Block_Groups/FeatureServer/0/query")[1]
) {
  
  outFields <- paste0(outFields, collapse = ',')
  
  # for a vector of blockgroup FIPS, use arcgis API to obtain map boundaries of just those blockgroups

  fips = bgfips
  
  expectedtype = 'blockgroup'
  
  ftype = fipstype(fips)
  if (all(is.na(ftype))) {
    warning('no valid fips')
    return(NULL)
  }
  if (!all(ftype[!is.na(ftype)] %in% expectedtype)) {
    stop("expected all valid fips to be for", expectedtype)
  }
  fips = fips_lead_zero(fips)
  fips = fips[fips_valid(fips)]
  if (length(fips) == 0) {stop('no valid fips')}
  
  if (length(fips) > 50) {
    
    # The API does let you get >50 at once but instead of figuring out that syntax, this function works well enough
    batchsize <- 50
    batches <- 1 + (length(fips) %/% batchsize)
    # ***  add code here to handle 50 at a time and assemble them
    out <- list()
    for (i in 1:batches) {
      first <- 1 + ((i - 1) * batchsize)
      last <- min(first + batchsize - 1, length(fips))
      out[[i]] <- shapes_blockgroups_from_bgfips(fips[first:last], outFields = outFields, myservice = myservice)
    }
    out <- do.call(rbind, out)
    return(out)
    # warning("Cannot get so many blockgroup shapes in one query, via this API, as coded! Using first 50 only.")
    # fips <- fips[1:50]
  }
  
  if (grepl("ejscreen", myservice, ignore.case = TRUE)) {FIPSVARNAME <- "ID"} else {FIPSVARNAME <- "FIPS"}
  myurl <- httr2::url_parse(myservice)
  myurl$query <- list(
    where = paste0(paste0(FIPSVARNAME, "='", fips, "'"), collapse = " OR "),  ########################### #
    outFields = outFields,
    returnGeometry = "true",
    f = "geojson")
  request <- httr2::url_build(myurl)
  mymapdata <- sf::st_read(request) # data.frame not tibble
  return(mymapdata)
}
########################### # ########################### # ########################### # ########################### #



# places/ cities ####



####################################################### #
## examples
#
# place_st = c("Port Chester, NY", "White Plains, NY", "New Rochelle, NY")
# shp = shapes_places_from_placenames(place_st)
# 
# out <- ejamit(shapefile = shp)
# map_shapes_leaflet(shapes = shp, 
#                    popup = popup_from_ejscreen(out$results_bysite))
# ejam2excel(out, save_now = F, launchexcel = T)

#   fips = fips_place_from_placename("Port Chester, NY")
# seealso [shapes_places_from_placefips()] [shapes_places_from_placenames()]
#   [fips_place2placename()] [fips_place_from_placename()] [censusplaces]


# also see 
#  https://www2.census.gov/geo/pdfs/reference/GARM/Ch9GARM.pdf
#  https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2023/TGRSHP2023_TechDoc_Ch3.pdf 
#  https://github.com/walkerke/tigris?tab=readme-ov-file#readme
#  https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html#tigris-workflows
#
# For demographic data (optionally pre-joined to tigris geometries), see the tidycensus package.
# NAD 1983 is what the tigris pkg uses -- it only returns feature geometries for US Census data that default to NAD 1983 (EPSG: 4269) coordinate reference system (CRS).
#   For help deciding on appropriate CRS, see the crsuggest package.


## used by name2fips or fips_from_name 
# see https://www2.census.gov/geo/pdfs/reference/GARM/Ch9GARM.pdf

####################################################### #


#' Get shapefiles/ boundaries of census places like cities
#'
#' @param fips vector of 7-digit City/town/CDP codes as in [censusplaces$fips]
#' @seealso [shapes_from_fips()]
#' @return spatial data.frame for mapping
#' 
#' @keywords internal
#'
shapes_places_from_placefips <- function(fips) {
  
  expectedtype = 'city'
  
  ftype = fipstype(fips)
  if (all(is.na(ftype))) {
    warning('no valid fips')
    return(NULL)
  }
  if (!all(ftype[!is.na(ftype)] %in% expectedtype)) {
    stop("expected all valid fips to be for", expectedtype)
  }
  fips = fips_lead_zero(fips)
  fips = fips[fips_valid(fips)]
  if (length(fips) == 0) {stop('no valid fips')}
  
  ST <- unique(fips2state_abbrev(fips)) 

  shp <- tigris::places(ST)
  shp <- shp[match(fips, shp$GEOID), ] # filter using FIPS is more robust than trying to get exact name right
  return(shp)
}
####################################################### #


shapes_places_from_placenames <- function(place_st) {
  
  # name2fips()  uses fips_place_from_placename() 
  
  ## input here is in the format of place_st as is created from the censusplaces table
  ##   columns  placename  and ST field 
  ##   so it has lower case "city" for example like "Denver city" or "Funny River CDP"
  ## which is sometimes slightly different than found in TIGRIS places table
  ##  column NAMELSAD   and stateabbrev ST would be based on STATEFP field
  ## and "NAMELSAD' differs from the "NAME" column (e.g., Hoboken vs Hoboken city)
  
  # place_st = c('denver city, co',  "new york city, ny" )
  
  fips = fips_place_from_placename(place_st)  # get FIPS of each place
  fips = fips_lead_zero(fips)
  
  st = censusplaces$ST[match(as.integer(fips), censusplaces$fips)]
  # as.numeric since not stored with leading zeroes there !
  
  tp = tigris::places(unique(st))  # DOWNLOAD THE BOUNDARIES of all places in an ENTIRE STATE, for EACH STATE REQUIRED HERE
  shp = tp[match(fips, tp$GEOID), ] # use FIPS of each place to get boundaries
  return(shp)
}
####################################################### #



####################################################### ######################################################## #

## obsolete

shapes_places_from_placefips_oldway <- function(fips) {
  
  fips <- fips_lead_zero(fips)
  if (!all(as.integer(fips) %in% censusplaces$fips)) {stop("check fips - some are not found in censusplaces$fips")}
  
  st <- fips2state_abbrev(fips)
  place_nost <- fips_place2placename(fips, append_st = FALSE)
  
  shp <- tigris::places(st) %>% 
    tigris::filter_place(place_nost) # gets shapefile of those places boundaries from census via download
  return(shp)
}
####################################################### #

## obsolete

shapes_places_from_placenames_oldway <- function(place_st) {
  
  #   This earlier way just relies on the tigris pkg for search/filtering
  
  if (length(st) > 1) {cat("not tested/written to handle more than one state at a time\n")}
  
  # getst = function(x) {gsub(".*(..)", "\\1", x)}  # only works if exact format right like x = c("Port Chester, NY", "White Plains, NY", "New Rochelle, NY")
  # st = unique(getst(place_st))
  st = unique(post_comma(place_st))
  
  # getnost = function(x) gsub("(.*),.*", "\\1", x) # keep non-state parts, only works if exact format right
  # place_nost = getnost(place_st)
  place_nost = pre_comma(place_st)
  
  tigrisplaces <-  tigris::places(st) # places(st) is what limits tigrisplaces to just the State st
  shp <- tigrisplaces %>% tigris::filter_place(place_nost) # gets shapefile of those places boundaries from census via download
  
  # tigrisplaces$NAME is like Rockland     
  # tigrisplaces$NAMELSAD  is like Rockland city
  # tigrisplaces also has STATEFP, PLACEFP, geometry, ALAND, INTPTLAT, INTPTLON, etc.
  #   and in GA,e.g., has more fips than censusplaces does, somehow, and a few fips in censusplaces are not in tigrisplaces
  ## censusplaces has these: eparegion ST stfips      countyname countyfips         placename    fips
  
  return(shp)
}
####################################################### #




