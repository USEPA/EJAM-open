
############################################################################# #
#  shapes_from_fips  ####
############################################################################# #


#' Download shapefiles based on FIPS codes of States, Counties, Cities/CDPs, Tracts, or Blockgroups
#'
#' @param fips vector of one or more Census FIPS codes such as from [name2fips()]
#' 
#' @param myservice_blockgroup URL of feature service to get shapes from, 
#'   or "cartographic" or "tiger" to use approx or slow/accurate bounds from tidycensus and tigris packages.
#' @param myservice_tract URL of feature service to get shapes from, 
#'   or "cartographic" or "tiger" to use approx or slow/accurate bounds from tidycensus and tigris packages.
#' @param myservice_place only "tiger" is implemented
#' @param myservice_county URL of feature service to get shapes from, 
#'   or "cartographic" or "tiger" to use approx or slow/accurate bounds from tidycensus and tigris packages.
#'   Note State bounds are built into this package as data so do not need to be downloaded from a service.
#' @details when using tigris package ("tiger" as service-related parameter here),
#' it uses the year that is the default in the version of the tigris package that is installed.
#' You can use options(tigris_year = 2022) for example to specify it explicitly.
#' This function also sets options(tigris_use_cache = TRUE), but each individual shapes_xyz_from_ function may not specify.
#' @return spatial data.frame with one row per fips (assuming any fips are valid)
#' @examples
#'  # shp2 = shapes_from_fips("10001", "10005")
#' 
#'  fipslist = list(
#'   statefips = name2fips(c('DE', 'RI')),
#'   countyfips = fips_counties_from_state_abbrev(c('DE')),
#'   cityfips = name2fips(c('chelsea,MA', 'st. john the baptist parish, LA')),
#'   tractfips = substr(blockgroupstats$bgfips[300:301], 1, 12),
#'   bgfips = blockgroupstats$bgfips[300:301]
#'   )
#'   shp <- list()
#'   \donttest{
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
shapes_from_fips <- function(fips, 
                             myservice_blockgroup = "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/5/query",
                             myservice_tract = "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/4/query",
                             myservice_place = 'tiger',
                             myservice_county = 'cartographic' # or "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/2/query"
                             # myservice_state = built into the package as dataset
) {
  if (offline_cat()) {
    stop("Cannot download boundaries - No internet connection seems to be available.")
    # return(NULL)
  }
  
  suppressWarnings({
    ftype <- fipstype(fips)
  })
  shp <- NULL
  
  options(tigris_use_cache = TRUE) # But it seems to use cache anyway?
  # options(tigris_year = 2022) # uses default of the tigris package version installed
  
  if (all(ftype %in% 'blockgroup')) {
    shp <- try(shapes_blockgroups_from_bgfips(fips, myservice = myservice_blockgroup))
  }
  
  if (all(ftype %in% 'tract')) {
    shp <- try(shapes_tract_from_tractfips(fips, myservice = myservice_tract))
  }
  
  if (all(ftype %in% 'city')) {
    shp <- try(shapes_places_from_placefips(fips, myservice = myservice_place))
  }
  
  if (all(ftype %in% 'county')) {
    shp <- try(shapes_counties_from_countyfips(fips, myservice = myservice_county))
  }
  
  if (all(ftype %in% 'state')) {
    shp <- try(shapes_state_from_statefips(fips))
  }
  
  types <- c('blockgroup', 'tract', 'city', 'county', 'state')
  if (length(intersect(ftype, types)) > 1) {
    if (shiny::isRunning()) {
      validate("This dataset contains more than one type of FIPS code. Analysis can only be run on datasets with one type of FIPS codes.")
      shp <- NULL
    } else {
      stop("This dataset contains more than one type of FIPS code. Analysis can only be run on datasets with one type of FIPS codes.")
    }
  }
  if (inherits(shp, "try-error")) {
    if (shiny::isRunning()) {
      validate("unable to obtain Census unit boundaries to map the requested fips codes")
      shp <- NULL
    } else {
      stop("Error in downloading shapefile from API. Check your internet connection and the API URL.")
    }
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
  shp$FIPS <- shp$GEOID 
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
#'   But note the tidycensus and tigris R packages can more quickly get county boundaries for mapping.
#'   
#' @seealso [shapes_from_fips()]
#' @param countyfips FIPS codes as 5-character strings (or numbers) in a vector
#'   as from fips_counties_from_state_abbrev("DE")
#' @param outFields can be "*" for all, or can be
#'   just some variables like SQMI, POPULATION_2020, etc., or none
#' @param myservice URL of feature service to get shapes from
#'   or "cartographic" or "tiger" to use approx or slow/accurate bounds from tidycensus and tigris packages.
#'
#' @return spatial object via [sf::st_read()]
#'
#' @keywords internal
#'
shapes_counties_from_countyfips <- function(countyfips = '10001', outFields = c("NAME", "FIPS", "STATE_ABBR", "STATE_NAME", "POP_SQMI"), # "",
                                            myservice = c(
                                              "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/2/query",
                                              "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_and_States_with_PR/FeatureServer/0/query",
                                              'cartographic', 'tiger'
                                            )[3] 
) {
  
  acsendyear_carto_tiger = acsendyear()
  
  # for a vector of  FIPS, 
  # was using looped/batched arcgis API to obtain map boundaries of just those census units
  # but faster to use tigris and tidycensus packages
  
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
  
  tidycensus_ok <- TRUE
  if (nchar(Sys.getenv("CENSUS_API_KEY")) == 0) {
    tidycensus_ok <- FALSE
  }
  
  if (myservice[1] %in% c("cartographic", "tiger") && tidycensus_ok) {
    
    # use tidycensus pkg
    # library(tidycensus)
    # library(tidyverse)   
    
    if (myservice[1] == 'cartographic') {usecb = TRUE}
    if (myservice[1] == 'tiger') {usecb = FALSE}
    
    options(tigris_use_cache = TRUE) # But it seems to use cache anyway?
    
    # get_acs() can get all counties for a list of states, or selected counties from 1 state,
    #  but is not able to get just selected counties from more than 1 state in a single function call,
    #  probably since the parameters expect a vector of 3-digit county fips that do not include 2 state digits (or county names without )
    # so we have to 
    # a) loop over States here, get just selected counties from 1 state at a time (no)
    # or 
    # b) do 1 call to get ALL counties in specified states and then drop unrequested counties (yes)
    #
    # Get ALL counties in each relevant state, then drop unrequested ones
    
    mystates <- unique(fips2state_fips(fips))
    if ('72' %in% mystates) {warning("cannot map PR")}
    mystates <- setdiff(mystates, "72")
    
    ## e.g., testing
    # shp <- get_acs(
    #   geography = "county",
    #   variables = "B10001_001",
    #   state = c("44", "10"), 
    #   geometry = TRUE,
    #   year = 2022,
    #   show_call = TRUE
    # )
    
    shp <- tidycensus::get_acs(
      geography = "county",
      variables = "B10001_001",
      cb = usecb, ## TRUE MEANS FASTER DOWNLOAD BUT LESS ACCURATE
      state = mystates,
      # county =  substr(unique(fips), 3,5), # this function expects county fips to be only the county portion without the 2 state digits
      geometry = TRUE,
      year = as.numeric(acsendyear_carto_tiger),
      survey = 'acs5',
      # key = , # API key would go here
      show_call = TRUE
    )
    # now drop unrequested counties
    shp <- shp[shp$GEOID %in% fips, ] # GEOID is the 5-digit county fips here
    
    
    # tidycensus format initially:
    # > shp
    # Simple feature collection with 3222 features and 5 fields
    # Geometry type: MULTIPOLYGON
    # Dimension:     XY
    # Bounding box:  xmin: -179.1467 ymin: 17.88328 xmax: 179.7785 ymax: 71.38782
    # Geodetic CRS:  NAD83
    # First 10 features:
    #    GEOID                           NAME   variable estimate  moe                       geometry
    # 1  01069        Houston County, Alabama B10001_001     2458  326 MULTIPOLYGON (((-85.71209 3...
    # 2  01023        Choctaw County, Alabama B10001_001      464  150 MULTIPOLYGON (((-88.47323 3...
    
    # old output format: 
    # > shp = EJAM:::shapes_counties_from_countyfips(cfips)
    # > shp
    # Simple feature collection with 112 features and 5 fields
    # Geometry type: GEOMETRY
    # Dimension:     XY
    # Bounding box:  xmin: -179.1489 ymin: 30.22093 xmax: -84.88825 ymax: 71.36516
    # Geodetic CRS:  WGS 84
    # First 10 features:
    #               NAME  FIPS STATE_ABBR STATE_NAME POP_SQMI                       geometry
    # 1   Autauga County 01001         AL    Alabama     98.8 MULTIPOLYGON (((-86.41312 3...
    # 2   Baldwin County 01003         AL    Alabama    148.7 MULTIPOLYGON (((-87.56491 3...
    
    drop_comma_statename = function(countyname_state) {
      gsub(", .*$", "", countyname_state)
    }
    
    names(shp) <- gsub("GEOID", "FIPS", names(shp))
    names(shp) <- gsub("estimate", "pop", names(shp))
    names(shp) <- gsub("moe", "pop_moe", names(shp))
    shp$NAME <-  drop_comma_statename(fips2countyname(shp$FIPS))
    shp$STATE_ABBR <- fips2state_abbrev(shp$FIPS)
    shp$STATE_NAME <- fips2statename(shp$FIPS)
    myarea <- sf::st_area(shp)
    if (all(units(myarea)$numerator == c('m', 'm'))) {
      myarea = convert_units(myarea, from = 'sqmeters', towhat = 'sqmi')
      units(myarea)  <- "mi^2"
      sqmi = myarea
    } else {
      sqmi = NA # unsure of units so report pop density as NA
    }
    shp$POP_SQMI <- shp$pop / sqmi
    shp <- shp[ , c('NAME', 'FIPS', 'STATE_ABBR', 'STATE_NAME', 'POP_SQMI', 'geometry',
                    # now adding these 2 columns in this case:
                    'pop', 'pop_moe')]
    cat("Population estimate is from B10001_001 in American Community Survey 5yr survey ending", acsendyear_carto_tiger, " \n")
    
    # fips was input, shp$FIPS is output column but need to make the sort order like input order
    if (any(sort(shp$FIPS) != sort(fips))) {warning("fips codes found in shapefile of boundaries are not all the same as fips requested")}
    shp <- shp[match(fips, shp$FIPS), ]
    
    return(shp)
    
    ################## # ################## # ################## #                 
    
    ## NOTES ON NEWER FASTER WAY TO GET COUNTY BOUNDS FOR MAPPING
    
    ## EJAM:::shapes_counties_from_countyfips 
    ## using the API at services.arcgis.com in a loop of 50 counties at a time 
    ## was far too slow if you need >50 counties like all US counties.
    ##   e.g.,Might be 5-10 minutes for whole USA??? 
    ##   API in loop using batches of 50 took 30 seconds to download just 110 counties, as for system.time({shp = EJAM:::shapes_counties_from_countyfips(cfips) })
    ## so now using tidycensus pkg which relies on tigris pkg
    ## which takes about 4 seconds to get all US counties as approximate boundaries.
    ## and maybe 10-30 seconds to get more accurate TIGER/Line shapefiles the first time before cached.
    # https://walker-data.com/tidycensus/articles/spatial-data.html
    # geometry list-column describing the geometry of each feature, using the geographic coordinate system NAD 1983 (EPSG: 4269) which is the default for Census shapefiles. 
    ## tidycensus uses the Census cartographic boundary shapefiles for faster processing; 
    ## if you prefer the TIGER/Line shapefiles, set cb = FALSE in the function call.
    
    ################## #                 
    #            ## using tidycensus pkg
    # library(tidycensus)
    # library(tidyverse)
    # options(tigris_use_cache = TRUE) # But it seems to use cache anyway?
    # mystates = stateinfo$ST # 50+DC+PR
    # ## checked speeds:
    ## About 1-4 seconds for all counties faster cartographic bounds
    # system.time({
    #   mystates = stateinfo$ST 
    #   shp <- get_acs(
    #     cb = TRUE, ## FASTER DOWNLOAD BUT LESS ACCURATE
    #     state = mystates,
    #     #    county =  substr(fips_counties_from_state_abbrev(mystates), 3,5),
    #     geography = "county",
    #     variables = "B10001_001",
    #     geometry = TRUE,
    #     year = 2022
    #   )
    # })
    # 
    # ## About 14-25 seconds to DOWNLOAD more accurate TIGER/Line shapefiles the 1st time
    # ## (but once cached, just 3 seconds or sometimes up to 7 seconds)
    # system.time({
    #   mystates = stateinfo$ST 
    #   shp_tiger <- get_acs(
    #     cb = FALSE, # more accurate but slower download
    #     state = mystates,
    #     #    county =  substr(fips_counties_from_state_abbrev(mystates), 3,5),
    #     geography = "county",
    #     variables = "B10001_001",
    #     geometry = TRUE,
    #     year = 2022
    #   )
    # })
    ################## # ################## # ################## #         
    
  } else {
    # else:
    if (myservice[1] %in% c("cartographic", "tiger") && !tidycensus_ok) {
      # those were requested but failed due to problem with api key or tidycensus package
      warning(paste0("need tidycensus package and census API key to use myservice = '", myservice[1], "', so using default service instead"))
      myservice <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/2/query"
    } 
    
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
#' @param myservice URL of feature service to get shapes from, 
#'   (or, but not yet implemented, "cartographic" or "tiger" to use approx or slow/accurate bounds from tidycensus and tigris packages).
#' @seealso [shapes_from_fips()]
#' @return spatial object via [sf::st_read()] # sf-data.frame, not sf-tibble like [sf::read_sf()]
#'
#' @keywords internal
#'
shapes_tract_from_tractfips <- function(fips, outFields = c("FIPS", "STATE_ABBR", "SQMI"),
                                        myservice = c("https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/4/query",
                                                      "cartographic", "tigris")[1]) {
  
  if (myservice[1] %in% c("cartographic", "tiger")) {
    
    # see code in shapes_counties_from_countyfips() to possibly add those options
    
    warning("only arcgis service supported here for tracts currently, so using that")
    myservice <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/4/query"
  }
  
  outFields <- paste0(outFields, collapse = ',')
  
  if (is.null(myservice)) {
    myservice <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/4/query"
  }
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
  
  if (length(fips) > 50) {
    
    # The API does let you get >50 at once but instead of figuring out that syntax, this function works well enough
    batchsize <- 50
    batches <- 1 + (length(fips) %/% batchsize)
    # ***  add code here to handle 50 at a time and assemble them
    out <- list()
    for (i in 1:batches) {
      first <- 1 + ((i - 1) * batchsize)
      last <- min(first + batchsize - 1, length(fips))
      out[[i]] <- shapes_tract_from_tractfips(fips[first:last], outFields = outFields, myservice = myservice)
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
                                             "cartographic", "tiger")[1]
) {
  
  if (myservice[1] %in% c("cartographic", "tiger")) {
    
    # see code in shapes_counties_from_countyfips()
    
    warning("only arcgis service supported currently, so using that")
    myservice <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Boundaries_2022/FeatureServer/5/query"
  }
  
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
# For residential population data (optionally pre-joined to tigris geometries), see the tidycensus package.
# NAD 1983 is what the tigris pkg uses -- it only returns feature geometries for US Census data that default to NAD 1983 (EPSG: 4269) coordinate reference system (CRS).
#   For help deciding on appropriate CRS, see the crsuggest package.


## used by name2fips or fips_from_name 
# see https://www2.census.gov/geo/pdfs/reference/GARM/Ch9GARM.pdf

####################################################### #


#' Get shapefiles/ boundaries of census places like cities
#'
#' @param fips vector of 7-digit City/town/CDP codes as in [censusplaces$fips]
#' @param myservice only 'tiger' is implemented as source of boundaries, using the tigris package
#' @seealso [shapes_from_fips()]
#' @return spatial data.frame for mapping
#' 
#' @keywords internal
#'
shapes_places_from_placefips <- function(fips, myservice = 'tiger') {
  
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
  
  # check if census api key available if needed for tiger
  
  if (myservice[1] == 'tiger') {
    shp <- tigris::places(ST)
  } else {
    warning('other sources of boundaries not implemented, so using default')
    shp <- tigris::places(ST)
  }
  shp <- shp[match(fips, shp$GEOID), ] # filter using FIPS is more robust than trying to get exact name right
  shp$FIPS <- shp$GEOID # so all via shapes_from_fips() have a FIPS colname
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




