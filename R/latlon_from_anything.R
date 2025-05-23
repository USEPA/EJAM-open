#################### #
select_valid_file <- function(silentinteractive = FALSE) {

  # helper utility - select a file
  # interactively non in shiny, let user select a file (full path and filename) by browsing to it
  # that is a valid type of file for use as an input to ejamit()
  
  #  see  shapefile_from_any()
  #  c("zip", "gdb", "geojson", "json", "kml", "shp", "shx", "dbf", "prj")
  # *.zip, *.gdb, *.geojson, *.json, *.kml, *.shp, *.shx, *.dbf, *.prj
  if (interactive() && !silentinteractive) {
    if (rstudioapi::isAvailable()) {
      selected_pathfile <- rstudioapi::selectFile(
        caption = "", label = "",
        path = testdata(quiet = TRUE),
        # filter = "", # allow anything to be picked and validate allowed formats later?
        #   or else list all the shp and latlon types allowed like
        filter = paste0("Excel/csv/Shapefiles (*.xlsx, *.xls, *.csv, *.zip, *.gdb, *.geojson, *.json, *.kml, *.shp)"), 
          #  *.shx, *.dbf, *.prj, *.cpg, ## not relevant since selectFile doese not let you select multiple files ? but if filter allows .shp, these others do appear just not .cpg ?
        existing = TRUE
      )
    } else {
      selected_pathfile <- base::file.choose(new = FALSE)
    }
  } else {
    stop("Must specify one of these: sitepoints, fips, or shapefile")
  }
  return(selected_pathfile)
}
#################### #

sitetype_from_filepath <- function(filepath) {

  # helper utility - what kind of file is it? (latlon, fips, shp)
  # try to infer the type of data provided by the file as a valid input to ejamit() parameters sitepoint or fips or shp
  ext <- tools::file_ext(filepath)
  if (ext %in% c("xlsx", "xls", "csv")) {
    # either fips or latlon...

    # must read it to know if fips or latlon!?
    # sitepoints_from_any() using  latlon_from_anything() should be able to obtain lat lon directly or via addresses geocoded

    mytable = read_csv_or_xl(filepath)
    # fips <- fips_from_table()
    seems_like_fips <- FALSE


    if (seems_like_fips) {
      return("fips")
    } else {
      return("latlon")
    }
  } else if (ext %in% c("zip", "gdb", "geojson", "json", "kml", "shp")) {
    # not shx dbf prj cpg
    return("shp")
  } else {
    stop(paste0("File type not recognized: ", ext))
  }
}
#################### #

#################### #
#' helper - given filename, figure out type and return list of input params for ejamit()
#' Do not actually read file but get list of sitepoints, fips, shapefile args to pass to ejamit()
#'
#' @param file a file name (with path) to look at
#'
#' @returns named list, with sitepoints, fips, shapefile as names
#'
#' @keywords internal
#'
sites_from_file <- function(file) {

  selected_pathfile <- file
  # infer type
  filetype <- sitetype_from_filepath(selected_pathfile)

  # sitepoints = NULL # actually should be missing if not applicable
  fips = NULL
  if (filetype == "latlon") {
    sitepoints <- selected_pathfile
    shapefile = NULL
    fips = NULL
  }
  if (filetype == "shp") {
    sitepoints = NULL
    shapefile <- selected_pathfile
    fips = NULL
  }
  if (filetype == "fips") {
    sitepoints = NULL
    shapefile = NULL
    fipstable <- read_csv_or_xl(fname = basename(selected_pathfile), path = dirname(selected_pathfile))
    fips <- fips_from_table(fips_table = fipstable)
  }

  return(list(sitepoints = sitepoints, fips = fips, shapefile = shapefile))

  ## if we had to omit sitepoints instead of it being null
  # if (is.null(sitepoints)) {
  #   return(list(fips = fips, shapefile = shapefile))
  # } else {
  #   return(list(sitepoints = sitepoints, fips = fips, shapefile = shapefile))
  # }
}
#################### #



#' Get lat/lon flexibly - from file, data.frame, data.table, or lat/lon vectors
#'
#' @description Try to figure out if user provided latitude / longitude
#'   as vectors, data.frame, file, or interactively pick file.
#'
#' @details
#' Also see closely related function [sitepoints_from_any()].
#'
#' This function relies on
#'
#'  [read_csv_or_xl()]  and
#'
#'  [latlon_df_clean()]
#'  which in turn uses [latlon_infer()] [latlon_as.numeric()] [latlon_is.valid()]
#'
#'
#'  A draft function [read_and_clean_points()] would a more general way to get points,
#'
#'  but is still work in progress... it is similar to latlon_from_anything()
#'
#'   except it also uses these functions:
#'
#'   [latlon_from_regid()],  [latlon_from_programid()]
#'
#'   and could eventually use  _from_naics() etc.
#'
#'   Even more generally, FIPS and shapefile inputs could be read through a
#'   single wrapper function at some point.
#'
#' @param anything If missing and interactive mode in RStudio, prompts user for file. Otherwise,
#'   this can be a filename (csv or xlsx, with path), or data.frame/ data.table/ matrix,
#'  or vector of longitudes (in which case y must be the latitudes).
#'   File or data.frame/data.table/matrix must have columns called lat and lon, or names that can
#'   be inferred to be that by latlon_infer()
#' @param lon_if_used If anything parameter is a vector of longitudes, lon_if_used must be the latitudes. Ignored otherwise.
#' @param interactiveprompt If TRUE (default) and in interactive mode not running shiny,
#'    will prompt user for file if "anything" is missing.
#' @param invalid_msg_table Set to TRUE to add columns "valid" and "invalid_msg" to output
#' @param set_invalid_to_na used by latlon_df_clean()
#' @seealso [sitepoints_from_any()] which is like this but also adds ejam_uniq_id column,
#'   and see [read_csv_or_xl()] and [latlon_df_clean()]
#' @return A data.frame that has at least columns lon and lat (and others if they were in anything),
#'   and a logical column called "valid"
#' @examples
#'  latlon_from_anything(testpoints_10)
#'  latlon_from_anything(testpoints_10$lat, testpoints_10$lon)
#'  pts = c("33,-100", "32,-101")
#'  latlon_from_anything(pts)
#'  pts = data.frame(Longitude = testpoints_10$lon, Latitude = testpoints_10$lat)
#'  latlon_from_anything(pts)
#'  pts = data.table(Lat = testpoints_10$lat, Long = testpoints_10$lon)
#'  latlon_from_anything(pts)
#'  \donttest{
#'  if (interactive()) {
#'    pts <- latlon_from_anything()
#'  }}
#'  \donttest{
#'  pts = system.file("testdata/latlon/testpoints_10.xlsx", package = "EJAM")
#'  latlon_from_anything(pts)
#'  }
#'
#'
#' @export
#'
latlon_from_anything <- function(anything, lon_if_used, interactiveprompt = TRUE, invalid_msg_table = FALSE, set_invalid_to_na = TRUE) {


  if (missing(anything) || is.null(anything) || all(length(anything) == 0) || all(is.na(anything)) || all("" == anything)) {
    if (interactive() && !shiny::isRunning() && interactiveprompt) {
# could switch to use select_valid_file
      if (!rstudioapi::isAvailable()) {
        x <- file.choose()
        # if somehow the user is interactive like in R console NOT using RStudio
      } else {
        x <- rstudioapi::selectFile("Select a file",
                                    caption = "Select xlsx or csv with lat,lon values",
                                    filter = "excel/csv Files (*.xlsx,*.xls,*.csv)",
                                    path = '.' ) # or could use testdata() as in read_csv_or_xl()
      }
    } else {
      if (shiny::isRunning()) {
        warning("file path/name needed but not provided")
        return(NULL)
      } else {
        stop("file path/name needed in latlon_from_anything(), but not provided")
      }
    }} else {
      x <- anything
    }

  # figure out if x is a filename or data.table or data.frame
  # of lat, lon values, and clean it up for use.
  # otherwise, do the same assuming anything,lon_if_used are lat,lon values as vectors.
  if (data.table::is.data.table(x)) data.table::setDF(x) # syntax is easier here this way. note that a data.table is also a list and data.frame
  if (is.list(x) & !is.data.frame(x)) {x <- as.data.frame(x)} # like if x <- list(lon = 1:5, lat = 1:5)
  if (is.matrix(x) | is.array(x) ) {x <- as.data.frame(x)}

  if (!is.data.frame(x)) { # also if data.table (but not if a matrix or a non-df-list or array or vector)
    if (is.atomic(x) && is.character(x) & length(x) == 1) {
      # seems to be a file name with path, so read it
      if (file.exists(x)) {
        pts <- read_csv_or_xl(x)
      } else {
        if (shiny::isRunning()) {
          warning(paste0(x, ' is not a filepath/name that exists, and otherwise must be a vector of latitudes or a table of points'))
          return(NULL)
        } else {
          stop(paste0(x, ' is not a filepath/name that exists, and otherwise must be a vector of latitudes or a table of points'))
        }
      }
    } else {
      # x aka anything was not a file, and is still not a data.frame, so
      # a) first 2 input params should be lat,lon vectors, or
      # b) x should be a vector of csv pairs and lon_if_used should be missing

      if (missing(lon_if_used)) {
        # either x is a vector of csv pairs or nothing worked.
        if (is.atomic(x) && all(grepl(",", x))) {
          pts <- latlon_from_vectorofcsvpairs(x)
        } else {
          # FAILED
          if (shiny::isRunning()) {
            warning('the first input parameter could not be interpreted as a valid latitude or table of lat,lon or filename')
            return(NULL)
          } else {
            stop('the first input parameter could not be interpreted as a valid latitude or table of lat,lon or filename')
          }
        }

      } else {
        #  anything,lon_if_used should be lat,lon vectors, or nothing worked

        if (is.atomic(x) && is.atomic(lon_if_used) && is.vector(x) && is.vector(lon_if_used)) {
          x <- as.numeric(x)
          lon_if_used <- as.numeric(lon_if_used)
          pts <- data.frame(lat = x, lon = lon_if_used)
        } else {
          # FAILED
          if (shiny::isRunning()) {
            warning('the input parameters could not be interpreted as valid inputs')
            return(NULL)
          } else {
            stop('the input parameters could not be interpreted as valid inputs')
          }
        }
      }
    }
  } else {
    # x is now a data.frame
    pts <- x
  }
# This will try to geocode any street addresses to create lat lon if no latlon found but addresses are found
  pts <- latlon_df_clean(pts, invalid_msg_table = invalid_msg_table, set_invalid_to_na = set_invalid_to_na) # This does latlon_infer() and latlon_as.numeric() and latlon_is.valid()

  return(pts)
}
########################################################### #


#' Get lat/lon flexibly - from file, data.frame, data.table, or lat/lon vectors
#' @inherit latlon_from_anything
#' @return A data.frame that has at least columns lon and lat (and others if they were in x)
#' @export
#' @keywords internal
#'
latlon_any_format <- function(anything, lon_if_used, interactiveprompt = TRUE, invalid_msg_table = FALSE) {
  latlon_from_anything(anything = anything, lon_if_used = lon_if_used, interactiveprompt = interactiveprompt, invalid_msg_table = invalid_msg_table)
}
########################################################### #
