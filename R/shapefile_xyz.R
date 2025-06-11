# "Shapefiles" consist of several files with the same basename that reside in the same directory, only one of them having extension .shp.
#
# sf::read_sf() is an alias for sf::st_read(), with some modified default arguments.
#   read_sf returns an sf-tibble rather than an sf-data.frame.
#   read_sf is quiet by default/does not print information about the data source.

### list of functions here ####
# see source code Outline for list

# shapefile_from_any()
# shapefile_from_json()
# shapefile_from_zip()
# shapefile_from_gdb()
# shapefile_from_gdbzip()
# shapefile_from_folder()
# shapefile_from_filepaths()

# shapefile_filepaths_from_folder()
# shapefile_filepaths_valid()
# shapefile_filepaths_validize()
# shapefile_clean()

# shape_buffered_from_shapefile()
# shape_buffered_from_shapefile_points()
# shapefile_from_sitepoints()

# shapefile2latlon()
# latlon_from_shapefile()

############################################################################################## #


#' Read shapefile from any file or folder (trying to infer the format)
#'
#' @param path path of file(s) that is/are .gdb, .zip, .shp,
#'   .geojson, .json, etc., or folder
#'
#'   - If .zip or folder that has more than one shapefile in it,
#'     cannot be read by this function,
#'     and must be unzipped and handled separately.
#'
#'   - If folder, tries to read with [shapefile_from_folder()]
#'     Folder must contain one each of files with
#'     extensions .shp, .shx, .dbf, and .prj
#'
#'   - If .zip containing a folder, unzips, then tries to read with
#'     [shapefile_from_folder()] or [shapefile_from_gdbzip()] ?
#'
#'   - If .zip containing .gdb, reads with [shapefile_from_gdbzip()]
#'
#'   - If .gdb, reads with [shapefile_from_gdb()]
#'
#'   - If .json or .geojson, reads with [shapefile_from_json()]
#'
#'   - If .kml or .shp, uses [sf::st_read()]
#'
#'   - If vector of .shp, .shx, .dbf, and .prj file names
#'     (that may include paths), reads with [shapefile_from_filepaths()]
#'
#' @param cleanit set to FALSE if you want to skip validation and dropping invalid rows
#' @param crs passed to shapefile_from_filepaths() etc. and
#'    default is crs = 4269 or Geodetic CRS NAD83
#' @param layer optional layer name passed to [sf::st_read()]
#' @param inputname vector of shiny fileInput uploaded filenames
#' @param ... passed to [sf::st_read()]
#'
#' @return a simple feature [sf::sf] class object using [sf::st_read()]
#' @seealso [shapefile_from_folder()]
#'
#' @export
#'
shapefile_from_any <- function(path = NULL, cleanit = TRUE, crs = 4269, layer = NULL, inputname = NULL, ...) {

  # and see app_ui.R text and latlon_from_ and global_defaults_*.R
  oktypes_shp4 <- c("shp", "shx", "dbf", "prj") # ".sbn", ".sbx",".cpg" # others to possibly allow

  # even if use_shapefile_from_any is FALSE (ie using older version of app, without the shapefile_from_any() function), this function should allow all the valid types including the ones not yet allowed in older app
  oktypes_1 <- c("zip", "gdb", "geojson", "json", "kml", "shp") # see global_defaults_*.R
  oktypes <- c(oktypes_1, "shx", "dbf", "prj")
  #oktypes <-  c("zip", "gdb", "geojson", "json", "kml", "shp", "shx", "dbf", "prj")
  oktypes_1_text <- paste0(oktypes_1, collapse = ', ')
  oktypes_text <- paste0(oktypes, collapse = ', ')
  oktypes_text_star_dot <- paste0("*.", paste0(oktypes, collapse = ', *.'))
  caption_text <- paste0("Select a file (", oktypes_text, ")", " [or Cancel to specify a whole folder]")
  filter_text  <- paste0("Shapefiles (", oktypes_text_star_dot, ")")
  # test cases:  see unit tests file

  # if already a sf object, just return it as-is but use cleanit and crs (and layer??)
  if ("sf" %in% class(path)) {
    if (cleanit) {
      path <- shapefile_clean(path, crs = crs)  # includes st_transform(path, crs)
    } else {
      path <- sf::st_transform(path, crs = crs)
    }
    # path = shapefix(path) # also do here?
    return(path) # input param called "path" actually was already a spatial object so just return it
  }

  # if it is already just a regular nonspatial data.frame/data.table, try to convert it to spatial if it has lat/lon columns (but we want polygons not points for ejamit or server)
  if (!is.null(path) && is.data.frame(path)) {
    path <- shapefile_from_sitepoints(path)
    if (inherits(path, "try-error")) {
      if (shiny::isRunning()) {
        warning("cannot interpret data.frame as valid table of lat,lon values")
        return(NULL)
      } else {
        stop("cannot interpret data.frame as valid table of lat,lon values")
      }
    } else {
      return(path)
    }
  }

  # if path invalid/not provided, ask RStudio user to specify a file or folder
  if (any(is.null(path)) || any(is.na(path)) || any(length(path)) == 0 || any(!is.character(path)) || !is.atomic(path)) {
    if (interactive() && !shiny::isRunning()) {

      # This lets RStudio user point to file OR folder   # gdb does not quite work in the filter since it is a folder not file really
      path <- rstudioapi::selectFile(caption = caption_text,
                                     filter = filter_text,
                                     path = getwd(), existing = TRUE)
      if (is.null(path)) {
        path <- rstudioapi::selectDirectory(caption = "Select Folder", path = getwd())
      }
      if (any(is.null(path)) || any(is.na(path)) || any(length(path)) == 0 || any(!is.character(path)) || !is.atomic(path)) {
        # if they clicked Cancel or something else went wrong
        if (shiny::isRunning()) {
          warning("need to specify valid path")
          return(NULL)
        } else {
          stop("need to specify valid path")
        }
      }

    } else {
      if (shiny::isRunning()) {
        warning("need to specify valid path") #
        return(NULL)
      } else {
        stop("need to specify valid path")
      } #
    }
  }

  if (length(path) == 1) {
    x <- NULL
    if (file.exists(path)) {

      if (!(tolower(tools::file_ext(path)) %in% oktypes_1)) {
        if (!dir.exists(path)) {
          warning(paste0("If single path provided, it should be one of these: ", oktypes_1_text, "; or a folder"))
        }} # but maybe st_read() will figure it out anyway?

      # true if like testdata/shapes/portland.gdb
      if (tolower(tools::file_ext(path)) == "gdb") {
        x = (shapefile_from_gdb(path, ...))                    # DOES NOT ALLOW FOR USING cleanit or crs here so far ***
      }
      if (tolower(tools::file_ext(path)) == "zip") {
        x = (shapefile_from_zip(path, cleanit = cleanit, crs = crs, ...))
      }
      if (tolower(tools::file_ext(path)) %in% c("json", "geojson")) {
        x = (shapefile_from_json(path, cleanit = cleanit, crs = crs, ...))
      }
      if (tolower(tools::file_ext(path)) %in% c("kml", "shp")) {
        x = (sf::st_read(path, ...))
      }
      if (is.null(x)) {
        x = try(shapefile_from_folder(folder = path, cleanit = cleanit, crs = crs, ...), silent = TRUE)
      }
      if (is.null(x) || inherits(x, "try-error")) {
        # try one more option
        # st_read() will guess at format from file extension, like .shp, etc.  see https://r-spatial.github.io/sf/articles/sf2.html
        x <- try(sf::st_read(path, layer = layer, ...), silent = TRUE)
        if (is.null(x) || inherits(x, "try-error")) {
          warning(paste0("Cannot read file using sf::st_read()"))
          return(NULL)
        }
      }

    } else {
      # !file.exists(path)  Not sure this case can work
      # st_read() will guess at format from file extension, like .shp, etc.  see https://r-spatial.github.io/sf/articles/sf2.html
      x <- try(sf::st_read(path, layer = layer, ...), silent = TRUE)
    }
    if (is.null(x) || inherits(x, "try-error")) {
      warning(paste0("Cannot read file. If single path provided, it should be one of these: ", oktypes_1_text, "; or a folder"))
      return(NULL)
    }

  } else {
    filepaths <- path
    x <- try(shapefile_from_filepaths(filepaths, cleanit = cleanit, crs = crs, inputname = inputname, ...), silent = TRUE)
    if (is.null(x) || inherits(x, "try-error")) {
      warning("Cannot read file(s).")
      return(NULL)
    }
  }
  if (is.null(x)) {
    return(NULL)
  } else {
    return(
      shapefix(x)
    )
  }
}
############################################################################################## #


#' read .json or .geojson shapefile data
#'
#' @param path path and filename
#' @param cleanit optional, whether to use [shapefile_clean()]
#' @param crs passed to [shapefile_from_filepaths()] etc. and
#'    default is crs = 4269 or Geodetic CRS NAD83
#' @param layer optional layer name passed to [sf::st_read()]
#' @param ... passed to [sf::st_read()]
#'
#' @return like output of [sf::st_read()]
#'
#' @export
#'
shapefile_from_json <- function(path, cleanit = TRUE, crs = 4269, layer = NULL, ...) {

  if (missing(layer) || any(is.null(layer))) {
    shp <-  sf::st_read(path, ...) # it sees .geojson extension and knows it is GeoJSON
  } else {
    shp <-  sf::st_read(path, layer = layer, ...) # it sees .geojson extension and knows it is GeoJSON
  }
  sf::st_crs(shp) <- crs
  if (cleanit) {shp <- shapefile_clean(shp)}

  return(shp)
}
############################################################################################## #


#' read zipped .zip that may contain a geodatabase file or .shp file etc.
#'
#' @param path path and filename for .zip file
#' @param cleanit optional, whether to use [shapefile_clean()]
#' @param crs passed to [shapefile_from_filepaths()] etc. and
#'    default is crs = 4269 or Geodetic CRS NAD83
#' @param layer optional layer name passed to [sf::st_read()]
#' @param ... passed to [sf::st_read()]
#'
#' @return like output of [sf::st_read()]
#'
#' @export
#'
shapefile_from_zip <- function(path, cleanit = TRUE, crs = 4269, layer = NULL, ...) {

  # look at contents of zip
  # - if gdb, use x <- shapefile_from_gdbzip()
  # - if .shp etc. files, unzip files into tempfolder, then use x <- shapefile_from_folder(tempfolder)
  # - if folder, unzip foldername into tempfolder then use x <- shapefile_from_folder(foldername)


  td <- tempdir()
  gname <- unzip(path, list = TRUE)
  gname <- gname$Name

  ################# #
  if (all("gdb" == tolower(tools::file_ext(dirname(gname))))) {
    # looks like zip had a .gdb in it
    x <- suppressWarnings(   try(shapefile_from_gdbzip(path, ...), silent = TRUE)  )
    if (!inherits(x, "try-error")) {
      cat(path, "appears to be .zip containing .gdb \n")
      return(x)
    } else {
      warning("looked like .gdb was in .zip but failed to read using shapefile_from_gdbzip()")
      return(NULL)
    }
  }
  ################# #

  if (all(dirname(gname) == ".")) {
    # .zip contains just filenames (no folder)
    cat(path, "appears to be .zip containing files \n")
    unzip(path, exdir = (td <- file.path(tempdir(), "tempsubdir") ) )

    # may want to check file types here? what if zip had a json?

    shp <- shapefile_from_filepaths(
      filepaths = shapefile_filepaths_from_folder(td), cleanit = cleanit, crs = crs, ...)

    #remove files from temp directory to ensure processing is occurring on current files
    f <- list.files(file.path(file.path(tempdir(), "tempsubdir") ), include.dirs = F, full.names = T, recursive = T)
    file.remove(f)

    return(shp)

  } else {
    # zip contains folder(s)

    gname <- unique(dirname(gname))
    if (length(gname) > 1) {
      warning("zip contains more than one folder, returning NULL")
      return(NULL)
    }

    cat(path, "appears to be .zip containing a folder \n")
    unzip(path, exdir = (td <- file.path(tempdir(), "tempsubdir") ) )

    shp <- shapefile_from_folder(file.path(td, gname), cleanit = cleanit, crs = crs, ...)

    #remove files from temp directory to ensure processing is occurring on current files
    f <- list.files(file.path(file.path(tempdir(), "tempsubdir") ), include.dirs = F, full.names = T, recursive = T)
    file.remove(f)

    # cat("not able to determine format\n")
    # shp <- shapefile_from_json(shp, cleanit = cleanit, crs = crs, layer = layer, ...) # should work for any format that is like a shapefile
  }
  return(shp)
}
############################################################################################## #


#' read .gdb geodatabase file via sf::st_read()
#'
#' @param fname path and filename of .gdb file
#' @param layer optional name of layer, see [sf::st_read()]
#' @param ... passed to [sf::st_read()]
#'
#' @return like output of [sf::st_read()] but with ejam_uniq_id column 1:NROW()
#' @examples
#'   # npl <- sf::st_read("~/../Desktop/NPL/NPL_Boundaries.gdb")
#'   # npl <- shapefile_from_gdb("~/../Desktop/NPL/NPL_Boundaries.gdb",
#'   #   layer = "SITE_BOUNDARIES_SF")
#'   # npl <- shapefile_from_gdbzip("~/../Desktop/NPL/NPL_Boundaries.zip")
#'   # mapview::mapview(npl[x$STATE_CODE == "CA", ])
#'
#' @export
#'
shapefile_from_gdb <- function(fname, layer = NULL, ...) {

  if (missing(fname)) {
    message('fname not specified so looking in current folder')
    fname <- dir(pattern = "*.gdb")
  }
  if (length(fname) != 1)  {
    stop("fname must be length 1")
  }
  if (!file.exists(fname)) {
    stop(fname, " not found")
  }
  if (tolower(tools::file_ext(fname)) != "gdb") {
    stop("fname must have extension .gdb")
  }

  if (!is.null(layer)) {
    shp <- sf::st_read(fname, layer = layer, ...)
  } else {
    # shp <- sf::st_read(fname)
    ## st_read() itself handles warnings when no layer specified.
    lrz <- sf::st_layers(fname)
    if (length(lrz$name) > 1) {
      print(lrz)

      if (interactive() & !shiny::isRunning()) {
        # ask which layer
        layer <- rstudioapi::showPrompt("Layer selection", "Which layer?", default = lrz$name[1])
        if (!(layer %in% lrz$name)) {
          warning("layer ", layer, " not found - reading the first layer")
          layer = lrz$name[1]
        }
      } else {
        warning("layer ", layer, " not found - reading the first layer")
        layer = lrz$name[1]
      }
    } else {
      layer = lrz$name[1]
    }
    shp <- sf::st_read(fname, layer = layer, ...)
  }

  return(
    dplyr::mutate(shp, ejam_uniq_id = dplyr::row_number()) # number them 1:N
  )
}
############################################################################################## #


#' read .zip that contains geodatabase file via unzip and st_read
#'
#' @param fname path to .zip file that contains a .gdb file
#' @param layer optional name of layer, see [sf::st_read()]
#' @param ... passed to [sf::st_read()]
#'
#' @return see [shapefile_from_gdb()]
#'
#' @export
#'
shapefile_from_gdbzip <- function(fname, layer = NULL, ...) {

  if (missing(fname)) {

    # could interactively allow one to point to a .zip file instead of just looking in working directory?

    message('fname not specified so looking in current folder')
    fname <- dir(pattern = "*.zip")
  }
  if (length(fname) != 1)  {
    stop("fname must be length 1")
  }
  if (!file.exists(fname)) {
    stop(fname, " not found")
  }
  if (tolower(tools::file_ext(fname)) != "zip") {
    stop("fname must have extension .zip")
  }
  td <- tempdir()
  gname <- unzip(fname, list = TRUE)
  gname <- gname$Name
  gname <- unique(dirname(gname))
  if (length(gname) != 1) {
    stop("zip file does not seem to have a .gdb file in it")
  }
  unzip(fname, exdir = td)
  shp <- shapefile_from_gdb(file.path(td, gname), layer = layer, ...)
  return(shp)
}
############################################################################################## #


#' Read shapefile from a folder
#'
#' @param folder path of folder that contains the files (.shp, .shx, .dbf, and .prj)
#' @param cleanit set to FALSE if you want to skip validation and dropping invalid rows
#' @param crs passed to shapefile_from_filepaths() default is crs = 4269 or Geodetic CRS NAD83
#' @param ... passed to [sf::st_read()]
#'
#' @return a shapefile object using sf::st_read()
#'
#' @examples \donttest{
#'   testfolder <- system.file("testdata/shapes/Portland_neighborhoods", package = "EJAM")
#'   testshape <- shapefile_from_folder(testfolder)
#'
#'   testpaths <- shapefile_filepaths_from_folder(testfolder)
#'   testshape <- shapefile_from_filepaths(testpaths)
#'
#'   ## if interactive(), R user can point to right folder or select the right set of files:
#'   # testshape <- shapefile_from_filepaths()
#'   # testshape <- shapefile_from_folder()
#'
#'   x <- get_blockpoints_in_shape(testshape)
#'   leaflet::leaflet(x$polys) %>% leaflet::addTiles() %>% leaflet::addPolygons(color = "blue")
#'   DT::datatable(out$results_bysite)
#'
#'   }
#' @seealso [shapefile_from_folder()]
#'
#' @export
#'
shapefile_from_folder <- function(folder = NULL, cleanit = TRUE, crs = 4269, ...) {

  if (is.null(folder)) {
    if (interactive() && !shiny::isRunning()) {
      folder <- rstudioapi::selectDirectory(caption = "Select a folder that contains the files (.shp, .shx, .dbf, and .prj)", path = getwd())
      # and cpg is ok but not essential?
    } else {
      if (shiny::isRunning()) {
        warning("need to specify folder where shapefiles are") #
        return(NULL)
      } else {
        stop("need to specify folder where shapefiles are")
      } #
    }
  }

  # *** might want to change it to be flexible and examine what is in the folder instead of requiring it be .shp etc.

  shapefile_from_filepaths(filepaths = shapefile_filepaths_from_folder(folder), cleanit = cleanit, crs = crs, ...)
}
############################################################################################## #


#' Read shapefile from disk based on the filenames given
#'
#' @param filepaths vector of full paths with filenames (types .shp, .shx, .dbf, and .prj) as strings
#' @param cleanit set to FALSE if you want to skip validation and dropping invalid rows
#' @param crs if cleanit = TRUE, crs is passed to shapefile_clean()
#'   default is crs = 4269 or Geodetic CRS NAD83
#'    Also can check this via x <- sf::st_crs(sf::st_read()); x$input
#' @param layer optional name of layer to read
#' @param inputname vector of shiny fileInput uploaded filenames
#' @param ... passed to [sf::st_read()]
#'
#' @return a shapefile object using [sf::st_read()]
#' @seealso [shapefile_from_folder()]
#'
#' @export
#'
shapefile_from_filepaths <- function(filepaths = NULL, cleanit = TRUE, crs = 4269, layer = NULL, inputname = NULL, ...) {

  if (is.null(filepaths)) {
    if (interactive() && !shiny::isRunning()) {
      filepaths <- rstudioapi::selectFile("Select .shp or .dbf file", filter = "Shapefiles (*.shp;*.dbf)", path = getwd())
      filepaths <- shapefile_filepaths_validize(filepaths)
      # based on the one actual file specified, returns full set of what valid names would be even if they are not all in that folder
      # *** do we want to confirm all the essential files are there? file.
    } else {
      if (shiny::isRunning()) {
        warning("need vector of full paths and filenames that must include all these extensions .shp, .shx, .dbf, and .prj ")
        # and cpg is ok but not essential?
        return(NULL)
      } else {
        stop("need vector of full paths and filenames that must include all these extensions .shp, .shx, .dbf, and .prj ")
      } # and cpg is ok but not essential?
    }
  } else {
    filepaths <- shapefile_filepaths_validize(filepaths, inputname = inputname)
    # based on the one actual file specified, returns full set of what valid names would be even if they are not all in that folder
  }

  if (shapefile_filepaths_valid(filepaths = filepaths)) {

    if (!all(file.exists(filepaths))) {
      warning("not all of these files were found: ", paste0(filepaths, collapse = ", "))
    }

    if (cleanit) {
      shpfilepath <- filepaths[grepl(".*shp$", filepaths, ignore.case = TRUE)]  # one (not more) files that end in .shp
      if (length(shpfilepath) > 1) {warning("using only ", shpfilepath[1], ", the first of more than one .shp file found"); shpfilepath <- shpfilepath[1] }
      # note this will add  ejam_uniq_id =  row_number()

      ## check for all layer names in .shp
      layer_names <- sf::st_layers(shpfilepath)$name
      if (is.null(layer)) {
        ## use first layer if multiple
        if (length(layer_names) > 1) {
          warning(paste0('More than 1 layer found; will use the first layer, named "',layer[1],'"'))
          layer <- layer_names[1]
        } else {
          layer <- layer_names
        }
      } else {
        ## exit if user-entered layer is not valid
        if (!(layer %in% layer_names)) {
          warning(paste0('No layer named "',layer, '" was found'))
          return(NULL)
        }
      }

      return(
        shapefile_clean(
          sf::st_read(shpfilepath, layer = layer, ...), # , crs = crs  should be left out here ?
          crs = crs
        )
      )

    } else {
      # for shiny, do cleaning/check in server so it can offer messages
      shpfilepath <- filepaths[grepl(".*shp$", filepaths, ignore.case = TRUE)] # one or more files that end in .shp
      ## check for all layer names in .shp
      layer_names <- sf::st_layers(shpfilepath)$name
      if (is.null(layer)) {
        ## use first layer if multiple
        if (length(layer_names) > 1) {
          warning(paste0('More than 1 layer found; will use the first layer, named "',layer[1],'"'))
          layer <- layer_names[1]
        } else {
          layer <- layer_names
        }
      } else {
        ## exit if user-entered layer is not valid
        if (!(layer %in% layer_names)) {
          warning(paste0('No layer named "', layer, '" was found'))
          return(NULL)
        }
      }
      shp <- sf::st_read(shpfilepath, layer = layer, ...)  # , crs = crs  should be left out here ?
      return(
        dplyr::mutate(shp, ejam_uniq_id = dplyr::row_number()) # number them
      )
    }
  } else {
    return(NULL) # validation did the warning
  }
}
############################################################################################## #


#' Get list of valid filenames comprising shapefile including paths
#'
#' @param folder path of folder that contains the files (.shp, .shx, .dbf, and .prj)
#'
#' @return string vector of filenames including full paths
#' @seealso [shapefile_from_folder()]
#'
#' @export
#'
shapefile_filepaths_from_folder <- function(folder = NULL) {

  if (is.null(folder)) {
    if (interactive() && !shiny::isRunning()) {
      folder <- rstudioapi::selectDirectory(caption = "Select a folder that contains the files (.shp, .shx, .dbf, and .prj)", path = getwd())
      # and cpg is ok but not essential?
    } else {
      if (shiny::isRunning()) {
        warning("need to specify folder where shapefiles are") #
        return(NULL)
      } else {
        stop("need to specify folder where shapefiles are")
      } #
    }
  }
  list.files(path = folder,
             full.names = TRUE,
             pattern = ".*(dbf|prj|shp|shx|cpg)$",   # with cpg
             ignore.case = TRUE, include.dirs = FALSE, recursive = FALSE)
}
############################################################################################## #


#' Confirm files have ALL the extensions .shp, .shx, .dbf, and .prj
#'
#' @param filepaths vector of full paths with filenames (types .shp, .shx, .dbf, and .prj) as strings
#'
#' @return logical, indicating if all 4 extensions are found among the filepaths
#' @seealso [shapefile_filepaths_validize()] [shapefile_from_folder()]
#'
#' @export
#'
shapefile_filepaths_valid <- function(filepaths) {

  infile_ext <- tools::file_ext(filepaths)
  # does not need .cpg ?
  ok <- all(c('shp','shx','dbf','prj') %in% tolower(infile_ext)) # note it ignores case here now
  if (ok) {
    return(TRUE)
  } else {
    warning("need vector of full paths and filenames that must include all these extensions .shp, .shx, .dbf, and .prj ")
    # and cpg is ok but not essential?
    return(FALSE)
  }
}
############################################################################################## #


#' Convert filepath(s) into one complete set (if possible) of a single basename and extensions .shp, .shx, .dbf, .prj
#'
#' @param filepaths vector of full path(s) with filename(s) as strings
#' @param inputname vector of shiny fileInput uploaded filenames
#'
#' @return assuming only 1 base filename was provided
#'   (among the files with extensions .shp, .shx, .dbf, .prj)
#'   and it had at least one of the
#'   4 valid extensions (.shp, .shx, .dbf, and .prj),
#'   returns a vector of exactly four filepaths, one with each extension.
#'   But returns NULL if more than one base name was provided (since ambiguous),
#'   or none of 4 extensions was provided.
#'   Ignores and drops files with other extensions.
#' @seealso [shapefile_filepaths_valid()] [shapefile_from_folder()]
#'
#' @export
#'
shapefile_filepaths_validize <- function(filepaths, inputname = NULL) {

  mydir <- unique(dirname(filepaths))[1] # should only be one directory, but just in case do this
  keepfiles <- tools::file_ext(filepaths) %in% c('shp','shx','dbf','prj')
   filepaths <- filepaths[keepfiles]

  if(!is.null(inputname)){
    outfiles <- file.path(mydir, inputname[keepfiles])

    # rename files from ugly tempfilename to original filename of file selected by user to upload
    purrr::walk2(filepaths, outfiles, ~file.rename(.x, .y))
    filepaths <- outfiles

  }
  uniquebasenames <- unique(basename(tools::file_path_sans_ext(filepaths)))

  if(length(uniquebasenames) == 0){
    warning('No basename found -- returning NULL')
    return(NULL)
  } else if (length(uniquebasenames) > 1) {
    warning("More than one filename (excluding extensions) was found -- returning NULL")
    return(NULL)
  }
  infile_ext <- tools::file_ext(filepaths)
  if (any(tolower(infile_ext) %in% c('shp','shx','dbf','prj'))) {
    filepaths <- file.path(mydir, paste0(tools::file_path_sans_ext(uniquebasenames), c('.shp','.shx','.dbf','.prj')))
    return(filepaths)
  } else {
    warning("Need path/filenames with at least one of these extensions: .shp, .shx, .dbf, and .prj -- returning NULL")
    # and cpg is ok but not essential?
    return(NULL)
  }
}
############################################################################################## #


#' Drop invalid rows, warn if all invalid, add unique ID, transform (CRS)
#'
#' @param shp a shapefile object using sf::st_read()
#' @param crs used in shp <- sf::st_transform(shp, crs = crs), default is crs = 4269 or Geodetic CRS NAD83
#'
#' @return like input shp, but applying crs and dropping if not valid,
#'   plus column ejam_uniq_id 1:NROW()
#' @seealso [shapefile_from_folder()]
#'
#' @export
#'
shapefile_clean <- function(shp, crs = 4269) {

  # add error checking ***

  if (nrow(shp) > 0) {
    if ("ejam_uniq_id" %in% names(shp)) {warning("ejam_uniq_id columns was already in shp, but replacing it now!")}
    shp <- dplyr::mutate(shp, ejam_uniq_id = dplyr::row_number()) # number them before dropping invalid ones,
    #   so that original list can be mapped to results list more easily

    shp <- shp[sf::st_is_valid(shp), ]          # determines valid shapes, to use those and drop the others
    ## or...  ## *** shouldnt it also drop st_is_empty ones as in ejamit() ?
    #  shp <- shp[sf::st_is_valid(shp) & !sf::st_is_empty(shp), ]

    shp <- sf::st_transform(shp, crs = crs)  # NEED TO DOCUMENT THE ASSUMPTION IT USES THIS CRS ***

  } else {

    warning('No shapes found in file uploaded.')
    shp <- NULL
  }
  return(shp)
}
############################################################################################## #


#' shape_buffered_from_shapefile - add buffer around shape
#' @details Just a wrapper for [sf::st_buffer()]
#'
#' @param shapefile spatial object like areas at high risk or areas with facilities to be analyzed
#' @param radius.miles width of buffer to add to shapefile
#'   (in case dist is a units object, it should be
#'   convertible to arc_degree if x has geographic coordinates,
#'   and to st_crs(x)$units otherwise)
#' @param crs used in st_transform()  default is crs = 4269 or Geodetic CRS NAD83
#' @param ... passed to st_buffer()
#' @return same format as [sf::st_buffer()] returns
#' @seealso [get_blockpoints_in_shape()] [shapefile_from_sitepoints()] [shape_buffered_from_shapefile_points()]
#' @examples
#' # Within 3 miles of the county borders
#' fips_counties_from_state_abbrev("DE")[1]
#' x = shapes_counties_from_countyfips("10001")
#' xtra = shape_buffered_from_shapefile(x, radius.miles = 3)
#' map_shapes_leaflet(x) %>%
#'   map_shapes_leaflet_proxy(xtra, color = "black")
#'
# (ignoring projectiong/datum for this example)
#'
#' @export
#'
shape_buffered_from_shapefile <- function(shapefile, radius.miles, crs = 4269, ...) {

  # add error checking ***

  return(sf::st_buffer(shapefile %>%  sf::st_transform(crs = crs), #
                       dist = units::set_units(radius.miles, "mi"), ...))
}
############################################################################################## #


#' shape_buffered_from_shapefile_points - add buffer around shape (points, here)
#' @details Just a wrapper for [sf::st_buffer()]
#'
#' @param shapefile_points spatial object like areas at high risk or areas with facilities to be analyzed
#' @param radius.miles width of buffer to add to shapefile_points
#'   (in case dist is a units object, it should be
#'   convertible to arc_degree if x has geographic coordinates,
#'   and to st_crs(x)$units otherwise)
#' @param crs used in st_transform()  default is crs = 4269 or Geodetic CRS NAD83
#' @param ... passed to st_buffer()
#' @return same format as [sf::st_buffer()] returns
#' @seealso [get_blockpoints_in_shape()] [shapefile_from_sitepoints()] [shape_buffered_from_shapefile_points()]
#' @examples
#' map_shapes_leaflet(
#'   shape_buffered_from_shapefile_points(
#'     shapefile_from_sitepoints(testpoints_100),
#'     radius.miles = 3
#'   )
#' )
#' # (ignoring projections for this example)
#' # compare to
#' mapfast(testpoints_100)
#'
#' @export
#'
shape_buffered_from_shapefile_points <- function(shapefile_points, radius.miles = NULL, crs = 4269, ...) {

  ################# #
  # check latlon

  checklatlon <- function(df) {
    ## warn cannot map if no latlon or shp
    if (all(is.na(df$lat)) || all(is.na(df$lon))) {
      # *** probably it was analysis of FIPS or Shapefile, not latlon
      cat(
        'Shapefile of results gets mapped in the shiny app, but
this save function does not work for ejamit analysis of polygons from Shapefile or analysis of FIPS unless
shapefile provided as shp to join it to table of results.
Except, if Counties were analyzed, see  mapfastej_counties() \n')
      # warning(   "latlon at all sites are NA values")
      return(NA)
    }
    if (any(is.na(df$lat)) || any(is.na(df$lon))) {warning("latlon at some sites are NA values")}
  }

  x <- checklatlon(shapefile_points)
  if (length(x) == 1 && is.na(x)) {
    # warns if any NA, exits/returns NA if all lat or all lon are NA
    return(NA)
  }
  ################### #
  ## get radius.miles

  if (is.null(radius.miles) || radius.miles == 0) {
    if ('radius.miles' %in% names(shapefile_points)) {
      radius.miles <- shapefile_points$radius.miles
    } else {
      warning('radius.miles is missing/NULL/0, so a zero width buffer will be added to create a shapefile of circles around points')
      radius.miles <- 0
    }
  }
  ################### #
  # convert format to spatial

  ## just does sf::st_as_sf()
  ## later will want to handle ejamit() outputs where shapefile was analyzed not just circles around points
  # usedpoints <- "sfc_POINT" %in% class(st_geometry(bysite_shp))

  if (!("sf" %in% class(shapefile_points)) ) {
    message("input was not a simple feature object, but will convert to one")
    shapefile_points <- try({
      shapefile_from_sitepoints(shapefile_points, crs = crs)
    }, silent = TRUE)
    if (inherits(shapefile_points, "try-error")) {
      stop("could not convert to simple feature object")
    }
  }
  ################### #
  ## add (circular) buffers

  shapefile_points_out <- shape_buffered_from_shapefile(
    shapefile = shapefile_points,
    radius.miles = radius.miles,
    crs = crs,
    ... = ...)

  # ## *** see if buffering etc removes the columns lat,lon   and  adds a column at the end called geometry
  # ## so its columns are not directly comparable to column names of ejamitout$results_bysite
  # ##    note these do get added by shapefile_from_sitepoints() above ***
  #
  # shapefile_points_out$lat <- shapefile_points$lat
  # shapefile_points_out$lon <- shapefile_points$lon

  return(shapefile_points_out)
}
############################################################################################## #


#' Convert table of lat,lon points/sites into sf:: shapefile
#'
#' Creates a simple feature (sf) dataframe from points
#' @param sitepoints a data.table or data.frame with columns called lat,lon (or aliases of those)
#' @param crs used in st_as_sf() default is crs = 4269 or Geodetic CRS NAD83
#' @param ... passed to [sf::st_as_sf()]
#'
#' @return A shapefile via [sf::st_as_sf()].
#'  Note other columns get returned,
#'  and the lat,lon columns do get returned but as "lat" and "lon" even if they were provided as aliases of those
#' @seealso [get_blockpoints_in_shape()] [shapefile_from_sitepoints()] [shape_buffered_from_shapefile_points()]
#'
#' @export
#'
shapefile_from_sitepoints <- function(sitepoints, crs = 4269, ...) {

  stopifnot(is.data.frame(sitepoints))
  sitepoints <- try(latlon_any_format(sitepoints), silent = TRUE) # infers lat,lon colnames from aliases, e.g., latlon_any_format(data.table(latitude = testpoints_10$lat, longitude = testpoints_10$lon))
  if (inherits(sitepoints, "try-error")) {
    stop("cannot interpret as lat,lon points")
  }
  # note other columns get returned, but the lat,lon columns do not get returned but get turned into the geometry column
  shpcoord <- sf::st_as_sf(sitepoints, coords = c('lon', 'lat'), crs = crs, ...) #   want 4269
  shpcoord <- cbind(shpcoord, sitepoints[, c("lat", "lon")])
  return(shpcoord)
}
############################################################################################## #


#' Convert shapefile (class sf) of points to data.table of lat, lon columns
#'
#' Makes lat and lon columns, from a sfc_POINT class geometry field,
#'   via [sf::st_coordinates()]
#' @param shp shapefile that is class sf, as from [shapefile_from_any()]
#'   or [sf::st_read()], with geometry column that has points
#'   so is class sfc_POINT
#' @param include_only_latlon set to FALSE to have function return
#'   lat lon columns plus all of columns in shp. If TRUE, just returns
#'   lat lon columns.
#' @return data.table with columns named lat and lon,
#'   and optionally all from shp as well,
#'   as can be used as input to [ejamit()], [mapfast()], etc.
#' @aliases latlon_from_shapefile
#' @export
#'
shapefile2latlon <- function(shp, include_only_latlon = TRUE) {

  if (!("sf" %in% class(shp)) ||
      !("geometry" %in% names(shp)) ||
      !("sfc_POINT" %in% class(shp$geometry))) {
    stop("shp must be class sf, and shp$geometry must be class sfc_POINT")
  }
  pts <- data.table::data.table(sf::st_coordinates(shp))
  data.table::setnames(pts, old = "X", new = "lat")
  data.table::setnames(pts, old = "Y", new = "lon")
  if (include_only_latlon) {
    # done
  } else {
    if ("lat" %in% names(shp) || "lon" %in% names(shp)) {
      warning("replacing lat and or lon columns with values from geometry field")
      if ("lat" %in% names(shp)) {names(shp) <- gsub("lat", "lat_original", names(shp))}
      if ("lon" %in% names(shp)) {names(shp) <- gsub("lon", "lon_original", names(shp))}
    }
    pts <- data.table::data.table(pts, shp)
  }
  message("note that no changes were made to coordinate reference system - input CRS is ", sf::st_crs(pts)$input)
  print(sf::st_crs(pts))
  return(pts)
}
############################################################################################## #


#' @export
#'
latlon_from_shapefile <- function(shp, include_only_latlon = TRUE) {
  shapefile2latlon(shp, include_only_latlon)
}
############################################################################################## #
