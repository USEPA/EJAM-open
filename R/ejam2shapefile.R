


#' export EJAM results as geojson/zipped shapefile/kml for use in ArcPro, EJScreen, etc.
#'
#' @param ejamitout output of EJAM such as from [ejamit()]
#' @param file optional filename with no path, with extension one of "geojson"/"json", "shp", "zip", "kml"
#'   (where zip and shp both mean a .zip file that is a zipped set of .shp format files)
#'   Ignored if save=F.
#' @param folder optional - If omitted (and not running in shiny and if interactive() mode),
#'   this function prompts you to specify the folder where the file should be saved.
#'   If omitted and not running in shiny or not interactive() mode, it uses tempdir().
#'   Ignored if save=F.
#' @param save whether to save file - if FALSE, it returns the object not the file path
#' @param crs optional coord ref system
#' @param shortcolnames Whether to cut colnames to 10 characters only if using .shp format
#' @param varnames optional vector of which colnames of ejamitout$results_bysite
#'   to include in shapefile. DJefault is all other than averages, ratios, and raw EJ scores.
#'   Can be "all" or NULL to include all columns.
#' @param shp data.frame that is also "sf" class, with "geometry" column for mapping,
#'   rows exactly corresponding to those in ejamitout$results_bysite
#' @return path to saved file
#' @examples \donttest{
#'   # folder = getwd()
#'   # out <- ejamit(testpoints_100 , radius = 3.1)
#'   # file <- ejam2shapefile(out, file = "test100_3miles.geojson", folder = folder)
#'
#'   out <- testoutput_ejamit_10pts_1miles
#'   file <- ejam2shapefile(out)
#'   shp <- shapefile_from_any(file)
#'   map_shapes_leaflet(shp)
#'   }
#' @details FIELD NAMES (indicator names) CURRENTLY ARE TRUNCATED AND NUMBERED TO BE ONLY 10 CHARACTERS MAX.
#'
#' see
#'   [Shapefile format basics from arcgis.com](https://doc.arcgis.com/en/arcgis-online/reference/shapefiles.htm)
#'
#' @export
#'
ejam2shapefile <- function(ejamitout,
                           file = "EJAM_results_bysite_date_time.geojson",
                           folder = tempdir(), # only used if not specified and in shiny or not interactive
                           save = TRUE,
                           crs = 4269,
                           shortcolnames = TRUE, varnames = "basic250",
                           shp = NULL
) {
  # ,   ...) {

  #  ejamitout <- testoutput_ejamit_10pts_1miles; crs = 4269; file = "bysite.shp" ;  folder =  "~/../Downloads"  # getwd()
  if ('results_bysite' %in% names(ejamitout)) {
    df <- data.table::setDF(ejamitout$results_bysite)
  } else {
    # in case just 1 table was passed to this function
    if (is.data.frame(ejamitout)) {
      df <- data.table::setDF(ejamitout)
    } else {
      stop('ejamitout must be a data.table or at least a data.frame')
    }
  }

  # WHICH COLUMNS? ####
  if (is.null(varnames) || all(is.na(varnames)) || varnames[1] == "all") {
    varnames <- "all"
    # df <- df
  } else {
    if (all(varnames[1] == "basic250")) {
      # because shapefiles have a cap on number of fields in some implementations
      # omits averages, ratios, and raw EJ scores, which are not essential or are not in typical EJScreen outputs
      names_basic250 <- sort(grep("^avg|^state.avg|^ratio|^EJ.D|^state.EJ", names(df), invert = T, value = T))
      ok <- names_basic250 %in% names(df)
      if (any(!ok)) {warning("Some of basic250 varnames not found in ejamitout$results_bysite")}
      if (all(!ok)) {stop("None of basic250 varnames found in ejamitout$results_bysite") }
      df <- df[ , names_basic250[ok]]
      message("Using only basic 250 or so columns -
To include averages, ratios, and raw EJ scores, set varnames = 'all' or NULL.
To include specific columns provides those as a character vector of varnames.")
    } else {

      if ((is.null(shp)) & !('radius.miles' %in% varnames)) {
        varnames = c(varnames, 'radius.miles')
        } # radius will be needed to draw circles, unless shp provided
      ok <- varnames %in% names(df)
      if (any(!ok)) {warning("Some specified varnames not found in ejamitout$results_bysite")}
      if (all(!ok)) {stop("No specified varnames found in ejamitout$results_bysite") }
      df <- df[ , varnames[ok]]
    }
  }

  # URLS ####
  ## shapefile may not support 255 or larger number of characters in a field like the URLs so they get truncated
  ##  should at least alter or maybe just remove the url columns
  urlcols = which(grepl("a href=", names(df)))
  if (length(urlcols) > 0) {
    df[ , urlcols] <- unlinkify(df[ , urlcols])
  }

  if (!is.null(shp)) {
    if (!all(c("sf", "data.frame") %in%  class(shp))) {stop('shp must be class "sf" and "data.frame" ')}
    # shp <- shapefix(shp) # ?? in case problems with columns but should have fixed when imported.
    if (NROW(shp) != NROW(df)) {stop("ejamitout$results_bysite and shp must have exactly the same number of rows, matching each other")}
    bysite_shp <- cbind(shp, df) # or could merge using ejam_uniq_id? but output of ejamit()$results_bysite should already have 1 row for every row of original shapefile input, even invalid ones, so merge typically won't be needed here.
    ## note class(cbind(df,shp)) is just data.frame but class(cbind(shp,df)) is  "sf" and data.frame !
  } else {

 ######################################################################################### #

    # LATLON + Radius (no shp provided) ####
    ## Try to create circles at lat,lon pts ####

    bysite_shp <- shape_buffered_from_shapefile_points(df, radius.miles = NULL, crs = crs)

    ######################################################################################### #

  }

  # SAVE  ####
  if (!save) {
    return(bysite_shp)
  } else {

    ## folder OK? ####
    if (interactive() && !shiny::isRunning()) {
      if (missing(folder)) {
        folder <- rstudioapi::selectDirectory("Select/confirm Folder to Save in", path = folder)
      }
    }
    if (!dir.exists(folder)) {stop("folder does not exist")}
    # folder <- normalizePath(folder) # ?? converts from x/y/z  to  x\\y\\z  on windows.

    ## file and type OK? ####
    ftype <- tools::file_ext(file) # it removes the dot
    if (missing(file)) {
      file <- create_filename(ext = paste0(".", ftype), file_desc = "results_bysite") # e.g.,  "EJAM_results_bysite_20240901_162119.shp"
    }
    if (basename(file) != file) {
      stop("file parameter must not include path, only filename with extension - use folder parameter to specify folder")
    }
    ok.ext <- c("shp", "geojson", "kml", "json", "zip")
    if (!tools::file_ext(file) %in% ok.ext) {
      stop(paste0('file extension must be one of \"', paste0(ok.ext, collapse = "\", \""), '\"'))
    }
    ##################################### #
    
    # is bysite_shp data OK?
    
    if (NROW(bysite_shp) == 0) {stop("no data in shapefile")}
    
    ##################################### #

    ## .geojson, .json, .kml    ####

    if (!(ftype %in% c("shp", "zip"))) {

      finalpath = paste0(normalizePath(folder), "\\", file)
      if (file.exists(finalpath)) {
        warning("File by that name already exists, but will overwrite it.")
        file.remove(finalpath)
      }

      if (ftype %in% c("geojson", "json")) {
        sf::st_write(
          obj = bysite_shp,
          dsn = finalpath,
          driver = "GeoJSON", # "json" is not recognized but this way it works
          delete_layer = TRUE, # delete_layer not supported?
          append = FALSE
        )
      } else {
        sf::st_write(
          obj = bysite_shp,
          dsn = finalpath,
          # driver = "KML", # infers it from extension
          delete_layer = TRUE, # delete_layer not supported?
          append = FALSE
        )
      }

    }
    ##################################### #

    ## .shp  ####

    if (ftype %in% c("shp", "zip")) {
      ### need >=10 character colnames to save as .shp file format. see sf:::abbreviate_shapefile_names etc.
      ### so shortening them but "geometry" must not be changed
      if (shortcolnames) {
        names(bysite_shp)[names(bysite_shp) != "geometry"] <- paste0(
          substr(names(bysite_shp)[names(bysite_shp) != "geometry"] , 1, 7),
          1:length(names(bysite_shp)[names(bysite_shp) != "geometry"]))
        names(bysite_shp) <- tolower(names(bysite_shp))
        ###  but renaming ejam_uniq_id  is not ideal - try to keep it?
        # bysite_shp$ejam_uniq_id <- bysite_shp$ejam_4
      }
      #  Creating a 256th field, but some DBF readers might only support 255 fields

      if (ftype == "zip") {
        zipname <- file
        ftype <- "shp"
        file <- paste0(tools::file_path_sans_ext(file), ".shp")
      } else {
        zipname <- paste0(file, ".zip")
      }
      tds <- file.path(tempdir(), ftype)
      if (!dir.exists(tds)) {dir.create(tds)}
      if (!dir.exists(tds)) {stop('could not create temp directory')}
      if (file.exists(file.path(tds, file))) {
        warning("File by that name already exists, but will overwrite it.")
        file.remove(file.path(tds, file))
      }
      sf::st_write(
        obj = bysite_shp,
        dsn = file.path(tds, file),
        delete_layer = TRUE, # delete_layer not supported?
        append = FALSE
      )
      if (!file.exists(file.path(tds, file))) {stop('could not write to file at ', file.path(tds, file))}

      # now make it a zip file
      zipfullpath <- file.path(normalizePath(folder), zipname)
      # get shp-related filenames in temp dir
      fname_noext <- tools::file_path_sans_ext(file)
      fnames <- dir(tds, pattern = fname_noext)
      fnames <- fnames[!grepl("zip$", fnames)] # but make sure no zip file is among those
      # delete any old version of zip
      if (file.exists(zipfullpath)) {file.remove(zipfullpath)}
      # write zip to folder, using shp files in temp dir
      zip(zipfullpath, files = file.path(tds, fnames), extras = c('-j', '-D'))
      # Note:
      # -D should prevent storing Directory info,
      # -j is supposed to use no path info so files are all in root of .zip and there are not folders inside the .zip
      if (!file.exists(zipfullpath)) {stop('could not create zip file at ', zipfullpath)}
      finalpath <- zipfullpath
    }
    ##################################### #

    return(finalpath)
  }
}
################################################################################### #
