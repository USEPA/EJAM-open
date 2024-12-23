


#' DRAFT - export EJAM results as shapefile/geojson/kml for use in ArcPro, EJScreen, etc.
#'
#' @param ejamitout output of EJAM such as from [ejamit()]
#' @param fname optional filename with no path, with extension one of "geojson"/"json", "shp", "kml"
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
#' @param sf_data.frame data.frame that is also "sf" class, with "geometry" column for mapping,
#'   rows exactly corresponding to those in ejamitout$results_bysite
#' @return path to saved file
#' @examples \dontrun{
#'   # folder = getwd()
#'   # out <- ejamit(testpoints_100 , radius = 3.1)
#'   # fname <- ejam2shapefile(out, "test100_3miles.geojson", folder = folder)
#'   
#'   out <- testoutput_ejamit_10pts_1miles
#'   fname <- ejam2shapefile(out)
#'   shp <- shapefile_from_any(fname)
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
                           fname = "EJAM_results_bysite_date_time.geojson", 
                           folder = tempdir(), # only used if not specified and in shiny or not interactive 
                           save = TRUE, 
                           crs = 4269, 
                           shortcolnames=TRUE, varnames = "basic250",
                           sf_data.frame = NULL
) { 
  # ,   ...) {
  
  #  ejamitout <- testoutput_ejamit_10pts_1miles; crs = 4269; fname = "bysite.shp" ;  folder =  "~/../Downloads"  # getwd()
  
  df <- data.table::setDF(ejamitout$results_bysite)
  
  # WHICH COLUMNS? ####
  if (is.null(varnames) || all(is.na(varnames)) || varnames[1] == "all") {
    varnames <- "all"
    # df <- df
  } else {
    if (all(varnames[1] == "basic250")) {
      # because shapefiles have a cap on number of fields in some implementations
      # omits averages, ratios, and raw EJ scores, which are not essential or are not in typical EJScreen outputs
      names_basic250 <- sort(grep("^avg|^state.avg|^ratio|^EJ.D|^state.EJ", names(df), invert = T, value = T)) 
      ok <- names(df) %in% names_basic250
      if (any(!ok)) {warning("Some specified varnames not found in ejamitout$results_bysite")}
      if (all(!ok)) {stop("No specified varnames found in ejamitout$results_bysite") }
      df <- df[ , ok]
      message("Using only basic 250 or so columns - 
To include averages, ratios, and raw EJ scores, set varnames = 'all' or NULL.
To include specific columns provides those as a character vector of varnames.")
    } else {
      ok <- names(df) %in% varnames
      if (any(!ok)) {warning("Some specified varnames not found in ejamitout$results_bysite")}
      if (all(!ok)) {stop("No specified varnames found in ejamitout$results_bysite") }
      df <- df[ , ok]
    }
  }
  
  # URLS ####
  ## shapefile may not support 255 or larger number of characters in a field like the URLs so they get truncated
  ##  should at least alter or maybe just remove the url columns
  urlcols = which(grepl("a href=", names(df)))
  if (length(urlcols) > 0) {
    df[ , urlcols] <- unlinkify(df[ , urlcols])
  }
  
  if (!is.null(sf_data.frame)) {
    if (!all(c("sf", "data.frame") %in% sapply(sf_data.frame, class))) {stop('sf_data.frame must be class "sf" and "data.frame" ')}
    # sf_data.frame <- shapefix(sf_data.frame) # ?? in case problems with columns but should have fixed when imported.
    if (!NROW(sf_data.frame) == NROW(df)) {stop("ejamitout$results_bysite and sf_data.frame must have exactly the same number of rows, matching each other")}
    df <- cbind(df, sf_data.frame) # check this works
  } else {
    
    # LATLON? no shp provided ####
    
    ## cant map if no shp & no latlon ####
    if (all(is.na(df$lat)) || all(is.na(df$lon))) {
      # *** probably it was analysis of FIPS or Shapefile, not latlon
      cat(
        'Shapefile of results gets mapped in the shiny app, but 
this save function does not work for ejamit analysis of polygons from Shapefile or analysis of FIPS unless 
shapefile provided as sf_data.frame to join it to table of results.
Except, if Counties were analyzed, see  mapfastej_counties() \n')
      # warning(   "latlon at all sites are NA values")
      return(NA)
    } else {
      
      if (any(is.na(df$lat)) || any(is.na(df$lon))) {warning("latlon at some sites are NA values")}
    }
    ## create circles at lat,lon pts ####
    bysite_shp <- shapefile_from_sitepoints(df, crs = crs)
    
    ## just does sf::st_as_sf()
    ## later will want to handle ejamit() outputs where shapefile was analyzed not just circles around points
    # usedpoints <- "sfc_POINT" %in% class(st_geometry(bysite_shp))
    
    ## add circular buffers ####
    radius <- df$radius.miles
    bysite_shp <-  shape_buffered_from_shapefile_points(bysite_shp, radius.miles = radius, crs = crs)
    ## note this removes the columns lat,lon   and  adds a column at the end called geometry
    ## so its columns are not directly comparable to column names of ejamitout$results_bysite
    bysite_shp$lat = df$lat
    bysite_shp$lon = df$lon
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
    
    ## fname and type OK? ####
    ftype <- tools::file_ext(fname) # it removes the dot
    if (missing(fname)) {
      fname <- create_filename(ext = paste0(".", ftype), file_desc = "results_bysite") # e.g.,  "EJAM_results_bysite_20240901_162119.shp"
    }
    if (basename(fname) != fname) {
      stop("fname must not include path, only filename with extension - use folder parameter to specify folder")
    }
    ok.ext <- c("shp", "geojson", "kml", "json")
    if (!tools::file_ext(fname) %in% ok.ext) {
      stop(paste0('fname extension must be one of \"', paste0(ok.ext, collapse = "\", \""), '\"'))
    }
    
    ##################################### # 
    
    ## .geojson, .json, .kml    ####
    
    if (ftype != "shp") {
      
      finalpath = paste0(normalizePath(folder), "\\", fname)
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
    
    if (ftype == "shp") {
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
      
      tds <- file.path(tempdir(), ftype)
      if (!dir.exists(tds)) {dir.create(tds)}
      if (!dir.exists(tds)) {stop('could not create temp directory')}
      if (file.exists(file.path(tds, fname))) {
        warning("File by that name already exists, but will overwrite it.")
        file.remove(file.path(tds, fname))
      }
      sf::st_write(
        obj = bysite_shp,
        dsn = file.path(tds, fname),
        delete_layer = TRUE, # delete_layer not supported?
        append = FALSE
      )
      if (!file.exists(file.path(tds, fname))) {stop('could not write to file at ', file.path(tds, fname))}
      zipname <- paste0(fname, ".zip")
      fname_noext <- gsub( paste0("\\.", tools::file_ext(fname), "$"), "", dir(tds, pattern = fname))  # ?? 
      fnames <- dir(tds, pattern = fname_noext)
      fnames <- fnames[!grepl("zip$", fnames)]
      if (file.exists(zipname)) {file.remove(zipname)}
      zipfullpath <- paste0(normalizePath(folder), "\\", zipname)
      zip(zipfullpath, files = file.path(tds, fnames), extras = c('-j', '-D')) 
      #  
      # -D should prevent storing Directory info, 
      # -j is supposed to use no path info so files are all in root of .zip and there are not folders inside the .zip
      if (!file.exists(zipfullpath)) {stop('could not create zip file at ', zipfullpath)}
      finalpath = zipfullpath
    }
    ##################################### # 
    
    return(finalpath)
  }
}
################################################################################### #
