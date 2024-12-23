
###################################################################### #
if (1 == 0) {
  
  # to simplify calling this function from a NON-shiny context, to avoid saying shapefix(shp)[['shp']]
  # Check shiny::!is_running()  and if so, set attributes to shp returned
  # so the attributes provide these values like num_valid_pts_uploaded_SHP
  # or it could be a module
  
  
  ## added this to  shapefile_from_any() (but not each of the helper functions it uses for each file type)
  
  shp <- shapefix(shp)
  
  
  ##  add this to shiny app_server.R, replacing similar code that was being used there:
  ## or if server uses shapefile_from_any() then this has already been done
  
  shp <- shapefix(shp)
  if (!is.null(attr(shp, "disable_buttons_SHP"))) {disable_buttons[['SHP']] <- attr(shp, "disable_buttons_SHP")}
  if (!is.null(attr(shp, "num_valid_pts_uploaded_SHP"))) {num_valid_pts_uploaded[['SHP']] <- attr(shp, "num_valid_pts_uploaded_SHP")}
  if (!is.null(attr(shp, "invalid_alert_SHP"))) {invalid_alert[['SHP']] <- attr(shp, "invalid_alert_SHP")}
  if (!is.null(attr(shp, "an_map_text_shp")))  {an_map_text[['SHP']] <- attr(shp, "an_map_text_shp")}
  # do we need to remove those attributes now, or just leave them there?
}
###################################################################### #


#' shapefix cleans a spatial data.frame, flags invalid rows, add id if missing, etc.
#' @description a way for app_server, and ejamit() via shapefile_from_any(),
#'  to both use this one function to do the same thing
#'  whether or not in a reactive context 
#' @param shp simple feature data.frame
#' @param crs coordinate reference system, default is 4269
#'
#' @return returns all rows of shp, but adds columns "valid" and "invalid_msg"
#'   and adds attributes shiny can use to update some reactives,
#'   and standardizes "geometry" as the sfc column name.
#'   
#' @keywords internal
#'
shapefix = function(shp,
                    # disable_buttons_SHP = NULL, # probably dont need to know its prior state in shiny
                    # num_valid_pts_uploaded_SHP = NULL, 
                    # invalid_alert_SHP = NULL, 
                    # an_map_text_shp = NULL,
                    crs = 4269) {
  
  ## THIS IS A WAY FOR app_server, and ejamit() via shapefile_from_any(),
  ##  to both use this one function
  ##  to do the same thing whether or not in a reactive context.
  
  if (is.null(shp)) {
    disable_buttons_SHP <- TRUE
    shiny::validate("Uploaded file should have valid file extension(s) - shp, shx, dbf, prj, etc.") # which does stop() if not in shiny
  }
  # if (any(sf::st_geometry_type(shp) == "POINT") & !interactive() & shiny::isRunning()) {
  #   disable_buttons_SHP <- TRUE
  #   shiny::validate("Shape file must be of polygon geometry.") # which does stop() if not in shiny
  # }
  # Drop Z and/or M dimensions from feature geometries, resetting classes appropriately
  shp <- sf::st_zm(shp)
  # Use standard column name, "geometry", for the spatial info # fixed, e.g., shp = shapefile_from_any(system.file("testdata/shapes/portland.gdb.zip", package="EJAM"))
  if (any(grepl("sfc",lapply(shp,class)))) {
    colnames(shp)[grepl("sfc",lapply(shp,class))] <- "geometry"
    sf::st_geometry(shp) <- "geometry"
  }
  
  if (nrow(shp) > 0) {
    
    # count valid rows ####
    shp_valid_check <- terra::is.valid(terra::vect(shp), messages = T)
    shp_is_valid <- shp_valid_check$valid
    numna <- sum(!shp_is_valid)
    num_valid_pts_uploaded_SHP  <- length(shp_is_valid) - sum(!shp_is_valid)
    invalid_alert_SHP <- numna
    # "siteid" added ####
    shp <- dplyr::mutate(shp, siteid = dplyr::row_number())
    # crs ####
    shp <- sf::st_transform(shp, crs = crs)
    an_map_text_shp <- NA # ignored if !is.null(), or maybe will have to handle this as it gets returned?
    
  } else {
    
    invalid_alert_SHP <- 0 # hides the invalid site warning
    an_map_text_shp <- HTML(NULL)  # hides the count of uploaded sites/shapes
    disable_buttons_SHP <- TRUE
    ## return message if in shiny and do stop() if not in shiny
    shiny::validate('No shapes found in file uploaded.')
  }
  # "valid" flag added  ####
  disable_buttons_SHP <- FALSE
  shp$valid <- shp_is_valid
  
  # "ejam_uniq_id" added ####
  
  if (!("ejam_uniq_id" %in% names(shp))) {
    shp <- cbind(ejam_uniq_id = 1:NROW(shp), shp)
  } else {
    if (!all.equal(1:NROW(shp), shp$ejam_uniq_id)) {  
      warning("ejam_uniq_id already is a column in the shapefile, but is not 1 through N. However, it will NOT be overwritten.")
    }
  }
  # "invalid_msg" added ####
  shp$invalid_msg <- NA
  shp$invalid_msg[shp$valid == F] <- shp_valid_check$reason[shp$valid == F]
  shp$invalid_msg[is.na(shp$geometry)] <- 'bad geometry'
  
  # pass info back to shiny for reactives, but if NULL, an attribute gets removed here
  attr(shp, "disable_buttons_SHP") <- disable_buttons_SHP
  attr(shp, "num_valid_pts_uploaded_SHP") <- num_valid_pts_uploaded_SHP
  attr(shp, "invalid_alert_SHP") <- invalid_alert_SHP
  attr(shp, "an_map_text_shp") <- an_map_text_shp
  return(shp)
}
############################################################ # 
