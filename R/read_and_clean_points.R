
#' Flexibly read .csv or .xlsx of lat/lon points or facility IDs
#' 
#' @details  *** THIS WOULD REPLACE SOME OF THE CODE IN server.R
#' 
#'    and may want to merge with EJAM   ::latlon_from_anything()
#'    
#'   so one can read in a file or table or vectors - any format - 
#'    and the other figures out if it is latlon or program IDs or registry IDs (but not NAICS?)
#'    and I guess turns those into lat, lon, siteid as below.  
#' 
#'   EJAM  ::  latlon_from_anything() uses
#' 
#'   EJAMbatch.summarizer   ::  read_csv_or_xl() and
#'   
#'    EJAM   ::latlon_df_clean()  which uses  EJAM  ::  latlon_infer()  [latlon_as.numeric()] [latlon_is.valid()]
#' 
#' 
#'  read_and_clean_points()
#' 
#'   would be the most general / flexible broadest way to get points, but is still work in progress 
#' 
#'   is similar to what is done by EJAM  ::latlon_from_anything()
#'   
#'   except it also uses these functions:
#'   
#'   EJAM  ::latlon_from_siteid()  
#'   
#'   EJAM  ::latlon_from_programid()  but not  _from_naics() ?
#'   
#' @param filepath filename that can include path
#' @param default_points what to return if no matches
#' @seealso [read_csv_or_xl()]
#' @return data.frame with lat, lon, etc. columns
#' 
#' @keywords internal
#' @export
#'
read_and_clean_points <- function(filepath, default_points=NULL) {
  
  warning('work in progress - it can replace similar code in server function')
  stop(' this needs EJAM pkg data files and latlon_from_siteid() and latlon_from_programid() ')
  
  ## We should disable upload of a crazy number of points 
  
  # MIGHT WANT TO ADD CODE HERE as from   latlon_from_anything() 
  # THAT IS FLEXIBLE & convenient IN ALLOWING input to be x,y or IDs or filename or table, etc.
  
  
  # try to read the file ####
  cat(paste0('trying to read ', filepath, '\n'), file = stderr()) # just prints to console
  
  pts_filecontents <- read_csv_or_xl(filepath)
  
  if(is.null(pts_filecontents)){
    stop("No pts_filecontents loaded")
  }
  
  # if uploaded with zero length somehow, return default points or NULL ####
  if (0 == length(pts_filecontents)) {
    pts_filecontents <- default_points  # was defined in global.R  
  } else {
    
    # LAT/LON: if any columns seem to be lat/lon, use those ####
    
    pts_filecontents <- latlon_df_clean(pts_filecontents)
    #    a set of latlon cleaning functions using latlon_infer(), latlon_as.numeric(), latlon_is.valid()
    if ('lat' %in% names(pts_filecontents) & 'lon' %in% names(pts_filecontents)) {
      # ALL SET - using lat/lon
      if (('registry_id' %in% names(pts_filecontents) ) | ('pgm_sys_id' %in% names(pts_filecontents))) {
        shiny::showModal(shiny::modalDialog(title = 'Warning', 'lat/lon found, so ignoring registry_id/pgm_sys_id', easyClose = TRUE))
      }
    } else {
      if ('FacLong' %in% names(pts_filecontents) & 'FacLat' %in% names(pts_filecontents)) {
        #  ECHO column names  - but latlon_infer() has already renamed them anyway, actually so we can't get here probably 
        names(pts_filecontents) <- gsub('FacLat', 'lat', names(pts_filecontents)); names(pts_filecontents) <- gsub('FacLong', 'lon', names(pts_filecontents)) # as used by leaflet, and so names are unique even when uploaded table is merged with EJScreen results
        # the variable names latitude and longitude are compatible with leaflet() but we will not rename them except for that one purpose right when mapping
        # ALL SET - using FacLat/FacLong
        if (('registry_id' %in% names(pts_filecontents) ) | ('pgm_sys_id' %in% names(pts_filecontents))) {
          shiny::showModal(shiny::modalDialog(title = 'Warning', 'lat/lon found, so ignoring registry_id/pgm_sys_id', easyClose = TRUE))
        }
      }  else {
        
        # if only "ID" or "id" column found, try interpret it as program or registry id ####
        
        if (!('registry_id' %in% names(pts_filecontents) ) & !('pgm_sys_id' %in% names(pts_filecontents))) {
          # as last resort, try to see if id or ID or Id column works as a registry ID
          if ("id" %in% tolower(names(pts_filecontents))) {
            idcol <- (names(pts_filecontents)[tolower(names(pts_filecontents)) == 'id'])[1]
            if (any(is.na(as.numeric(as.vector(unlist(pts_filecontents[,"idcol"])))))) {
              ## id might be pgm_sys_id ####
              # since some are not numeric, so try interpreting it that way
              pts_filecontents$pgm_sys_id <- pts_filecontents[,"idcol"]
            } else {
              ## id might be registry id ####
              # since all are numeric, so try interpreting it that way
              pts_filecontents$registry_id <- pts_filecontents[,"idcol"]
            }
          }
        }
        
        if ('registry_id' %in% names(pts_filecontents)) {
          
          # registry_id: Query FRS by facility registry ID ####
          
          # prior code, when it was in server.R :
          # showModal(modalDialog(title = "Please Wait", paste0("querying FRS based on facility registry_id to get lat and lon (ignores pgm_sys_id column since registry_id is present)", ''), easyClose = TRUE))
          # x <- try(locate_by_id(id = pts_filecontents$registry_id, type = 'frs'))
          # pts_filecontents$lat <- as.numeric(x$Latitude83)
          # pts_filecontents$lon <- as.numeric(x$Longitude83)
          
          cat("\nQuerying copy of FRS via facility registry_id to get lat, lon (ignores pgm_sys_id column since registry_id is present)\n")
         
          
          
          stop(' this needs EJAM  data files and latlon_from_siteid()  ')
           x <- try( latlon_from_siteid(siteid = pts_filecontents$registry_id))
          
           
           
           
           # error handling could go here
          # *** MIGHT NEED TO CONFIRM THE LENGTH AND SORT ORDER OF x and pts_filecontents are the same, and NA handled correctly, etc. 
          pts_filecontents$lat <- x$lat
          pts_filecontents$lon <- x$lon 
          
          if ('pgm_sys_id' %in% names(pts_filecontents)) {shiny::showModal(shiny::modalDialog(title = 'Warning', 'registry_id found, so ignoring pgm_sys_id', easyClose = TRUE))}
          # SOME CODE ASSUMES INPUT POINTS MATCH 1 TO 1 OUTPUT POINTS- CREATES PROBLEM IF INPUT ROW COUNT DIFFERS FROM OUTPUT ROW COUNT, WHICH MAYBE COULD HAPPEN FOR QUERY ON ID, AND ESPECIALLY IF QUERY ON NAICS, FOR EXAMPLE.
        } else {
          
          ## pgm_sys_id: Query FRS by program system ID ####
          
          if ('pgm_sys_id' %in% names(pts_filecontents)) {
            
            # prior code, when it was in server.R 
            # showModal(modalDialog(title = "Please Wait", paste0("querying FRS based on facility pgm_sys_id to get lat and lon", ''), easyClose = TRUE))
            # x <- try(locate_by_id(id = pts_filecontents$pgm_sys_id, type = 'program'))
            # pts_filecontents$lat <- as.numeric(x$Latitude83)
            # pts_filecontents$lon <- as.numeric(x$Longitude83)
            
            cat("\nQuerying copy of FRS via facility pgm_sys_id to get lat, lon\n")
            
            
            
            stop(' this needs EJAM  data files and latlon_from_programid()  ')
            x <- try( latlon_from_programid(programid = pts_filecontents$pgm_sys_id))
            
            
            # error handling could go here!!
            pts_filecontents$lat <- x$lat
            pts_filecontents$lon <- x$lon 
            # SOME CODE ASSUMES INPUT POINTS MATCH 1 TO 1 OUTPUT POINTS- CREATES PROBLEM IF INPUT ROW COUNT DIFFERS FROM OUTPUT ROW COUNT, WHICH MAYBE COULD HAPPEN FOR QUERY ON ID, AND ESPECIALLY IF QUERY ON NAICS, FOR EXAMPLE.
          } else {
            shiny::showModal(shiny::modalDialog(title = "Error", paste0("The file must have columns named lat and lon, or registry_id, or pgm_sys_id. Headers must be in row 1, data starting in row 2.", ''), easyClose = TRUE))
            pts_filecontents <- default_points  # defined in global.R   This line is so default example is shown instead of uploaded file that does not have correct columns 
          }
        }
      }
    }
  }
  return(pts_filecontents)
}
