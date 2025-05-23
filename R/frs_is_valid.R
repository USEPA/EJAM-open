

#' Validate FRS Registry ID table uploaded (just checks colname, mostly)
#' 
#' @description Check for proper colname (or what seems to be a valid alias)
#' @details note it checks aliases (REGISTRY_ID, RegistryID, regid, siteid) in that order 
#'   and once a valid name is found then even if it fails to actually 
#'   contain valid ids, the func does not go back and try the rest of the possible aliases, 
#'   so if the two cols were regid and siteid and only siteid had valid registry ID values,
#'   this func would fail to figure that out and would say they were invalid.
#' @param frs_upload upload frs registry IDs table converted to data frame (or data.table gets handled too)
#'   with those ids in a column whose name is among allowed aliases that get tried here:
#'   the colname with the FRS regids must be one of REGISTRY_ID, RegistryID, regid, siteid,
#'   checked in that order of preference.
#' @return boolean value (valid or not valid)
#'   
#' @keywords internal
#'
frs_is_valid <- function(frs_upload) {
  
  if (is.data.table(frs_upload)) {frs_upload <- setDF(copy(frs_upload))}
  
  if (!is.data.frame(frs_upload) | NROW(frs_upload) == 0 | all(is.na(frs_upload))) {
    warning("invalid frs_upload - must be a data.frame of at least 1 row")
    return(FALSE)
  }
  
  okname <- FALSE
  
  if ("REGISTRY_ID" %in% colnames(frs_upload)) { # frs data.table uses this colname
    okname <- TRUE
    # assume values are OK since colname was right? faster to assume that but inconsistent with checking below. 
    return(TRUE)
  } else {
    
    if ("RegistryID" %in% colnames(frs_upload)) {
      colnames(frs_upload) <- gsub("RegistryID", "REGISTRY_ID", colnames(frs_upload)) # ECHO uses this colname
      warning("assuming RegistryID column has REGISTRY_ID values")
      okname <- TRUE
    } else {
      
      if ("regid" %in% colnames(frs_upload)) {
        colnames(frs_upload) <- gsub("regid", "REGISTRY_ID", colnames(frs_upload))
        warning("assuming regid column has REGISTRY_ID values")
        okname <- TRUE
      } else {
        
        if ("siteid" %in% colnames(frs_upload)) {
          colnames(frs_upload) <- gsub("siteid", "REGISTRY_ID", colnames(frs_upload))
          warning("assuming siteid column has REGISTRY_ID values")
          okname <- TRUE
        }
      }
    }
  }
  
  if (!okname) {
    warning('no recognized valid colname holding registry id values was found - must be one of REGISTRY_ID, RegistryID, regid, siteid')
    return(FALSE)
  }
  
  if (NROW(frs_from_regid(frs_upload$REGISTRY_ID)) == 0) {
    warning("The column presumed to contain FRS REGISTRY_ID values had no entries that could be found in the FRS with that id")
    return(FALSE) # not even 1 value was found to be a registry id in the frs table among frs$REGISTRY_ID
  } else {
    return(TRUE) # at least 1 value was found in frs$REGISTRY_ID (which does NOT mean they all were valid ids!!)
  }
}
