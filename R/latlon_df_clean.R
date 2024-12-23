
#' Get lat lon columns and clean up those latitude and longitude columns in a data.frame
#' 
#' @description Utility to identify lat and lon columns, renaming and cleaning them up.
#' 
#' @details Tries to figure out which columns seem to have lat lon values, renames those in the data.frame. 
#'   Cleans up lat and lon values (removes extra characters, makes numeric)
#' @param df data.frame With columns lat and lon or names that can be interpreted as such
#' @param invalid_msg_table Set to TRUE to add columns "valid" and "invalid_msg" to output
#' @param set_invalid_to_na if not set FALSE, it replaces invalid lat or lon with NA values
#' @seealso Used by [latlon_from_anything()]. Uses [latlon_infer()] [latlon_is.valid()] [latlon_as.numeric()]
#' @return Returns the same data.frame but with relevant colnames changed to lat and lon,
#'    and invalid lat or lon values cleaned up if possible or else replaced with NA,
#'    and columns "valid" and "invalid_msg"
#'
#' @examples #  x <- latlon_df_clean(x)
#'  latlon_df_clean(testpoints_bad, set_invalid_to_na = F, invalid_msg_table = T)
#'  
#' @keywords internal
#'
latlon_df_clean <- function(df, invalid_msg_table = FALSE, set_invalid_to_na = TRUE) {
  if (missing(df)) {
    warning('No value provided for argument "df".')
    return(NULL)
  }
  # figure out which columns seem to have lat lon values, rename those in the data.frame
  # $ signifies the end of a string, so only will be removed if at end
  names(df) <- latlon_infer(gsub(".1$", "", names(df)))
  
  # Cleans up lat and lon values (removes extra characters, makes numeric)
  if ('lat' %in% names(df) & 'lon' %in% names(df)) {
    df$lon <- latlon_as.numeric(df$lon)
    df$lat <- latlon_as.numeric(df$lat)
  } else {
    warning("Dataframe does not have both lat and lon columns")
    # removed since latlon_infer already creates warning
    #warning('lat or lon column cannot be inferred from colnames of df')
  }
  # validate to some extent (are the lat lon plausible values)
  validinfo <- latlon_is.valid(lat = df$lat, lon = df$lon, invalid_msg_table = invalid_msg_table)
  if (!invalid_msg_table) {
    ok = validinfo
    df <- data.table(df,valid = ok, invalid_msg = ifelse(ok, "", "latlon invalid"))
  } else {
    ok <- validinfo$valid
    df <- data.table(df, valid = ok,invalid_msg = invalid_msg_table)
    
  }
  if (any(!ok) & set_invalid_to_na) {
    # warning and console msg are done in latlon_is.valid()
    ## convert invalid latlons to NA
    df[!ok, c('lat','lon')] <- NA
  }
  return(df)
}


