
#' Get lat lon columns (or create them from geocoding addresses), and clean up those columns in a data.frame
#'
#' @description Utility to identify lat and lon columns (or addresses), renaming and cleaning them up.
#'
#' @details Tries to figure out which columns seem to have lat lon values,
#'   or addresses that can be converted to lat lon columns, renames those in the data.frame.
#'   Cleans up lat and lon values (removes extra characters, makes numeric)
#' @param df data.frame With columns lat and lon or names that can be interpreted as such,
#'   or addresses that can be geocoding to create lat lon columns
#' @param invalid_msg_table Set to TRUE to add columns "valid" and "invalid_msg" to output
#' @param set_invalid_to_na if not set FALSE, it replaces invalid lat or lon with NA values
#' @seealso Used by [latlon_from_anything()]. Uses [latlon_infer()] [latlon_is.valid()] [latlon_as.numeric()]
#' @return Returns the same data.frame but with relevant colnames changed to lat and lon,
#'    or lat,lon added based on addresses,
#'    and invalid lat or lon values cleaned up if possible or else replaced with NA,
#'    and optional columns "valid" and "invalid_msg"
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
    # try to interpret as street addresses if possible
    adds = address_from_table(df)
    if (all(0 == sapply(adds, nchar))) {
      cat("cannot find address in table so no geocoding tried")
    }
    cat("trying to use geocoding to add lat and lon columns \n")
    ## requires  AOI  pkg be attached!
    dflatlon = latlon_from_address(adds)
    if (!is.null(dflatlon)) {
    df <- cbind(df, dflatlon)
    }
    if ('lat' %in% names(df) && 'lon' %in% names(df)) {
      if (!all(is.na(df$lat)) && !all(is.na(df$lon))) {
        message("latlon_from_address_table() added lat and lon columns using geocoding")
        }
    }

    # removed since latlon_infer already creates warning
    #warning('lat or lon column cannot be inferred from colnames of df')
  }
  # validate to some extent (are the lat lon plausible values)
  validinfo <- latlon_is.valid(lat = df$lat, lon = df$lon, invalid_msg_table = invalid_msg_table)
  if (!invalid_msg_table) {
    ok = validinfo
    #### actually, we should NOT add those columns since invalid_msg_table is FALSE
    # df <- data.table(df, valid = ok, invalid_msg = ifelse(ok, "", "latlon invalid"))
  } else {
    ok <- validinfo$valid
    df <- data.table(df, valid = ok, invalid_msg = validinfo$invalid_msg)

  }
  if (any(!ok) & set_invalid_to_na) {
    # warning and console msg are done in latlon_is.valid()
    ## convert invalid latlons to NA
    df[!ok, c('lat','lon')] <- NA
  }
  return(df)
}


