

#' Map popups - Simple map popup from a data.table or data.frame, one point per row
#' @description Creates popup vector leaflet::addCircles or leaflet::addPopups can use.
#' @details Each popup is made from one row of the data.frame. 
#'   Each popup has one row of text per column of the data.frame
#' @param x, a data table or data frame
#' @param n Show the first n columns of mypoints, in popup. "all" means all of them.
#' @param column_names default is all, or a vector of column names from x to use.
#'   If some of column_names requested are not found in names(x), 
#'   a warning is given and NA values returned for those names not in x.
#'   If some of names(x) not requested by column_names, they are left out.
#' @param labels default is column_names - vector used to label 
#'   the elements in the popup. Must be same length as column_names
#' @param testing can set to TRUE while testing function
#' @return A vector of strings, one per row or map point, 
#'   with a line break separating column elements
#' @examples  
#'  dat <- data.table(
#'    RegistryId = c("110071102551", "110015787683"),
#'    FacilityName = c("USDOI FWS AK MARITIME NWR etc", "ADAK POWER PLANT"),
#'    LocationAddress = c("65 MI W. OF ADAK NAVAL FACILITY", "100 HILLSIDE BLVD"),
#'    CityName = c("ADAK", "ADAK"),
#'    CountyName = c("ALEUTIAN ISLANDS", "ALEUTIANS WEST"),
#'    StateAbbr = c("AK", "AK"),
#'    ZipCode = c("99546", "99546"),
#'    FIPSCode = c("02010", "02016"),
#'    lat = c(51.671389,51.8703), lon = c(-178.051111, -176.659),
#'    SupplementalLocation = c(NA_character_,NA_character_))
#'  
#'  ## add popups only
#'  leaflet::leaflet(dat) |> leaflet::addTiles() |> leaflet::addPopups(popup = popup_from_any(dat))
#'  
#'  ## add circles with clickable popups
#'  leaflet::leaflet(dat) |> leaflet::addTiles() |> leaflet::addCircles(popup = popup_from_any(dat))
#'  
#'  ## convert to data frame, works the same way 
#'  dat_df <- as.data.frame(dat_df)
#'  leaflet::leaflet(dat) |> leaflet::addTiles() |> leaflet::addCircles(popup = popup_from_any(dat))
#'   
#' @export
#'
popup_from_any <- function(x, column_names = names(x), labels = column_names, n = "all", testing = FALSE) {
  
  if (!is.data.frame(x)) {
    stop("x must be a data.frame for popup_from_any() to be able to create map popups")
  }
  if (testing) {print('popup_from_any'); print(names(x)); print(labels); print(n)}
  if (n == "all" | n > NCOL(x)) {
    # nothing
  } else {
    if (data.table::is.data.table(x)) {
      x <- x[1:n]
    } else {
      x <- x[, 1:n, drop = FALSE]
    }
  }
  if (missing(column_names)) {column_names <- names(x)} # because maybe n was used and now x has fewer cols than defaults assume
  
  # could warn if both n is specified and some of column_names requested are beyond n
  if (any(!(column_names %in% names(x)))) {
    
    #   If some of column_names requested are not found in names(x), 
    #   a warning is given and NA values returned for those names not in x.
    if (n == "all") {
      warning('some of requested column_names not found in x for popups - filling with NA values')
    } else {
      warning('some of requested column_names not found in first n columns of x (specified by n parameter here) for popups - filling with NA values even if small n resulted in ignoring some of cols specified by column_names param')
    }
    emptycols <- data.frame(matrix(NA, nrow = NROW(x), ncol = length(setdiff(column_names, names(x)))))
    names(emptycols) <- setdiff(column_names, names(x))
    ## warning - leaving these unfound ones at far right of found ones, not in same sort order as supplied via column_names !
    x <- cbind(x, emptycols) # still a dt if it was one?
  }
  
  #  If some of names(x) not requested by column_names, they are left out.
  
  if (data.table::is.data.table(x)) {
    x <- x[, ..column_names]
  } else {
    x <- x[, column_names, drop = FALSE]
  }
  
  if (missing(labels) & !missing(column_names)) {labels <- column_names}
  if (length(labels) != length(column_names)) {
    labels <- column_names
    warning("for map popups, column_names and labels must be same length. Using column_names as labels.")
  }
  
  ## create vector of popups with column labels, length=# of rows of data.table
  if (data.table::is.data.table(x)) {
    popup_vec <- sapply(1:NROW(x), function(row_num) paste(labels, x[row_num  ], sep = ': ', collapse = '<br>'))
  } else {
    popup_vec <- sapply(1:NROW(x), function(row_num) paste(labels, x[row_num, ], sep = ': ', collapse = '<br>'))
  } 
  
  return(popup_vec)
}
############################################################################## #
