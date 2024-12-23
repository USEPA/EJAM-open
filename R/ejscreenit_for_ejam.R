

#' Get EJScreen results directly from EJScreen servers via their API
#' 
#' A wrapper for EJAM to use ejscreenit() 
#' 
#' @param sitepoints table with lat and lon columns
#' @param radius in miles
#' @param fips fips code (or possibly a vector of fips codes)
#' @param shapefile not implemented
#' @param namestr optional text
#' @param fillmissingcolumns optional. set to TRUE if you want the output
#'   to have exactly all the same columns as the EJAM table would, and
#'   fill in with NA values all the columns not provided by EJScreen API.
#' @param nosave passed to [ejscreenit()]
#' @param nosee passed to [ejscreenit()]
#' @param fillmissingcolumns passed to [ejscreenapi2ejam_format()]
#' @param ... other parameters passed to [ejscreenit()] 
#' @seealso [ejscreen_vs_ejam()] [ejscreenapi2ejam_format()] which it uses.
#' @return a data.table that looks like output of ejamit()$results_bysite
#' 
#' @export
#'
ejscreenit_for_ejam <- function(sitepoints, radius=3, 
                                fips = NULL, shapefile = NULL,
                                namestr = '',
                                nosave = TRUE,
                                nosee = TRUE,
                                fillmissingcolumns = FALSE,
                                ...) {

  if (!is.null(shapefile)) {warning('shapefile not implemented yet')}
  
  out <- ejscreenit(sitepoints, radius = radius, 
                    fips = fips, shapefile = shapefile,
                    namestr = namestr,
                    nosave = nosave,
                    nosee = nosee,
                    ...)
  # could say nicenames=F, but even without that it gets renamed in the next step
  
  out <- ejscreenapi2ejam_format(out, fillmissingcolumns = fillmissingcolumns)  # , ejamcolnames = ejamcolnames
  
  return(out)
}
############################################################ #


#' EJAM/EJSCREEN comparisons - Convert output of ejscreenapi_plus to format of ejamit table of sites
#' 
#' Used by [ejscreenit_for_ejam()] to make it easier to compare EJScreen and EJAM results
#' 
#' @param ejscreenapi_plus_out results of ejscreenapi_plus() or also 
#'   could be results of ejscreenit()$table even though the colnames differ,
#'   because they get converted here in that case.
#'   Alternatively also can be the whole list output of ejscreenit() 
#'   not just ejscreenit()$table, which this function will figure out.
#' 
#' @param fillmissingcolumns optional. set to TRUE if you want the output
#'   to have exactly all the same columns as the EJAM table would, and
#'   fill in with NA values all the columns not provided by EJScreen API.
#' @param ejamcolnames optional. if specified as vector of colnames, it 
#'   overrides the assumed colnames that would have been taken to be
#'   colnames(testoutput_ejamit_10pts_1miles$results_bysite). 
#'   Any colnames you specify here will be the colnames of the output
#'   if fillmissingcolumns = TRUE, or else those not in names(ejscreenapi_plus_out)
#'   will be omitted.
#' @return A data.table not just data.frame, with some or all of the columns
#'   found in output of ejamit()$results_bysite
#' @seealso [ejscreenit_for_ejam()] [ejscreen_vs_ejam()]
#' @export
#'
#' @examples
#' \dontrun{
#'   y1 <- ejscreenit_for_ejam(testpoints_10[1:2, ], radius = 1)
#' 
#'    x <- ejscreenapi_plus(testpoints_10[1:2, ], radius = 1)
#'    y <- ejscreenapi2ejam_format(x)
#'    ejamvars <- names(testoutput_ejamit_10pts_1miles$results_bysite)
#'    all.equal(
#'      names(y), 
#'      ejamvars[ejamvars %in% names(y)]
#'   )
#'   
#'   z <- ejscreenapi2ejam_format(x, fillmissingcolumns = T)
#'   all.equal(names(z), ejamvars)
#'   
#'   # for convenience also can do this:
#'   x <- ejscreenapi2ejam_format()
#'   
#'   }
#'   
#'   
#'   
ejscreenapi2ejam_format <- function(ejscreenapi_plus_out, fillmissingcolumns = FALSE, ejamcolnames=NULL) {
  
  if (!is.null(ejamcolnames)) {
    ejamvars <- ejamcolnames
  } else { 
    if (exists("testoutput_ejamit_10pts_1miles")) {
    ejamvars <- colnames(testoutput_ejamit_10pts_1miles$results_bysite)
    } else {
      stop('missing ejamcolnames and also missing testoutput_ejamit_10pts_1miles$results_bysite to get colnames from ')
    }
  } 
  x <- ejscreenapi_plus_out
  if ("table" %in% names(x) && is.data.frame(x$table)) {
    # they passed all of ejscreenit() not just ejscreenit()$table
    # which is what happens when using ejscreenit_for_ejam() actually
    x <- x$table
  }
  
  # just in case, try to convert as if they were long names as in output of ejscreenit()
  colnames(x) <- fixcolnames(colnames(x), "long", "r")

  # Remove columns from API output that are not in the EJAM output format 
  
  keepthese <- which(colnames(x) %in% ejamvars)
  setDF(x) # but should already be a data.frame not data.table if coming from ejscreenapi_plus() 
  x <- x[ , keepthese]
  
  if (fillmissingcolumns) {
    # could be done more efficiently - this was drafted quickly
    y <- data.frame(matrix(NA_integer_, nrow = NROW(x), ncol = length(ejamvars)))
    colnames(y) <- ejamvars
    sharednames <- intersect(colnames(x) , ejamvars)
    y[ , sharednames] <- x[ , sharednames]
    x <- y
    setDT(x)
  } else {
    sharednames_in_ejam_order <- ejamvars[ejamvars %in% names(x)]
    setDT(x)
    setcolorder(x, sharednames_in_ejam_order)
  }
  # make sure class is same for each column of ejscreenapi output as it is in ejam output
  shouldbelike <- copy(testoutput_ejamit_10pts_1miles$results_bysite)
  setDF(shouldbelike)
  setDF(x)
  for (i in 1:NCOL(x)) {
    class(x[, colnames(x)[i]]) <- 
      class(shouldbelike[, colnames(x)[i]])
  }
  setDT(x)
  
  return(x)
  
  ### test it:
  # x <- ejscreenapi_plus(testpoints_10[1:2, ], radius = 1)
  # y <- ejscreenapi2ejam_format(x)
  # ejamvars <- names(testoutput_ejamit_10pts_1miles$results_bysite)
  # all.equal(
  #   names(y), 
  #   ejamvars[ejamvars %in% names(y)]
  # )
  
}
############################################################ #
