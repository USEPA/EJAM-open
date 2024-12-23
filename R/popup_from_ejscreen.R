#' Map popups - Create the popup text for maps of EJ results
#' 
#' @description This creates the HTML text that appears in map popups.
#' @details Popup shows up in a window when you 
#'   click on a site on the map, when viewing the results of EJ analysis of 
#'   each site. 
#'   
#'   THIS IS CURRENTLY HARD CODED TO USE EJScreen VARIABLE NAMES.
#'   
#'   It provides raw scores (but not for EJ indexes) and US and State percentiles if available:
#'   
#'   - some site id info fields if found
#'   - latitude, longitude, and size of area around site
#'   - Demographic Indicators like population count, Demographic Indicator, etc.
#'   - Environmental Indicators
#'   - EJ Indexes
#'   - web link(s) to map or report
#'
#' @param out like ejamit()$results_bysite, but also it can be full list from ejamit().
#'   The table of raw data in data.frame form, with results of EJ analysis.
#' @param linkcolname Name of one column in the table that has links to some URL
#' @param linkcolname2 Another like linkcolname
#' @param linkcolname3 another 
#' @param verbose TRUE or FALSE, can see more details reported when function is used.
#'
#' @return HTML ready to be used for map popups
#' 
#' @keywords internal
#' @export
#' 
popup_from_ejscreen <- function(out, linkcolname='EJScreen Report', linkcolname2='EJScreen Map', linkcolname3='EJScreenACS', verbose=FALSE) {
  
  if ("results_bysite" %in% names(out)) {
    # looks like not just 1 table was provided
    out <- out$results_bysite
  }
  if (data.table::is.data.table(out)) {out <- data.table::copy(out); data.table::setDF(out)}
  
  ############################################ #
  # SPECIFY indicators/VARIABLE NAMES  ####
 
  names_d_pop <- names_d
  names_d_nice_pop      <- fixcolnames(names_d_pop, 'r', 'shortlabel')
  names_d_pctile_pop       <- names_d_pctile
  names_d_state_pctile_pop <- names_d_state_pctile
  
  names_e_pop <- names_e
  names_e_nice_pop  <- fixcolnames(names_e_pop, 'r', 'shortlabel')
  names_e_pctile_pop       <- names_e_pctile
  names_e_state_pctile_pop <- names_e_state_pctile
  # EJ:  include the us, normal and supplemental, all in this one variable list:
  names_ej_pctile_pop       <- c(names_ej_pctile,       names_ej_supp_pctile) 
  names_ej_state_pctile_pop <- c(names_ej_state_pctile, names_ej_supp_state_pctile)
  names_ej_pctile_pop_USORSTATE <- fixcolnames(names_ej_pctile_pop, 'r', 'shortlabel')
  names_ej_pctile_pop_USORSTATE <- gsub("(.*)( \\(.*\\%ile.*)$", "\\1", names_ej_pctile_pop_USORSTATE) # removes " (US%ile)" 
  
  ############################################ #
  # text explaining units of measure like ug/m3
  
  popupunits <- map_headernames[ "" != (map_headernames$units), c("units", 'rname')]
  names(popupunits) <- c("units","evar")
  popupunits.api <- popupunits
  ############################################ #
  # ensure expected colnames are in outputs; replace empty values with NA, or this crashes
  
  colstofix <- c(
    names_e_pop, 
    names_e_pctile_pop, names_e_state_pctile_pop,
    names_d_pop,
    names_d_pctile_pop, names_d_state_pctile_pop, # does not include  D subgroups
    names_ej_pctile_pop, names_ej_state_pctile_pop # includes  supplementary EJ indexes too
  )
  if (verbose) {
    cat('\nThese column names were expected but not found in outputs:\n\n')
    print(setdiff(colstofix, names(out)))
    cat('\nThese column names were in outputs but not expected by code:\n\n')
    print(setdiff(names(out), colstofix))
  }
  colstofix <- intersect(colstofix, names(out))
  out[, colstofix] <- sapply(out[, colstofix], function(mycol) {ifelse(mycol == '', NA, mycol)})
  # actually we probably need to remove from consideration all columns that are not in out...
  # but what if there are differences between names_e_pop, the pctile version of that, the nice names version of that, etc.
  # ie their lengths differ. the code is not robust to that.  ***
  
  ############################################ #
  # define functions to write map popup text with given indicator names 
  ############################################ #
  ### Now  table_signif_round_x100() not ejscreensignifarray.api() for the rounding and sig figs and x100 scaling.
  ############# ## #
  ## how many significant digits to report?
  # esigfigs <- map_headernames[ "" != (map_headernames$sigfigs), c("sigfigs", 'rname')]
  # names(esigfigs) <- c("sigfigs", "evar")
  # esigfigs.api <- esigfigs
  # ejscreensignifarray.api <- function(dat, digits = 'ejscreen') {
  #   if (missing(digits) || is.null(digits) || any(digits %in% 'ejscreen')) {
  #     suppressWarnings({
  #       # digits <- as.numeric(esigfigs.api[match(colnames(dat), esigfigs.api$evar), 'sigfigs'])
  #       digits <- as.numeric(varinfo(colnames(dat), 'sigfigs')$sigfigs)
  #     })
  #   }
  #   signifarray.api <- function(dat, digits = 6) {
  #     if (!(is.data.frame(dat))) {dat <- as.data.frame(dat)}
  #     digits[is.na(digits)] <- 22 # if it was NA for some reason, use 22 which is the most allowed and avoids any rounding
  #     y <- mapply(FUN = signif, x = dat, digits = digits)
  #     return(y)
  #   }
  #   return( signifarray.api(dat = dat, digits = digits))
  # }
  ############################################ #
  pctileAsText <- function(x) {
    # note: x should be a vector or data.frame of percentiles from 0 to 100, not 0 to 1
    result <- sapply(x, function(z) {
      paste(floor(z), '%ile', sep = '')
    })
    result[is.na(x)] <- NA
    return(result)
  }
  ############################################################################# #  
  
  make.popup.d.api <- function(d, 
                               pctile, state.pctile, 
                               prefix = 'pctile.text.', 
                               # state.prefix = 'state.pctile.text.',
                               basenames = colnames(d)) {
    
    # function to MAKE THE POPUP TEXT FOR JUST THE DEMOGRAPHIC INDICATORS ####
    
    # add some code here for when some or even all of these names are missing (just leave those out of popup). ***
    # if no names at all are found that are expected, just at least have a blank popup or something like an id number. ***
    # possibly create a simple popup of all?? columns in out, if none of the expected column names are found?? 
    
    if (missing(basenames)) {
      # might add code to handle cases like only one row, matrix not df, etc?
      basenames <- colnames(d)
    }
    ptext <- function(x, y.us, y.st) {
      # By this point, x already should be a rounded percentage 0 - 100, not 0-1
      paste(x, '% (', pctileAsText(y.us), ' in US, ', pctileAsText(y.st),' in State)', sep = '')
    }
    x <- mapply(
      FUN = ptext,
      d,
      pctile, 
      state.pctile
    )
    if (NCOL(x) == 1) {
      x <- as.data.frame(as.list(x)) 
    } else {
      x <- data.frame(x, stringsAsFactors = FALSE) 
      # as.data.frame(as.list(x)) # need to confirm this works for case where only 1 valid row (site)
    }
    # this is a bit confusing since it will be columns called pctile.pm but actually contain value, us pctile, and state pctile
    colnames(x) <- paste(prefix, basenames, sep = '') 
    x[is.na(d)] <- NA
    rownames(x) <- rownames(d)
    return(x)
  }
  ############################################################################# #  
  
  make.popup.ej.api <- function(pctile, state.pctile, labels) {
    
    ## pctile and state.pctile should be named lists of values where names are rnames like state.pctile.EJ.DISPARITY.pm.supp 
    #        pctile <- 1:100; names(pctile) <- c(names_ej_pctile,  names_ej_supp_pctile)
    #  state.pctile <- 1:100; names(pctile) <- c(names_ej_state_pctile, names_ej_supp_state_pctile)
    # make.popup.ej.api(pctile = pctile, state.pctile = state.pctile)
    
    # function to MAKE THE POPUP TEXT FOR JUST EJ INDEXES
    # (for us and state, for either regular or suppl, for vectors of named values) ####
    
    if (missing(labels)) {
      names_ej_pctile_pop_USORSTATE_here <- fixcolnames(names(pctile), 'r', 'shortlabel')
      names_ej_pctile_pop_USORSTATE_here <- gsub("(.*)( \\(.*\\%ile.*)$", "\\1", names_ej_pctile_pop_USORSTATE_here) # removes " (US%ile)" 
      labels <- names_ej_pctile_pop_USORSTATE_here
    }
    ptext_ej <- function(y.us, y.st) {
      paste(pctileAsText(y.us), ' in US, ', pctileAsText(y.st),' in State', sep = '')
    }
    x <- mapply(
      FUN = ptext_ej,
      pctile, state.pctile
    )
    if (NCOL(x) == 1) {
      x <- as.data.frame(as.list(x)) 
    } else {
      x <- data.frame(x, stringsAsFactors = FALSE)
    }
    names(x) <- labels
    return(x)
  }
  ############################################################################# #  
  
  make.popup.e.api <- function(e,
                               pctile, state.pctile,
                               prefix = 'pctile.text.',
                               basenames = colnames(e),
                               units = NULL,
                               sigfigs = NULL) {
    
    # function to MAKE THE POPUP TEXT FOR JUST THE ENVT INDICATORS ####
    
    # add some code here for when some or even all of these names are missing (just leave those out of popup). ***
    # if no names at all are found that are expected, just at least have a blank popup or something like an id number. ***
    # possibly create a simple popup of all?? columns in out, if none of the expected column names are found?? 
    if (missing(basenames)) {
      # might add code to handle cases like only one row, matrix not df, etc?
      basenames <- colnames(e)
    }
    #  # replaced with  table_signif_round_x100() in body of popup_from_ejscreen()
    # e <- data.frame(ejscreensignifarray.api(e, digits = sigfigs), stringsAsFactors = FALSE)
    
    if (missing(units) || is.null(units)) {
      units <- popupunits.api$units[match(basenames, popupunits.api$evar)]
      units[is.na(units)] <- ''
    }
    if (NCOL(e) == 1) {e <- data.frame(t(e)); rownames(e) <- NULL}
    x <- mapply(
      FUN = function(x, u, y.us, y.st) {
        paste(x, ' ', u, ' (', pctileAsText(y.us), ' in US, ', pctileAsText(y.st), ' in State)', sep = '')
      },
      e,
      units,
      pctile, 
      state.pctile
    )
    if (NCOL(x) == 1) {x <- as.data.frame(as.list(x)) } else {
      x <- data.frame(x, stringsAsFactors = FALSE) 
      # as.data.frame(as.list(x)) # need to confirm this works for case where only 1 valid row (site)
    }
    # this is a bit confusing since it will be columns called pctile.pm but actually contain value, us pctile, and state pctile
    colnames(x) <- paste(prefix, basenames, sep = '') 
    
    x[is.na(e)] <- NA
    rownames(x) <- rownames(e)
    return(x)
  }
  ############################################################################# #  
  
  # CREATE POPUPS via those functions ####
  
  # add some code here for when some or even all of these names are missing (just leave those out of popup). ***
  # if no names at all are found that are expected, just at least have a blank popup or something like an id number. ***
  # possibly create a simple popup of all?? columns in out, if none of the expected column names are found?? 
  # handle case where some of names_d are not in out, and same for other variables
  # assume that pctile and nice versions are not missing if the base version is here!
  
  dok <- which(names_d_pop %in% names(out)) 
  eok <- which(names_e_pop %in% names(out))
  pok <- which(names_ej_pctile_pop %in% names(out))
  
  if (length(dok) > 0) {
    names_d_pop      <- names_d_pop[dok]
    names_d_nice_pop <- names_d_nice_pop[dok]
    names_d_pctile_pop       <- names_d_pctile_pop[names_d_pctile_pop %in% names(out)] #  there are state and US ones, so 2x as many as there are names_d
    names_d_state_pctile_pop <- names_d_state_pctile_pop[names_d_state_pctile_pop %in% names(out)] # there are state and US ones, so 2x as many as there are names_d
    if (length(names_d_pctile_pop) != length(names_d_state_pctile_pop) || length(names_d_pctile_pop) != length(names_d_pop)) {
      warning("mismatch between numbers of names found in out for names_d_pop, names_d_pctile_pop, and names_d_state_pctile_pop")
    }
  }
  if (length(eok) > 0) {
    names_e_pop      <- names_e_pop[eok]
    names_e_nice_pop <- names_e_nice_pop[eok]
    names_e_pctile_pop       <- names_e_pctile_pop[names_e_pctile_pop %in% names(out)] #  there are state and US ones, so 2x as many as there are names_d
    names_e_state_pctile_pop <- names_e_state_pctile_pop[names_e_state_pctile_pop %in% names(out)] #  there are state and US ones, so 2x as many as there are names_d
    if (length(names_e_pctile_pop) != length(names_e_state_pctile_pop) || length(names_e_pop) != length(names_e_pctile_pop)) {
      warning("mismatch between numbers of names found in out for names_e_pop, names_e_pctile_pop and names_e_state_pctile_pop")
    }
  }
  if (length(pok) > 0) {
    names_ej_pctile_pop <- names_ej_pctile_pop[pok]
    names_ej_state_pctile_pop <- names_ej_state_pctile_pop[names_ej_state_pctile_pop %in% names(out)]
    if (length(names_ej_pctile_pop) != length(names_ej_state_pctile_pop)) {
      warning("mismatch between numbers of names found in out for names_ej_pctile_pop and names_ej_state_pctile_pop")
    }
  }
  
  ################################################################ #
  
  # SIGNIF, ROUND, X100 ####
  
  out <- table_signif_round_x100(out)  # convert to rounded numbers, incl. % as 0-100 no decimals.
  
  ################################################################ #
  # popups text for DEMOG (US & STATE PCTILES) ####
  if (length(dok) == 0) {
    warning('none of names_d were found in out')
    poptext.d <- NULL
  } else {
    poptext.d <-  make.popup.d.api(
      d = out[, names_d_pop], ################### # # #   DROPPED THIS x100 AND HANDLE IT ELSEWHERE ***
      pctile = out[, names_d_pctile_pop], 
      state.pctile = out[, names_d_state_pctile_pop], 
      prefix = '')
    names(poptext.d) <- names_d_nice_pop
  }
  
  # popups text for ENVT  (US & STATE PCTILES) ####
  if (length(eok) == 0) {
    warning('none of names_e were found in out')
    poptext.e <- NULL
  } else {
    poptext.e <-  make.popup.e.api(
      e = out[, names_e_pop],
      pctile = out[, names_e_pctile_pop], 
      state.pctile = out[, names_e_state_pctile_pop], 
      prefix = ''
    ) 
    names(poptext.e) <- names_e_nice_pop
  }
  
  # popups text for EJ INDEXES (US & STATE PCTILES, FOR NORMAL & SUPPLEMENTARY EJ INDEXES) ####
  if (length(pok) == 0) {
    message('none of names_ej_pctile found in out')
    poptext.ej <- NULL
  } else {
    poptext.ej <-  make.popup.ej.api(
      pctile = out[, names_ej_pctile_pop], 
      state.pctile = out[, names_ej_state_pctile_pop], 
      labels = names_ej_pctile_pop_USORSTATE
    )
  }
  
  if (linkcolname  %in% names(out)) {pops_link1 <- paste0(out[ , linkcolname] , '<br>')} else {pops_link1 <- paste0(rep(NA, NROW(out)), '<br>')}
  if (linkcolname2 %in% names(out)) {pops_link2 <- paste0(out[ , linkcolname2], '<br>')} else {pops_link2 <- paste0(rep(NA, NROW(out)), '<br>')}
  if (linkcolname3 %in% names(out)) {pops_link3 <- paste0(out[ , linkcolname3], '<br>')} else {pops_link3 <- paste0(rep(NA, NROW(out)), '<br>')}
  
  if ('ejam_uniq_id' %in% names(out)) {pops_ejam_uniq_id <- paste0('ejam_uniq_id: ', out$ejam_uniq_id, '<br>')} else {pops_ejam_uniq_id <- ''}
  if ('id'           %in% names(out)) {pops_id           <- paste0('id: ',           out$id,           '<br>')} else {pops_id           <- ''}
  if ('siteid'       %in% names(out)) {pops_siteid       <- paste0('siteid: ',       out$siteid,       '<br>')} else {pops_siteid       <- ''}
  if ('sitenumber'   %in% names(out)) {pops_sitenumber   <- paste0('sitenumber: ',   out$sitenumber,   '<br>')} else {pops_sitenumber   <- ''}
  if ('sitename'     %in% names(out)) {pops_sitename     <- paste0('sitename: ',     out$sitename,     '<br>')} else {pops_sitename     <- ''}
  if ('radius.miles' %in% names(out)) {pops_radmile      <- paste0('Area within ',   out$radius.miles, ' miles of site', '<br>')} else {pops_radmile <- ''}
  
  if (length(poptext.d) > 0) {
    pops_d <- paste0(
      '<b>', 'Demographic Indicators: ', '</b>',                 '<br>',
      'Population: ', prettyNum(out$pop, big.mark = ','),        '<br>',
      apply(poptext.d, FUN = function(x) paste0(names(x), ': ', x, collapse = '<br>'), MARGIN = 1), '<br>'
    )
  } else {
    pops_d <- ""
  }
  
  if (length(poptext.e) > 0) {
    pops_e <- paste0(
      '<b>', 'Environmental Indicators: ', '</b>',               '<br>',
      apply(poptext.e, FUN = function(x) paste0(names(x), ': ', x, collapse = '<br>'), MARGIN = 1), '<br>'
    )
  } else {
    pops_e <- ''
  }
  
  if (length(poptext.ej) > 0) {
    pops_ej <- paste0(
      '<b>', 'EJ Indexes: ',              '</b>',               '<br>',
      apply(poptext.ej, FUN = function(x) paste0(names(x), ': ', x, collapse = '<br>'), MARGIN = 1), '<br>'
    )
  } else {
    pops_ej <- ''
  }
  
  z <- paste0(
    '<b>',
    pops_ejam_uniq_id,
    pops_id,
    pops_siteid,
    pops_sitenumber, 
    pops_sitename,
    '</b>',
    pops_radmile,
    paste0('long, lat: ',  out$lon, ', ', out$lat,             '<br>'),
    
    pops_d, 
    pops_e,
    pops_ej,
    
    # LINK IN POPUP
    pops_link1,    # out[ , linkcolname] ,           '<br>',
    #    url_linkify(out[ , linkcolname] , 'EJScreen Report'), '<br>',
    pops_link2,    # out[ , linkcolname2],    '<br>',
    #    url_linkify(out[ , linkcolname2], 'EJScreen Map'),  '<br>',
    pops_link3, 
    sep = '<br>'
  )
  
  if (verbose) {popup_print(z)} #  was  cat(gsub('<br>','  <br>\n',z))  #  see in console   
  return(z) # invisible(z) did not seem to work for some reason
}
############################################################################# #  

#' Helper function to view popup info in an interactive session - easier format to view
#'
#' @param x output of [popup_from_ejscreen()]
#' @param linkregex see source
#' @param linksimple see source
#' @seealso [popup_from_ejscreen()]
#' 
#' @keywords internal
#' @export
#'
popup_print <- function(x, linkregex='<a href.*>(.*)<.*', linksimple='\\1') {
  
  # simple utility to let you view contents of map popups, in the console:
  cat(paste0(gsub(linkregex, linksimple, gsub('<br>','  \n', x)), collapse = '\n\n'))
}
############################################################################# #  
