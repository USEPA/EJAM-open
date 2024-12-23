


#' statestats_means - convenient way to see STATE MEANS of indicators for a list of states (that can have repeats)
#' @description Given a vector of 2-char ST abbrevs, and vector of colnames in statestats table (indicator names),
#'   return data.frame of state averages
#' @param ST vector of 2-char ST abbrevs, or all values can be "USA" to get duplicate rows like found in ejamit()$results_bysite[, names_d_avg]
#' @param varnames vector of colnames in statestats table (indicator names)
#' @param PCTILES "mean"
#'
#' @return data.frame of state averages for those, one row per ST provided (can have repeats) and colnames are varnames.
#' 
#' @keywords internal
#' @export
#'
statestats_means_bystates <- function(ST = unique(EJAM::statestats$REGION), varnames = names_these, PCTILES = "mean") {
  
  # Help add (to a table of info on multiple states) columns of a few indicators with averages of appropriate state for each row
  # Given a vector of 2-char ST abbrevs, and vector of colnames in statestats table (indicator names),
  # return data.frame of state averages for those, one row per ST provided (can have repeats) and colnames are varnames.
  # see also [statestates_means()] and [statestats_query()]
  if (length(PCTILES) > 1) {stop('PCTILES must be a single value like "mean"')}
  ST <- toupper(ST)
  if (all(ST %in% "USA")) {
    
    meansby  = usastats[usastats$PCTILE == PCTILES, c("REGION", varnames)]
    x = meansby[match(ST, meansby$REGION), varnames] # drop = F ? 
    rownames(x) <- NULL  
    return(x)
    
  } else {
    
    if (!(all(ST %in% statestats$REGION))) {
      warning('all of ST values must be in statestats$REGION - returning no averages for any, even matching states')
      x = data.frame(matrix(NA, nrow = length(ST), ncol = length(varnames)))
      colnames(x) <- varnames
      return(x)
    }
    
    meansbystate  = statestats[statestats$PCTILE == PCTILES, c("REGION", varnames)]
    x = meansbystate[match(ST, meansbystate$REGION), varnames] # drop = F ? 
    rownames(x) <- NULL
    return(x)
  }
}

################################################################################ #


#' statestats_means - convenient way to see STATE MEANS of ENVIRONMENTAL and DEMOGRAPHIC indicators
#' 
#' @inheritParams statestats_query
#' 
#' @export
#'
statestats_means <- function(ST=unique(EJAM::statestats$REGION), varnames=c(EJAM::names_e, EJAM::names_d, EJAM::names_d_subgroups_nh), PCTILES="mean", dig=4) {
  x = statestats_query(ST = ST, varnames = varnames, PCTILES = PCTILES,  dig = dig)
  # x$REGION = NULL; 
  x$PCTILE = NULL # so t(x) wont make everything into character class
  x = t(x)
  colnames(x) = "st.avg"
  return(x)
}
################################################################ #


#' statestats_query - convenient way to see mean, pctiles of Env or Demog indicators from lookup table
#'
#' @param ST vector of state abbreviations, or USA
#' @param varnames names of columns in lookup table, like "proximity.rmp"
#' @param PCTILES vector of percentiles 0-100 and/or "mean"
#' @param dig digits to round to
#'
#' @examples \dontrun{
#' 
#' usastats_querye() 
#' #  data.frame where names_e are the names(), 
#' #  means plus other percentiles, and there are other cols REGION PCTILE
#' 
#' avg.in.us                # This is a data.frame, 1 row, where colnames are indicators
#' avg.in.us[names_e]          # subset is a data.frame!
#' unlist(avg.in.us[names_e])  # to make it a vector
#' 
#' usastats_means()        # This is a matrix, with 1 col, and indicator names are rownames
#' usastats_means(names_e)     # subset is a matrix        and indicator names are rownames
#' usastats_means()[names_e, ] # subset is a named vector  and indicator names are  names
#' 
#' usastats_means()
#' statestats_query()
#' 
#' statestats_query()[,names_d]
#' statestats_query(varnames = names_d)
#' 
#' statestats_query()[,names_e]
#' statestats_query(varnames = names_e)
#' 
#' statestats_query(varnames = names_d_subgroups)
#' head(statestats_query(varnames = longlist))
#' 
#' ## in USA overall, see mean and key percentiles
#' # for all demog and envt indicators
#' usastats_query() # or statestats_query('us') 
#' # can say us or US or USA or usa etc.
#' usastats_query(PCTILES = 'mean')
#' usastats_means() # same but nicer looking format in console
#' usastats_means(dig=4)
#' 
#' # long list of variables:
#' x = intersect(EJAM::names_all_r,  names(EJAM::usastats))
#' usastats_means(x)
#' 
#' usastats[!(usastats$PCTILE < 50), c("PCTILE", names_d)]
#' usastats[!(usastats$PCTILE < 50), c("PCTILE", names_e)]
#' 
#' ## in 1 state, see mean and key percentiles for all demog and envt indicators
#' statestats_query('MD')
#' 
#' ## in 1 state, see mean and key percentiles for just demog indicators
#' statestats_queryd('MD')
#' 
#' ## 1 indicator in 1 state, see a few key percentiles and mean
#' statestats_query('MD','proximity.tsdf')
#' 
#' ## mean of 1 indicator for each state
#' statestats_query(varnames = 'proximity.tsdf')
#' 
#' ## using full blockgroup dataset, not lookup tables of percentiles, 
#' blockgroupstats[, lapply(.SD, function(x) mean(x, na.rm=T)),
#'    .SDcols= c(names_d, names_e)]
#' 
#' ##   see all total counts (not just US means), 
#' ##   demographics including subgroups, 
#' ##   but not environmental indicators.
#' t(blockgroupstats[, lapply(.SD, function(x) mean(x, na.rm=T)),
#'     .SDcols= c(names_e, names_d)])
#' 
#' }
#' 
#' @export
#' 
statestats_query <- function(ST=sort(unique(EJAM::statestats$REGION)), 
                             varnames=c(EJAM::names_e, EJAM::names_d), 
                             PCTILES=NULL, dig=3) {
  if (length(ST) == 1) {
    if ( substr(tolower(ST),1,2) == "us") {
      if (is.null(PCTILES)) {PCTILES <- c("mean", 0,5,50,80,90,95,99,100)}
      x <-  with(usastats, usastats[  PCTILE %in% PCTILES, c('REGION', 'PCTILE', varnames)])
      x[ , varnames] <- round(x[ , varnames], dig)
      rownames(x) <- NULL
      return(x)
    }
  }
  
  if (is.null(PCTILES)) {
    if (missing(ST)) {
      # if all states by default, then just show means
      PCTILES <- "mean"
    } else {
      # if 1 or more states specified, but pctiles not specified, show these defaults:
      PCTILES <- c("mean", 0,5,50,80,90,95,99,100)
    }
  }
  x <-  with(statestats, statestats[REGION %in% ST & PCTILE %in% PCTILES, c('REGION', 'PCTILE', varnames)])
  x[ , varnames] <- round(x[ , varnames], dig)
  rownames(x) <- NULL
  x
}
################################################################################ #


#' statestats_queryd - convenient way to see mean, pctiles of DEMOG indicators from lookup table
#' 
#' @inherit statestats_query params return description details seealso examples
#' 
#' @export
#'
statestats_queryd <- function(ST=sort(unique(EJAM::statestats$REGION)), 
                              varnames= c(EJAM::names_d, EJAM::names_d_subgroups_nh), 
                              PCTILES=NULL, dig=4) { 
  if (is.null(PCTILES))  {
    statestats_query(ST = ST, varnames = varnames, PCTILES = NULL, dig = dig)
  } else {
    statestats_query(ST = ST, varnames = varnames, PCTILES = PCTILES, dig = dig)
  }
}
################################################################################ #


#' statestats_querye - convenient way to see mean, pctiles of ENVIRONMENTAL indicators from lookup table
#' 
#' @inherit statestats_query params return description details seealso examples
#' 
#' @export
#'
statestats_querye <- function(ST=sort(unique(EJAM::statestats$REGION)), varnames=  EJAM::names_e , 
                              PCTILES=NULL, dig=4) { 
  if (is.null(PCTILES))  {
    statestats_query(ST = ST, varnames = varnames, PCTILES = NULL, dig = dig)
  } else {
    statestats_query(ST = ST, varnames = varnames, PCTILES = PCTILES, dig = dig)
  }
}
################################################################################ #
################################################################################ #
################################################################################ #



#' usastats_query - convenient way to see US mean, pctiles of Envt and Demog indicators in lookup table
#' 
#' @details A long list of variables: usastats_query(intersect(EJAM::names_all_r,  names(EJAM::usastats)))
#' @inherit statestats_query return description details seealso examples
#' @param varnames names of columns in lookup table, like "proximity.rmp"
#' @param PCTILES vector of percentiles 0-100 and/or "mean"
#' @param dig how many digits to round to
#' 
#' @export
#'
usastats_query   <- function(varnames = c(EJAM::names_e, EJAM::names_d, EJAM::names_d_subgroups_nh),
                             PCTILES = NULL, 
                             dig = 4) {
  statestats_query(ST = "us", varnames = varnames, PCTILES = PCTILES, dig = dig)
  ## see all total counts too not just US means for just demographics not envt, including subgroups:
  # t(round( ustotals2(bg = blockgroupstats),2))
  # t(round(rbind(
  #   ustotals2(bg=ejscreen package file bg22), 
  #    ustotals2(bg = blockgroupstats)
  # ),3))
}
################################################################################ #


#' usastats_querye - convenient way to see US mean, pctiles of ENVIRONMENTAL indicators in lookup table
#' 
#' @inherit statestats_query return description details seealso examples
#' @param varnames names of columns in lookup table, like "proximity.rmp"
#' @param PCTILES vector of percentiles 0-100 and/or "mean"
#' @param dig how many digits to round to
#' 
#' @export
#'
usastats_querye  <- function(varnames=EJAM::names_e, PCTILES=NULL, dig=4) {
   statestats_query(ST = "us", varnames = varnames, PCTILES = PCTILES, dig = dig)
}
################################################################################ #


#' usastats_queryd - convenient way to see US mean, pctiles of DEMOGRAPHIC indicators in lookup table
#' 
#' @inherit statestats_query return description details seealso examples
#' @param varnames names of columns in lookup table, like "proximity.rmp"
#' @param PCTILES vector of percentiles 0-100 and/or "mean"
#' @param dig how many digits to round to
#' 
#' @export
#'
usastats_queryd  <- function(varnames=c(EJAM::names_d, EJAM::names_d_subgroups_nh), PCTILES=NULL, dig=4) {
  statestats_query(ST = "us", varnames = varnames, PCTILES = PCTILES, dig = dig)
}
################################################################################ #


#' usastats_means - convenient way to see US MEANS of ENVIRONMENTAL and DEMOGRAPHIC indicators
#' 
#' @inheritParams usastats_query
#' 
#' @export
#'
usastats_means <- function(varnames=c(EJAM::names_e, EJAM::names_d, EJAM::names_d_subgroups_nh), PCTILES=NULL, dig=4) {
  x = usastats_query(PCTILES = "mean", varnames = varnames, dig = dig)
  x$REGION = NULL; x$PCTILE = NULL # so t(x) wont make everything into character class
  x = t(x)
  colnames(x) = "us.avg"
  return(x)
}


################################################################################ #
################################################################################ #
################################################################################ #





### variations that were drafted that do roughly the same kinds of things:





lookup_means <- function(varnames = intersect(EJAM::names_all_r,  names(EJAM::usastats)), zones = "USA") {
  
  #    examples
  #
  #  t(round(lookup_means(), 3))
  
  
  # 1 variable, 1 zone
  if (length(varnames) == 1 & length(zones) == 1) {
    return(
      lookup_mean1zone(varname = varnames, zone = zones)
    )
  }
  
  # N variables, 1 zone
  if (length(varnames) > 1 & length(zones) == 1) {
    return(
      lookup_mean1zone(varname = varnames, zone = zones)
    )
  }
  
  # 1 variable, N zones
  if (length(varnames) == 1 & length(zones) > 1) {
    return(
      sapply(zones, FUN = function(z) lookup_mean1zone(varname = varnames, zone = z))
    )
  }
  
  # N variables, N zones
  if (length(varnames) > 1 & length(zones) > 1) {
    
    # N variables, N zones - ASSUME THEY WANT A MATRIX (NOT A SINGLE VECTOR BASED ON THE NTH VARIABLE IN THE NTH ZONE, LIKE VIA MAPPLY)
    # so this can return a matrix, one row per zone, one col per variable name
    # varnames is a fixed vector that is used for all zones.
    
    return(
      sapply(zones, FUN = function(z) lookup_mean1zone(varname = varnames, zone = z))
    )
  }
}
################################################################################ #


lookup_mean1zone <- function(varname,
                             zone = "USA",
                             lookup = EJAM::usastats) {
  if (!("USA" %in% zone)) {lookup <- EJAM::statestats}
  if (length(zone) > 1) {stop('can only report on one state or USA overall')}
  lookup[lookup$PCTILE == "mean" & lookup$REGION == zone, varname]
}
################################################################################ #

