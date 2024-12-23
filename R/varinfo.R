
#' Get metadata about a variable, like its type, short name, long definition, decimal places, etc.
#'
#' This is just a way to query map_headernames, which has info about each indicator or
#' variable used in EJAM.
#' @details See map_headernames for what kind of information is available there.
#' But if a variable appears twice+ in var or in map_headernames, info returned only for the 1st row of those.
#' 
#' Developers may wish to know about these functions,
#' several of which are not exported:
#' 
#' - [varinfo()]
#' - [varname2varcategory_ejam()]
#' - [varname2vartype_ejam()]
#' - [table_rounding_info()]
#' - EJAM table_signif_round_x100()
#' 
#' - [fixcolnames()]
#' - [fixcolnames_infer()]
#' - EJAM fixcolnames2related()
#' - EJAM fixmapheadernamescolname()
#' - EJAM is.numericish()
#' 

#' - EJAM varin_map_headernames()
#' - EJAM namesbyvarlist()
#' - EJAM names_whichlist()
#' - EJAM names_whichlist_multi()
#' - EJAM names_whichlist_multi_key()
#' - EJAM formula_varname()
#' 
#' and the various related data objects like map_headernames and namez
#' 
#' @param var vector of variable names such as c("pctlowinc", "pm") or c(names_d, names_d_subgroups)
#'   (and must be found in the column of map_headernames indicated by varnametype parameter below).
#'
#' @param info types of metadata/info needed, such as "decimals", "long", etc.
#'   which should be among colnames of map_headernames,
#'   or alias like "long" as allowed by [fixcolnames()]
#'
#' @param varnametype optional. colname of map_headernames to use when looking for var,
#'   like "rname" or "api" or "long"
#'
#' @seealso [fixcolnames()] [table_rounding_info()] 
#' @return data.frame of 1 or more rows, 1 or more columns, where
#'
#'  rowsnames are var (indicators like "pctmin")
#'
#'  colnames are info (metadata   like "decimals")
#'
#'  Cells of table are metadata such as what type of indicator is that var, how many
#'  decimal places of rounding should be displayed for it in tables, etc.
#'
#'  Results can be character, numeric, etc. depending on what info is requested
#'
#' @examples
#' varinfo("traffic.score", "decimals")
#' varinfo(names_d, "long")
#' myvars <- c(names_d, names_d_subgroups, names_e)
#' myinfo <- "percentage"
#' cbind(  is.a.percentage = varinfo(myvars, myinfo) )
#' cbind(varinfo(names_all_r, "pctile."))
#' myinfo <- "long"
#' cbind(varinfo(myvars, myinfo) )
#' table_rounding_info(names_e)
#' 
#' varinfo(
#'  var = c(names_these, names_d_pctile),
#'  info = c(
#'  "topic_root_term", "varcategory", "vartype", "percentage", "pctile.", "calculation_type"
#' ))
#'
#' varinfo(names_all_r, c("varcategory", "varlist", "in_api", "in_bgcsv"))
#'
#' cbind(
#'   namez$d, 
#'   names_d, 
#'   varinfo(names_d, "varlist"), 
#'   usavg = unlist(avg.in.us[names_d]), 
#'   usavg_varname = EJAM:::fixcolnames2related(names_d, "usavg"), 
#'   categ = varname2varcategory_ejam(names_d)
#' )
#' 
#' names(map_headernames)
#' t(varinfo(names_d[1]))
#' 
#' @export
#' @keywords internal
#'
varinfo <- function(var = map_headernames$rname, info=colnames(map_headernames), varnametype="rname") {

  # rows    are var  (indicators like "pctmin")
  # columns are info (metadata   like "decimals")

  info_or_alias <- info
  info_true_name <- fixmapheadernamescolname(info_or_alias)
  x <- map_headernames[match(var, map_headernames[ , varnametype]), info_true_name, drop = FALSE]
  rownames(x) <- make.unique(var) # BUT NOTE if a variable appears twice+ in map_headernames, info returned only for the 1st row of those
  colnames(x) <- info_or_alias
  
  # convenient view in RStudio:
  if (NROW(x) <= 10 && NCOL(x) >= 10 && interactive()) {
    print(t(x))
  }
  
  return(x)
}
#################################################################### #

#
#     ## tests/ examples:
#     t1 <- t2 <- t3 <- t4 <- tt1 <- tt2 <- tt3 <- tt4 <- NA
#     x = "rname"
#     x = "long" # fails if just using varinfo2
#     t1 <- varinfo(var =   'pctmin',       info = 'decimals')
#     t2 <- varinfo(var = c('pctmin','pm'), info = 'decimals')
#     t3 <- varinfo(var =   'pctmin',       info = c('decimals', x))
#     t4 <- varinfo(var = c('pctmin','pm'), info = c('decimals', x))
#     t1; t2; t3; t4
#     str(t1); str(t2); str(t3); str(t4)
#
#     ################ #
#
#     t1 <- t2 <- t3 <- t4 <- tt1 <- tt2 <- tt3 <- tt4 <- NA
#     x <- c( "rname",
#             'api' , 'apiname',
#           'acs', 'acsname',
#             'csv' , 'csvname',
#             'r' , 'rname',
#             'original' ,
#              
#             'long', 'longname')
#     t1 <- varinfo(var =   'pctmin',       info = 'decimals')
#     t2 <- varinfo(var = names_all_r, info = 'decimals')
#     t3 <- varinfo(var =   'pctmin',       info = c('decimals', x))
#     t4 <- varinfo(var = names_all_r, info = c('decimals', x))
#     #t1; t2; t3; t4
#     #str(t1); str(t2); str(t3); str(t4)
#
#
# #################################################################### #

