#' Get order of variable names to sort by, as seen in EJScreen Community Report
#'
#' @param varnames vector of indicator variables names from blockgroupstats, bgej, etc., 
#'   such as "pm", "pctlowinc", "pctile.EJ.DISPARITY.traffic.score.eo" etc.
#'   and others as found in names_all_r, or specific subsets of those like 
#'   in c(names_d, names_d_subgroups, names_e) and  
#'   c(names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile)
#' @param s1 name of column in map_headernames to get sort info from
#' @param s2 optional like s1 but secondary to s1
#' @param s3 optional tertiary
#' @return vector as from order(), to be used in sorting a data.frame for example
#' @export
#'
#' @examples 
#'   cbind(table_order_variables(c(names_d, names_d_subgroups, names_e)))
#'   
#'   out <- testoutput_ejamit_10pts_1miles
#'   vars <- out$formatted[ , 'indicator']
#'   vars <- fixcolnames(vars, 'long', 'r')
#'   out$formatted[table_order_variables(vars), ]
#'   
#'
table_order_variables <- function(varnames, s1 = 'ejscreensort', s2 = 'sortvarlistEJSCREENREPORT', s3 = 'sort_within_varlistEJSCREENREPORT') {
  
  # s1 <- as.numeric(map_headernames[ , s1][match(varnames, map_headernames$rname, nomatch = 1e7)])
  s1 <- as.numeric(fixcolnames(varnames, 'r', s1)) ; s1[is.na(s1)] <- 1e7 # NA is last in order() anyway
 
   if (!is.null(s2)) {
  # s2 <- as.numeric(map_headernames[ , s2][match(varnames, map_headernames$rname, nomatch = 1e7)])
  s2  <- as.numeric(fixcolnames(varnames, 'r', s2))  ; s2[is.na(s2)] <- 1e7
   } else {
     s2 <- rep(0, length(varnames))
   }
  if (!is.null(s3)) {
  # s3 <- as.numeric(map_headernames[ , s3][match(varnames, map_headernames$rname, nomatch = 1e7)])
  s3  <- as.numeric(fixcolnames(varnames, 'r', s3))   ; s3[is.na(s3)] <- 1e7
  } else {
    s3 <- rep(0, length(varnames))
  }
  ejscreenorder <- order(s1, s2, s3)
  return(ejscreenorder)
}
