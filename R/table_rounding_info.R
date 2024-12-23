
#' How many decimal places to round to for given variable(s)
#'
#' @param var vector of variable names such as c("pctlowinc", "pm") or c(names_d, names_d_subgroups)
#' @param varnametype which column of map_headernames to use when looking for var, like "rname" or "api" or "long"
#' @seealso [table_signif_round_x100()] [table_signif()] [table_round()] [table_x100()]
#' @return named vector same size as var, with var as names.
#' @examples
#'   table_rounding_info("pm")
#'   table_round(8.252345, "pm")
#'   table_round(8, "pm")
#'   
#'   cbind(table_rounding_info(names_all_r), fixcolnames(names_all_r, "r", "long"))
#'  
#' @keywords internal
#'
table_rounding_info <- function(var, varnametype="rname") {
  
  # Also see the internal helper function  round2nearest_n()  which lets you explicitly round to nearest 100, e.g.
  
  as.numeric(  # in case it was still stored as character in map_headernames
    as.vector(unlist(varinfo(var = var, info = "decimals", varnametype = varnametype)))
  )
}
