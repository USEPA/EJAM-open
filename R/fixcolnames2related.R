
#' Get name of related avg, pctile, or ratio variable name
#' 
#' Given names_d, e.g., returns names_d_ratio_to_state_avg
#' 
#' @details Given basic variable name(s) like "pctlowinc" or names_e,
#'   see what the related variable names are for storing the 
#'   US or State percentiles, averages, or ratios to averages 
#'   of the given variables. 
#'   
#'   Only works for variable names among these: 
#'   
#'   c(names_e, names_d, names_d_subgroups)
#'   
#' @param namesnow vector of one or more basic Envt or Demog indicator variable names
#'    found in c(names_e, names_d, names_d_subgroups)
#' @param relatedtype One of "usavg", "stateavg", "uspctile", "statepctile", "usratio", "stateratio"
#'    (but not any of the other values among 
#'    unique(map_headernames$vartype) since those give ambiguous answers).
#'
#' @seealso [varinfo()] [fixcolnames()]
#' @return vector as long as namesnow (or just returns namesnow if relatedtype is invalid)
#'
#' @examples 
#' names_d
#' fixcolnames2related(names_d, 'stateratio')
#' names_d_ratio_to_state_avg
#' fixcolnames2related(names_e, "stateavg")
#' fixcolnames2related(names_e, "usvag")
#' paste0("avg.", names_e)
#' fixcolnames2related(names_e, "usratio")
#' # names_ej # does not work with this as input
#' # fixcolnames2related(names_ej, "uspctile") # does not return names_ej_pctile
#'   
#' @keywords internal
#'
fixcolnames2related <- function(namesnow, relatedtype = c(
  'usavg', 'stateavg', 'uspctile', 'statepctile', 'usratio', 'stateratio')) {
  
  if (!all(namesnow %in% c(names_e, names_d, names_d_subgroups))) {warning('Does not work for namesnow not among c(names_e, names_d, names_d_subgroups)')}
  if (!all(relatedtype %in% c("usavg", "stateavg", "uspctile", "statepctile", "usratio", "stateratio"))) {warning('Does not work for relatedtype not among c("usavg", "stateavg", "uspctile", "statepctile", "usratio", "stateratio")')}
  fixcolnames(
    namesnow, 
    'topic_root_term', 
    'r', 
    mapping_for_names = map_headernames[map_headernames$vartype == relatedtype, ]
  )
}
