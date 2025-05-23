
#' utility to check if a variable or term is in map_headernames and where
#'
#' @param query variable name or fragment (or regular expression) to look for in map_headernames
#'   columns, looking within just column names listed in cols_with_names.
#'   Or a vector of query terms in which case this returns one column per query term.
#' @param ignore.case optional, like in grepl()
#' @param cols_with_names optional, colnames of map_headernames to check in
#' @param exact set to TRUE for only exact matches
#' @return data.frame of info about where query was found and how many hits.
#' @examples
#' varin_map_headernames("spanish")
#' varin_map_headernames("lowinc")
#' varin_map_headernames("pop")
#' varin_map_headernames("POV", ignore.case = T)
#' varin_map_headernames("POV", ignore.case = F)
#' 
#' varin_map_headernames( "traffic.score", exact = T)
#' 
#' varin_map_headernames( "traffic" )
#' 
#' t(varinfo("traffic.score", 
#'   info = c("oldname","apiname", "acsname" ,"csvname", 
#'   "basevarname", 'shortlabel', 'longname', 'varlist')))
#' 
#' @seealso [varinfo()]
#' 
#' @export
#' @keywords internal
#'
varin_map_headernames <- function(query = "lowinc", ignore.case = TRUE, exact = FALSE,
                                  cols_with_names = c("oldname",
                                                      "apiname", 
                                                      "api_synonym",
                                                      "acsname" ,
                                                      "csvname",
                                                      "ejscreen_csv",
                                                      "rname",
                                                      "topic_root_term",
                                                      "basevarname",
                                                      "denominator",
                                                      "shortlabel",
                                                      
                                                      "longname",
                                                      "description",
                                                      "csvlongname",
                                                      "api_description",
                                                      "acs_description",
                                                      "varlist"
                                  )) {
  
  cols_with_names <- cols_with_names[cols_with_names %in% names(map_headernames)]
  mh <- map_headernames[, cols_with_names]
  
  if (exact) {query <- paste0("^", query, "$")}
  
  out <- list()
  
  for (i in seq_along(query)) {
    
    if (ignore.case) {
      exactmatch = sapply(mh, function(x) {
        tolower(query[i]) %in% 
          tolower(x)
      })
    } else {
      exactmatch = sapply(mh, function(x) {
        query[i] %in% x
      })
    }
    
    out[[i]] <- data.frame(
      exactmatch = exactmatch,
      
      # grepmatch.anycase = sapply(mh, function(x) {
      # any(grepl(query, as.vector(x), ignore.case = ignore.case))
      # }),
      # grepmatch.anycase.hitlist = sapply(mh, function(x) {
      #   paste0(grep(query, as.vector(x), ignore.case = ignore.case, value = T), collapse = ",")
      # }),
      
      grepmatch.hitcount = sapply(mh, function(x) {
        sum(grepl(query[i], as.vector(x), ignore.case = ignore.case))
      }), 
      
      example = sapply(mh, function(x) {
        (grep(query[i], as.vector(x), ignore.case = ignore.case, value = TRUE)[1])
      }) 
    )
    
    if (length(query) > 1) {
      out[[i]] <- out[[i]][, "grepmatch.hitcount", drop = FALSE] # drop exactmatch and example columns if multiple terms in query
    }
  }
  if (length(query) > 1) {
    out  <- do.call(cbind, out)
    if (exact) {
      # get rid of the ^ and $ that had been added at start and end of each query term to get back to the parameter as provided
      query <-  gsub( ".{1}(.*).{1}", "\\1", query)
    }
    colnames(out) <- query
    return( out)
  } else {
    return(out[[1]])
  }
}
