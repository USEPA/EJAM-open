
#' See which of the lists of names a single term appears in
#' 
#' @details EJAM::namez has a list of lists of names used for indicators or variables, such as
#'   namez$d_subgroups_state_pctile which is a vector of terms like
#'     "state.pctile.pcthisp", "state.pctile.pctnhba", etc.
#' @param x term, like part or all of a variable name, such as state.avg
#' @param exact whether to look for exact matches
#' @param grepmatching whether to look for matches via grep (partial match)
#' @param ignore.case.exact whether to ignore capitalization in exact matches
#' @param ignore.case.grep passed to grep as ignore.case param
#' @param keylists if true, only report for the key lists not friendly, all, these, need types.
#' @param exactonly to limit output to rows with exact matches
#'
#' @seealso [varinfo()]
#' @return a data.frame of whichlist, exactmatch, grepmatch, and grephits (examples)
#' 
#' @examples  
#'    x <- names_whichlist("rsei", ignore.case.exact = T, ignore.case.grep = T)
#'    subset(x, !grepl("friendly", x$whichlist))
#'    
#'    subset(x,  grepl("friendly", x$whichlist))
#'    subset(namez, names(namez) != "all_r" & names(namez) %in%
#'       subset(x, x$grepmatch == "yes" & !grepl("friendly", x$whichlist))$whichlist  )
#'    grep("\\.eo$", namez$ej, value = T)
#'
#' @keywords internal
#' 
names_whichlist <- function(x, exact=T, grepmatching=T, ignore.case.exact=FALSE, ignore.case.grep = FALSE, keylists=F, exactonly=FALSE) {
  
  hits = vector()
  everylist = names(namez)
  hitlist = data.frame(
    whichlist = everylist,
    exactmatch  = rep( "", length(everylist)),
    grepmatch = rep("", length(everylist)),
    grephits =  rep("", length(everylist))
  )
  for (i in 1:length(namez)) {
    if (grepmatching & any(grepl(x, unlist(namez[i]), ignore.case = ignore.case.grep))) {
      # cat('grep match in', everylist[i], '\n')
      hits <- c(hits, everylist[i])
      hitlist[i,'grepmatch'] <- 'yes'
      examples = grep(x, unlist(namez[i]), ignore.case = ignore.case.grep, value = TRUE)
      if (length(examples) > 3) {examples <- c(examples[1:3], "etc.") }
      hitlist[i, 'grephits'] <- paste0( examples, collapse = ', ')
    }
    if (exact) {
      hit = FALSE
      if (ignore.case.exact) {
        if ( tolower(x) %in% tolower(unlist(namez[i])) ) { hit <- TRUE}
      } else {
        if (x %in% unlist(namez[i])) {hit <- TRUE}
      }
      if ( hit) {
        # cat('exact match in', everylist[i], '\n')
        hits <- c(hits, everylist[i])
        hitlist[i,'exactmatch'] <- 'yes'
      }
    }
  }
  if (keylists) {
    hitlist <- hitlist[hitlist$whichlist %in% grep('friendly|all|these|need|^state_pctile$', names(namez), value = T, invert = T),]
  }
  if (exactonly) {
    return(hitlist[hitlist$exactmatch == 'yes',]) 
  } else {
    return(hitlist)
  }
}
##################################################################### #


#' See which lists of names the given indicator names are in
#' 
#' @param x vector of names (query terms)
#' @param ... passed to names_whichlist()
#'
#' @seealso [varinfo()]
#' @return a list of sets of names
#' 
#' @keywords internal
#' 
names_whichlist_multi = function(x, ...) {
  
  out = list()
  for (i in 1:length(x)) {
  out[[i]] <- names_whichlist(x[i], ...)  
  }
  # out <- do.call(rbind, out)
  return(out)
}
##################################################################### #


#' See which key lists of names the given indicator names are in
#' 
#' @param x vector of names
#' @param ... passed to names_whichlist_multi()
#'
#' @seealso [varinfo()]
#' @return vector maybe
#' 
#' @keywords internal
#' 
names_whichlist_multi_key = function(x, ...) {
  
  sapply( names_whichlist_multi( x, exactonly = T, keylists = T, ...) , function(x) x$whichlist)
}
