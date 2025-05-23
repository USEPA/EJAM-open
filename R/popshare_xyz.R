##################################################################### # 

#' top X percent of sites account for what percent of residents?
#' 
#' What fraction of total population is accounted for by the top X percent of places?
#' @param pop vector of population totals across places,
#'   like out$results_bysite$pop where out is the output of ejamit()
#' @param x a fraction of 1, the share of all places (or a vector of values)
#' @param astext if TRUE, return text of description of results
#' @param dig rounding digits for text output
#' @return A fraction of 1 (or a vector of results) or text
#' @examples 
#'  x <- testdata_ejamit_output_100pts_1miles$results_bysite
#'  popshare_p_lives_at_what_pct(x$pop, p = 0.50, astext=TRUE)
#'  popshare_p_lives_at_what_n(  x$pop, p = c(0.50, 0.67, 0.80, 0.95))
#'  popshare_at_top_x_pct(       x$pop, x = c(0.25, 0.50, .90))
#'  popshare_at_top_n(           x$pop, n = c(1, 5, 10))
#'  
#' @export
#'
popshare_at_top_x_pct = function(pop, x=0.20, astext=FALSE, dig=0) {
  
  if (!is.vector(pop)) {
    warning('pop must be a vector')
    return(NULL)
  }
  
  pop = sort(pop,decreasing = T)
  frac = cumsum(pop) / sum(pop)
  share = quantile(frac, probs = x)
  
  sharetext <- paste0( paste0(round(100 * share, 0), "%"), collapse = ", ")
  xtext <- paste0( paste0(round(100 * x, dig), "%"), collapse = ", ")
  msg <- paste0(xtext, " of places account for ", sharetext, " of the total population")
  
  if (astext) {
    return(msg)
  } else {
    cat(msg)
    cat("\n\n")
    return(share)
  }
}
##################################################################### # 


#' top N sites account for what percent of residents?
#' 
#' What fraction of total population is accounted for by the top N places?
#' @param pop vector of population totals across places,
#'   like out$results_bysite$pop where out is the output of ejamit()
#' @param n the number of places to consider
#' @param astext if TRUE, return text of description of results
#' @param dig rounding digits for text output
#' @return A fraction of 1
#' @examples 
#'  x <- testdata_ejamit_output_100pts_1miles$results_bysite
#'  popshare_p_lives_at_what_pct(x$pop, p = 0.50, astext=TRUE)
#'  popshare_p_lives_at_what_n(  x$pop, p = c(0.50, 0.67, 0.80, 0.95))
#'  popshare_at_top_x_pct(       x$pop, x = c(0.25, 0.50, .90))
#'  popshare_at_top_n(           x$pop, n = c(1, 5, 10))
#'  
#' @export
#'
popshare_at_top_n = function(pop, n=10, astext=FALSE, dig=0) {
  
  if (!is.vector(pop)) {
    warning('pop must be a vector')
    return(NULL)
  }
  pop = sort(pop, decreasing = T)
  frac = cumsum(pop) / sum(pop)
  share = frac[n]
  
  sharetext <- paste0( paste0(round(100 * share, dig), "%"), collapse = ", ")
  ntext <- paste0( n,  collapse = ", ")
  msg <- paste0(ntext, " places account for ", sharetext, " of the total population")
  
  if (astext) {
    return(msg)
  } else {
    cat(msg)
    cat("\n\n")
    return(share)
  }
}
##################################################################### # 


#' how many sites account for P percent of residents?
#'
#' @param pop vector of population totals across places,
#'   like out$results_bysite$pop where out is the output of ejamit()
#' @param p share of population (0-1, fraction), vector of one or more
#' @param astext if TRUE, return text of description of results
#' @param dig rounding digits for text output
#'
#' @return vector of numbers of sites, or text about that
#' @examples 
#'  x <- testdata_ejamit_output_100pts_1miles$results_bysite
#'  popshare_p_lives_at_what_pct(x$pop, p = 0.50, astext=TRUE)
#'  popshare_p_lives_at_what_n(  x$pop, p = c(0.50, 0.67, 0.80, 0.95))
#'  popshare_at_top_x_pct(       x$pop, x = c(0.25, 0.50, .90))
#'  popshare_at_top_n(           x$pop, n = c(1, 5, 10))
#'  
#' @export
#'
popshare_p_lives_at_what_n <- function(pop, p, astext=FALSE, dig=0) {
  
  if (!is.vector(pop)) {
    warning('pop must be a vector')
    return(NULL)
  }
  
  pop = sort(pop, decreasing = T)
  frac = cumsum(pop) / sum(pop)
  n_low = findInterval(p, frac)
  n_high = n_low + 1
  p_low  = frac[n_low]
  p_high = frac[n_high]
  
  sharetext      <- paste0( paste0(round(100 * p,      dig), "%"), collapse = ", ") 
  sharetext_low  <- paste0( paste0(round(100 * p_low,  dig), "%"), collapse = ", ") 
  sharetext_high <- paste0( paste0(round(100 * p_high, dig), "%"), collapse = ", ") 
  
  ntext      <- paste0( n_low,  collapse = ", ")
  ntext_low  <- paste0( n_low,  collapse = ", ")
  ntext_high <- paste0( n_high, collapse = ", ")
  
  msg      <- paste0(ntext,      " places account for ", sharetext,      " of the total population (approx.)")
  msg_low  <- paste0(ntext_low,  " places account for ", sharetext_low,  " of the total population")
  msg_high <- paste0(ntext_high, " places account for ", sharetext_high, " of the total population")
  
  if (astext) {
    return(msg)
  } else {
    cat(paste0(msg_low, "\n", msg, "\n", msg_high))
    cat("\n\n")
    return(n_low)
  }
}
##################################################################### # 


#' what percent of sites account for P percent of residents?
#'
#' @param pop vector of population totals across places,
#'   like out$results_bysite$pop where out is the output of ejamit()
#' @param p share of population (0-1, fraction), vector of one or more
#' @param astext if TRUE, return text of description of results
#' @param dig rounding digits for text output
#'
#' @return vector of fractions 0-1 of all sites, or text about that
#' @examples 
#'  x <- testdata_ejamit_output_100pts_1miles$results_bysite
#'  popshare_p_lives_at_what_pct(x$pop, p = 0.50, astext=TRUE)
#'  popshare_p_lives_at_what_n(  x$pop, p = c(0.50, 0.67, 0.80, 0.95))
#'  popshare_at_top_x_pct(       x$pop, x = c(0.25, 0.50, .90))
#'  popshare_at_top_n(           x$pop, n = c(1, 5, 10))
#'  
#' @export
#'
popshare_p_lives_at_what_pct <- function(pop, p, astext=FALSE, dig=0) {
  
  if (!is.vector(pop)) {
    warning('pop must be a vector')
    return(NULL)
  }
  
  pop = sort(pop, decreasing = T)
  frac = cumsum(pop) / sum(pop)
  n_low = findInterval(p, frac)
  n_high = n_low + 1
  p_low  = frac[n_low]
  p_high = frac[n_high]
  
  siteshare      <- n_low  / length(pop)
  siteshare_low  <- n_low  / length(pop)
  siteshare_high <- n_high / length(pop)
  
  sharetext      <- paste0( paste0(round(100 * p,      dig), "%"), collapse = ", ") 
  sharetext_low  <- paste0( paste0(round(100 * p_low,  dig), "%"), collapse = ", ") 
  sharetext_high <- paste0( paste0(round(100 * p_high, dig), "%"), collapse = ", ") 
  
  sitesharetext      <- paste0(round(100 * siteshare,      dig), "%",  collapse = ", ")
  sitesharetext_low  <- paste0(round(100 * siteshare_low,  dig), "%",  collapse = ", ")
  sitesharetext_high <- paste0(round(100 * siteshare_high, dig), "%",  collapse = ", ")
  
  msg      <- paste0(sitesharetext,      " of places account for ", sharetext,      " of the total population (approx.)")
  msg_low  <- paste0(sitesharetext_low,  " of places account for ", sharetext_low,  " of the total population")
  msg_high <- paste0(sitesharetext_high, " of places account for ", sharetext_high, " of the total population")
  
  if (astext) {
    return(msg)
  } else {
    cat(paste0(msg_low, "\n", msg, "\n", msg_high))
    cat("\n\n")
    return(siteshare_low)
  }
}
##################################################################### # 
