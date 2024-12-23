

#' ejam2means - quick look at averages, via ejamit() results
#'
#' @param ejamitout as from ejamit()
#' @param vars colnames in ejamitout$results_bysite
#' @param w weights
#'
#' @return means in a useful format
#' @examples
#' # pctiles
#' ejam2quantiles(out,  vars = names_e_state_pctile, topx_insteadof_pctile = F) 
#' # ratios  
#' ejam2quantiles(out, vars = names_e_ratio_to_state_avg,  topx_insteadof_pctile = T)
#' # see the quantiles and mean together
#' t(rbind(
#'   ejam2quantiles(out, vars = names_e_ratio_to_state_avg, probs = 0.5, topx_insteadof_pctile = T), 
#'   AvgPerson = ejam2means(out, vars = names_e_ratio_to_state_avg)))
#' # these tell you the same thing:
#' out$results_summarized$keystats[rownames(out$results_summarized$keystats) %in% names_e_ratio_to_state_avg, ]
#' ejam2means(out, vars = names_e_ratio_to_state_avg)
#' 
#' @export
#'
ejam2means = function(ejamitout, vars = names_these, w = dt$pop) { 
  
  dt = ejamitout
  #   dt = testoutput_ejamit_100pts_1miles$results_bysite ; vars = names_these;  w = dt$pop
  
  # another source - has avg site and avg person already
  print(
    ejamitout$results_summarized$keystats[ rownames(ejamitout$results_summarized$keystats) %in% vars, ]
  )
  
  if ("results_bysite" %in% names(dt)) {
    # if output of ejamit() was provided instead of just 1 table
    dt <- dt$results_bysite
  }
  if (is.data.frame(dt) && !is.data.table(dt)) {setDT(dt ); wasdt = FALSE} else {wasdt = TRUE}
  
  y = (sapply(dt[, ..vars], function(x) collapse::fmean(x, w = w)))
  # named vector
  if (!wasdt) {setDF(dt)}
  
  y = data.frame(t(y))
  
  y = table_signif_round_x100(y) # table_round(table_x100(y, names_pct_as_fraction_ejamit))  # 
  
  junk = capture.output({printable <- cbind(wtdmean = lapply(as.list(y), print))  }) 
  rownames(printable) <- fixcolnames(rownames(printable), 'r', 'long')
  print(printable); cat('\n\n')
  
  invisible(y)
}
######################################## #


#' ejam2quantiles - draft - info about quantiles of places or people
#'
#' @param ejamitout as from ejamit()
#' @param vars colnames in ejamitout$results_bysite
#' @param w weights
#' @param probs as written is always the pctile, but default is to report on the top X%, or 100-probs
#' @param topx_insteadof_pctile logical
#'
#' @return info about quantiles of places or people
#' 
#' @export
#' @keywords internal
#'
ejam2quantiles = function(ejamitout, vars = names_e_state_pctile, w = dt$pop, probs = c(50, 90)/100, topx_insteadof_pctile = T) { 

  # complicated to explain but can be useful - work in progress / drafted
  # probs as written is always the pctile, but default is to report on the top X%, or 100-probs
  
  
  dt = ejamitout
  
  #   dt = testoutput_ejamit_100pts_1miles$results_bysite ; vars = names_these;  w = dt$pop; probs = c(50,95)/100
  ## paste0("state.pctile.", names_these)
  
  if ("results_bysite" %in% names(dt)) {
    # if output of ejamit() was provided instead of just 1 table
    dt <- dt$results_bysite
  }
  if (missing(w)) {w = dt$pop}
  if (is.data.frame(dt) && !is.data.table(dt)) {setDT(dt ); wasdt = FALSE} else {wasdt = TRUE}
  
  if (length(probs) < 2) { # not coded to handle this case so add one for now
    if (probs == 0.5) {
      probs = c(probs, 0.9)
      message("reporting on median (50%) also") 
    } else { 
      probs = c(0.5, probs)
      message("reporting on median (50%) also")
    }
  }
  
  y = (sapply(dt[, ..vars], function(x) collapse::fquantile(x, probs, w = w)))
  if (!wasdt) {setDF(dt)}
  
  y = data.frame(y)
  
  y = table_signif_round_x100(y) # table_round(fix_pctcols_x100(y, names_pct_as_fraction_ejamit)) #  
  
  printable = t(y) # all get rounded back to 2 decimals but it is just a quick look
  rownames(printable) <- fixcolnames(rownames(printable), 'r', 'long')
  if (topx_insteadof_pctile) {
    nopct = function(pctile_of_100) {gsub("%", "", pctile_of_100)}
    pctile_of_100 = nopct(colnames(printable))
    pctile2topx = function(pctile_of_100) paste0(100 - as.numeric(pctile_of_100),"%")
    topx = pctile2topx(pctile_of_100)
    colnames(printable) <- topx
    cat("\n\n  Top X% of the residents here have indicators at or above these levels: \n\n")
  } else {
    cat("\n\n  X% of the residents here have indicators at or below these levels: \n\n")
  }
  print(printable); cat('\n\n')
  
  # if pctile vs pctile
  # x percent of pop here is at p pctile overall, or in the top T% of US/ST, T=100-p,  expect x = T, and striking if T << x
  top_pctile = 100 * probs[length(probs)]
  headerpct = ifelse(topx_insteadof_pctile, 100 - top_pctile, top_pctile)
  xhere = printable[ , NCOL(printable)] # values in the column
  worstvalue = xhere[which.max(xhere)]
  worstindicator = rownames(printable)[which.max(xhere)]
  directiontxt = ifelse(topx_insteadof_pctile, "at least", "up to")
  directiontxt2 = ifelse(topx_insteadof_pctile, "up to", "at least")
  cat("For example,\n", colnames(printable)[NCOL(printable)], "of residents here have",  # same as headerpct
      directiontxt, worstvalue, "as their", worstindicator, "value\n",
      "and", 100 - headerpct, "% of residents here have", directiontxt2, worstvalue, "\n\n")   
  
  invisible(y)
}
######################################## #
