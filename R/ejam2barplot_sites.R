################################################################# #


#' barplot comparing sites on 1 indicator, based on table of site data
#' a quick way to plot a calculated variable at each site, which ejam2barplot_sites() can't
#' @param results_bysite table like from ejamit()$results_bysite, a table of sites,
#'  one row per site, column names at least varname (and "ejam_uniq_id" if names.arg not specified) 
#' @param varname name of a column in results_bysite, bar height
#' @param names.arg optional vector of labels on the bars, like short site names or IDs
#' @param main optional, for barplot
#' @param xlab optional, for barplot
#' @param ylab optional, for barplot, plain English version of varname, indicator that is bar height
#' @param sortby set to FALSE if you want to have no sorting, or to an increasing vector
#'   that provides the sort order
#' @param topn optional, show only the top n sites 
#' @param ... passed to barplot()
#' 
#' @inherit ejam2barplot_sites examples 
#' 
#' @return  same as [barplot()]
#' @seealso [ejam2barplot_sites()]
#' 
#' @export
#'
plot_barplot_sites <- function(results_bysite, varname = "pctlowinc", names.arg = NULL, 
                               main = "Comparison of Sites", xlab = "Sites", ylab = NULL, 
                               sortby = NULL, topn = 5, ...) {
  
  if (missing(results_bysite) | !is.data.frame(results_bysite)) {
    stop("results_bysite table required, such as from ejamit()$results_bysite")
  }
  if (!(varname %in% colnames(results_bysite))) {
    stop("varname must be in colnames(results_bysite)")
  }
  if (is.null(ylab) | missing(ylab)) {ylab <- fixcolnames(varname, "r", "shortlabel")}
  if (is.null(names.arg) | missing(names.arg)) {names.arg <- results_bysite$ejam_uniq_id}
  # sites <- data.frame(results_bysite)
  h <- data.frame(results_bysite)[, varname]
  if (is.null(sortby) | missing(sortby)) {
    sortby <- h
  }
  
  if (length(sortby) == 1 && is.logical(sortby) && sortby == FALSE) {
    # sortby = -1 * seq_len(NROW(sites)) # do not sort it
  } else {
    h <-                 h[order(sortby, decreasing = TRUE)]
    names.arg <- names.arg[order(sortby, decreasing = TRUE)]
  }
  h = h[1:topn]
  names.arg = names.arg[1:topn]
  
  barplot(height = h,
          names.arg = names.arg, 
          main = main, xlab = xlab, ylab = ylab, 
          ...)
  
}
################################################################# #


#' barplot comparing groups of sites on 1 indicator, based on table of grouped site data
#' 
#' @param results_bytype table like from ejamit_compare_types_of_places()$results_bytype,
#'  a table of site groups,
#'  one row per type (group), column names at least varname (and "ejam_uniq_id" if names.arg not specified) 
#' @param varname name of a column in results_bytype, bar height
#' @param names.arg optional vector of labels on the bars, like the types of sites represented by each group
#' @param main optional, for barplot
#' @param xlab optional, for barplot
#' @param ylab optional, for barplot, plain English version of varname, indicator that is bar height
#' @param sortby set to FALSE if you want to have no sorting, or to an increasing vector
#'   that provides the sort order
#' @param topn optional, show only the top n groups (site types) -- Does not show all
#'   by default -- only shows top n groups.
#' @param ... passed to barplot()
#' @seealso [ejam2barplot_sitegroups()] 
#' 
#' @inherit ejam2barplot_sitegroups examples 
#'
#' @return same as [barplot()]
#' 
#' @export
#'
plot_barplot_sitegroups = function(results_bytype, varname = "Demog.Index", names.arg = NULL, 
                                   main = "Sites by Type", xlab = "Groups or Types of Sites", ylab = NULL, 
                                   sortby = NULL, topn = 10, ...) {
  if (missing(results_bytype) | !is.data.frame(results_bytype)) {
    stop("results_bytype table required, such as from ejamit_compare_types_of_places()$results_bytype")
  }
  if (!(varname %in% colnames(results_bytype))) {
    stop("varname must be in colnames(results_bytype)")
  }
  if (is.null(ylab) | missing(ylab)) {ylab <- fixcolnames(varname, "r", "shortlabel")}
  if (is.null(names.arg) | missing(names.arg)) {names.arg <- results_bytype$types}
  
  plot_barplot_sites(results_bysite = results_bytype, varname = varname, names.arg = names.arg, 
                     main = main, xlab = xlab, ylab = ylab, 
                     sortby = sortby, topn = topn, ...)
}
################################################################# #


#' Barplot comparing sites on 1 indicator, based on full output of ejamit()
#' easy high-level function for getting a quick look at top few sites
#' @param ejamitout list like output of ejamit(), where one element is table of sites,
#'  one row per site, column names at least varname (and "ejam_uniq_id" if names.arg not specified) 
#' @param varname name of a column in results_bysite, bar height
#' @param names.arg optional vector of labels on the bars, like short site names or IDs
#' @param main optional, for barplot
#' @param xlab optional, for barplot
#' @param ylab optional, for barplot, plain English version of varname, indicator that is bar height
#' @param sortby set to FALSE if you want to have no sorting, or to an increasing vector
#'   that provides the sort order
#' @param topn optional, show only the top n sites -- Does not show all sites
#'   by default -- only shows top n sites.
#' @param ... passed to barplot()
#'
#' @return same as [barplot()] 
#' @seealso [plot_barplot_sites()] 
#' @examples 
#' # Quickly compare top few sites by population count nearby
#' out <- copy(testoutput_ejamit_10pts_1miles)
#' ejam2barplot_sites(out, "pop")
#' 
#' # Show all 10,
#' ejam2barplot_sites(out, "traffic.score", topn = 10, cex.names = 0.8)
#' 
#' # Sort by site id
#' ejam2barplot_sites(out, "blockcount_near_site", topn = 10,
#'   sortby = -1 * out$results_bysite$ejam_uniq_id)
#' 
#' # Plot a calculated variable
#' sites <- copy(out$results_bysite)
#' sites$log_traffic = log10(sites$traffic.score)
#' plot_barplot_sites(sites, "log_traffic", ylab = "Traffic Score (log10 scale)", topn = 10)
#' 
#' # On a large monitor, 100 sites with legible labels if the window is wide enough
#' ejam2barplot_sites(testoutput_ejamit_100pts_1miles, topn = 100, cex.names = 0.4)
#' 
#' @export
#' 
ejam2barplot_sites <- function(ejamitout, varname = "pctlowinc", names.arg = NULL, 
                               main = "Comparison of Sites", xlab = "Sites", ylab = NULL, 
                               sortby = NULL, topn = 5,
                               ...) {
  
  if (!("results_bysite" %in% names(ejamitout))) {
    stop("ejamitout must be a list such as from ejamit(), with one element of the list being a table named results_bysite")
  }
  plot_barplot_sites(
    results_bysite = ejamitout$results_bysite, 
    varname = varname, 
    names.arg = names.arg, 
    main = main, xlab = xlab, ylab = ylab, 
    sortby = sortby, topn = topn, 
    ...
  )
}
################################################################# #


#' Barplot comparing groups of sites on 1 indicator, for output of ejamit_compare_types_of_places()
#' easy high-level function for getting a quick look at top few groups of sites
#' @param ejamitout list that is output of ejamit_compare_types_of_places(), where one element is 
#'   a table named results_bytype
#' @param varname name of a column in results_bytype, bar height
#' @param names.arg optional vector of labels on the bars, like the types of sites represented by each group
#' @param main optional, for barplot
#' @param xlab optional, for barplot
#' @param ylab optional, for barplot, plain English version of varname, indicator that is bar height
#' @param sortby set to FALSE if you want to have no sorting, or to an increasing vector
#'   that provides the sort order
#' @param topn optional, show only the top n groups (site types) -- Does not show all
#'   by default -- only shows top n groups.
#' @param ... passed to barplot()
#' @seealso [plot_barplot_sitegroups()] [ejamit_compare_distances2plot()]
#' @details see [ejamit_compare_types_of_places()] for more examples 
#' @examples 
#'  out <- ejamit_compare_types_of_places(testpoints_10[1:4, ], 
#'    typeofsite <- c("A", "B", "B", "C"))
#'    cbind(Rows_or_length = sapply(out, NROW))
#'   
#'  ejam2barplot_sitegroups(out, "sitecount_unique", topn=3, sortby = F)
#'  
#' @return same as [barplot()]
#' 
#' @export
#'
ejam2barplot_sitegroups <- function(ejamitout, varname = "pctlowinc", names.arg = NULL, 
                                    main = "Sites by Type", xlab = "Groups or Types of Sites", ylab = NULL, 
                                    sortby = NULL, topn = 10, ...) {
  
  if (!("results_bytype" %in% names(ejamitout))) {
    stop("ejamitout must be a list such as from ejamit_compare_types_of_places(), with one element of the list being a table named results_bytype")
  }
  if (is.null(names.arg)) {names.arg <-  ejamitout$types}
  plot_barplot_sitegroups(
    results_bytype = ejamitout$results_bytype, 
    varname = varname, 
    names.arg = names.arg, 
    main = main, xlab = xlab, ylab = ylab, 
    sortby = sortby, topn = topn, 
    ...
  )
}
################################################################# #
