# # exported functions here:
# 
# ejamit_compare_distances() - to see indicators by distance, for sites as a whole (overall)
# ejamit_compare_distances2plot - to plot that
# ejamit_compare_distances_fulloutput - to get full set of ejamit() results at each distance

#    To compare stats across a few distances at just one site, use 
#    ejamit_compare_distances(sitepoints[1, ])
#    or alternatively, 
#    out_bydistance2results_bydistance_bysite

# # internal:
# 
# distance_trends - which indicator has strongest trend with distance?
# out_bydistance2results_bydistance - USED  BY ejamit_compare_distances() to see indicators by distance, for sites as a whole (overall)
# out_bydistance2results_bysite_bydistance - unused - to check one distance, all sites
# out_bydistance2results_bydistance_bysite - unused - to check one site, all distances
################################################################### #


#' Compare ejamit() full results for more than one radius
#' Helper used by ejamit_compare_distances() to run ejamit() once per radius, get FULL ejamit() output list per radius
#' @details You typically only need [ejamit_compare_distances()],
#'   which gives you just the summary overall at each distance,
#'   but if you want to retain the full outputs of ejamit() at each distance, 
#'   such as results for every site at every distances,
#'   you can use [ejamit_compare_distances_fulloutput()] and then to extract a slice of results, 
#'   use helper functions like
#'   
#'   * [out_bydistance2results_bydistance()]
#'   * [out_bydistance2results_bydistance_bysite()]
#'   * [out_bydistance2results_bysite_bydistance()]
#'   
#' @param sitepoints like for [ejamit()]
#' @param radii vector of radius values like 1:3 for [ejamit()]
#' @param donuts_not_cumulative set to TRUE to get results on each ring not each full circle
#' @param ... passed to [ejamit()]
#' @param quiet passed to [ejamit()]
#' @param silentinteractive passed to [ejamit()]
#' 
#' @examples 
#'   radii <- c(1,2,3,6,10)
#'   pts <- testpoints_10
#'   \dontrun{
#'   x <- ejamit_compare_distances_fulloutput(pts, radii = radii)
#'   }
#' @seealso wrapper [ejamit_compare_distances()] and helpers 
#'   [out_bydistance2results_bydistance()] [out_bydistance2results_bydistance_bysite()] [out_bydistance2results_bysite_bydistance()]

#' @return list you can think of as "out_bydistance"  
#'   where each element is the full output of ejamit() for 1 radius
#'
#' @export
#'
ejamit_compare_distances_fulloutput <- function(sitepoints, radii = c(1,2,3), 
                                                donuts_not_cumulative = FALSE,
                                                quiet = TRUE, silentinteractive = TRUE, 
                                                ...) {
  radii <- as.numeric(radii) # 1:2 is integer but c(1,2) is numeric, so this keeps class of radius.miles in output consistently always numeric, like for ejamit()
  if (length(radii) > 30 || max(radii, na.rm = T) > 31 || any(!is.numeric(radii)) || any(radii < 0.5)) {
    stop("radii must be numbers between 0.5 and 31, and 30 different radii is the max allowed.")
  }
  # accept interactively or from filepath or from object, infer lat/lon cols, assign ejam_uniq_id
  suppressMessages({
    # avoid message about ejam_uniq_id already being there since this might be called by ejamit_compare_distances() which already added ejam_uniq_id
    # but do sitepoints_from_any() here even if can be redundant, in case someone wants to use only ejamit_compare_distances_fulloutput() alone (not via ejamit_compare_distances() function)
    sitepoints <- sitepoints_from_any(sitepoints)
  })
  out_bydistance <- list()
  
  if (donuts_not_cumulative) {
    radius_donut_lower_edge <- c(0, radii[1:(length(radii) - 1)])
  } else {
    radius_donut_lower_edge <- rep(0, length(radii))
  }
  
  for (i in seq_along(radii)) {
    out_bydistance[[i]] <- ejamit(sitepoints = sitepoints,
                                  radius = radii[i],
                                  radius_donut_lower_edge = radius_donut_lower_edge[i],
                                  # donuts_not_cumulative = donuts_not_cumulative,
                                  quiet = quiet, silentinteractive = silentinteractive, 
                                  ...)
    # z[[i]] <- out[[i]]$results_overall # if were only retaining one row per distance, not results_bysite or results_bybg_people etc.
  }
  # z <- rbindlist(z)
  return(out_bydistance)
}
#################################################################### #


# internal function to extract/reassemble tables from ejamit_compare_distances_fulloutput()
# USED  BY ejamit_compare_distances() 

#' Extract summary from list of ejamit() runs at multiple distances
#' Get a table, one row per distance. Overall summary, not each site.
#' @details This will compile a results_bydistance table from 
#'   output of ejamit_compare_distances_fulloutput(),
#'   using the ejamit()$results_overall for each distance.
#' @param out_bydistance list of tables that is output of [ejamit_compare_distances_fulloutput()]
#' @return a table you can call results_bydistance,
#'   that is like ejamit()$results_overall, but that 
#'   has 1 row per distance (radius or buffer width)
#' @seealso [ejamit_compare_distances()] [ejamit_compare_distances_fulloutput()]
#' 
#' @keywords internal
#' 
out_bydistance2results_bydistance <- function(out_bydistance) {
  # bydistance_from_out_bydistance <- function(out_bydistance) {
  results_bydistance <- list()
  for (i in seq_along(out_bydistance)) {
    results_bydistance[[i]] <- out_bydistance[[i]]$results_overall
  }
  return(
    rbindlist(results_bydistance)  # a table, one row per distance (just overall, no site-bysite info)
  )
}
#################################################################### #


#' Compare EJAM results overall for more than one radius
#' Run ejamit() once per radius, get a summary table with a row per radius
#' 
#' @param sitepoints like for [ejamit()]
#' @param radii optional, vector of radius values like 1:3 for [ejamit()]
#' @param donuts_not_cumulative optional, when implemented, if set TRUE,
#'   would return results on areas in each "donut" or ring that is a distance bin,
#'   such as for 
#'   
#'   0 < R <= radii[1]
#'   
#'   radii[1] < R <= radii[2]
#'   
#'   etc.
#'   
#' @param ... optional, passed to [ejamit()]
#' 
#' @param quiet optional, passed to [ejamit()]
#' @param silentinteractive optional, passed to [ejamit()]
#' 
#' @param plot optional logical, set FALSE to avoid plotting
#' @param myvars optional, for plot, see default value
#' @param ylab optional, for plot, see default value
#' @param ylim optional, for plot, see default value
#' @param n optional, how many of the indicators to report on (printed to console),
#'   when reporting which indicators most strongly increase as radius decreases.
#' 
#' @examples 
#'   radii <- c(convert_units(5,"km","miles"), convert_units(50,"km","miles"))
#'   radii <- 1:10
#'   radii <- c(1, 10)
#'   pts <- testpoints_100
#'   pts <- testpoints_10
#'   
#'   bydist <- ejamit_compare_distances(pts, radii = radii)
#'   ejamit_compare_distances2plot(bydist, myvars = c(
#'     "ratio.to.avg.pctlowinc", "ratio.to.avg.pcthisp", "ratio.to.avg.pctnhba"))
#'   
#'   names(bydist) <- fixcolnames(names(bydist), "r", "shortlabel")
#'   
#'   
#' @seealso [plot_distance_by_pctd()], [distance_by_group()],
#'   and [ejamit_compare_distances_fulloutput()]
#' 
#' @return data.table you can call results_bydistance,
#'   like ejamit()$results_overall but with one row per radius
#'
#' @export
#'
ejamit_compare_distances <- function(sitepoints, radii = c(1,2,3), 
                                     donuts_not_cumulative = FALSE,
                                     quiet = TRUE, silentinteractive = TRUE, 
                                     plot = TRUE, 
                                     myvars = names_d_subgroups_ratio_to_state_avg, 
                                     ylab = "Ratio of Avg. within X miles to Avg. Statewide or Nationwide",
                                     ylim = c(0, 5),
                                     n = 1,
                                     ...) {
  
  # Options for what format to return 
  # from this function or similarly ejamit_compare_types_of_places()
  #  (you can convert between these but the code is a little awkward, so one should be the primary output format, and helper functions might reformat between these)
  #
  # results_bydistance table,
  # like results_overall but 1 row per distance.
  # Creating that is a bit awkward, via rbindlist(a list of the extracted list for all i of out[[i]]$resultsoverall )
  # and 
  # results_bybg_bysite, 1 table already covers all the distances and all the sites.
  #
  # plus one of these formats for the rest:
  #
  # A) MATCHES ejamit() FORMAT, but for one DISTANCE at a time:
  # LIST OF DISTANCE-SPECIFIC ejamit() lists,
  # A list of objects, one per radius, 
  #   each being exactly like the normal ejamit() output list, 
  #  for that 1 distance.
  #
  # B) Easier way to focus on 1 DISTANCE at a time:
  # LIST OF DISTANCE-SPECIFIC results_bysite TABLES,
  # where ROWS ARE SITES
  #
  # C) Easy way to focus on 1 SITE at a time:
  #  LIST OF SITE-SPECIFIC TABLES, 
  #  where ROWS ARE DISTANCES
  #  for that one site
  ################################################################################## #
  
  # Check and clean input points and radii
  sitepoints <- sitepoints_from_any(sitepoints) # also done again in ejamit_compare_distances_fulloutput()
  radii <- as.numeric(radii) # 1:2 is integer but c(1,2) is numeric, so this keeps class of radius.miles in output consistently always numeric, like for ejamit(). also done in ejamit_compare_distances_fulloutput() so this is redundant but if refactoring replaced the fulloutput approach this would ensure it still happens
  if (length(radii) > 30 || max(radii, na.rm = T) > 31 || any(!is.numeric(radii)) || any(radii < 0.5)) {
    stop("radii must be numbers between 0.5 and 31, and 30 different radii is the max allowed.")
  }

  # Run ejamit() in loop once per distance (not super efficient since dont need all the output tables)
  out_bydistance <- ejamit_compare_distances_fulloutput(
    sitepoints = sitepoints,
    radii = radii,
    donuts_not_cumulative = donuts_not_cumulative, 
    quiet = quiet, silentinteractive = silentinteractive,
    ...
  )
  ## old way retained only results_overall within loop which is more efficient but less generalized:
  # z = list()
  # for (i in seq_along(radii)) {
  #     z[[i]] <- ejamit(sitepoints = sitepoints, radius = radii[i], 
  #                            quiet = quiet, silentinteractive = silentinteractive, 
  #                            ...)$results_overall
  # }
  # results_bydistance = rbindlist(z)
  
  # Extract just results_overall for each distance, and rbind
  results_bydistance <- out_bydistance2results_bydistance(out_bydistance) 
  
  # Print key results in RStudio console
  availvars <- myvars[myvars %in% names(results_bydistance)]
  if (length(availvars) > 0) {
  shown = data.frame(round(t(results_bydistance[, ..availvars]), 1))
  colnames(shown) <- radii
  rownames(shown) <- fixcolnames(rownames(shown), 'r', 'shortlabel')
  cat("\n")
  print(shown)
  
  # Print notable results in RStudio console and show
  # Plot by distance
  
  if (plot) {
    cat("\n Indicators that most strongly get larger as you get closer: \n")
    
    print(
      ejamit_compare_distances2plot(results_bydistance,
                                    myvars = availvars,
                                    ylab = ylab,
                                    ylim = ylim,
                                    n = n
      )
    )
  }
  # return one row per distance (like results_overall, but for each distance)
  }
  return(results_bydistance)
}
#################################################################### #

## not exported or documented. internal function.

#' Which indicators fall most as proximity does? (i.e., are higher if closer to site)
#' Which variables have strongest trend with distance based on slope of linear fit
#' @details Used by [ejamit_compare_distances2plot()] which is used by [ejamit_compare_distances()]
#' @param results_bydistance data.frame of a few indicators, no other columns, taken from output of [ejamit_compare_distances()]
#' @param myvars optional, vector of some colnames of results_bydistance
#' @param radii optional vector - taken from results_bydistance$radius.miles
#' @param n optional number of indicators to list. n=3 would mean show the top 3.
#' @examples distance_trends(ejamit_compare_distances(testpoints_10, radii = c(1,3)))
#' @return vector of text names of indicators
#'   
distance_trends <- function(results_bydistance, 
                            myvars = names_d_subgroups_ratio_to_state_avg,
                            radii, # results_bydistance$radius.miles,
                            n = 1) {
  if (missing(radii)) {radii <- as.numeric(results_bydistance$radius.miles)}
  availvars <- myvars[myvars %in% names(results_bydistance)]
  if (length(availvars) > 0) {
  results_bydistance <- data.frame(results_bydistance)[, myvars]
  } else {
    results_bydistance <- data.frame(results_bydistance)
  }
  # REPORT STRONGEST TREND
  # fit line to points and report which has the most negative slope, e.g.
  slopes <- coef(lm(as.matrix(results_bydistance) ~ radii ))[2, ]
  topn <- head(sort(slopes), n)
  topn <- fixcolnames(names(topn), "r", "long")
  cat("\nIndicators that increase the most as you get closer: \n\n")
  return(topn)
}
#################################################################### #


#' plot indicators as a function of distance from point(s)
#' plot results of ejamit_compare_distances()
#'
#' @param results_bydistance output of [ejamit_compare_distances()], table similar to
#'   ejamit()$results_overall except it has one row per distance.
#' @param myvars optional, see [ejamit_compare_distances()]
#' @param ylab  optional, see [ejamit_compare_distances()]
#' @param ylim  optional, see [ejamit_compare_distances()]
#' @param n  optional, see [ejamit_compare_distances()]
#' @param ... optional, passed to plot
#' @return text vector length n, naming which indicators most strongly
#'   increase as you get closer to the site(s)
#'   
#' @export
#'
ejamit_compare_distances2plot <- function(results_bydistance, 
                                          myvars = names_d_subgroups_ratio_to_state_avg, 
                                          ylab = "Ratio of Avg. within X miles to Avg. Statewide or Nationwide",
                                          ylim = c(0, 5),
                                          n = 1, 
                                          ...) {
  
  # ejamit_compare_distances2plot()
  #  could be renamed  results_bydistance2plot() ?
  # or plot_results_bydistance() ?
  
  results_bydistance <- data.frame(results_bydistance)
  x <- results_bydistance[, myvars] 
  radii <- results_bydistance$radius.miles
  
  # REPORT TOP n INDICATORS (strongest trend with distance)
  
  topn <- distance_trends(x, myvars = myvars, radii = radii, n = n)
  # results_bydistance was already subsetted above for plot so distance_trends() doing it again is a bit redundant but need to pass myvars to be sure default is not used
  
  # PLOT
  
  subtext <- paste0(topn[1], " is the indicator increasing most as distance shrinks")
  mycolors <- palette.colors(NCOL(x))
  for (i in 1:(NCOL(x))) {
    if (i == 1) {
      plot(x = radii, y = x[, i], type = "b", col = mycolors[i],
           sub = subtext,
           # x labels are radii
           xlab = "Distance (radius) in miles",
           ylab = ylab,
           xlim = c(0, max(radii)),
           ylim = ylim, 
           ...)
    } else {
      points(x = radii, y = x[, i], type = "b", col = mycolors[i])
    }
  }
  legend("topright", legend = paste0("", fixcolnames(names(x), "r", "shortlabel"),""), fill = palette.colors(NCOL(x)))
  abline(h = 1, col = "lightgray")
  
  return(topn)
  # or maybe
  # return(fixcolnames(names(x), "r", "shortlabel"))
}
#################################################################### #


#' Barplot comparing ejamit_compare_distances() results for more than one radius
#' @param results_bydistance output of [ejamit_compare_distances()], table similar to
#'   ejamit()$results_overall except it has one row per distance.
#' @param myvars optional, see [ejamit_compare_distances()]
#' @param ylab  optional, see [ejamit_compare_distances()]
#' @param ylim  optional, see [ejamit_compare_distances()]
#' @param n  optional, see [ejamit_compare_distances()]
#' @param ... optional, passed to plot
#' @return text vector length n, naming which indicators most strongly
#'   increase as you get closer to the site(s)
#'   
#' @export
#' 
ejam2barplot_distances <- function(results_bydistance, 
                                   myvars = names_d_subgroups_ratio_to_state_avg, 
                                   ylab = "Ratio of Avg. within X miles to Avg. Statewide or Nationwide",
                                   ylim = c(0, 5),
                                   n = 1, 
                                   ...) {
  if (!is.data.frame(results_bydistance)) {
    stop("results_bydistance must be a data.frame output of ejamit_compare_distances()")
  }
  ejamit_compare_distances2plot(
    results_bydistance = results_bydistance, 
    myvars = myvars, 
    ylab = ylab,
    ylim = ylim,
    n = n,
    ... = ...
  )
  }
#################################################################### #


# internal function to extract/reassemble tables from ejamit_compare_distances_fulloutput()
# MAY NOT GET USED 

#' A way to focus on 1 DISTANCE (RADIUS) at a time (after a multidistance run), for the list of sites
#' Get a list of tables, one per distance. Each table has a row per site.
#' @details This function might not be used at all. 
#'   Extract results_bysite for each distance 
#'   from list of ejamit() runs at multiple distances
#' @param out_bydistance list of tables that is output of [ejamit_compare_distances_fulloutput()]
#' @return a LIST you can call results_bysite_bydistance (not results_bydistance_bysite),
#'   that is a list where each element is ejamit()$results_bysite for
#'   a unique distance (radius or buffer width)
#' @seealso [ejamit_compare_distances()]  [ejamit_compare_distances_fulloutput()]
#'   and internal functions [out_bydistance2results_bydistance()] [out_bydistance2results_bydistance_bysite()]
#'   
#' @keywords internal
#' 
out_bydistance2results_bysite_bydistance <- function(out_bydistance) {
  
  results_bysite_bydistance <- list()
  for (i in seq_along(out_bydistance)) {
    results_bysite_bydistance[[i]] <- out_bydistance[[i]]$results_bysite
  }
  return(
    (results_bysite_bydistance)  # a list of tables, one per distance. each table has a row per site.
  )
}
#################################################################### #


# internal function to extract/reassemble tables from ejamit_compare_distances_fulloutput()
# MAY NOT GET USED 

#' A way to focus on 1 SITE at a time, for a few radius choices
#' Get a list of tables, one per site. Each table has a row per distance.
#' @details This function might not be used at all. 
#'   Extract/create results_bydistance for each site, 
#'   from list of ejamit() runs at multiple distances
#' @param out_bydistance list of tables that is output of [ejamit_compare_distances_fulloutput()]
#' @return a LIST you can call results_bydistance_bysite (not results_bysite_bydistance),
#'   that is a list where each element is a table for 1 site (ejam_uniq_id value)
#'   with one row per distance (radius or buffer width). 
#'   This table is in the same format as the output of [ejamit_compare_distances()]
#'   or the internal function out_bydistance2results_bydistance()
#' @seealso [ejamit_compare_distances()] [ejamit_compare_distances_fulloutput()]
#'   and internal functions [out_bydistance2results_bysite_bydistance()] [out_bydistance2results_bydistance()]
#'   
#' @keywords internal
#' 
out_bydistance2results_bydistance_bysite <- function(out_bydistance) {
  
  results_bydistance_bysite <- list()
  
  # siteids <- sort(unique(out_bydistance[[1]]$results_bysite$ejam_uniq_id))
  ## note that rownumber and ejam_uniq_id are NOT the same! this will compile them 
  # by row not by ejam_uniq_id, so it must be used carefully
  
  for (sitenumber in 1:NROW(out_bydistance[[1]]$results_bysite)) {
    # one site, get all distances in a table for that site
    onesite_bydistance <- list()
    for (distance_i in seq_along(out_bydistance)) {
      onesite_bydistance[[distance_i]] <- out_bydistance[[distance_i]]$results_bysite[sitenumber, ]
    }
    results_bydistance_bysite[[sitenumber]] <- rbindlist(onesite_bydistance)
  }
  return(
    results_bydistance_bysite  # a list of tables, one per site found in results_bysite (by checking the first distance only)
  )
}
#################################################################### #
