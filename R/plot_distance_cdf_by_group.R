
#' Each groups distribution of distances
#'
#' @description SLOW / needs to be optimized.
#'   CDF Line Plots of cumulative share of each demographic group, within each distance
#'
#' @param results_bybg_people data.table from doaggregate()$results_bybg_people
#' @param radius_miles miles radius that was max distance analyzed
#' @param subgroups_type optional, can be set to "nh" or "alone".
#'   Specifies types of race ethnicity subgroups to use for demogvarname
#'   but only if demogvarname is not specified as a parameter.
#'   If neither is specified it tries to use default_subgroups_type
#'   if that is a variable set by global.R, since it cannot check the reactive variable input$subgroups_type
#'   outside the context of the web app.
#' @param demogvarname optional way to specify names of columns to use from results_bybg_people,
#'   e.g., c("pctlowinc", "pctmin"), or  namez$d, or
#'   could be a vector of subgroups such as namez$d_subgroups_nh that includes "pctnhba" etc.
#'   or namez$d_subgroups_alone that includes "pctba" etc.,
#'   but if demogvarname is not specified here as a parameter,
#'   this info could also be specified by the subgroups_type parameter here.
#'   If neither is specified, the function will try to use a default
#' @param demoglabel friendly text names for labelling graphic, like "Low income residents"
#' @param colorlist colors like "red" etc. for the demographic groups of interest
#' @param coloroverall color like "gray" for everyone as a whole
#' @param returnwhat If returnwhat is "table", invisibly returns a
#'   full table of sorted distances of blockgroups,
#'   cumulative count of demog groups at that block group's distance.
#'   If returnwhat is "plotfilename" then it returns the full path including filename of a .png in a tempdir
#'   If returnwhat is "plot" then it returns the plot object as needed for table_xls_format()
#' @param ... other parameters passed through to [points()]
#' @seealso [distance_by_group()] [ejamit()] for examples
#' @aliases plot_distance_cdf_by_group
#' @return see returnwhat parameter
#' @examples
#'  y <- ejamit(testpoints_100, radius = 3)
#'  
#'  # see barplot and table comparing groups to see which are closer to sites analyzed
#'  plot_distance_mean_by_group(y$results_bybg_people) # or distance_mean_by_group() synonym
#'  
#'  # table - proximity of sites for just one demog group vs rest of population
#'  print(distance_by_group(y$results_bybg_people,
#'    demogvarname = 'pctlowinc'))
#'    
#'  # plot cumulative share of group by distance vs overall population
#'   distance_by_group_plot(y$results_bybg_people,
#'      demogvarname = 'pctlowinc' )
#'      
#'  # plot cum. shares for two groups  
#'  # about 14% of black and 12% of asian residents have a site within 1 mile. 
#'  # 29% vs 21% have a site within 1.5 miles.
#'  round(xyz[findInterval(c(1, 1.5),  xyz$dist), ], 3) 
#'  
#'  # plot is too busy for all groups at once so this is a way to tap through them 1 by 1
#'  these = c(names_d, names_d_subgroups)
#'  for (i in 1:length(these)) {
#'    readline("press any key to see the next plot")
#'    print(distance_by_group_plot(y$results_bybg_people, demogvarname = these[i]) )
#'  }
#'  
#'
#' @export
#'
distance_by_group_plot <- function(
    results_bybg_people = NULL,
    radius_miles=round(max(
      results_bybg_people$distance_min_avgperson[!is.infinite(
        results_bybg_people$distance_min_avgperson)], na.rm = T), table_rounding_info("distance_min_avgperson")),
    subgroups_type = NULL, # e.g.
    demogvarname = NULL,  # e.g. namez$d # see note above about this param
    demoglabel = NULL,
    colorlist = colorspace::diverging_hcl(length(demogvarname)),
    coloroverall ="black",
    returnwhat = "table",
    ...) {

  if (missing(results_bybg_people)) {
    warning("missing results_bybg_people - returning test example results / plot")
    if (exists("testoutput_doaggregate_1000pts_1miles")) {
      results_bybg_people <- testoutput_doaggregate_1000pts_1miles$results_bybg_people
    } else {return(NA)}
  }

  # Figure out what demog variables to use for plot
  if (is.null(demogvarname)) {
    if (!is.null(subgroups_type)) {
      # user specified a type
      if (subgroups_type == "nh")    {demogvarname <- names_d_subgroups_nh}    # namez$d_subgroups_nh}
      if (subgroups_type == "alone") {demogvarname <- names_d_subgroups_alone} # namez$d_subgroups_alone}
      # and plotting "both" is not visually useful and nh makes more sense
      if (!(subgroups_type %in% c('nh','alone'))) {
        # user specified an invalid type here
        warning(subgroups_type, ' is not a valid subgroups_type - trying to use a default value instead')
        if (exists("default_subgroups_type")) {
          #  user did not provide valid type but global.R provides a default
          demogvarname <- namez$d_subgroups_nh # default if default_subgroups_type from global.R is something other than one of these 2:
          if (default_subgroups_type == "nh")    {demogvarname <- names_d_subgroups_nh}     # namez$d_subgroups_nh}
          if (default_subgroups_type == "alone") {demogvarname <- names_d_subgroups_alone } # namez$d_subgroups_alone}
        } else {
          # no type provided by user or global.R
          # warning('trying to use a default for demogvarname')
          demogvarname <- names_d_subgroups_nh  # namez$d_subgroups_nh
        }
      }
    } else {
      # use default demogvarname
      demogvarname <- c(names_d, names_d_subgroups)  # too many for this plot? this was default in related function
      # demogvarname <- names_d[1]  # just 1
      # demogvarname <- names_d_subgroups # a few
    }
  } else {
    # validate this user-provided demogvarname below to confirm those colnames exists
  }

  # Figure out what labels to use for those demog variables
  if (!is.null(demoglabel)) {
    demoglabel <- fixcolnames(demogvarname, 'r', 'shortlabel') # renames those it is able to, using   fixcolnames
  }
  if (length(demoglabel) != length(demogvarname)) {
    warning("length of demoglabel and demogvarname must be the same - trying to use defaults for labels instead")
    demoglabel <- fixcolnames(demogvarname, 'r', 'shortlabel') # renames those it is able to, using  fixcolnames
  }

  if (is.list(results_bybg_people) & ("results_bybg_people" %in% names(results_bybg_people))) {
    # assume it was a mistake and they meant to provide out$results_bybg_people not out itself
    results_bybg_people <- results_bybg_people$results_bybg_people
  }
  if (!is.data.frame(results_bybg_people)) {
    warning('results_bybg_people must be a data.frame or data.table - returning empty results')
    x = data.table(dist = NA,
                   cumall_d    = NA,
                   cumall_nond = NA)
    cnames = paste0(c('cumall_d','cumall_nond'), demogvarname)
    setnames(x, c('dist', cnames))
    return(x)
  }

  miss <- setdiff( c(demogvarname , 'distance_min_avgperson'), names(results_bybg_people))
  if (length(miss) > 0) {
    warning('These must be colnames of results_bybg_people but were not found: ', paste0(miss , collapse = ", "))
    x = data.table(dist = NA,
                   cumall_d    = NA,
                   cumall_nond = NA)
    cnames = paste0(c('cumall_d','cumall_nond'), demogvarname)
    setnames(x, c('dist', cnames))
    return(x)
  }

  x <- results_bybg_people # not a full slow copy... done by reference using data.table::

  # plot is too slow for huge datasets and can just plot a random sample of points if so huge:
  if (NROW(x) > 5000) {
    x <- x[sample(1:NROW(x), 5000, replace = F), ]
    warning('plotting just a random sample of 5,000 of these block groups to show pattern quickly')
  }

  # SHOULD IT USE distance_avg or distance_min_avgperson ?? ***
# maybe should only work with x[, c(..demogvarname , 'distance_min_avgperson')]  not all columns now? ***
  # if Inf distance in that min_avgperson column (not sure why it happens), just use distance_min which seems to have valid numbers.
  fixthese = (is.infinite(x$distance_min_avgperson) | is.na(x$distance_min_avgperson))
  if (any(fixthese)) {
    x$distance_min_avgperson[fixthese  ]  <-  x$distance_min[fixthese]
  }


  # for a bg near 2+ sites, use the distance that is shorter (the distance to the closest of those sites)
  x[ , distance_min_avgperson := min(distance_min_avgperson, na.rm = TRUE), by = "bgid"] #  very few duplicate bgid values should be here - just when 2 sites near a bg
  # remove duplicated blockgroups now that you saved distance to closest site for each bg. since here we do not need stats site by site, so use shorter distance for any bg that is near 2+ sites.
  x <- unique(x, by = "bgid")

  # specify demographic groups and reference (overall)
  x[ , overall := 1] # this represents the entire population, or 100 percent of the bg, to compare to x percent in any given subgroup of interest
  demogvarname <- c("overall", demogvarname)

  # countvarname <- gsub("^min$","mins", gsub("pct","",demogvarname))
  # this will not work for other indicator names, only those like pctlowinc as percentage and lowinc as count
  # maybe instead use   names_d_count (but note none for Demog.Index, Demog.Index.Supp),   names_d_subgroups_count

  x[ , overall := pop] # the count not the percent
  demoglabel <- c("Everyone", demoglabel)

  # for each demog group, calculate the cumulative share of all people in the group, as distance increases among all the blockgroups
  # what is the most efficient way to do this  ?
  # we actually can just use counts that are in x, not calc counts from pop * pct
  x[ , pop := as.numeric(pop)]

  data.table::setorder(x, distance_min_avgperson)

  # x[ , .SD := lapply(.SD, FUN = as.numeric), .SDcols = demogvarname]  ???
  cumdata <- x[ , lapply(.SD, FUN = function(z) collapse::fcumsum(pop * z, fill = T) / sum(pop * z, na.rm = TRUE)),
                .SDcols = demogvarname]
  cumdata$dist <- x$distance_min_avgperson # has NA values

  # if (returnwhat == "plot") { # if plot is done using ggplot2, it can be returned and then saved etc via   ggsave()

  plot(cumdata$dist, 100 * cumdata[ , overall],
       col = coloroverall,
       pch = NA_integer_, type = 'l', lty = "dotted",
       main = "Share of each Demographic Group Residing at Various Distances from Sites",
       xlab = "Distance from nearest site (for the avg resident in the blockgroup)",
       ylab = paste0("Of all the residents within ", radius_miles," miles, what % have a site within X miles?"),
       ylim = c(0, 100))
if (length(demogvarname) > 1) {
  for (i in 2:length(demogvarname)) {

    # distance_cdf_by_group_plot  is not written in a way that makes it easy to vectorize, so this could be rewritten

    data.table::setDF(cumdata)
    points(cumdata$dist, 100 *  cumdata[ , demogvarname[i]],
           col = colorlist[i - 1],
           pch = c(0:6,15:25, 7:14)[i], # various base R shapes for the points
           ...)
  }
}
  legend("topleft", legend =  demoglabel, lty =  c("dotted", rep("solid", length(colorlist))) , pt.bg = c(coloroverall, colorlist),  col = c(coloroverall, colorlist), pch =  c(NA_integer_, 1:6,15:25, 7:14)[1:length(demogvarname)])

  # }
  if (returnwhat == "plotfilename") {
    fname = "distance_cdf.png"
    mytempdir = tempdir()
    png(file.path(mytempdir, fname), width = 2000, height = 1000)
    plot(cumdata$dist, 100 * cumdata[ , overall],
         col = coloroverall,
         pch = NA_integer_, type = 'l', lty = "dotted",
         main = "Share of each Demographic Group Residing at Various Distances from Sites",
         xlab = "Distance from nearest site (for the avg resident in the blockgroup)",
         ylab = paste0("Of all the residents within ", radius_miles," miles, what % have a site within X miles?"),
         ylim = c(0, 100))

    for (i in 2:length(demogvarname)) {

      # distance_cdf_by_group_plot  is not written in a way that makes it easy to vectorize, so this could be rewritten

      data.table::setDF(cumdata)
      points(cumdata$dist, 100 *  cumdata[ , demogvarname[i]],
             col = colorlist[i - 1],
             pch = c(0:6,15:25, 7:14)[i], # various base R shapes for the points
             ...)
    }

    legend("topleft", legend =  demoglabel, lty =  c("dotted", rep("solid", length(colorlist))) , pt.bg = c(coloroverall, colorlist),  col = c(coloroverall, colorlist), pch =  c(NA_integer_, 1:6,15:25, 7:14)[1:length(demogvarname)])
    dev.off()
    return(file.path(mytempdir, fname))
  }

  if (returnwhat == "table") {
    invisible(cumdata)
  }

}
############################################################################################################# #

#' What percentage of this demographic group's population lives less than X miles from a site?
#'
#' @description This plots the cumulative share of residents found within each distance,
#'   for a single demographic group.
#'
#'   This function, distance_cdf_by_group_plot(), is based on ejamit()$results_bybg_people,
#'   which provides only block group resolution information about distance.
#'   For block resolution analysis of distance by group, see [plot_distance_by_pctd()].
#'
#' @details The function distance_cdf_by_group_plot is SLOW - ***needs to be optimized
#'
#' @param results_bybg_people data.table from doaggregate()$results_bybg_people
#' @param radius_miles miles radius that was max distance analyzed
#' @param demogvarname name of column in results_bybg_people, e.g., "pctlowinc"
#' @param demoglabel friendly text name for labelling graphic, like "Low income residents"
#' @param color1 color like "red" for demographic group of interest
#' @param color2 color like "gray" for everyone else
#' @seealso [distance_by_group()] [getblocksnearbyviaQuadTree()] for examples
#' @inherit distance_by_group_plot examples
#' @return invisibly returns full table of sorted distances of blockgroups, cumulative count of demog group at that block group's distance,
#' and cumulative count of everyone else in that block group
#'
#' @export
#'
distance_cdf_by_group_plot <- function(results_bybg_people,
                                       radius_miles=round(max(results_bybg_people$distance_min_avgperson, na.rm = T), table_rounding_info("distance_min_avgperson")),
                                       demogvarname="Demog.Index", demoglabel=demogvarname,
                                       color1="red", color2="black") {

  if (is.list(results_bybg_people) & ("results_bybg_people" %in% names(results_bybg_people))) {
    # assume it was a mistake and they meant to provide out$results_bybg_people not out itself
    results_bybg_people <- results_bybg_people$results_bybg_people
  }
  if (length(demogvarname) > 1) {
    warning('this function handles one demog group at a time - trying to use first one only')
    demogvarname <- demogvarname[1]
  }
  miss <- setdiff( c(demogvarname , 'distance_min_avgperson'), names(results_bybg_people))
  if (length(miss) > 0) {
    warning('These must be colnames of results_bybg_people but were not found: ', paste0(miss , collapse = ", "))
    x = data.table(dist = NA,
                   cumall_d    = NA,
                   cumall_nond = NA)
    cnames = paste0(c('cumall_d','cumall_nond'), demogvarname)
    setnames(x, c('dist', cnames))
    return(x)
  }
  x <- results_bybg_people # not a full slow copy... done by reference using data.table::
  data.table::setorder(x, distance_min_avgperson)

  # remove duplicated blockgroups, since here we do not need stats site by site, so use shorter distance for any bg that is near 2+ sites.
  x[ , distance_min_avgperson := min(distance_min_avgperson, na.rm = TRUE), by = "bgid"]
  x <- unique(x, by = "bgid")

  # should recode this to use directly the counts of D instead of recreating them via pop * .SD

  cumdata <- x[ , .(
    dist = distance_min_avgperson,
    # count_d = pop *   .SD,
    cumall_d    = collapse::fcumsum(pop *   .SD,       fill = TRUE) / sum(pop *        .SD,  na.rm = TRUE) ,
    cumall_nond = collapse::fcumsum(pop * (1 -   .SD), fill = TRUE) / sum(pop * (1 -   .SD), na.rm = TRUE)),
    .SDcols = demogvarname]
  # actual names will be dist, cumall_d.pctlowinc, cumall_nond.pctlowinc  for example, but using partial colname below still works:
  plot(cumdata$dist, 100 * cumdata$cumall_d,
       col = color1,
       main = "Share of each Demographic Group Residing at Various Distances from Facilities",
       xlab = "Living within X miles of facilities",
       ylab = paste0("% of all residents within ", radius_miles," miles"),
       ylim = c(0,100)
  )
  points(cumdata$dist, 100 * cumdata$cumall_nond, col = color2)
  legend("topleft", legend = c(demoglabel, paste0("All other residents")), fill = c(color1, color2))
  cat('This takes a very long time to plot for 1,000 sites, e.g.... please wait... \n\n')
  print(  distance_by_group(x, demogvarname = demogvarname, demoglabel = demoglabel) )
  invisible(cumdata)
}
############################################################################################################# #


#' alias for distance_by_group_plot
#' @return see [distance_by_group_plot()]
#' @inherit distance_by_group_plot
#'
#' @export
#'
plot_distance_cdf_by_group <- distance_by_group_plot    # function(...) {distance_by_group_plot(...)}

# distance_by_group_plot is a synonym for plot_distance_cdf_by_group

