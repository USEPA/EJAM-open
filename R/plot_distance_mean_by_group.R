

#' Barplot of Average Proximity, by Group
#'
#' Shows distance to sites, for residents in each demographic group (vs everyone else).
#'   plot_distance_mean_by_group() and plot_distance_by_group() are synonymous.
#'
#' @details Note that the ratio shown is a ratio of distance among others to distance of a given group,
#'   so values below 1 mean the given demographic group lives closer to facilities.
#'   A value of 0.85 would mean the group is only 85% as far from a site as everyone else.
#'
#'   Note it is in miles assuming input was in miles, and the distance for each resident is
#'    actually the average distance of all residents within their Census block (not block group),
#'    and when a site is very close to the block internal point (like a centroid)
#'    relative to the size of the block, the distance to the average resident in the block is
#'    estimated as 90 percent of the effective radius, which is what the radius of the block
#'    would be if it were the same area in square meters or miles but circular in shape.
#'
#'    This is the approach used in EJScreen to estimate average proximity of a block resident in
#'    cases where the block is extremely close to the site or the site may actually be inside the block,
#'    or exactly on top of the internal point of the block, in which case zero would not be an
#'    appropriate estimate of the distance, hence this adjustment is made in EJAM [getblocksnearby()]
#'
#' @param results_bybg_people data.table from `doaggregate()$results_bybg_people`
#' @param demogvarname vector of column names like "pctlowinc" etc.
#' @param demoglabel vector of labels like "Low Income Residents" etc.
#' @param returnwhat If returnwhat is "table", invisibly returns a
#'    data.frame with group, ratio, avg_distance_for_group, avg_distance_for_nongroup.
#'   If returnwhat is "plotfilename" then it returns the full path including filename of a .png in a tempdir
#'   If returnwhat is "plot" then it returns the plot object as needed for table_xls_format() ?
#' @param graph logical optional, set to FALSE to not show the barplot and only save the file of it
#' @seealso [distance_by_group()] [distance_by_group_plot()]
#' @return see parameter returnwhat
#' @inherit distance_by_group_plot examples
#'
#' @aliases plot_distance_by_group
#' @export
#'
plot_distance_mean_by_group <- function(results_bybg_people,
                                        demogvarname=NULL, # namez$d, namez$d_subgroups),
                                        demoglabel=fixcolnames(demogvarname,"r","shortlabel"),
                                        graph=TRUE, returnwhat="table") {
  
  if (all(is.na(results_bybg_people$radius.miles))) { #Fips code ejam outputs do not have radius.miles, so check this to see if fips input. 
    warning("plot_distance_mean_by_group does not work with NA distances This includes outputs from ejamit with fips inputs")
    return(NA)
  }

  if (is.null(demoglabel) & is.null(demogvarname)) {
    demoglabel <- fixcolnames(c(names_d, names_d_subgroups), oldtype = 'r', newtype = 'shortlabel')
  }
  if (is.null(demogvarname)) {demogvarname <- c(names_d, names_d_subgroups)} # available from EJAM package. cannot safely put this info in the defaults of the functions without referring to pkg name but want to avoid doing that so this code will work even pkg not installed and just loaded data files and sourced code

  if (!is.data.frame(results_bybg_people)) {
    warning('results_bybg_people must be a data.frame or data.table - returning empty results')
    return(NA)
  }
  miss <- setdiff(c('distance_min_avgperson', demogvarname  ), names(results_bybg_people))
  if ( length(miss) > 0) {
    warning('These must be column names in results_bybg_people but are missing: ', paste0( miss, collapse = ", "))
    return(
      NA
    )
  }
  
  if (all(is.nan(results_bybg_people$distance_min_avgperson))) {
    warning('distance_min_avgperson contains only NaN values')
    return(NA)
  }

  dlist <- demogvarname
  x <- list()
  for (i in 1:length(dlist)) {x[[i]] <- distance_by_group1(results_bybg_people, dlist[i])}
  x <- do.call(rbind, x)
  x <- matrix(unlist(x), ncol = 2)
  x <- as.data.frame(x, stringsAsFactors = F)
  colnames(x) <- c("avg_distance_for_group","avg_distance_for_nongroup")
  x$group <-  dlist
  rownames(x) <- demoglabel
  x$ratio <- x$avg_distance_for_group / x$avg_distance_for_nongroup
  x <- x[ , c("group", "ratio", "avg_distance_for_nongroup", "avg_distance_for_group") ]

  i.min <- which.min(x$ratio)
  mingroup = gsub("% ", "", rownames(x)[i.min])
  mingrouptext = paste0(mingroup, " is only ", round(100*x$ratio[i.min],0),"% as far as everyone else, ",
                        round(x$avg_distance_for_group[   i.min],2)," miles vs ",
                        round(x$avg_distance_for_nongroup[i.min],2)," miles")

  if (returnwhat == "plot") {
    # how? just plot it and assume ggplot2::ggsave() can find it or does it need to be via ggplot not barplot?
    graph <- TRUE
  }

  if (graph) {
    colorlist <- ifelse(x$ratio < 1, "yellow", "gray")
    colorlist[i.min] <- "orange"

    barplot(x$ratio, names.arg = substr(rownames(x),1,13), cex.names = 0.6,
            main = "Ratio of avg distance from site \n among group residents vs non-group",
            xlab = paste0("Groups closer to sites are highlighted, with closest in orange (",mingrouptext,")"),
            col = colorlist, ylab = "Average distance from site(s) for residents in given group / for residents not in given group")
    abline(h = 1, col = "gray")
  }

  if (returnwhat == "plotfilename") {
    library(ggplot2)
    
    fname <- "distance_cdf.png"
    mytempdir <- tempdir()
    
    data <- data.frame(
      Group = substr(rownames(x), 1, 13),
      Ratio = x$ratio,
      Color = ifelse(x$ratio < 1, "yellow", "gray")
    )
    data$Color[i.min] <- "orange" 
    
    plot <- ggplot(data, aes(x = Group, y = Ratio, fill = Color)) +
      geom_bar(stat = "identity", show.legend = FALSE) + 
      geom_hline(yintercept = 1, color = "gray") + 
      scale_fill_identity() + 
      labs(
        title = "Ratio of avg distance from site \n among group residents vs non-group",
        x = paste0("Groups closer to sites are highlighted, with closest in orange (", mingrouptext, ")"),
        y = "Average distance from site(s) for residents in given group / for residents not in given group"
      ) +
      theme_minimal(base_size = 15) +
      theme(
        axis.text.x = element_text(angle = -30, hjust = 0.5, vjust = 0.5, size = 14),  
        axis.text.y = element_text(size = 14),  
        axis.title.y = element_text(size = 16, margin = margin(r = 20)), 
        axis.title.x = element_text(size = 16, margin = margin(t = 20)),  
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),  
        plot.margin = margin(t = 20, r = 20, b = 100, l = 100)    
      )
    
    ggsave(filename = file.path(mytempdir, fname), plot = plot, width = 20, height = 12, dpi = 100)
    
    
    return(file.path(mytempdir, fname))
  }

  if (returnwhat == "table") {
    x$nearer <- x$ratio < 1
    x$nearest <- i.min == 1:NROW(x)
    x$ratio <- round(x$ratio, 3)
    x$avg_distance_for_group    <- round(x$avg_distance_for_group,    2)
    x$avg_distance_for_nongroup <- round(x$avg_distance_for_nongroup, 2)
    x <- x[ , c('group', 'nearest', 'nearer', 'ratio', 'avg_distance_for_group', 'avg_distance_for_nongroup')]
    return(x)
  }
}
################################################################################# #

#' Avg distance of each demog group (of multiple groups) - Table or Plot
#' @inherit plot_distance_mean_by_group
#' @inheritParams plot_distance_mean_by_group
#' @export
#'
plot_distance_by_group <- plot_distance_mean_by_group
######################################################### #


#' Avg distance of each demog group (of multiple groups) - Table or Plot
#'
#' distance_mean_by_group() and distance_mean_by_group() are synonymous, and are like
#'   [plot_distance_mean_by_group()], but show a table not plot, by default.
#'
#' @inherit plot_distance_mean_by_group
#' @inherit distance_by_group_plot examples
#'
#' @aliases distance_by_group plot_distance_by_group
#'
#' @export
#'
distance_mean_by_group <- function(results_bybg_people,
                                   demogvarname=NULL,
                                   demoglabel=fixcolnames(demogvarname,"r","shortlabel"),
                                   returnwhat="table", graph=FALSE) {

  if (is.null(demogvarname)) {
    demogvarname <- c(names_d, names_d_subgroups)
  } # available from EJAM package. cannot safely put this info in the defaults of the functions without referring to pkg name but want to avoid doing that so this code will work even pkg not installed and just loaded data files and sourced code

  plot_distance_mean_by_group(
    results_bybg_people = results_bybg_people,
    demogvarname = demogvarname,
    # namez$d, namez$d_subgroups),
    demoglabel = demoglabel,
    returnwhat = returnwhat,
    graph = graph)
}
################################################################################# #

#' See plot_distance_mean_by_group()
#' @inherit plot_distance_mean_by_group
#' @return  See [plot_distance_mean_by_group()]
#' @export
#'
distance_by_group     <- function(results_bybg_people,
                                  demogvarname=NULL,
                                  demoglabel=fixcolnames(demogvarname,"r","shortlabel"),
                                  returnwhat="table", graph=FALSE) {
  if (is.null(demogvarname)) {demogvarname <- c(names_d, names_d_subgroups)} # available from EJAM package. cannot safely put this info in the defaults of the functions without referring to pkg name but want to avoid doing that so this code will work even pkg not installed and just loaded data files and sourced code

  plot_distance_mean_by_group(
    results_bybg_people = results_bybg_people,
    demogvarname = demogvarname,
    # namez$d, namez$d_subgroups),
    demoglabel = demoglabel,
    returnwhat = returnwhat,
    graph = graph)
}
################################################################################# #


#' Get average distance for ONE demographic group versus everyone else
#'
#' @details
#'  Note on Avg Distance and range of distances in each Demog group, & %D as function of distance:
#'
#'  We have info on each blockgroup near each site, which means some small % of those bgs are duplicated in this table:
#'
#'     results_bybg_people
#'
#'  Mostly we want overall (not by site) to know avg and cum distrib of distances in each demog,
#'
#'  (and also %D as a function of continuous distance),
#'
#'  and for those stats we would want to take only unique blockgroups from here,
#'  using the shorter distance, so the distribution of distances does not doublecount people.
#'
#' But we might also want to see that distribution of distances by D for just 1 site?
#'
#' And we might also want to see the %D as a function of continuous distance at just 1 site?
#'
#' So to retain flexibility doaggregate() reports all instances of blockgroup-site pairings.
#'
#' @param results_bybg_people data.table from doaggregate()$results_bybg_people
#' @param demogvarname e.g., "pctlowinc"
#' @param demoglabel e.g., "Low Income Residents"
#' @seealso [plot_distance_mean_by_group()]  [distance_by_group()]
#' @inherit distance_by_group_plot examples
#' @return list of 2 numbers: avg_distance_for_group and avg_distance_for_nongroup
#'
#' @keywords internal
#'
distance_by_group1 <- function(results_bybg_people,
                               demogvarname=varlist2names('names_d')[1],
                               demoglabel=fixcolnames(demogvarname, "r", "shortlabel")) {

  if (is.list(results_bybg_people) & ("results_bybg_people" %in% names(results_bybg_people))) {
    # assume it was a mistake and they meant to provide out$results_bybg_people not out itself
    results_bybg_people <- results_bybg_people$results_bybg_people
  }
  miss <- setdiff(c('distance_min_avgperson', 'bgid', 'pop',   demogvarname  ), names(results_bybg_people))
  if ( length(miss) > 0) {
    warning('These must be column names in results_bybg_people but are missing: ', paste0( miss, collapse = ", "))
    return(list(
      avg_distance_for_group     = NA,
      avg_distance_for_nongroup  = NA
    ))
  }

  # remove duplicated blockgroups, since here we do not need stats site by site, so use shorter distance for any bg that is near 2+ sites.
  results_bybg_people[ , distance_avg := min(distance_min_avgperson, na.rm = TRUE), by = "bgid"] # bug? they seem to be identical so taking min does nothing here???
  results_bybg_people[is.infinite(distance_avg), distance_avg := NA]
  results_bybg_people <- unique(results_bybg_people, by = "bgid")
  # results_bybg_people$distance_avg[ is.infinite(results_bybg_people$distance_min_avgperson)]  <- NA
  distance_avg_d    <- results_bybg_people[ , sum(distance_min_avgperson * pop *      .SD,  na.rm = TRUE) / sum(pop *      .SD,  na.rm = TRUE), .SDcols = demogvarname]
  distance_avg_nond <- results_bybg_people[ , sum(distance_min_avgperson * pop * (1 - .SD), na.rm = TRUE) / sum(pop * (1 - .SD), na.rm = TRUE), .SDcols = demogvarname]
  # cat(
  #   round( distance_avg_d,   3), " vs ",
  #   round( distance_avg_nond,3),
  # " is average distance among ", demoglabel, " vs others." , "\n")
  return(list(
    avg_distance_for_group     = distance_avg_d,
    avg_distance_for_nongroup = distance_avg_nond))
}
################################################################################# #
