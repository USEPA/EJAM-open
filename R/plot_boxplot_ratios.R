#' For the outputs of ejscreenit(), get boxplots of Residential Population Percentages across sites as ratios to US means
#'
#' @description boxplots show range of scores here vs range in US overall
#' @md
#' @details
#' See [ejam2boxplot_ratios()] now for ratios plots.
#'
#' See [plot_boxplot_pctiles()] now espec. for percentiles.
#'
#' IMPORTANTLY,
#'   NOTE this uses the ratio at each site
#'   USING THE AVERAGE RESIDENT AT THAT SITE,
#'   SO A BOXPLOT SHOWS ONE DOT PER SITE AND THE BOXPLOT IS NOT POP WTD
#'   MEANING IT SHOWS THE MEDIAN AND 75TH PERCENTILE SITE NOT RESIDENT, ETC.
#'
#' This function originally was used for [ejscreenit()] output, and
#' was just a quick interim solution that could be replaced.
#' It assumed colnames were not r variable names.
#'
#'  To communicate whether this is skewed to the right
#'  (more high scores than might expect) also could say that
#'  X% OF SITES OR PEOPLE have scores in top Y% of US range, >= 100-Y percentile.
#'  e.g., 20% of these sites have scores at least in the top 5% of US scores
#'  (which is more/less than one might expect
#'    - leaving aside statistical significance
#'  ie whether this could be by chance if sites were randomly picked
#'  from US block groups or people's bg scores)
#'
#' @param x ratios derived from
#'   a data.frame that is the output of ejscreen analysis, for example:
#'   ```
#'   df <- ejscreenit(testpoints_5)$table
#'   df <- testoutput_ejscreenapi_plus_5
#'   x <- calc_ratios_to_avg(df)$ratios_d
#'
#'   ```
#' @param selected_dvar_colname  default is the first column name of x, such as "Demog.Index"
#'   if given a table with just ratios that are named as regular indicators,
#'   as with output of calc_ratios_to_avg()$ratios_d,
#'   but it tries to figure out if ratios are available and what the base name is
#'   in case output of ejamit() was provided.
#' @param selected_dvar_nicename default is the "short" name of selected_dvar_colname
#'   as converted using [fixcolnames()]
#' @param towhat_nicename default is "US average"
#' @param wheretext Use in plot subtitle. Default is "Near" but could be "Within 5km of" for example.
#'   If it is a number, n, it will set wheretext to "Within n miles of"
#' @param maxratio largest ratio to plot in case of outliers, so plot looks better
#'
#' @return same format as output of [ggplot2::ggplot()]
#'
#' @examples
#'   # x <- testoutput_ejscreenit_50$table # or
#'   x <- testoutput_ejscreenapi_plus_5
#'   myradius <- x$radius.miles[1]
#'   plot_boxplot_ratios(calc_ratios_to_avg(x)$ratios_d, wheretext = myradius)
#'   #plot_boxplot_ratios(calc_ratios_to_avg(x)$ratios_e, wheretext = myradius)
#'
#' @export
#'
plot_boxplot_ratios <- function(x, selected_dvar_colname=varlist2names('names_d')[1], selected_dvar_nicename=selected_dvar_colname, towhat_nicename='US average',
                            maxratio = 5, wheretext="Near") {

  if (is.list(x) & "results_bysite" %in% names(x)) {
    x <- x$results_bysite
    if (selected_dvar_colname)
    x <- as.data.frame(x)[, selected_dvar_colname]
  } # for convenience, in case x was output of ejamit()
  if (is.data.table(x)) {x <- as.data.frame(x)}
  if (is.list(x) & is.data.frame(x[[1]]) & "ratios_d" %in% names(x)) {x <- x$ratios_d } # for convenience, in case you said  plot_boxplot_ratios(calc_ratios_to_avg(out))



  if (!(selected_dvar_colname %in% names(x))) {
    message(paste0(selected_dvar_colname, ' not found in x - using the one with max ratio'))

    # which indicator has the highest ratio among all sites?
    maxvar <- names(which.max(sapply(x, max)))
    selected_dvar_colname  <- maxvar
  }
  # now just use semi-long aka friendly varnames for all the rest of the function
  names(x)              <- fixnames_to_type(names(x),                oldtype = "rname", newtype = "shortlabel")
  selected_dvar_colname <- fixnames_to_type((selected_dvar_colname), oldtype = "rname", newtype = "shortlabel")
  if (missing(selected_dvar_nicename)) {selected_dvar_nicename <- selected_dvar_colname}

  DemogRatio75th <- round(stats::quantile(x[ , selected_dvar_colname], 0.75, na.rm = TRUE), 2) #NEED TO LOOK AT
  #DemogRatio50th <- round(stats::quantile(x[ , selected_dvar_colname], 0.50, na.rm = TRUE), 2)
  mymaintext <- paste0("Ratios to ", towhat_nicename, ", as distributed across these sites")
  if (length(wheretext) != 1) {warning('wheretext must be length 1. replacing with At'); wheretext <- "At"}
  if (is.numeric(wheretext)) {
    wheretext <- paste0("Within ", wheretext," miles of")
  }
  mysubtext <- paste0(
    wheretext,
    ' at least one site, ', selected_dvar_nicename, ' is ',
    round(max(x[ , selected_dvar_colname], na.rm = TRUE), table_rounding_info(selected_dvar_colname)), 'x the ', towhat_nicename, '\n', #NEED TO LOOK AT
    # 'Near most of these ', NROW(x),' sites, ', selected_dvar_nicename,
    # ' is at least ', DemogRatio50th, 'x the ', towhat_nicename, '\n',
    'and at 1 in 4 it is at least ', DemogRatio75th, 'x the ', towhat_nicename
  )
  # note on using ggplot() inside your own function:
  # If your wrapper has a more specific interface with named arguments,
  # you need "enquote and unquote":
  # scatter_by <- function(data, x, y) {
  #   x <- enquo(x)
  #   y <- enquo(y)
  #   ggplot(data) + geom_point(aes(!!x, !!y))
  # }
  # scatter_by(mtcars, disp, drat)

  x %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    ggplot2::ggplot() +
    ggplot2::geom_boxplot() +

    ## set limits for ratio on y axis - use hard limit at 0, make upper limit 5% higher than max limit
    scale_y_continuous(limits = c(0, maxratio), expand = expansion(mult = c(0, 0.05))) +
    # ylim(c(0, maxratio)) +  # simpler way

    ggplot2::aes(x = name, y = value, fill = name) +
    ggplot2::ggtitle(mymaintext, subtitle = mysubtext) +
    ggplot2::theme(text       = ggplot2::element_text(size = 12))     +
    ggplot2::theme(axis.text.x  = ggplot2::element_text(size = 12,
                                                      angle = 45, vjust = 1, hjust = 1))  +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 12))  +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 24))  +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 24), legend.position = "none") +
    ggplot2::scale_fill_viridis_d(alpha = 0.6) +
    ggplot2::geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
    ggplot2::theme_bw() +
    # hrbrthemes xxx :: xxx theme_ipsum() +
    # NOTE: Either Arial Narrow or Roboto Condensed fonts are required to use hrbrthemes themes.
    # Use hrbrthemes xxx :: xxx import_roboto_condensed() to install Roboto Condensed and if Arial Narrow is not on your system, please see https://bit.ly/arialnarrow
    ggplot2::xlab("") +
    ggplot2::geom_abline(slope = 0, intercept = 1)
}
########################################################################## #

#' Make boxplot of ratios to US averages
#' @details
#'  IMPORTANT:
#'   NOTE this uses the ratio at each site
#'   USING THE AVERAGE RESIDENT AT THAT SITE,
#'   SO A BOXPLOT SHOWS ONE DOT PER SITE AND THE BOXPLOT IS NOT POP WTD
#'   MEANING IT SHOWS THE MEDIAN AND 75TH PERCENTILE SITE NOT RESIDENT, ETC.
#'
#' @param ejamitout output from an EJAM analysis, like from [ejamit()]
#' @param radius buffer radius used for an analysis
#' @param varnames currently only works with names_d and names_d_subgroups
#' @param main can specify a main title to use instead of default
#' @param maxratio largest ratio to plot in case of outliers, so plot looks better
#'
#' @returns ggplot object
#'
#' @examples ejam2boxplot_ratios(testoutput_ejamit_1000pts_1miles, radius=1)
#'
#' out <- testoutput_ejamit_100pts_1miles
#' ejam2boxplot_ratios(out, radius=1)
#' ejam2boxplot_ratios(out)
#' ## not ## ejam2boxplot_ratios(out$results_bysite)
#'
#' @export
#'
ejam2boxplot_ratios <- function(ejamitout, radius, varnames = c(names_d, names_d_subgroups),
                                main = NULL, maxratio = 5) {

  ## *** much of this is plotting code is based on plot_boxplot_ratios() - should consolidate

  if (missing(radius)) {radius <- ejamitout$results_bysite$radius.miles[1]}

  rationames <- paste0('ratio.to.avg.', varnames)
  if (is.data.table(ejamitout$results_bysite)) {
  ratio.to.us.d.bysite <- ejamitout$results_bysite[ ,  c(..rationames)]
  } else {
    ratio.to.us.d.bysite <- ejamitout$results_bysite[ , rationames]
  }

  # ratio for average person overall
  ratio.to.us.d    <- unlist(
    ejamitout$results_overall[ , c(rationames )])

  ## assign column names (could use left_join like elsewhere)
  names(ratio.to.us.d.bysite) <-  fixcolnames(varnames, 'r', 'short') # is this right?

  ## pivot data from wide to long - now one row per indicator
  ratio.to.us.d.bysite <- ratio.to.us.d.bysite %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'indicator') %>%
    ## replace Infs with NAs - these happen when indicator at a site is equal to zero
    dplyr::mutate(value = dplyr::na_if(.data$value, Inf)) #%>%

  # NOTE NOW ratio.to.us.d.bysite IS A tibble, not data.frame, and is in LONG format now. !!!

  ## find max of ratios ####
  #
  max.ratio.d.bysite <- max(ratio.to.us.d.bysite$value, na.rm = TRUE)
  max.name.d.bysite <- ratio.to.us.d.bysite$indicator[which.max(ratio.to.us.d.bysite$value)]
  #
  # ## now using simpler way, just use a maxratio parameter to do this.
  # ## old way was using max_limit, etc.:
  # ## specify  upper bound for ratios (will drop values above this from graphic)
  # # perhaps want a consistent y limits to ease comparisons across multiple reports the user might run.
  # #  If the max value of any ratio is say 2.6, we might want ylim to be up to 3.0,
  # #  if the max ratio is 1.01, do we still want ylim to be up to 3.0??
  # #  if the max ratio or even max of 95th pctiles is >10, don't show it, but
  # #  what if the 75th pctile value of some indicator is >10? expand the scale to always include all 75ths.
  #
  # q75.maxof75s <- max(quantile(ratio.to.us.d.bysite$value, 0.75, na.rm = TRUE),na.rm = TRUE)
  # ylimit <- ceiling(q75.maxof75s) # max of 75th pctiles rounded up to nearest 1.0x?
  # max_limit <- max(maxratio, ylimit, na.rm = TRUE) #
  ## find 75th %ile of sites, ratios for the indicator with the max ratio
  # q75.ratio.d.bysite <- quantile(ratio.to.us.d.bysite$value[ratio.to.us.d.bysite$indicator == max.name.d.bysite], 0.75, na.rm = TRUE)

  ## paste subtitle for boxplot
  subtitle <- paste0('Within ', radius,' miles of one site, ',
                     max.name.d.bysite, ' is ', round(max.ratio.d.bysite,1), 'x the US average\n' #,
                     # 'and 1 in 4 sites is at least ', round(q75.ratio.d.bysite, 2), 'x the US average' # old way
  )

  ## specify # of characters to wrap indicator labels
  n_chars_wrap <- 13
  towhat_nicename <- "US Average"
  if (is.null(main) || missing(main) || '' %in% main) {
    mymaintext <- paste0("Ratios to ", towhat_nicename, ", as distributed across these sites")
  } else {
    mymaintext <- main
  }

  ##################################################################################### #
  # to use for dot showing the mean ratio of each indicator *** NOT USED?
  # meanratios <- data.frame(
  #   indicator = fixcolnames(varnames, 'r', 'short'),      # is this right?
  #   value = unlist(ratio.to.us.d[rationames])
  # )

  ## much of this is plotting code is based on plot_boxplot_ratios() - should consolidate

  ggplot2::ggplot(
    ratio.to.us.d.bysite,
    # mydata,
    ggplot2::aes(x = indicator, y = value )
  ) + #, fill = indicator)) +
    ## draw boxplots
    ggplot2::geom_boxplot() +

    #  show average PERSON's ratio to US,  for each boxplot column (probably need to debug)
    # geom_point(
    #   data =  meanratios,
    #   aes(x = reorder(indicator, meanratios), y = value), colour = "orange", size = 2
    # ) +

    #  show average SITE's ratio to US,  for each boxplot column  ?
    stat_summary(fun.y = mean, geom = "point",
                 shape = 20, size = 5, color = "gray", fill = "gray") +

    ## wrap indicator labels on x axis
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, n_chars_wrap)) +

    ## set limits for ratio on y axis - use hard limit at 0, make upper limit 5% higher than  maxratio
    ggplot2::scale_y_continuous(limits = c(0, maxratio), expand = ggplot2::expansion(mult = c(0, 0.05))) +

    ## alternate/old version that clipped top and bottom axes exactly at (0, max_limit)
    # ggplot2::scale_y_continuous(limits = c(0, max_limit), expand = c(0, 0)) +

    ## add horizontal line at 1
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1)) +
    ## set plot axis labels and titles
    ggplot2::labs(x = "",
         y = "Ratio of Indicator values in selected locations\n vs. US average value",
         subtitle = subtitle,
         title = mymaintext ) +

    ## draw individual dot per site? at least for small datasets?/few facilities - removed as they cover up boxplots with large datasets
    #geom_jitter(color = 'black', size = 0.4, alpha = 0.9, ) +

    ## set color scheme ?
    # actually do not need each a color, for boxplot.
    # scale_fill_brewer(palette = 'Dark2') +

    ggplot2::theme_bw() +
    ggplot2::theme(
      ## set font size of text
      text = ggplot2::element_text(size = 14),
      #axis.text  = ggplot2::element_text(size = 16),
      ## set font size of axis titles
      axis.title = ggplot2::element_text(size = 16),
      ## center and resize plot title
      plot.title = ggplot2::element_text(size = 22, hjust = 0.5),
      ## center subtitle
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      ## hide legend
      legend.position = 'none'
    )  # end of ggplot section
}
