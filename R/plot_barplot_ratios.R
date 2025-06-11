
#' Barplot of ratios of residential population (or other) scores to averages - simpler syntax
#'
#' @aliases plot_barplot_ratios_ez
#' @param ejamitout like from [ejamit()]
#' @param sitenumber default is all sites from ejamitout$results_overall, and 
#'   if an integer, it is the row number to show from ejamitout$results_bysite.
#'   Important: note this is the row number which is 
#'   NOT necessarily the same as the ejamitout$results_bysite$ejam_uniq_id
#' @param varnames vector of indicator names that are ratios to avg, like 
#'   c(names_d_ratio_to_avg , names_d_subgroups_ratio_to_avg)
#'   but could be c(names_d_ratio_to_state_avg , names_d_subgroups_ratio_to_state_avg)
#' @param main title of plot - must change to note it vs. State if not comparing to US avg.
#' @param ... passed to [plot_barplot_ratios_ez()]
#' @examples 
#' 
#' # Check a long list of indicators for any that are elevated
#' 
#' out <- testoutput_ejamit_100pts_1miles
#' 
#' ejam2barplot(out,
#'   varnames = names_these_ratio_to_avg,
#'   main = "Envt & Demog Indicators at Selected Sites Compared to State Averages")
#'   
#' ejam2barplot(out,
#'   varnames = names_these_ratio_to_state_avg,
#'   main = "Envt & Demog Indicators at Selected Sites Compared to State Averages")
#' 
#' # Residential population percentages only
#' 
#' # vs nationwide avg
#' ejam2barplot(out)
#' 
#' # vs statewide avg
#' ejam2barplot(out, 
#'   varnames = c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg),
#'   main = "Residential Populations at Selected Sites Compared to State Averages")
#' 
#' # Environmental only
#' 
#' ejam2barplot(out,
#'   varnames = c(names_e_ratio_to_avg, names_e_ratio_to_state_avg),
#'   main = "Environmental Indicators at Selected Sites Compared to Averages")
#'   
#'  ## select your own ratio-type indicators that are available
#'  ## -- and you could see the range of available ratio indicators like this:
#'  \dontrun{
#'  varinfo(
#'    grep("ratio",
#'         names(testoutput_ejamit_10pts_1miles$results_overall),
#'         value = TRUE),
#'    info = c("varlist", "shortname")
#'  )
#'    }
#'  
#'  # helper functions related to ejam2barplot()
#'  
#'   plot_barplot_ratios_ez(
#'     out, 
#'     varnames = c(names_d_ratio_to_avg , names_d_subgroups_ratio_to_avg)
#'   )
#'   
#'   # same plot but with function that would need more work to format inputs:
#'   plot_barplot_ratios(
#'       unlist(out$results_overall[ ,
#'       c(..names_d_ratio_to_avg , ..names_d_subgroups_ratio_to_avg) ])
#'       )
#'       
#' @return ggplot
#'
#' @export
#'
ejam2barplot = function(ejamitout, varnames = c(names_d_ratio_to_avg , names_d_subgroups_ratio_to_avg),
                        sitenumber = NULL,
                        main = "Residential Populations at the Analyzed Locations Compared to US Overall", 
                        ...) {
  
  if (is.null(sitenumber)) {
    # ejamitout <- ejamitout$results_overall
    single_location <- FALSE
    row_index <- NULL
  } else {
    # ejamitout <- ejamitout$results_bysite # [sitenumber, ] gets done by plot_barplot_ratios_ez()
    single_location <- TRUE
    row_index <- sitenumber
  }
  # ejam2barplot(out,varnames = c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg), main = "Demographics at Analyzed Locations Compared to Statewide")
  plot_barplot_ratios_ez(out = ejamitout,
                         varnames = varnames,
                         single_location = single_location, row_index = row_index,
                         main =  main, 
                         ... = ...)
}
############################################################################################# #

#' Helper - Barplot of ratios of indicators (at a site or all sites overall) to US or State average
#' @rdname ejam2barplot
#' @details
#' Used by and similar to [ejam2barplot()], which is an easier way to do this!
#'   This function requires you to specify single_location = TRUE when 
#'   using the row_index param. The [ejam2barplot()] function just uses a sitenumber parameter.
#' 
#' This function is more flexible than [plot_barplot_ratios()], which it relies on,
#'   since this lets you specify 
#'   whether to use overall results from ejamit()$results_overall
#'   or just one site from ejamit()$results_bysite
#' 
#' @param out the list of tables that is the output of ejamit() or a related function
#' @param single_location set to TRUE if using row_index to view one site,
#'  set to FALSE to view overall results from out$results_overall
#' @param row_index the number of the row to use from out$results_bysite
#' @param ... passed to plot_barplot_ratios()
#' 
#' @export
#' @keywords internal
#'
plot_barplot_ratios_ez = function(out, varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg),
                                  main = "Residential Populations at the Analyzed Locations Compared to US Overall",
                                  single_location = FALSE, row_index = NULL, ...) {
  if (single_location && !is.null(row_index)) {
    ## check if data.table (SHP is data.frame)
    if (is.data.table(out$results_bysite)) {
      data_to_plot <- unlist(out$results_bysite[row_index, varnames, with = FALSE])  
    } else {
      data_to_plot <- unlist(out$results_bysite[row_index, varnames])
    }
    
  } else {
    ## check if data.table (SHP is data.frame)
    if (is.data.table(out$results_overall)) {
      data_to_plot <- unlist(out$results_overall[, varnames, with = FALSE])
    } else {
      data_to_plot <- unlist(out$results_overall[, varnames])  
    }
    
  }
  
  plot_barplot_ratios(data_to_plot, main = main, ...)
}
############################################################################################# #


#' helper - Barplot of ratios of residential population percentages (or other scores) to averages (or other references)
#' @rdname ejam2barplot
#' @details If the parameter called main has the word "State" in it, then the legend 
#'   will refer to "State Average" instead of "US Average"
#' @param ratio.to.us.d.overall named list of a few ratios to plot, but see [ejam2barplot()]
#'   for an easier way to specify which indicator to show.
#' @param shortlabels optional, names to use for plot - should be same length as named list ratio.to.us.d.overall
#' @param mycolorsavailable optional (best to leave as default)
#' @param main optional, title for plot, like "Stats at the Analyzed Locations Compared to US Overall"
#' @param ylab optional, label for y axis
#' @param caption text for a key defining some terms that are abbreviations
#' 
#' @seealso  [ejam2ratios()] [ejam2barplot()] [plot_barplot_ratios_ez()] [table_xls_format()]
#' @return ggplot should be returned
#' 
#' @export
#' 
plot_barplot_ratios <- function(ratio.to.us.d.overall,
                                shortlabels = NULL,
                                mycolorsavailable = c("gray","yellow","orange","red"),
                                main = "Residential Populations at the Analyzed Locations Compared to US Overall",
                                ylab = "Ratio vs. Average",
                                caption = "NH = \"non-Hispanic\"\nNHA = \"non-Hispanic alone, aka single race\"") {
  
  if (is.null(main) || "" %in% main) {main <- "Residential Populations at the Analyzed Locations Compared to US Overall"}
  ########################################################## #
# NOTES
    # 
    # 
    # **SOME GENERAL NOTES, DURING EJAM DEVELOPMENT**
    # 
    # For plots in general, see:
    # 
    # - <https://echarts4r.john-coene.com/articles/themes.html>
    # - <https://exts.ggplot2.tidyverse.org/gallery>
    # 
    # 
    # **For BARPLOTS, see/ merge/consolidate:**
    # 
    # - output$view1_summary_plot <- renderPlot({v1_summary_plot()}) and v1_summary_plot <- reactive({ })
    #   in EJAM server for Short Report if  bar type
    # - output$summ_display_bar <- renderPlot({  }) contains its own plot code not a reactive
    #   in EJAM server for tab showing barplots in Detailed Results
    # - plot_barplot_ratios() drafted function in EJAM
    # 
    # 
    # **For BOXPLOTS, see:**
    # 
    # - v1_summary_plot <- reactive({ })     and output$view1_summary_plot <- renderPlot({v1_summary_plot()})
    #    - in EJAM server for SHORT report if box type, and
    #    - in EJAM server for LONG report passed as a parameter
    # - plot_boxplot_ratios()
    #    (NOT in EJAM server for Detailed Results interactive views)
    # - ejscreenapi_script() code also relevant?
    # - box/scatter examples in ggplot, <https://r-graph-gallery.com/89-box-and-scatter-plot-with-ggplot2.html>
    # - boxplots in base R, <https://www.r-bloggers.com/2023/09/how-to-reorder-boxplots-in-r-a-comprehensive-guide>
    # 
    # **For HISTOGRAMS, see:**
    # 
    # - output$summ_display_hist <- renderPlot   in EJAM server for interactive views
  ########################################################## #
  
  # ratio.to.us.d.overall <-   unlist(  out$results_overall[ , c(..names_d_ratio_to_avg, ..names_d_subgroups_ratio_to_avg )]  )
    # ratio.to.us.d.overall <- ratio.to.us.d()  # reactive already available
  # if (isTRUE(all.equal(names(ratio.to.us.d.overall), c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg)))) {
  #
  # 
  # }
  if (is.null(shortlabels)) {
    shortlabels <- fixcolnames(names(ratio.to.us.d.overall), oldtype = "r", newtype = "shortlabel")
    shortlabels <- gsub("^Ratio to US avg", "", shortlabels)  # Remove the prefix
    shortlabels <- gsub("^Ratio to State avg", "", shortlabels)  # Remove the prefix
    supershortnames <- gsub(' \\(.*', '', gsub("People of Color", "POC", shortlabels))
    names(ratio.to.us.d.overall) <- supershortnames
  }
    names(ratio.to.us.d.overall) <- shortlabels

  ratio.to.us.d.overall[is.infinite(ratio.to.us.d.overall)] <- 0
  # use yellow/orange/red for ratio >= 1x, 2x, 3x  #  work in progress
  mycolors <- mycolorsavailable[1 + findInterval(ratio.to.us.d.overall, c(1.01, 2, 3))]

  # barplot(ratio.to.us.d.overall,
  #         main = 'Ratio vs. US Average for Residential Population Indicators',
  #         cex.names = 0.7,
  #         col = mycolors)
  # abline(h=1, col="gray")

thisdata <-  data.frame(name = factor(names(ratio.to.us.d.overall),levels = names(ratio.to.us.d.overall)),
             value = ratio.to.us.d.overall,
             color =  factor(
               mycolors,
               levels = c( "red","orange","yellow","gray") #Set correct order from least to most
             )) %>%
    ## drop any indicators with Inf or NaNs
    dplyr::filter(is.finite(.data$value))

thisdata$name <- factor(thisdata$name) # factor(thisdata$name, levels = thisdata$name)

#Dynamically generate the color legend based on title
if (isTRUE(getOption("shiny.testmode"))) {
  set.seed(12345)
}
if (grepl("State", main, ignore.case = TRUE)) {
  color_labels <- c(
    "red" = "At least 3x State Average",
    "orange" = "2-3x State Average",
    "yellow" = "1-2x State Average",
    "gray" = "Below State Average"
  )
  legendTitle <- "Ratio vs State Average"
} else {
  color_labels <- c(
    "red" = "At least 3x US Average",
    "orange" = "2-3x US Average",
    "yellow" = "1-2x US Average",
    "gray" = "Below US Average"
  )
  legendTitle <- "Ratio vs US Average"
}

thisplot <- thisdata %>%
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = color)) +
    ggplot2::geom_bar(stat = 'identity', show.legend = TRUE) +
    ## Legend for colors of bars
    ggplot2::scale_fill_manual(
      values = setNames(mycolorsavailable, mycolorsavailable),
      labels = color_labels,
      name = legendTitle,
      drop = FALSE
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
    x = NULL,
    y = ylab,
    title = main,
    caption = caption
   ) +
    #scale_x_discrete(labels = scales::label_wrap(7)) +    # requires scales package
    #scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +

  # add horizontal line at ratio = 1
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1)) +

    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05), add = c(0, 0))) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
    axis.text.x = ggplot2::element_text(size = 9, angle = 45, hjust = 1, vjust = 1), 
    legend.title = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 10),
    legend.position = "bottom"
  )

return(thisplot)

  # ggplot2::ggplot(
  #   ratio.to.us.d.overall,
  #   aes(x = indicator, y = value)
  # ) +
  #   geom_boxplot() +
  #   geom_hline(aes(yintercept = 1)) +
  #   labs(x = "",
  #        y = "Ratio of Indicator values for avg. person in selected locations\n vs. US average value",
  #        title = 'Ratio vs. US Average for Residential Population Indicators')
}
