
#' Barplot of ratios of demographic (or other) scores to averages - simpler syntax
#' @aliases plot_barplot_ratios_ez
#' @param ejamitout like from [ejamit()]
#' @param sitenumber default is all sites from ejamitout$results_overall, and 
#'   if an integer, it is the site number to show from ejamitout$results_bysite
#' @param varnames vector of indicator names that are ratios to avg, like 
#'   c(names_d_ratio_to_avg , names_d_subgroups_ratio_to_avg)
#'   but could be c(names_d_ratio_to_state_avg , names_d_subgroups_ratio_to_state_avg)
#' @param main title of plot - must change to note it vs. State if not comparing to US avg.
#' @param ... passed to [plot_barplot_ratios()]
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
#' # Demographics only
#' 
#' # vs nationwide avg
#' ejam2barplot(out)
#' 
#' # vs statewide avg
#' ejam2barplot(out, 
#'   varnames = c(names_d_ratio_to_state_avg, names_d_subgroups_ratio_to_state_avg),
#'   main = "Demographics at Selected Sites Compared to State Averages")
#' 
#' # Environmental only
#' 
#' ejam2barplot(out,
#'   varnames = c(names_e_ratio_to_avg, names_e_ratio_to_state_avg),
#'   main = "Environmental Indicators at Selected Sites Compared to Averages")
#'   
#' @return ggplot
#'
#' @export
#'
ejam2barplot = function(ejamitout, varnames = c(names_d_ratio_to_avg , names_d_subgroups_ratio_to_avg),
                        sitenumber = NULL,
                        main = "Demographics at the Analyzed Locations Compared to US Overall", ...) {
  
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

#' Same as ejam2barplot() but ejam2barplot() handles a sitenumber parameter
#' 
#' @inheritParams ejam2barplot
#' @param out the list of tables that is the output of ejamit() or a related function
#' @param single_location set to TRUE if using row_index to view one site,
#'  set to FALSE to view overall results from out$results_overall
#' @param row_index the number of the row to use from out$results_bysite
#' @inheritDotParams ejam2barplot
#' 
#' @export
#'
plot_barplot_ratios_ez = function(out, varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg),
                                  main = "Demographics at the Analyzed Locations Compared to US Overall",
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


#' Barplot of ratios of demographic (or other) scores to averages (or other references)
#'
#' @param ratio.to.us.d.overall named list of a few ratios to plot, but see [ejam2barplot()]
#'   for an easier way to specify which indicator to show.
#' @param shortlabels names to use for plot - should be same length as named list ratio.to.us.d.overall
#' @param mycolorsavailable leave as default
#' @param main title for plot, like "Demographics at the Analyzed Locations Compared to US Overall"
#' @examples
#'   
#'   ejam2barplot(testoutput_ejamit_100pts_1miles)
#'   
#'   plot_barplot_ratios(unlist(testoutput_ejamit_1000pts_1miles$results_overall[ , c(..names_d_ratio_to_avg , ..names_d_subgroups_ratio_to_avg) ]))
#'
#' @seealso [table_xls_format()] [ejam2ratios()] [ejam2barplot()] 
#' @return ggplot should be returned
#' @export
plot_barplot_ratios <- function(ratio.to.us.d.overall,
                                shortlabels = NULL,
                                mycolorsavailable=c("gray", "yellow", "orange", "red"),
                                main = "Demographics at the Analyzed Locations Compared to US Overall") {
  
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
    # - boxplots_ratios()
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
    supershortnames <- gsub(' \\(.*', '', gsub("People of Color","POC", shortlabels))
    names(ratio.to.us.d.overall) <- supershortnames
  }
    names(ratio.to.us.d.overall) <- shortlabels


  ratio.to.us.d.overall[is.infinite(ratio.to.us.d.overall)] <- 0
  # use yellow/orange/red for ratio >= 1x, 2x, 3x  #  work in progress
  mycolors <- mycolorsavailable[1 + findInterval(ratio.to.us.d.overall, c(1.01, 2, 3))]

  # barplot(ratio.to.us.d.overall,
  #         main = 'Ratio vs. US Average for Demographic Indicators',
  #         cex.names = 0.7,
  #         col = mycolors)
  # abline(h=1, col="gray")

thisdata <-  data.frame(name = names(ratio.to.us.d.overall),
             value = ratio.to.us.d.overall,
             color =  factor(
               mycolors,
               levels = c("gray", "yellow", "orange", "red") #Set corect order from least to most
             )) %>%
    ## drop any indicators with Inf or NaNs
    dplyr::filter(is.finite(value))

thisdata$name <- factor(thisdata$name) # factor(thisdata$name, levels = thisdata$name)

#Dynamically generate the color legend based on title
if (grepl("State", main, ignore.case = TRUE)) {
  color_labels <- c(
    "gray" = "Below State Average",
    "yellow" = "1-2x State Average",
    "orange" = "2-3x State Average",
    "red" = "At least 3x State Average"
  )
  legendTitle <- "Ratio vs State Average"
} else {
  color_labels <- c(
    "gray" = "Below US Average",
    "yellow" = "1-2x US Average",
    "orange" = "2-3x US Average",
    "red" = "At least 3x US Average"
  )
  legendTitle <- "Ratio vs US Average"
}

thisplot <- thisdata %>%
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = color)) +
    ggplot2::geom_bar(stat = 'identity') +
    ## Legend for colors of bars
    ggplot2::scale_fill_manual(
      values = setNames(mycolorsavailable, mycolorsavailable),
      labels = color_labels,
      name = legendTitle
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = NULL, y = 'Ratio vs. Average', #fill = 'Legend',
                  title = main) +
    #scale_x_discrete(labels = scales::label_wrap(7)) +    # requires scales package
    #scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +

  # add horizontal line at ratio = 1
  ggplot2::geom_hline(ggplot2::aes(yintercept = 1)) +

    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05), add = c(0, 0))) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(20,100,20,20), "points"),
                   plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 10 , angle = -30, hjust = 0, vjust = 1),
                   legend.title = ggplot2::element_text(size = 12),  
                   legend.text = ggplot2::element_text(size = 10)   
                   ) + #
    NULL

return(thisplot)

  # ggplot2::ggplot(
  #   ratio.to.us.d.overall,
  #   aes(x = indicator, y = value)
  # ) +
  #   geom_boxplot() +
  #   geom_hline(aes(yintercept = 1)) +
  #   labs(x = "",
  #        y = "Ratio of Indicator values for avg. person in selected locations\n vs. US average value",
  #        title = 'Ratio vs. US Average for Demographic Indicators')
}
