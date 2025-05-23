


#' Make ridgeline plot of ratios of residential population percentage to its average
#'
#' @param out like from ejamit()
#' @param varnames vector of colnames in out$results_bysite, the ratio variables
#' @param maxratio largest ratio to plot in case of outliers, so plot looks better
#' @param main optional alternative title for plot
#' @examples
#'  out <- testoutput_ejamit_1000pts_1miles
#'  plot_ridgeline_ratios_ez(out)
#'
#' @export
#'
plot_ridgeline_ratios_ez <- function(out, varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg), 
                                     main = 'Variations among Sites',
                                     maxratio = 5) {
  
  ratio.to.us.d.bysite <- out$results_bysite[, varnames, with = FALSE]
  
  x <- as.matrix(ratio.to.us.d.bysite)
  x <- data.table::data.table(x)
  # now cap the ratio, for better plot, in the function below
  
  plot_ridgeline_ratios(x, maxratio = maxratio, main = main)
}
############################################################################################# #


#' Make ridgeline plot of ratios of residential population percentage to its average
#'
#' @param ratio.to.us.d.bysite named list of a few ratios to plot (data.frame)
#' @param shortlabels names to use for plot - should be same length as named list ratio.to.us.d.overall
#' @param maxratio largest ratio to plot in case of outliers, so plot looks better
#' @examples
#'  out <- testoutput_ejamit_1000pts_1miles
#'  ratio.to.us.d.bysite <- out$results_bysite[ ,  c(
#'    ..names_d_ratio_to_avg, ..names_d_subgroups_ratio_to_avg
#'    )]
#'  # plot_ridgeline_ratios(ratio.to.us.d.bysite)
#'  # cap the ratio, for better plot
#'  x <- as.matrix(ratio.to.us.d.bysite)
#'  plot_ridgeline_ratios(data.table::data.table(x))
#'
#' @export
#'
plot_ridgeline_ratios <- function(ratio.to.us.d.bysite, shortlabels = NULL,
                                  main = 'Variations among Sites',
                                  maxratio = 5) {
  
  if (!is.null(main) && !('' %in% main)) {
    # use what they provided
  } else {
    main <- 'Variations among Sites'
  }
  
  # if (is.null(dim(ratio.to.us.d.bysite))) {
  #   # seems like only 1 variable as vector ?
  #   ratio.to.us.d.bysite <- data.frame(Indicator = ratio.to.us.d.bysite)
  # }
  if (is.null(shortlabels)) {
    shortlabels <- fixcolnames(names(ratio.to.us.d.bysite), oldtype = "r", newtype = "shortlabel")
    supershortnames <- gsub(' \\(.*', '', gsub("People of Color","POC", shortlabels))
    names(ratio.to.us.d.bysite) <- supershortnames
  }

## pivot data from wide to long - now one row per indicator
ratio.to.us.d.bysite <- ratio.to.us.d.bysite %>%
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'indicator') %>%
  ## replace Infs with NAs - these happen when indicator at a site is equal to zero
  dplyr::mutate(value = dplyr::na_if(.data$value, Inf)) #%>%
# NOTE NOW ratio.to.us.d.bysite IS A tibble, not data.frame, and is in LONG format now. !!!

# ridgeline Plot - need to adjust xlim so max is about a ratio of 3.0 (or less if none are >=3x)
ggplot2::ggplot(ratio.to.us.d.bysite, ggplot2::aes(x = `value`, y = `indicator`, fill = ..x..)) + # Please use `after_stat(x)` instead. Warning: The dot-dot notation (`..x..`) was deprecated in ggplot2 3.4.0.
  ggridges::geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  ggplot2::xlim(0, 5) +
  ggplot2::scale_fill_viridis_c(name = "Ratio to US Overall Value", option = "C") +
  ggplot2::labs(title = main) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
}

# https://r-graph-gallery.com/294-basic-ridgeline-plot.html#color
# https://r-graph-gallery.com/294-basic-ridgeline-plot.html#shape
#  (ggplot2)
#  (ggridges) # listed in DESCRIPTION file Imports
#  (viridis) # listed in DESCRIPTION file Imports
#  (hrbrthemes) # listed in DESCRIPTION file Imports
