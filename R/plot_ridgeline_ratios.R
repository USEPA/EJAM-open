


#' Make ridgeline plot of ratios of demographic score to its average
#'
#' @param out like from ejamit()
#' @param varnames vector of colnames in out$results_bysite, the ratio variables
#' @param maxratio cap on ratios to show so plot looks better (all values above cap get replaced by cap before plotting)
#' @examples
#'  out <- testoutput_ejamit_1000pts_1miles
#'  plot_ridgeline_ratios_ez(out)
#'
#' @export
#'
plot_ridgeline_ratios_ez <- function(out, varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg), maxratio = 5) {

  ratio.to.us.d.bysite <- out$results_bysite[, varnames, with = FALSE]

  # cap the ratio, for better plot
  x <- as.matrix(ratio.to.us.d.bysite)
  x[x > maxratio] <- maxratio
  x <- data.table::data.table(x)

  # ratio.to.us.d.bysite <- out$results_bysite[ ,  c(
  #   ..names_d_ratio_to_avg,
  #   ..names_d_subgroups_ratio_to_avg
  # )]
  # x <- ratio.to.us.d.bysite

  plot_ridgeline_ratios(x)
}
############################################################################################# #


#' Make ridgeline plot of ratios of demographic score to its average
#'
#' @param ratio.to.us.d.bysite named list of a few ratios to plot (data.frame)
#' @param shortlabels names to use for plot - should be same length as named list ratio.to.us.d.overall
#' @examples
#'  out <- testoutput_ejamit_1000pts_1miles
#'  ratio.to.us.d.bysite <- out$results_bysite[ ,  c(
#'    ..names_d_ratio_to_avg, ..names_d_subgroups_ratio_to_avg
#'    )]
#'  # plot_ridgeline_ratios(ratio.to.us.d.bysite)
#'  # cap the ratio, for better plot
#'  x <- as.matrix(ratio.to.us.d.bysite)
#'  x[x > 5] <- 5
#'  plot_ridgeline_ratios(data.table::data.table(x))
#'
#' @export
#'
plot_ridgeline_ratios <- function(ratio.to.us.d.bysite, shortlabels = NULL) {
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
  dplyr::mutate(value = dplyr::na_if(value, Inf)) #%>%
# NOTE NOW ratio.to.us.d.bysite IS A tibble, not data.frame, and is in LONG format now. !!!

# ridgeline Plot - need to adjust xlim so max is about a ratio of 3.0 (or less if none are >=3x)
ggplot(ratio.to.us.d.bysite, aes(x = `value`, y = `indicator`, fill = ..x..)) +
  ggridges::geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  viridis::scale_fill_viridis(name = "Ratio to US Overall Value", option = "C") +
  labs(title = 'Ratio to US Overall for each Demographic Indicator across these Sites') +
  hrbrthemes::theme_ipsum() +
  theme(
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
