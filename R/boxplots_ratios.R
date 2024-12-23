#' boxplots of demographics across sites as ratios to US means
#' 
#' @description boxplots show range of scores here vs range in US overall
#' @md
#' @details  See [plot_boxplot_pctiles()] now espec. for percentiles.
#' 
#' This function originally was used for ejscreenit() output, and 
#' was just a quick interim solution that could be replaced.
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
#' @param x data.frame that is the output of ejscreen analysis, for example:
#'   ```
#'   x <- ejscreenit(testpoints_5)$table
#'   x <- testoutput_ejscreenapi_plus_5
#'   ```
#' @param selected_dvar_colname  default is "Demog.Index"
#' @param selected_dvar_nicename default is "Demog.Index"
#' @param towhat_nicename default is "US average"
#' @param wheretext Use in plot subtitle. Default is "Near" but could be "Within 5km of" for example.
#'   If it is a number, n, it will set wheretext to "Within n miles of"
#' @return same format as output of [ggplot2::ggplot()]
#' @import ggplot2
#' @import tidyr
#' @import viridis
#'
#' @examples
#'   # x <- testoutput_ejscreenit_50$table # or
#'   x <- testoutput_ejscreenapi_plus_5
#'   myradius <- x$radius.miles[1]
#'   boxplots_ratios(calc_ratios_to_avg(x)$ratios_d, wheretext = myradius)
#'   #boxplots_ratios(calc_ratios_to_avg(x)$ratios_e, wheretext = myradius)
#'
#' @export
#'
boxplots_ratios <- function(x, selected_dvar_colname=varlist2names('names_d')[1], selected_dvar_nicename=selected_dvar_colname, towhat_nicename='US average',
                            wheretext="Near") {
  
  if (is.list(x) & "results_bysite" %in% names(x)) {x <- x$results_bysite} # for convenience, in case x was output of ejamit()
  if (is.data.table(x)) {x <- as.data.frame(x)}
  if (is.list(x) & is.data.frame(x[[1]]) & "ratios_d" %in% names(x)) {x <- x$ratios_d } # for convenience, in case you said  boxplots_ratios(calc_ratios_to_avg(out))
  if (!(selected_dvar_colname %in% names(x))) {
    message(paste0(selected_dvar_colname, ' not found in x - using the one with max ratio'))

    # which indicator has the highest ratio among all sites?
    maxvar <- names(which.max(sapply(x, max)))
    selected_dvar_colname  <- maxvar
    selected_dvar_nicename <- fixcolnames(selected_dvar_colname, 'r', 'long')
  }
  # now just use semi-long aka friendly varnames for all the rest of the function
  names(x)              <- fixnames_to_type(names(x),                oldtype = "rname", newtype = "shortlabel")
  selected_dvar_colname <- fixnames_to_type((selected_dvar_colname), oldtype = "rname", newtype = "shortlabel")

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
    ggplot2::aes(x = name, y = value, fill = name, ymax = 5) +
    ggplot2::ggtitle(mymaintext, subtitle = mysubtext) +
    ggplot2::theme(text       = ggplot2::element_text(size = 16))     +
    ggplot2::theme(axis.text  = ggplot2::element_text(size = 16))  +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 16))  +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 24))  +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 24), legend.position = "none") +
    viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    ggplot2::geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
    # hrbrthemes xxx :: xxx theme_ipsum() +
    # NOTE: Either Arial Narrow or Roboto Condensed fonts are required to use hrbrthemes themes.
    # Use hrbrthemes xxx :: xxx import_roboto_condensed() to install Roboto Condensed and if Arial Narrow is not on your system, please see https://bit.ly/arialnarrow
    ggplot2::xlab("") +
    ggplot2::geom_abline(slope = 0, intercept = 1)
}

