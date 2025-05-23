#' Barplot of ratios of 1 State's indicator averages vs US overall
#'
#' @param ST state abbreviation like "NY"
#' @param varnames vector of character names of raw indicator variables that are among
#'   names(statestats), like "pm" or "pctlowinc" or a vector like
#'   names_d or names_e
#'
#' @return similar to [plot_barplot_ratios()]
#' @examples
#' plot_st_v_us("CA", names_these)
#' 
#' @export
#'
plot_st_v_us <- function(ST="CA", varnames=names_these) {
  x <- statestats_query(ST, PCTILES = 'mean', varnames = varnames, dig = 8)
  x[c("REGION", "PCTILE")] <- NULL
  xx <- x / avg.in.us[names(x)] 
  # statename <- fips2statename(fips_state_from_state_abbrev(ST))
  plot_barplot_ratios(xx, main = "State Average vs US Average")
}
