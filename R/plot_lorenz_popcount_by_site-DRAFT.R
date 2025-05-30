
# Could show share of total population across sites, e.g., 
# or any other cumulative distribution

# (tidyverse)
# (ggplot2)
# (magrittr)
# (hrbrthemes)

#                       (gglorenz)


#  



#' lorenz plot bysite (cumulative share of x vs cum share of y) - DRAFT/EXPERIMENTAL
#' COMPARES TWO subsets OF SITES (or people??)
#' @param bysite from ejamit()$results_bysite
#' @param radius miles ************ tbd
#'
#' @return a ggplot
#' @export
#'
plot_lorenz_popcount_by_site <- function(bysite, radius) {
  
  bysite$`Demog Index State Percentile` <- ifelse(bysite$state.pctile.pctlowinc >= 80, "High Demog.Index (at least 80th pctile in State)", "All Other Sites")
  
  bysite |>
    filter("Demog Index State Percentile" %in% c( "High Demog.Index (at least 80th pctile in State)", "All Other Sites")) |>
    # ggplot(aes(pop)) +   
    ggplot2::ggplot(ggplot2::aes(x = pop, colour = "Demog Index State Percentile")) +
    
    gglorenz::stat_lorenz(desc = TRUE) +
    ggplot2::coord_fixed() +
    ggplot2::geom_abline(linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale=1)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale=1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Cumulative Percentage of the Sites (Facilities)",
         y = "Cumulative Percentage of Total Population Near All Sites Overall",
         title = "Differences in Size of Population Living Near Site",
         caption = paste0("Total number of sites analyzed: ", NROW(bysite), " "))
}


#' lorenz plot bybg_people (cumulative share of x vs cum share of y) - DRAFT/EXPERIMENTAL
#' COUNT OF SITES (or PEOPLE?) BY BIN
#' 
#' @param bybg_people from ejamit()$results_bybg_people
#' @param varname ************ tbd
#'
#' @return a ggplot
#' @export
#'
plot_lorenz_distance_by_dcount <- function(bybg_people, varname) {
  
  bysite |>
    ggplot2::ggplot(ggplot2::aes(x = distance_min_avgperson, n = pop * pctnhaa)) +
    
    gglorenz::stat_lorenz(desc = TRUE) +
    ggplot2::coord_fixed() +
    ggplot2::geom_abline(linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(labels = scales::label_percent(scale=1))+
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale=1))+  
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Cumulative Share of the Distances",
         y = "Cumulative Percentage of Total Low Income Residents",
         title = "Distance distribution in one group",
         caption = paste0("Total number of sites analyzed: ", NROW(bysite), " "))
}
