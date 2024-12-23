
#' Ratios at each site, of avg dist of group / avg dist of everyone else near site
#' 
#' Like [distance_by_group()] but for multiple sites - DRAFT FUNCTION
#' 
#' @details There are two aspects of proximity to consider when analyzing
#'   demographic groups within a certain fixed distance (radius) from a single
#'   facility point (or a whole set of facilities). These two ways of summarizing
#'   proximity are complementary: 
#'   
#'   1. Which groups tend to live nearby in the sense of being within the selected radius
#'   versus outside the radius? In other words, which groups are "overrepresented"
#'   within X miles of the site? This treats proximity as a yes/no, binomial
#'   question -- a resident is nearby or not. It would focus on whether someone is
#'   anywhere within 3 miles, say, and ignore the differences between
#'   being 1, 2, or 3 miles away.
#'   
#'   2. Among the residents within X miles of the site, which groups live especially close to the
#'   facility? This question recognizes proximity is a continuous variable, and
#'   focuses on the difference between 1 mile, 1.5 miles, etc. However, it only
#'   looks at residents within the X miles radius area analyzed, so it fails
#'   to recognize that some groups tend to live more than 3 miles away, for example.
#'   This perspective does not take into account which groups are overrepresented
#'   within the original total radius near a site.
#'   
#'   This function does the second of these two types of analysis. It reports,
#'   only among those anywhere inside the radius, which groups are closer to the site.
#'   
#'   In a specific location, for example, one demographic group could be
#'   underrepresented within 3 miles, but those few who are in the group
#'   still might live right next to the facility in which case
#'   their average distance would be higher than that of any other group
#'   because this function only counts those within the radius analyzed. 
#'   
#'   In some other location, the opposite could occur -- if one group is 
#'   overrepresented within 3 miles, they still might all live in a community
#'   about 2.9 miles away from the site -- that would mean their distance from
#'   the site on average is greater (or their proximity score is lower)
#'   than other groups within 3 miles of the site.
#'   
#'   
#' @param bybg such as [ejamit()]$results_bybg_people
#'
#' @return table of ratios, one col per site, one row per indicator
#' @seealso [ejamit_compare_distances()] [distance_by_group_plot()] [plot_distance_mean_by_group()] [distance_by_group()] [distance_mean_by_group()]
#' @examples 
#'   # distance_by_group_by_site(
#'   #   testoutput_ejamit_10pts_1miles$results_bybg_people
#'   # )
#' 
#' @export
#' 
distance_by_group_by_site <- function(bybg) {

  # out = ejamit(testpoints_100[1:10, ], radius = 6.2)
  # bybg = out$results_bybg_people[ejam_uniq_id < 11, ]
  
  ids <- bybg[ , unique(ejam_uniq_id)]
  x = list()
  for (i  in 1:length(ids)) {
    
    y = plot_distance_mean_by_group(bybg[ejam_uniq_id ==  ids[i]], graph = FALSE)
    
    # print(x[[i]])  # > str(x[[1]])
    # 'data.frame':	18 obs. of  6 variables:
    # $ group                    : chr  "Demog.Index" "Demog.Index.Supp" "pctlowinc" "pctlingiso" ...
    # $ nearest                  : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    # $ nearer                   : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    # $ ratio                    : num  1.08 1.06 1.09 1.1 1.07 ...
    # $ avg_distance_for_group   : num  3.49 3.49 3.5 3.67 3.54 3.53 3.41 3.26 3.11 3.48 ...
    # $ avg_distance_for_nongroup: num  3.24 3.3 3.21 3.33 3.31 3.3 3.31 3.34 3.43 3.27 ...
   
    ## valid distance values returned
    if (!all(is.na(y))) {
      
      x[[i]] <- y$ratio
      names(x[[i]]) <- rownames(y)
    } else {
      ## if some invalid NaN distances returned
      #warning('Nan values found, so no distances returned')
      return(NA)
    }
  }
  x = cbind.data.frame(x)
  colnames(x) <- ids
  cat("Ratios at each site, of avg dist of group / avg dist of everyone else near site: \n\n")
   return(x)
}
