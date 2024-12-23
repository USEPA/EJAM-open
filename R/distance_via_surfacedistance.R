
 
#' Convert surface distance to actual distance
#' 
#' @description
#'     \preformatted{
#'     Just a simple formula:
#'    earthRadius_miles <- 3959
#'    angle_rad <- x/earthRadius_miles
#'    # Calculate  radius * cord length
#'    return( earthRadius_miles * 2*sin(angle_rad/2) )
#'    }
#' @param x surface distance in miles
#'
#' @keywords internal
#'
distance_via_surfacedistance <- function(x){
  
  return( 7918*sin(x/7918) )
}
