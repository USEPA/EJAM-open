

#' validate industry NAICS codes
#'
#' @param code vector of one or more
#'   numeric or character codes like c(22, 111, 4239, 423860)
#'
#' @return logical vector, TRUE means valid
#' @examples
#'   naics_is.valid(c(22, "022", " 22", 111, "4239", 423860))
#'   # table(naics_is.valid(frs_by_naics$NAICS)) / NROW(frs_by_naics)
#'
naics_is.valid <- function(code) {
  
  code %in% NAICS  # EJAM::NAICS  # EJAM::naicstable$code
  
}



