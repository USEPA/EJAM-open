
#' Just remove percentage signs and convert N/A to "NA"
#' 
#' @param x data.frame from ejscreenapi output 
#' @return data.frame
#'
#' @noRd
#' @export
#'
makenumericdfFORSHINY <- function(x) {
  
  cleanit <- function(z) {
    as.data.frame(lapply(z, function(y) {
      # gsub('th', '', 
      # gsub('<', '', 
      # gsub(' miles', '', 
      gsub('N/A', 'NA', 
           gsub('%','', 
                 # gsub(',', '',
                      y)) #) #   )) )
                }),
           stringsAsFactors = FALSE) 
      }
  
  clean <- suppressWarnings(cleanit(x))
  
  for (i in 1:NCOL(x)) {
    if (all(is.na(suppressWarnings(as.numeric(clean[, i])))) & !all(is.na(clean[, i]))) {
      # was not all NA but got forced to NA via as.numeric means was a real character col
    } else {
      clean[, i] <- suppressWarnings( as.numeric(clean[, i]) )
    }
  }
  return(clean)  
  }
