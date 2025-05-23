#' helper function that reports on how long buffering took
#'
#' @param start start time 
#' @param end end time
#' @param n how many buffers were completed
#'
#' @return text string summarizing the speed
#' @seealso [speedmessage()]
#' 
#' @keywords internal
#' @export
#'
speedreport <- function(start,end,n) {
  
  # define speedreport function ####
  # report time elapsed and avg speed
  benchmark.start <- start
  benchmark.end <- end
  total.benchmark <- difftime(benchmark.end, benchmark.start)
  total.seconds   <- difftime(benchmark.end, benchmark.start, units = 'secs')
  perhour <- round((n / as.numeric(total.seconds)) * 3600, 0)
  if (total.seconds >= 1) {
    #cat('\n')
    cat(paste0(
      'Rate of ',
      format(round((n / as.numeric(total.seconds)) * 3600, 0), big.mark = ',', scientific = FALSE), 
      ' buffers per hour: ',
      format(n,big.mark = ',', scientific = FALSE),
      ' lat/long pairs took ',
      format(round(    as.numeric(total.seconds), 0),         big.mark = ',', scientific = FALSE),
      ' seconds'
    )  )
    cat('\n')
  }
  # print(round(total.benchmark, 1))
  invisible(perhour)
}
################################################################# #


#' estimate how long it will take to get buffer batch results
#' @param n number of points to buffer at
#' @param perhourslow n per hour if slow (conservative estimate of time needed)
#' @param perhourfast n per hour if fast
#' @param perhourguess n per hour best guess
#' @seealso [speedreport()]
#' 
#' @keywords internal
#' 
speedmessage <- function(n, perhourslow = 1000, perhourfast = 12000, perhourguess = 6000) {
  
  fast_minutes  <- round(60 * n / perhourfast, 1)
  guess_minutes <- round(60 * n / perhourguess, 1)
  slow_minutes  <- round(60 * n / perhourslow, 1)
  
  msg <- paste0(
    'Results for ', n, ' points may take ',
    guess_minutes, ' minutes (but up to ',
    # fast_minutes, '-', 
    slow_minutes,')'
  )
  return(msg)
}
################################################################# #
