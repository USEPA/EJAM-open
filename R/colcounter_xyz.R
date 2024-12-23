#' Count columns with Value (at or) above (or below) threshold
#' 
#' @param x Data.frame or matrix of numbers to be compared to threshold value.
#' @param threshold numeric threshold value to compare to
#' @param or.tied if TRUE, include ties (value in x equals threshold)
#' @param na.rm if TRUE, used by colcounter to count only the non-NA columns in given row
#' @param below if TRUE, count x below threshold not above threshold
#' @param one.cut.per.col if FALSE, compare 1 threshold to all of x.
#'   If TRUE, specify one threshold per column.
#' @return vector of counts as long as NROW(x)
#' @seealso [colcounter_summary_all()] [colcounter_summary()] [colcounter_summary_cum()] [colcounter_summary_pct()] [colcounter_summary_cum_pct()]
#' 
#' @examples \dontrun{
#'   pdata <- data.frame(a=rep(80,4),b=rep(93,4), col3=c(49,98,100,100))
#'  # pdata <- data.frame(testoutput_ejamit_10pts_1miles$results_bysite)[ , names_e_pctile]
#'  pcuts <-  5 * (0:20) 
#' colcounter_summary(        pdata, pcuts)
#' colcounter_summary_pct(    pdata, pcuts)
#' colcounter_summary_cum(    pdata, pcuts)
#' colcounter_summary_cum_pct(pdata, pcuts)
#' colcounter_summary_cum_pct(pdata, 5 * (10:20))
#'
#' x80 <- colcounter(pdata, threshold = 80, or.tied = T)
#' x95 <- colcounter(pdata, threshold = 95, or.tied = T)
#' table(x95)
#' tablefixed(x95, NCOL(pdata))
#' cbind(at80=tablefixed(x80, NCOL(pdata)), at95=tablefixed(x95, NCOL(pdata)))
#'   }
#'
#' @export
#'
colcounter <- function(x, threshold, or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE) {
  # Function to count SCORES ABOVE BENCHMARK(S) at each place, returns list as long as NROW(x).
  #
  
  if (is.null(dim(x))) {
    numcols <- 1
    if (shiny::isRunning()) {
      warning('expected data.frame as x but has only 1 dimension')
      return(rep(NA, length(x)))
    } else {
      stop('expected data.frame as x but has only 1 dimension')
    }
  } else {
    numcols <- dim(x)[2]
  }
  if (missing(threshold)) {
    if (one.cut.per.col) {
      threshold <- colMeans(x, na.rm = na.rm)
    } else {
      threshold <- rowMeans(x, na.rm = na.rm)
    }
  }
  if (one.cut.per.col) {
    if (length(threshold) != NCOL(x)) {
      if (shiny::isRunning()) {
        warning('length of threshold should be same as number of columns in x if one.cut.per.col=T')
        return(rep(NA, length(x)))
      } else {
        stop('length of threshold should be same as number of columns in x if one.cut.per.col=T')
      }
    }
    x <- t(as.matrix(x)) # this allows it to compare vector of N thresholds to N columns
  } else {
    if ((length(threshold) != NROW(x)) & (length(threshold) != 1)) {
      if (shiny::isRunning()) {
        warning('length of threshold should be 1 or same as number of columns in x, if one.cut.per.col=F')
        return(rep(NA, length(x)))
      } else {
        stop('length of threshold should be 1 or same as number of columns in x, if one.cut.per.col=F')
      }
    }
  }
  if (below) {
    if  (or.tied) { y <- ( x <= threshold) }
    if (!or.tied) { y <- ( x <  threshold) }
  } else {
    if  (or.tied) { y <- ( x >= threshold) }
    if (!or.tied) { y <- ( x >  threshold) }
  }
  if (one.cut.per.col) {y <- t(y)}
  count.per.row <- rowSums(y, na.rm = na.rm)
  return(count.per.row)
}
######################################## #


#' Summarize how many rows have N columns at or above (or below) various thresholds?
#' 
#' @description 
#' Like colcounter() or cols.above.count()
#'   but will handle multiple thresholds to compare to each indicator, etc.
#'   
#'   Table of counts, percents, cumulative counts, cumulative percents
#'   of places with N, or at least N, of the indicators
#'   at or above the benchmark(s)
#'   
#' @param x Data.frame or matrix of numbers to be compared to threshold value,
#'   like percentiles for example.
#' @param thresholdlist vector of numeric threshold values to compare to
#' @param or.tied if TRUE, include ties (value in x equals threshold)
#' @param na.rm if TRUE, used by [colcounter()] to count only the non-NA columns in given row
#' @param below if TRUE, count x below threshold not above threshold
#' @param one.cut.per.col if FALSE, compare each threshold to all of x.
#'   If TRUE, specify one threshold to use for each column.
#' @seealso [colcounter_summary_all()] [colcounter_summary()] [colcounter_summary_cum()] [colcounter_summary_pct()] [colcounter_summary_cum_pct()]
#'    [tablefixed()]
#' @return A table of frequency counts
#'
#' @examples
#'  pdata <- data.frame(a=rep(80,4),b=rep(93,4), col3=c(49,98,100,100))
#'   ### pdata <- EJAM::blockgroupstats[ , names_e_pctile]
#'  pcuts <-  5 * (0:20)  
#' colcounter_summary(        pdata, pcuts)
#' colcounter_summary_pct(    pdata, pcuts)
#' colcounter_summary_cum(    pdata, pcuts)
#' colcounter_summary_cum_pct(pdata, pcuts)
#' colcounter_summary_cum_pct(pdata, 5 * (10:20))
#' a3 <- colcounter_summary_all(    pdata, pcuts)
#'
#' x80 <- colcounter(pdata, threshold = 80, or.tied = T)
#' x95 <- colcounter(pdata, threshold = 95, or.tied = T)
#' table(x95)
#' tablefixed(x95, NCOL(pdata))
#' cbind(at80=tablefixed(x80, NCOL(pdata)), at95=tablefixed(x95, NCOL(pdata)))
#' 
#' @keywords internal
#'
colcounter_summary <- function(x, thresholdlist, or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE) {

  ccount <- NCOL(x)
  if (ccount == 1) x <- data.frame(x)
  countpersite_table <- sapply(
    thresholdlist,
    FUN = function(thiscut) {
      tablefixed(
        colcounter(x, thiscut, or.tied = or.tied, na.rm = na.rm, below = below, one.cut.per.col = one.cut.per.col),
        ccount
      )
    }
  )
  colnames(countpersite_table) <-  thresholdlist
  dimnames(countpersite_table) <- list(count.of.cols = rownames(countpersite_table), threshold = thresholdlist)
  return(countpersite_table)
}
######################################## ######################################### #


#' Summarize how many rows have AT LEAST N columns at or above (or below) various thresholds
#' 
#' See colcounter_summary() for details and examples
#' 
#' @param x Data.frame or matrix of numbers to be compared to threshold value,
#'   like percentiles for example.
#' @param thresholdlist vector of numeric threshold values to compare to
#' @param or.tied if TRUE, include ties (value in x equals threshold)
#' @param na.rm if TRUE, used by colcounter to count only the non-NA columns in given row
#' @param below if TRUE, count x below threshold not above threshold
#' @param one.cut.per.col if FALSE, compare each threshold to all of x.
#'   If TRUE, specify one threshold to use for each column.
#' @seealso [colcounter_summary_all()] [colcounter_summary()] [colcounter_summary_cum()] [colcounter_summary_pct()] [colcounter_summary_cum_pct()]
#'
#' @keywords internal
#'
colcounter_summary_cum <- function(x, thresholdlist, or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE) {
  apply(colcounter_summary(x, thresholdlist = thresholdlist, or.tied = or.tied, na.rm = na.rm,below = below,one.cut.per.col = one.cut.per.col),
        MARGIN = 2, FUN = function(thiscol) rev(cumsum(rev(thiscol))))
}
######################################## ######################################### #


#' Summarize what percent of rows have N columns at or above (or below) various thresholds
#' 
#' See colcounter_summary() for details and examples
#' 
#' @param x Data.frame or matrix of numbers to be compared to threshold value,
#'   like percentiles for example.
#' @param thresholdlist vector of numeric threshold values to compare to
#' @param ... passed to colcounter_summary()
#'   like or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE
#' @seealso [colcounter_summary_all()] [colcounter_summary()] [colcounter_summary_cum()] [colcounter_summary_pct()] [colcounter_summary_cum_pct()]
#'
#' @keywords internal
#'
colcounter_summary_pct <- function(x, thresholdlist, ...)  {
  100 * round(colcounter_summary(x, thresholdlist = thresholdlist, ...) / NROW(x), 2)
  }
######################################## ######################################### #


#' Summarize what percent of rows have AT LEAST N columns at or above (or below) various thresholds
#'
#' @param x Data.frame or matrix of numbers to be compared to threshold value,
#'   like percentiles for example.
#' @param thresholdlist vector of numeric threshold values to compare to
#' @param ... passed to colcounter_summary_cum()
#'   like or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE
#' @seealso [colcounter_summary_all()] [colcounter_summary()] [colcounter_summary_cum()] [colcounter_summary_pct()] [colcounter_summary_cum_pct()]
#'
#' @keywords internal
#'
colcounter_summary_cum_pct <- function(x, thresholdlist, ...) {
  100 * round(colcounter_summary_cum(x, thresholdlist = thresholdlist, ...) / NROW(x), 2)
  }
######################################## ######################################### #


#' Summarize count (and percent) of rows with exactly (and at least) N cols >= various thresholds
#' 
#' @description This wraps 4 functions to return 4 tables:
#'   using [colcounter_summary()], [colcounter_summary_pct()],
#'   [colcounter_summary_cum()], [colcounter_summary_cum_pct()]
#'   For another view and text explanations of the findings, see
#'   [count_sites_with_n_high_scores()]
#'   
#' @param x Data.frame or matrix of numbers to be compared to threshold value,
#'   like percentiles for example.
#' @param thresholdlist vector of numeric threshold values to compare to
#' @param ... passed to the 4 functions
#'   like or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE
#' @seealso [colcounter_summary_all()] [colcounter_summary()] [colcounter_summary_cum()] [colcounter_summary_pct()] [colcounter_summary_cum_pct()]
#' 
#' @return A table of cumulative frequency counts etc., including
#'   count, cum, pct, cum_pct 
#'   
#' @examples
#'     # df <-  bg22[ , names.ej.pctile]
#'  df <- data.frame(a=rep(80,4),b=rep(93,4), col3=c(49,98,100,100))
#'  bench <- 5 * (0:20)
#'  a3 <- colcounter_summary_all(df, bench)
#'  a3[,'95',]
#'  a3[,,'cum_pct']
#'  a3['0',,]; a3[1,,]
#'  a3[dim(a3)[1],,]
#'  # a3['12',,]; a3[13,,]
#'  
#'  \dontrun{
#'  barplot(colcounter_summary_cum_pct(pdata, pcuts)[ , '80'],
#'     ylab='% of places', xlab='# of indicators at/above threshold',
#'     main='% of places with at least N/12 indicators >=80th percentile')
#'
#'  barplot(colcounter_summary(pdata, pcuts)[2:13 , '95'],
#'     ylab='# of places', xlab='# of indicators at/above threshold',
#'     main='# of places with exactly N/12 indicators >=95th percentile')
#'
#'   # pdata <- ejscreen package file bg22[ , names.e.pctile]
#'   colcounter_summary_cum_pct(pdata,c(50,80,90,95))
#'   xs <- 1:12
#'   plot(x=xs, y=colcounter_summary_cum_pct(pdata, 50)[xs+1],
#'    type='b', col='gray', ylim=c(0, 100),
#'     main='% of places with at least x/12 indicators >=Nth percentile',
#'      ylab='% of places', xlab='# of indicators')
#'   points(xs, colcounter_summary_cum_pct(pdata, 80)[xs+1], type='b', col='blue')
#'   points(xs, colcounter_summary_cum_pct(pdata, 90)[xs+1], type='b', col='orange')
#'   points(xs, colcounter_summary_cum_pct(pdata, 95)[xs+1], type='b', col='red')
#'   legend(x = 'topright', legend = paste0('>= ', c(50, 80, 90, 95),'th percentile'),
#'    fill = c('gray', 'blue', 'orange', 'red'))
#'
#'   # pdata <- bg22[ ,  names.ej.pctile]
#'   colcounter_summary_cum_pct(pdata,c(50,80,90,95))
#'   xs <- 1:12
#'   plot(x=xs, y=colcounter_summary_cum_pct(pdata, 50)[xs+1], 
#'     type='b', col='gray', ylim=c(0, 40),
#'     main='% of places with at least x/12 indicators >=Nth percentile', ylab='% of places', 
#'     xlab='# of indicators')
#'   points(xs, colcounter_summary_cum_pct(pdata, 80)[xs+1], type='b', col='blue')
#'   points(xs, colcounter_summary_cum_pct(pdata, 90)[xs+1], type='b', col='orange')
#'   points(xs, colcounter_summary_cum_pct(pdata, 95)[xs+1], type='b', col='red')
#'   legend(x = 'topright', legend = paste0('>= ', c(50, 80, 90, 95),'th percentile'), 
#'     fill = c('gray', 'blue', 'orange', 'red'))
#' }
#'
#' @export
#'
colcounter_summary_all <- function(x, thresholdlist, ...) {
  
  listall <- list(
    counts =  colcounter_summary(        x, thresholdlist = thresholdlist, ...),
    cum =     colcounter_summary_cum(    x, thresholdlist = thresholdlist, ...),
    pct =     colcounter_summary_pct(    x, thresholdlist = thresholdlist, ...),
    cum_pct = colcounter_summary_cum_pct(x, thresholdlist = thresholdlist, ...)
  )
  bincount <- length(0:NCOL(x))
  arrayall <- array(NA, dim = c(bincount, length(thresholdlist), 4))
  for (i in 1:4) {arrayall[ ,, i] <- listall[[i]]}
  dimnames(arrayall) <- list(count = 0:NCOL(x), cut = thresholdlist, 
                             stat = c('count', 'cum', 'pct', 'cum_pct'))
  arrayall
}
######################################## ######################################### #
