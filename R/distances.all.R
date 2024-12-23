

# THIS MAY BE USED ONLY FOR distance_near_eachother()


#' Find all distances between two sets of points (based on lat/lon)
#'
#' @description Returns all the distances from one set of geographic points to another set of points.
#' Can return a matrix of distances (m x n points) or vector or data.frame with one row per pair.
#' Lets you specify units and whether you need lat/lon etc, but essentially just a wrapper for
#' the \pkg{sf} package for the [sf::st_distance] and [sf::st_as_sf] functions.
#' 
#' @details  \preformatted{
#'   *** Probably slower than it needs to be partly by using data.frame 
#'    instead of matrix class? Maybe 10-20 percent faster if as.df=FALSE than if TRUE 
#'   Just using distances.all is reasonably fast? 
#'   When it was still using sp and not sf package, it was 
#'     (30-40 seconds for 
#'     100 million distances, but slow working with results so large),
#'  Sys.time(); x=distances.all(testpoints_n(1e5), testpoints_n(1000), 
#'    return.crosstab=TRUE); Sys.time()  
#'  
#'        IF NO PROCESSING OTHER THAN CROSSTAB 
#'  Sys.time(); x=distances.all(testpoints_n(1e6), testpoints_n(100),
#'     return.crosstab=TRUE); Sys.time() 
#'  
#'        (1m x 100, or 100k x 1000) 
#'  Sys.time(); x=distances.all(testpoints_n(1e6), testpoints_n(300), 
#'    return.crosstab=TRUE); Sys.time() 
#'     seconds for 300 million pairs.  
#'   plus_____ seconds or so for x[x>100] <- Inf  
#'        # so 11m blocks to 1k points could take >xxx minutes! 
#'        (you would want to more quickly remove the ones outside some radius) 
#'         
#'            About xxx seconds per site for 11m blocks?  
#'            
#'     Sys.time(); x=distances.all(testpoints_n(1e5), testpoints_n(1000), 
#'       units='miles',return.rownums=TRUE); Sys.time() 
#'   xxx SECONDS IF DATA.FRAME ETC. DONE 
#'       TO FORMAT RESULTS AND GET ROWNUMS
#'    Sys.time(); x=distances.all(testpoints_n(1e5), testpoints_n(1000), 
#'      units='miles',return.rownums=TRUE)$d; Sys.time()
#'    xxx SECONDS IF DATA.FRAME ETC. DONE 
#'       TO FORMAT RESULTS AND GET ROWNUMS IN distances.all 
#'     }
#' @param frompoints A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed.
#' @param topoints   A matrix or data.frame with two cols, 'lat' and 'lon' with datum=WGS84 assumed.
#' @param units A string that is 'miles' by default, or 'km' for kilometers,
#'   specifying units for distances returned.
#' @param return.crosstab Logical value, FALSE by default. If TRUE, value returned is a matrix of the distances,
#'   with a row per frompoint and col per topoint.
#' @param return.rownums Logical value, TRUE by default. If TRUE, value returned also includes two extra columns:
#'   a col of index numbers starting at 1 specifying the frompoint and a similar col specifying the topoint.
#'   If crosstab=TRUE, ignores return.rownums and return.latlons
#' @param return.latlons Logical value, TRUE by default. If TRUE, value returned also includes four extra columns,
#'   showing fromlat, fromlon, tolat, tolon.
#'   If crosstab=TRUE, ignores return.rownums and return.latlons
#' @param as.df Logical, default is TRUE, in which case returns a data.frame (unless vector), otherwise a matrix (unless vector).
#' @return By default, returns a dataframe that has 3 columns: fromrow, torow, distance
#'   (where fromrow or torow is the row number of the corresponding input, starting at 1).
#'   If return.crosstab=FALSE, which is default, and return.rownums and/or return.latlons is TRUE,
#'   returns a row per from-to pair, and columns depending on parameters, sorted first cycling through all topoints for first frompoint, and so on.
#'   If return.crosstab=FALSE and return.rownums and return.latlons are FALSE, returns a vector of distances in same order as rows described above.
#'   If return.crosstab=TRUE, returns a matrix of distances, with one row per frompoint and one column per topoint.
#' @seealso [latlon_infer()] [get.distances()] which allows you to specify a search radius and
#'   get distances only within that radius which can be faster,
#'   [get.distances.prepaired()] for finding distances when data are already formatted as pairs of points,
#'   [get.nearest()] which finds the distance to the single nearest point
#'   within a specified search radius instead of all topoints, and
#'   proxistat or proxistat2  which will which create a proximity score for each spatial unit
#'   based on distances to nearby points.
#' @examples
#' set.seed(999)
#' t1=testpoints_500[1,c("lon", "lat")]
#' t10=testpoints_500[1:10,c("lon", "lat")]
#' t100=testpoints_500[1:100,c("lon", "lat")]
#' t1k=rbind(testpoints_500, testpoints_500)
#'
#' distances.all(t1, t1)
#' distances.all(t1, t10[2, , drop = FALSE])
#' x=distances.all(t10, t100[1:20 , ], units = 'km')
#'  plot(x$tolon, x$tolat,pch='.')
#'  points(x$fromlon, x$fromlat)
#'  with(x, linesegments(fromlon, fromlat, tolon, tolat ))
#'  with(x[x$d < 500, ], linesegments(fromlon, fromlat, tolon, tolat ,col='red'))
#'
#'    test.from <- structure(list(fromlat = c(38.9567309094, 45),
#'      fromlon = c(-77.0896572305, -100)), .Names = c("lat", "lon"),
#'      row.names = c("1", "2"), class = "data.frame")
#'
#'    test.to <- structure(list(tolat = c(38.9575019287, 38.9507043428, 45),
#'     tolon = c(-77.0892818598, -77.2, -90)),
#'     .Names = c("lat", "lon"), class = "data.frame",
#'     row.names = c("1", "2", "3"))
#'  test.to.NA = rbind(c(NA,NA), test.to[2:3,])
#'  test.from.NA = rbind(test.from[1,], c(NA,NA))
#'  
#' distances.all(test.from, test.to)
#' distances.all(test.from, test.to, return.crosstab=TRUE)
#' distances.all(test.from, test.to, return.rownums=FALSE)
#' distances.all(test.from, test.to, return.latlons=FALSE)
#' distances.all(test.from, test.to, return.latlons=FALSE, 
#'   return.rownums=FALSE)
#'
#'      # test cases
#' distances.all(test.from,    test.to.NA)
#' distances.all(test.from.NA, test.to)
#' distances.all(test.from.NA, test.to.NA)
#' distances.all(test.from[1,],test.to[1,],return.rownums=F,
#' return.latlons=F)
#' distances.all(test.from[1,],test.to[1,],return.rownums=FALSE,
#' return.latlons=TRUE)
#' distances.all(test.from[1,],test.to[1,],return.rownums=TRUE,
#' return.latlons=FALSE)
#' distances.all(test.from[1,],test.to[1,],return.rownums=TRUE,
#' return.latlons=TRUE)
#'
#' distances.all(test.from[1,],test.to[1:3,],return.rownums=F,
#' return.latlons=F)
#' distances.all(test.from[1,],test.to[1:3,],return.rownums=FALSE,
#' return.latlons=TRUE)
#' distances.all(test.from[1,],test.to[1:3,],return.rownums=TRUE,
#' return.latlons=FALSE)
#' distances.all(test.from[1,],test.to[1:3,],return.rownums=TRUE,
#' return.latlons=TRUE)
#'
#' distances.all(test.from[1:2,],test.to[1,],return.rownums=F,
#' return.latlons=F)
#' distances.all(test.from[1:2,],test.to[1,],return.rownums=FALSE,
#' return.latlons=TRUE)
#' distances.all(test.from[1:2,],test.to[1,],return.rownums=TRUE,
#' return.latlons=FALSE)
#' distances.all(test.from[1:2,],test.to[1,],return.rownums=TRUE,
#' return.latlons=TRUE)
#'
#' round(distances.all(test.from[1:2,],test.to[1:3,],return.rownums=F,
#' return.latlons=F),1)
#' distances.all(test.from[1:2,],test.to[1:3,],return.rownums=FALSE,
#' return.latlons=T)
#' distances.all(test.from[1:2,],test.to[1:3,],return.rownums=TRUE,
#' return.latlons=F)
#' distances.all(test.from[1:2,],test.to[1:3,],return.rownums=TRUE,
#' return.latlons=TRUE)
#' distances.all(test.from[1:2,],test.to[1:3,], return.rownums=TRUE,
#'   return.latlons=TRUE, units='km')
#' distances.all(test.from[1:2,],test.to[1:3,], return.rownums=TRUE,
#'   return.latlons=TRUE, units='miles')
#'
#' distances.all(test.from[1,],test.to[1:3, ], return.crosstab=TRUE)
#' distances.all(test.from[1:2,],test.to[1, ], return.crosstab=TRUE)
#' round(distances.all(test.from[1:2,],test.to[1:3, ],
#' return.crosstab=TRUE, units='miles'),2)
#' round(distances.all(test.from[1:2,],test.to[1:3, ],
#' return.crosstab=TRUE, units='km'),2)
#' 
#' @import sf
#' 
#' @export
#' 
distances.all <- function(frompoints, topoints, units='miles',
                              return.crosstab=FALSE, return.rownums=TRUE, return.latlons=TRUE, as.df=TRUE  
                               ) {
  
  if (!(units %in% c('km', 'miles'))) {stop('units must be "miles" or "km" ')}
  km.per.mile <- convert_units(1, 'miles', 'km') # about 1.60934
  
  #   if (paste(colnames(frompoints),collapse='')!='latlon')  {
  #     warning('frompoints colnames being changed to lat and lon, in that order')
  #     colnames(frompoints) <- c('lat', 'lon')
  #   }
  
  # handle cases where an input is only one row (one point)
  if (is.vector(frompoints)) {mycols <- names(frompoints); frompoints <- matrix(frompoints, nrow = 1); dimnames(frompoints)[[2]] = mycols }
  if (is.vector(topoints))   {mycols <- names(topoints);   topoints   <- matrix(topoints,   nrow = 1); dimnames(topoints)[[2]]   = mycols }
  
  colnames(frompoints) <- latlon_infer(names(frompoints))  
  colnames(topoints)   <- latlon_infer(names(topoints))   
  
  # ADD NA HANDLING 
  from_na <- is.na(frompoints[ , 'lon']) | is.na(frompoints[ , 'lat'])
  to_na   <- is.na(topoints[   , 'lon']) | is.na(topoints[   , 'lat'])
  originalfrom <- frompoints
  originalto <- topoints
  frompoints[from_na, ] <- c(0,0) # replace NA with 0 so that spatialpoints will not stop with error
  topoints[to_na, ]     <- c(0,0)

    # DEFAULT IS METERS now !
    frompoints.sf = sf::st_as_sf(frompoints, coords = c('lon','lat'), crs = "epsg:4326")
    topoints.sf   = sf::st_as_sf(topoints,   coords = c('lon','lat'), crs = "epsg:4326")
    results.matrix <- sf::st_distance(frompoints.sf, topoints.sf ) # maybe try tolerance = 1  for 1 meter tolerance to possibly speed it up
    rm(frompoints.sf, topoints.sf)

  
  # NA HANDLING 
  frompoints <- originalfrom
  topoints    <- originalto
  results.matrix[from_na, ] <- NA
  results.matrix[ , to_na]  <- NA
  rm(originalfrom, originalto)
  
  if (units == 'miles') {
    
    results.matrix <- (results.matrix / km.per.mile ) / 1000 # because sf func returns meters by default
    
  } else {
    
    results.matrix <-  results.matrix / 1000 # to get kilometers
    
  }
  if (return.crosstab) {
    # if crosstab=TRUE, ignore return.rownums and return.latlons
    if (as.df) {return(as.data.frame(results.matrix)  )  } else {return(results.matrix)  }
  } # this will return crosstab, i.e., tall matrix of fromrow, torow, distance (3 columns, one row per from-to pair)
  
  if (!return.rownums & !return.latlons) { return( as.vector( t(results.matrix) )  ) }
  
  if (!return.rownums & return.latlons) {
    # return tall matrix with 'fromlat', 'fromlon', 'tolat', 'tolon', 'd'
    
    results <- cbind(expand.gridMatrix(topoints[,'lat'], frompoints[,'lat']),expand.gridMatrix(topoints[,'lon'], frompoints[,'lon']) , as.vector( t(results.matrix) ) )
    colnames(results) <- c('tolat', 'fromlat', 'tolon', 'fromlon', 'd')
    if (as.df) {
      return(as.data.frame(results[ , c('fromlat', 'fromlon', 'tolat', 'tolon', 'd'), drop = FALSE] )  )
    } else {
      return( results[ , c('fromlat', 'fromlon', 'tolat', 'tolon', 'd')] )
    }
  }
  
  if (return.rownums & !return.latlons) {
    # return tall matrix with fromrow, torow, d
    # **** BUT THIS MAY BE TOO SLOW OR FAILS FOR LARGE NUMBERS LIKE 100k frompoints x 10K topoints
    
    results = cbind(expand.gridMatrix(1:length(topoints[,'lat']), 1:length(frompoints[,'lat'])), as.vector( t(results.matrix) )  )
    colnames(results) <- c('torow', 'fromrow', 'd')
    if (as.df) {
      return(as.data.frame(results[ , c('fromrow', 'torow', 'd'), drop = FALSE] )  )
    } else {
      return( results[ , c('fromrow', 'torow', 'd')] )
    }
  }
  
  if (return.rownums & return.latlons) {
    # return tall matrix with 'fromrow', 'torow', 'fromlat', 'fromlon', 'tolat', 'tolon', 'd'
    
    results = cbind(expand.gridMatrix(1:length(topoints[,'lat']), 1:length(frompoints[,'lat'])), as.vector( t(results.matrix) )  )
    colnames(results) <- c('torow', 'fromrow', 'd')
    
    results <- cbind(expand.gridMatrix(topoints[,'lat'], frompoints[,'lat']),expand.gridMatrix(topoints[,'lon'], frompoints[,'lon']) , results)
    colnames(results) <- c('tolat', 'fromlat', 'tolon', 'fromlon', 'torow', 'fromrow', 'd')
    if (as.df) {
      return(as.data.frame(results[ , c('fromrow', 'torow', 'fromlat', 'fromlon', 'tolat', 'tolon', 'd'), drop = FALSE]  )  )
    } else {
      return( results[ , c('fromrow', 'torow', 'fromlat', 'fromlon', 'tolat', 'tolon', 'd')]  )
    }
  }
}
