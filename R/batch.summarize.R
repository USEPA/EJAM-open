# batch.summarize.helpers functions are in a separate file ####

# batch.summarize()  ####

#' @title Core function that calculates summary stats across all sites and nearby people for EJScreen batch analysis
#' @description This is the function that takes the full tables of batch buffer results and calculates summary statistics 
#'   like maximum at any site, median across all sites, maximum percentile for all specified indicators at a given site, etc.
#'   It can be expanded to provide other summary stats by adding those other formulas to this code. 
#' @param sitestats A matrix or data.frame to summarize, one row per site, one column per variable. 
#'   Must have correct stats for all people near a given site. 
#'   Or full path to .csv or .xlsx file with that in a tab called "Each Site" as created by
#'   EJAM package ejam2excel function.
#' @param popstats  A matrix or data.frame to summarize, one row per site, one column per variable. 
#'   Must have reduced counts that count only once each unique person near one or more of the sites. 
#'   Used to sum population and get stats of distribution of each indicator across all unique individuals.
#' @param cols NOT USED YET. Specifies which colums of x should be summarized or used during summarization. A single string value 'all' as default to specify all, or a vector of colnames.
#' @param probs Vector of numeric values, fractions, to use as probabilities used in finding quantiles. Default is c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1)
#' @param na.rm Logical TRUE by default, specifying if na.rm should be used for sum(), mean(), and other functions.
#' @param wtscolname Name of the column that contains the relevant weights to be used (e.g., "pop")
#' @param thresholds list of vectors each with 1+ thresholds (cutpoints) used to count find sites 
#'  where 1+ of given set of indicators are at/above the threshold & how many of the indicators are.
#'  If an element of the list is a single number, that is used for the whole group (all the threshnames in that nth list element). 
#'  Otherwise/in general, each vector is recycled over the threshnames in corresponding list element, 
#'  so each threshname can have its own threshold like some field-specific benchmark, or they can all use the same threshold like 50.
#' @param threshnames list of vectors of character colnames defining fields in x that get compared to threshold, or to thresholds
#' @param threshgroups of 1+ character strings naming the elements of threshnames list, such as "EJ US pctiles"
#' @param rowfun.picked logical vector specifying which of the pre-defined functions (like at/above threshold) are needed and will be applied
#' @param colfun.picked  logical vector specifying which of the pre-defined functions (like colSums) are needed and will be applied
#' @param quiet optional logical, set to TRUE to stop printing results to console in RStudio. 
#' @param testing optional, default is FALSE. prints some debugging info if TRUE.
#' @export
#' @return output is a list with two named elements, rows and cols, where each is a matrix of summary stats.
#'   
#'   cols: Each element in a summary col summarizes 1 row (site) across all the RELEVANT cols of batch data (e.g., all US EJ Index percentiles)
#'   
#'   rows: Each element in a summary row summarizes 1 column (field) across all the rows of batch data.
#'   
#'   keystats: subset of results, for convenience
#'   
#'   keyindicators: subset of results, for convenience
#'   
batch.summarize <- function(sitestats, popstats, cols = 'all', 
                            wtscolname = 'pop', 
                            probs = c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1),
                            
                            thresholds = list(90), # only makes sense for the percentiles ones though!
                            # thresholds = list(90, 90),
                            threshnames = list(names(which(sapply(sitestats,  class) != 'character'))), # all numeric columns 
                            # threshnames = list(c(names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile)),
                            # threshnames = list(c(names_ej_pctile, names_ej_state_pctile), c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
                            threshgroups = list('variables'),
                            # threshgroups = list("EJ_or_Supp_EJ_US_or_State"),
                            # threshgroups = list("EJ-US-or-ST", "Supp-US-or-ST"),
                            
                            na.rm = TRUE, rowfun.picked = 'all', colfun.picked = 'all',
                            quiet = FALSE, testing = FALSE) {
  
  # ERROR CHECKING ####
  
  if (length(sitestats) == 1 && is.character(sitestats) && file.exists(sitestats)) {
    cat("seems like a file, so trying to read the 'Each Site' tab if .xlsx or the whole table if .csv")
    if (grepl(".csv$", sitestats)) {
      sitestats <- read.csv(sitestats)
    }
    if (grepl(".xlsx$", sitestats)) {
      sitestats <- openxlsx::read.xlsx(sitestats, "Each Site")
    }
    sitestats <- batch.clean(sitestats, namesfile = "keepnames") # not sure it is essential. now default is to not rename columns.
  }
  
  if (!missing(sitestats) && (missing(popstats) || is.null(popstats))) {popstats <- sitestats} # for convenience if ignoring weighting 
  if (missing(sitestats) || missing(popstats)) {stop('sitestats and popstats are outputs of batch processing in EJScreen and must each be a matrix or data.frame to be analyzed, where each row has stats on a buffer around some point called a site')}
  sitestats <- as.data.frame(sitestats) # in case it was a data.table, or tibble
  popstats  <- as.data.frame(popstats)
  
  if (cols[1] == 'all') {cols <- colnames(sitestats)}
  if (!all.equal(dim(sitestats), dim(popstats))) {stop('sitestats and popstats must be identical in number of rows and in number of columns')}
  if (!all.equal(colnames(sitestats), colnames(popstats))) {stop('sitestats and popstats must be identical in colnames')}
  if (any(!(cols %in% colnames(sitestats)))) {stop('invalid cols -- must be a vector of strings, all of which must be elements of names(sitestats)')}
  if (!is.atomic(probs) || !is.vector(probs) || !is.numeric(probs) || any(probs > 1) || any(probs < 0)) {stop('probs must be a numeric vector of fractions for quantile function')}
  
  if (1 != length(unique( length(thresholds), length(threshnames), length(threshgroups)))) {stop('lengths of thresholds list, threshnames list, threshgroups list must be identical') }
  if (!all(unlist(threshnames) %in% colnames(sitestats))) {
    warning("specified threshnames not all found in sitestats colnames, so using defaults")
    threshnames = list(names(which(sapply(sitestats,  class) != 'character')))
    threshgroups = list('variables')
    thresholds = list(90)
  }
  
  # numericols  <- names(which(sapply(sitestats,  class) != 'character'))
  
  # Set population weights to use unique residents, not double count if near 2 sites. #####
  stopifnot(wtscolname %in% names(sitestats))
  wts <- unlist(sitestats[ , wtscolname])
  ## set pop = 0 for invalid sites
  wts[is.na(wts)] <- 0
  
  #|################################################################################# ####
  # SUMMARY AT EACH SITE - Max or # of high scores at a site. ####
  #  1 summary stat = 1 COLUMN (1 site=1 row):
  ################################################################################# ##### #
  
  rowfuname <- vector()
  rowfun  <- list()
  rowargs <- list()
  i <- 0
  
  for (setnum in 1:length(threshgroups)) {
    # comparisons to thresholds(s), one set per element/vector in the list
    # one set of thresholds, which are recycled as needed to match length of this threshnames[[threshnum]] vector
    
    ############################################ #
    # ...At site, what is max indicator? ####
    # i.e., FOR EACH SITE WHAT IS THE MAX VALUE OF A FEW INDICATORS (PERCENTILES) AT THAT SITE?
    ############################################ #
    i = i + 1
    rowfuname[i] = paste('Max of', threshgroups[[setnum]])
    rowfun[[i]] = function(x, varnames, na.rm = na.rm) {
      rowMaxs.sofar <- suppressWarnings(rowMaxs2( x[ , varnames ], na.rm = na.rm))
      # replace negative infinity with NA, to handle cases where entire row was NA values so rowMaxs returned -Inf
      rowMaxs.sofar[is.infinite(rowMaxs.sofar)] <- NA
      return(rowMaxs.sofar)
    }
    rowargs[[i]] <- list(x = NULL, varnames = threshnames[[setnum]], na.rm = na.rm)
    
    
    ############################################ #
    # ...At site, how many indicators are > x percentile? ####
    # FOR EACH SITE HOW MANY OF A FEW INDICATORS (PERCENTILES) ARE AT/ABOVE A SPECIFIED THRESHOLD?
    ############################################ #
    i = i + 1
    rowfuname[i] = paste('Number of', threshgroups[[setnum]], 'at/above threshold of', paste(thresholds[[setnum]], collapse = '/' ) )
    rowfun[[i]] = function(x, varnames, threshold, or.tied, na.rm) {
        colcounter(       x[ , varnames], threshold = threshold, or.tied = or.tied, na.rm = na.rm )
      # cols.above.count( x[ , varnames], threshold = threshold, or.tied = or.tied, na.rm = na.rm )
    }
    rowargs[[i]] <- list(x = NULL, varnames = threshnames[[setnum]], threshold = thresholds[[setnum]], or.tied = TRUE, na.rm = na.rm)
    
    
    ############################################ #
    # ...At site, are any scores above X? 
    # NOT ESSENTIAL ONCE YOU HAVE THE NUMBER OF SITES AT/ABOVE
    ############################################ #
    #     i=i+1
    #     rowfuname[i]=paste('Are any', threshgroups[[setnum]], 'at/above threshold of', paste(thresholds[[setnum]], collapse='/' ) )
    #     rowfun[[i]]= function(x, varnames, threshold, or.tied, na.rm) {
    #       0 < colcounter( x[ ,  varnames], threshold=threshold, or.tied=or.tied, na.rm=na.rm ) 
    #     }
    #     rowargs[[i]] <- list(x=NULL, varnames=threshnames[[setnum]], threshold=thresholds[[setnum]], or.tied=TRUE, na.r = a.rm)
    
  }
  
  
  #|################################################################################# ####
  # SUMMARY STATS LIKE percent LOW-INCOME OVERALL OR AVG TRAFFIC SCORE  ####
  #- A SUMMARY OF EACH INDICATOR for THE WHOLE SET OF UNIQUE PEOPLE and over ALL SITES  
  # (e.g. overall percent lowincome, or avg person's traffic score, max, etc)
  # 1 summary stat = 1 ROW (1 env or demog INDICATOR = 1 column
  ###################################################################################### #
  
  # THESE SUMMARY FUNCTIONS RETURN 1 ROW EACH:
  
  colfuname <- vector()
  colfun <- list()
  bywhat <- vector() # specify if this function gets applied across people or across sites
  # n specifies the order in which summary stat rows will appear
  n = 0
  
  # ...avg site ####
  n = n + 1
  colfuname[n] = 'Average site'
  colfun[[n]] = function(x, ...) {colMeans(x, na.rm = na.rm)}
  bywhat[n] <- 'site'
  
  # ...avg person ####
  n = n + 1
  colfuname[n] = 'Average person'
  # *** above specified wts as popstats[ , wtscolname] since wts is not passed to this function later
  colfun[[n]] = function(x, ...) {collapse::fmean(x, w = wts, na.rm = na.rm)}
  bywhat[n] <- 'pop'
  
  # ...median site ####
  n = n + 1
  colfuname[n] = 'Median site'
  colfun[[n]] = function(x, ...) {
    sapply(x, FUN = function(y) {stats::median(y, na.rm = na.rm)})
  }
  bywhat[n] <- 'site'
  
  # ...median person ####
  n = n + 1
  colfuname[n] = 'Median person' 
  colfun[[n]] = function(x, ...) {
    collapse::fmedian(x = x, w = wts, na.rm = TRUE)
    # collapse::fquantile(x = y, w = wts, na.rm = TRUE, probs = 0.50)
    ## confirm this works as expected:
    # > x1 = sapply(testingdata,   Hmisc::wtd.quantile, weights = testingdata$pop, probs = 0.50)
    # > x2 = collapse::fmedian(x = testingdata, w = testingdata$pop, na.rm = TRUE)
    # > all.equal(as.vector(x1), as.vector(x2))
    #   TRUE !
  }
  bywhat[n] <- 'pop'
  
  # ...min ####
  n = n + 1
  colfuname[n] = 'Min'
  colfun[[n]] = function(x, ...) {
    # quiet warning about Inf or -Inf if no non-missing args and set those results to NA
    z = suppressWarnings( colMins2(x, na.rm = na.rm) )
    z[is.infinite(z)] <- NA
    return(z)
  }
  bywhat[n] <- 'site'
  
  # ...max ####
  n = n + 1
  colfuname[n] = 'Max'
  colfun[[n]] = function(x, ...) {
    # quiet warning about Inf or -Inf if no non-missing args and set those results to NA
    z = suppressWarnings( colMaxs2(x, na.rm = na.rm) )
    z[is.infinite(z)] <- NA
    return(z)
  }
  bywhat[n] <- 'site'
  
  # ...sum ####
  n = n + 1
  colfuname[n] = 'Sum'
  colfun[[n]] = function(x, ...) {colSums(x, na.rm = na.rm)}
  bywhat[n] <- 'pop'
  
  
  # ...Ratio of avgs or Avg of ratios?? (US) ####
  
  # EJAM package handles this on its own.
  
  # Ratio of (overall avg score) /(US avg)? is it same as Overall Avg of (Ratio of local score/usavg) ?
  # **** MIGHT NOT NEED THIS HERE IF RATIO TO USAVG IS ADDED AS ANOTHER TYPE OF RAW DATA 
  # IF RATIO IS ALREADY CALCULATED FOR EVERY SITE AND EVERY INDICATOR
  # THEN THE AVG SITE AND AVG PERSON FORMULAS ABOVE WILL CAPTURE THAT MEAN RATIO. 
  #
  # Indicator X for avg person nearby / score for avg person in US (US/Reg/ST)
  # Is this useful to put in the big table as a summary row? Probably? But need means in a row of their own ideally.
  # The summary tables do this, also, via avg score across all people over us mean, e.g.
  # but probably cleaner to do it as part of the big table's summary stats rows.
  # 
  # n = n + 1
  #  colfuname[n]='Ratio of indicator X AT AVG SITE OR AVG PERSON to US pop avg'
  #  colfun[[n]]=function(x, ...) function(x, usavgvalues, na.rm = TRUE) { x / usavgvalues } # that is not how the math will work - just a placeholder
  # bywhat[n] <- 'site'
  
  # ...Ratio to State avg? ####
  #i = i+1
  #rowfunames[i] = 'Ratio to State pop avg'
  #rowfun[i] = function(x, stateavgvalues, na.rm = TRUE) { x / stateavgvalues } # that is not how the math will work - just a placeholder
  
  # ...Count of sites? NOT NEEDED FOR EVERY SINGLE INDICATOR COLUMN ####
  #   n = n + 1
  #   colfuname[n]='Count of sites'
  #   colfun[[n]]=function(x, ...) {apply(x, 2, FUN=function(y) length(y))}
  #   bywhat[n] <- 'site'
  
  # ...Std Dev? NOT USEFUL- Percentiles might be, but not SD. ####
  #n = n + 1
  #  colfuname[n]='Standard Deviation'
  #  colfun[[n]]=function(x, ...) {apply(x, 2, FUN=function(y) {sd(y, na.rm=na.rm)}) }
  # bywhat[n] <- 'site'
  
  # ...Others? ####
  
  
  #|########################################## ####
  # SUMMARY STATS LIKE QUANTILES  ####
  # VIA FUNCTIONS TO CREATE MULTIPLE SUMMARY STAT ROWS EACH:
  # THAT REQUIRES A DIFFERENT APPROACH TO POPULATING THE RESULTS VARIABLE
  # 1) could append the group of quantiles to summarycols (use probs) outside the loop over functions, using rbind instead of [i] <-  
  # 2) could write each quantile as a single function - time consuming to write out & hard to accomodate probs. 
  # 3) could have each function also have a value that specifies how many rows or cols it will output & account for that (seems complicated)
  
  # ...quantiles ####
  n = n + 1
  # while not yet working, use other approach via rbind, later in this code:
  if (1 == 0) {
    just.rbind.quantiles <- FALSE
    if (length(probs) > 0) {
      myfunlist <- list()
      for (z in 1:length(probs)) {
        myfunlist[[z]] <- function(x, ...) {  }
        f <- (parse( " function (x) ", as.symbol("{"), paste('quantile(x,probs = probs[',z,'], na.rm = na.rm)'), '}' )) 
        body(myfunlist[[z]]) <- f
      }
      colfuname[n:(n - 1 + length(probs))]    <- paste('Percentile of sites', 100 * probs)
      colfun[[  n:(n - 1 + length(probs))  ]] <- myfunlist
      
      myfunlist <- list()
      
      ## should do any quantiles here or not in this batch.sum function at all ? this whole batch sum function was not designed with vectorization across probs, etc., as an option
      # f <- parse( " function (x) ", as.symbol("{"), paste('collapse::fquantile(x, w = wts, probs = probs    , na.rm = na.rm)'), '}' )  
      
      for (z in 1:length(probs)) {
        myfunlist[[z]] <- function(x, ...) {  }
        
        # *** may need to specify wts here as popstats[ , wtscolname]
        
        # # stop using Hmisc package wtd.quantile() and try collapse package fquantile() since EJAM already uses collapse and it is faster
        ## ?collapse::fquantile
        ## *** do not need to do it in a loop at all if using fquantile() !!!
        # f <- (parse( " function (x) ", as.symbol("{"), paste('wtd.quantile(x, weights = wts, probs = probs[',z,'], na.rm = na.rm)'), '}' )) 
        f <- 0 #placeholder
        f <- (parse( " function (x) ", as.symbol("{"), paste('collapse::fquantile(x, w = wts, probs = probs[',z,'], na.rm = na.rm)'), '}' )) 
        
        
        body(myfunlist[[z]]) <- f
      }
      
      colfuname[(n + length(probs)):((n - 1) + 2 * length(probs))]  <- paste('Percentile of people', 100 * probs)
      colfun[[  (n + length(probs)):((n - 1) + 2 * length(probs))]] <- myfunlist
      
      nextcol <- 1 + ((n - 1) + 2 * length(probs)) 
      # *** nextcol is defined but not used anywhere in this draft code
    } 
  } else {
    just.rbind.quantiles <- TRUE  
    # while not working
  }
  
  # colfuname[ nextcol ]=' '
  # colfun[[ nextcol  ]]=function(x, na.rm=TRUE) {  }
  
  
  
  #|################################################################################# ####
  # CALL SUMMARY FUNCTIONS #####
  ############################################ #
  
  colfuns.count.all <- length(colfun)
  rowfuns.count.all <- length(rowfun)
  
  # for now, just pick all of them by default. later allow user to select perhaps.
  if (colfun.picked == 'all') {
    colfun.picked = rep(TRUE, colfuns.count.all)  
  }
  if (rowfun.picked == 'all') {
    rowfun.picked = rep(TRUE, rowfuns.count.all) 
  }
  
  colfuns.count.picked <- sum(colfun.picked)
  rowfuns.count.picked <- sum(rowfun.picked)
  
  # preallocate space to store summary stats on only those picked
  summary.rows <- matrix(NA, nrow = colfuns.count.picked, ncol = ncol(sitestats)) # rows with summary stats summarizing all the columns. This will hold 1 row per summary stat, and same # cols as original table of batch results
  summary.cols <- matrix(NA, nrow = nrow(sitestats), ncol = rowfuns.count.picked ) # columns with summary stats summarizing all the rows
  
  summary.rows.names <- vector(length = colfuns.count.picked)
  summary.cols.names <- vector()
  
  # don't summarize character columns like name of site
  charcol <- sapply(sitestats, class) == 'character'
  
  ############################################ #
  # ...Create summary rows ------------ ######
  # where each element in a summary row summarizes 1 column (field) across all the rows of batch data
  ############################################ #
  
  for (i in 1:colfuns.count.picked) {
    
    fnum <- which(colfun.picked)[i]
    # bywhat  somehow is this: [1] "site" "pop"    "site" "pop"    "site"    "site" "pop" #  seems to be missing one pop element so subscript fails.
    if (bywhat[fnum] == 'site') {
      # right now wts are not passed from here to function- they are specified when function is specified
      # don't pass parameters since they are variables available in this memory space. like wts, na.rm, thresholds, probs, etc.
      # weight by the total pop count at each site?? why?
      # wts <- sitestats[ , wtscolname]
      summary.rows[i, ][  !charcol] <- as.vector( colfun[[fnum]](sitestats[ , !charcol]) )  
    } else {
      # wts <- popstats[ , wtscolname] # weight by the unique individuals assigned to a given site
      summary.rows[i, ][  !charcol] <- as.vector( colfun[[fnum]](popstats[ , !charcol]) )  
    }
    
    if (testing) {cat('now summary.rows[i, ] all cols is \n'); print(summary.rows[i, ])}
    
    summary.rows.names[i] <- colfuname[fnum]
    
    # could this have the wrong # of elements if na.rm=TRUE and it somehow doesn't return NA for one of the columns??
  }
  
  ############################################ #
  # ...Create summary cols ------------ ######
  # where each element in a summary col summarizes 1 row (site) across all the RELEVANT cols of batch data (e.g., all US EJ Index percentiles)
  ############################################ #
  
  for (i in 1:rowfuns.count.picked) {
    fnum <- which(rowfun.picked)[i]        # this line does not make sense?
    myargs <- rowargs[[fnum]]
    
    if (bywhat[fnum] == 'site') {
      myargs$x <- sitestats
    } else {
      myargs$x <- popstats
    }
    
    summary.cols[ , i] <- round(as.vector(
      do.call(rowfun[[fnum]], args = myargs)            ## ERROR HERE if only 1 indicator ?  if >1 group of indicators? 
    ), 2)
    
    summary.cols.names[ i] <- rowfuname[fnum]
  }
  
  ############################################ #
  # ...create useful rownames and colnames for the outputs ####
  ############################################ #
  
  rownames(summary.rows) <- summary.rows.names
  colnames(summary.rows) <- cols # colnames(sitestats)  # Not sure this works if only some cols selected to summarize
  
  colnames(summary.cols) <- summary.cols.names
  rownames(summary.cols) <- seq.int(length.out = NROW(sitestats))   # unlist(sitestats[ , 1]) # had assumed you want the first column to be used as the rownames, 
  # For summary cols, put a duplicate column of user's site names field first if it exists, so can freeze it when seeing summary stat columns view
  
  
  # column with 1 through total row count
  #                                                   which was OBJECTID as of 2/2015
  # rownames(colsout) <- rownames(sitestats)  # is another option
  
  ############################################ #
  # ...less elegant WAY TO APPEND QUANTILE SUMMARY ROWS: ######
  
  # disabled until fixed or dropped:
  
  
  ############################################ #
  #  if (just.rbind.quantiles) {
  #   quantile.rows     <- matrix(NA, nrow = length(probs), ncol = NCOL(sitestats)); rownames(quantile.rows)    <- paste('Percentile of sites',  100 * probs)
  #   wtd.quantile.rows <- matrix(NA, nrow = length(probs), ncol = NCOL(popstats)); rownames(wtd.quantile.rows) <- paste('Percentile of people', 100 * probs)
  #   colnames(quantile.rows) <- colnames(sitestats) # should be same as cols variable
  #   colnames(wtd.quantile.rows) <- colnames(sitestats) # ditto
  #   quantile.rows[     , !charcol] <- sapply(sitestats[ , !charcol], function(y) stats::quantile(y,     probs = probs, na.rm = na.rm) )
  #   
  #   
  #   
  #   
  #   # *** THIS IS WRONG - NEEDS TO BE FIXED OR NOT USED ! ***
  #   #
  #   # wtd.quantile.rows[ , !charcol] <- sapply( popstats[ , !charcol], function(y) weightedMedian(y, w = wts, na.rm = na.rm,  weights = wts) )
  #   ## # used matrixStats package for weighteMedian() but this seems to be the only place that package was ever needed. 
  #   #
  #   #### NEED TO FIX THE FORMAT OF THESE OUTPUT ROWS IF DONE HERE: 
  #   wtd.quantile.rows[ , !charcol] <-  collapse::fquantile(
  #     popstats[ , !charcol],
  #     probs = probs,  
  #     w = wts, 
  #     na.rm = na.rm
  #   )
  #   
  #   
  #   summary.rows <- rbind(summary.rows, quantile.rows)
  #   
  #   summary.rows <- rbind(summary.rows, quantile.rows, wtd.quantile.rows)
  # }
  
  #|################################################################################# ### #
  # RETURN LIST OF 2 TABLES: summary rows, summary columns ############### 
  summary.cols <- data.frame(summary.cols, stringsAsFactors = FALSE)
  summary.rows <- data.frame(summary.rows, stringsAsFactors = FALSE)
  # THIS FIXES BUG IN SORTING/ FORMATTING cols RESULTS AS NUMERIC VS CHARACTER
  # x$cols <- as.data.frame(x$cols, stringsAsFactors = FALSE)
  # but can't do this here WHILE STRINGS ARE IN SOME CELLS: ???? not sure what that note meant.
  # x$rows <- as.data.frame(x$rows, stringsAsFactors = FALSE)
  
  x <- list(rows = summary.rows, cols = summary.cols) 
  
  # provide some interesting subsets of outputs for convenience
  x$keystats <- round( t(x$rows[1:2,]), 2)
  x$keyindicators <- round(x$rows[ , c(names_wts, as.vector(names_d))] , 2)
  
  if (interactive() && !quiet) {
    print(x$keystats) # print a useful subset of info to console (for interactive use)
    # shows Average Person and Average Site for the list of indicators analyzed
    print(x$keyindicators)
    
  }
  
  invisible(x)
  
  # |---DONE--- ####
}
