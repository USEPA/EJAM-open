

#   SPEEDTEST   #########################################################################



#' Run EJAM analysis for several radii and numbers of sitepoints, recording how long each step takes
#' 
#' @details   
#'   This is essentially a test script that times each step of EJAM for a large dataset
#'    - pick a sample size (n) (or enter sitepoints, or set n=0 to interactively pick file of points in RStudio)
#'    - pick n random points
#'    - pick a few different radii for circular buffering
#'    - analyze indicators in circular buffers and overall (find blocks nearby and then calc indicators)
#'    - get stats that summarize those indicators
#'    - compare times between steps and radii and other approaches or tools
#'    
#' @param n optional, vector of 1 or more counts of how many random points to test, or 
#'   set to 0 to interactively pick file of points in RStudio (n is ignored if sitepoints provided)
#' @param sitepoints optional,  (use if you do not want random points) data.frame of points or 
#'   path/file with points, where columns are lat and lon in decimal degrees
#' @param weighting optional, if using random points, how to weight them, 
#'   such as facilities, people, or blockgroups. see [testpoints_n()]
#' @param radii optional, one or more radius values in miles 
#'   to use in creating circular buffers when findings residents nearby each of sitepoints.
#'   The default list includes one that is 5km (approx 3.1 miles)
#' @param test_getblocksnearby whether to include this function in timing - not used because always done
#' @param test_doaggregate  whether to include this function in timing
#' @param test_batch.summarize  whether to include this function in timing
#' @param test_ejamit whether to test only ejamit() 
#'   instead of its subcomponents like getblocksnearby(), doaggregate(), etc
#' @param logging logical optional, whether to save log file with timings of steps. 
#'   NOTE this slows it down though.
#' @param logfolder optional, name of folder for log file
#' @param logfilename optional, name of log file to go in folder
#' @param honk_when_ready optional, self-explanatory
#' @param saveoutput but this slows it down if set to TRUE to save each run as .rda file
#' @param plot whether to create plot of results
#' @param avoidorphans see [getblocksnearby()] or [ejamit()] regarding this param
#' @param getblocks_diagnostics_shown set TRUE to see more details on block counts etc.
#' @param ... passed to plotting function
#' @examples \dontrun{ 
#'   speedseen_few <- speedtest(c(50,500), radii=c(1, 3.106856), logging=FALSE, honk=FALSE)
#'   
#'   speedseen_nearer_to1k <- speedtest(n = c(1e2,1e3,1e4 ), radii=c(1, 3.106856,5 ),
#'     logging=TRUE, honk=FALSE)
#'   save( speedseen_nearer_to1k, file = "~/../Downloads/speedseen_nearer_to1k.rda")
#'   rstudioapi::savePlotAsImage(        "~/../Downloads/speedseen_nearer_to1k.png")
#'   
#'   speedseen_all <- speedtest(
#'     n = c(1e2,1e3,1e4), 
#'     radii=c(1, 3.106856, 5, 10, 31.06856), 
#'     logging=TRUE, honk=TRUE
#'   )
#'  }
#' @return EJAM results similar to as from the web app or [ejamit()] and also creates a plot
#' @seealso [speedtest_plot()] 
#' 
#' @keywords internal
#' 
speedtest <- function(n=10, sitepoints=NULL, weighting='frs', 
                      radii=c(1, 3.106856, 5, 10, 31.06856)[1:3], avoidorphans=FALSE,
                      test_ejamit = FALSE, test_getblocksnearby=TRUE, test_doaggregate=TRUE, test_batch.summarize=FALSE, 
                      logging=FALSE, logfolder='.', logfilename="log_n_datetime.txt", honk_when_ready=TRUE, 
                      saveoutput=FALSE, plot=TRUE, getblocks_diagnostics_shown=FALSE, ...) {
  
  n <- sort(n, decreasing = TRUE) # just to keep organized
  radii <- sort(radii, decreasing = TRUE) # IT WILL REPORT WRONG NUMBERS / WRONG ORDER OTHERWISE.
  rtextfile <- paste(radii,   collapse = "-")
  ntextfile <- paste(n, collapse = "-")
  
  if (test_ejamit) {
    if (any(test_getblocksnearby, test_doaggregate, test_batch.summarize)) {
      warning("test_ejamit=TRUE, so ignoring test_getblocksnearby, test_doaggregate, test_batch.summarize")
    }
    test_getblocksnearby = FALSE; test_doaggregate = FALSE; test_batch.summarize = FALSE
  }
  if (test_batch.summarize & !test_doaggregate) {  
    warning("cannot test batch.summarize without doing doaggregate")
    return(NULL)
  }
  cat('\nsee profvis::profvis({}) for viewing where the bottlenecks are \n\n')
  
  # Test script that times each step of EJAM for a large dataset
  
  # - pick a sample size (n) (or enter sitepoints, or set n=0 to interactively pick file of points in RStudio)
  # - pick n random points
  # - pick a few different radii for circular buffering
  # - analyze indicators in circular buffers and overall (find blocks nearby and then calc indicators)
  # - get stats that summarize those indicators
  # - compare times between steps and radii and other approaches or tools
  
  ######################### # 
  
  if (n[1] == 0 & is.null(sitepoints)) {
    if (interactive()) {sitepoints <- rstudioapi::selectFile("Select xlsx or csv file with lat,lon coordinates", path = ".", existing = FALSE)
    }
  } 
  
  if (logging |  !missing(logfolder) | !missing(logfilename))  {
    # PREP LOG FILE
    now <- function() {gsub("-","", gsub(':', '', gsub(' ', '_', Sys.time())))}
    logfilename <- paste0("logfile_", ntextfile, "_pts_", rtextfile, "_miles_", now(), ".txt")  # file.path(logfolder, fname)
    logto = file.path( logfolder, logfilename)
    # START LOGGING
    sink(file = logto, split = TRUE)
    on.exit(sink())
    cat("Started log "); print(Sys.time()); cat("\n") 
  }
  
  if (is.null(sitepoints)) {
    # PICK TEST DATASET(s) OF FACILITY POINTS 
    cat("Picking random points for testing.\n")
    # Also see test files in EJAM/inst/testdata/latlon/ 
    sitepoints <- list()
    # we can have the smaller sets of points be a random subset of the next larger, to make it more apples to apples
    nsorted <- sort(n,decreasing = TRUE)
    for (i in 1:length(n)) {
      if (i == 1) {sitepoints[[1]] <- testpoints_n(n = nsorted[1], weighting = weighting)} else {
        # *** Only the overall largest set uses weighted probabilities this way - subsets are uniform likelihood of each from large set
        # otherwise cannot easily take subsets without essentially rewriting testpoints_n() code
        sitepoints[[i]] <- sitepoints[[i - 1]][sample(1:nrow(sitepoints[[1]]), size = nsorted[i] ), ]
      }
    }
    cat("Finished picking random points for testing.\n\n")
  } else {
    sitepoints <- latlon_from_anything(sitepoints)
    if (!missing(n)) {warning("n ignored because sitepoints provided")}
    if (!missing(weighting)) {warning("weighting ignored because sitepoints provided")}
    n <- NROW(sitepoints)
    sitepoints <- list(sitepoints)
  }
  
  # PICK SOME OPTIONS FOR RADIUS 
  if (length(radii) > 10 | any(radii < 1)) {stop("Did you intend to provide more than 10 radius values? Cannot try more than 10 radius values in one run.")}
  # radii <- c(1, 3.106856, 5, 10, 20)
  # 3.1 miles IS 5 KM ... 5*1000/ meters_per_mile
  
  # MAKE SURE localtree INDEX OF ALL US BLOCKS IS AVAILABLE FROM EJAM PACKAGE 
  if (!exists("localtree")) {
    step0 <- system.time({
      cat("Creating national index of block locations (localtree) since it was not found.\n")
      indexblocks()
      quadtree <- localtree
      cat("Finished createTree()\n")
      
      #time to create quadtree 1.116 seconds
    })
    print(step0)
  }
  ####################### # 
  
  
  # START RUNNING ANALYSIS  ------------------------------------------------------------------ -
  
  
  ntext <- paste(paste0(n,     " sites"), collapse = ", ")
  rtext <- paste(paste0(radii, " miles"), collapse = ", ")
  cat("Size(s) of list(s) of points = ", ntext, '\n')
  cat("Radius choice(s)     = ", rtext, '\n')
  
  cat("test_ejamit = ", test_getblocksnearby, "\n")
  if (!test_ejamit) {
    cat("test_getblocksnearby = ", test_getblocksnearby, "\n")
    cat("test_doaggregate     = ", test_doaggregate, "\n")
    cat("test_batch.summarize = ", test_batch.summarize, "\n")
  }
  cat("saveoutput = ", saveoutput, "\n")
  
  nlist = n
  combonumber <- 0
  speedtable <- list()
  
  overall_start_time <- Sys.time() 
  
  cat("Starting analysis "); print(Sys.time()); cat("\n") 
  
  for (n in nlist) {
    
    i <- which(n == nlist) # should be just a counter like combonumber from 1 to x among nlist 
    cat("\n--------------------------------------------------------------------\n")
    cat("\nAnalyzing", n, "facilities:")
    
    
    for (radius in radii) {
      
      
      combonumber <- combonumber + 1
      cat("\n  Radius of", radius, "miles (Radius #", which(radius == radii), "of the", length(radii), 'being tested).\n')    
      
      start_time <- Sys.time()
      
      if (!test_ejamit) {
        
        mysites2blocks = NA
        # elapsed <- system.time({
        # cat('\nStarting getblocksnearby() to find Census blocks (by internal point) near each facility\n')
        # step1 = system.time({
        mysites2blocks <-  getblocksnearby(
          sitepoints = sitepoints[[i]],
          radius = radius, maxradius = 31.07,
          avoidorphans = avoidorphans)
        # })
        # cat("Finished getblocksnearby()\n")
        # print(step1)
        out <- NA
        if (test_doaggregate) {
          # step2 = system.time({
          cat('\nStarted doaggregate() to calculate each indicator for each site, and overall.\n')
          out <-  doaggregate(sites2blocks = mysites2blocks, silentinteractive = TRUE)
          # })
          # cat("Finished doaggregate()\n")
          # print(step2)
        }
        out <- NA
        if (test_batch.summarize ) {
          # step3 = system.time({
          # cat('Started batch.summarize() to calculate stats that summarize those indicators.\n')
          out2 <- batch.summarize(
            sitestats = data.frame(out$results_bysite)   # batch.summarize() should work with just this param 
            # popstats =  data.frame(out$results_bysite),
            ## user-selected quantiles to use
            #probs = as.numeric(input$an_list_pctiles),
            # thresholds = list(95) # compare variables to 95th %ile
          )
          # })
          # cat("  Finished batch.summarize()\n")
          # print(step3)
        }
      } else {
        # doing ejamit() because test_ejamit == TRUE
        cat('\nStarted ejamit() to calculate each indicator for each site, and overall.\n')
        out <- ejamit(
          sitepoints = sitepoints[[i]],
          radius = radius, # maxradius = 31.07,
          avoidorphans = avoidorphans, 
          silentinteractive = TRUE 
        )
        
        
      } 
      
      
      # cat(paste0("\nSpeed report for ", n, " points at ", radius, " miles: "))
      # cat("----------------------------------\n")
      #write time elapsed to csv?
      # write.csv(t(data.matrix(elapsed)),file=paste0("./inst/time_radius_",myradius,"_100k.csv"))
      perhour <- speedreport(start_time, Sys.time(), n)
      speedtable[[combonumber]] <- list(points = n, miles = radius, perhr = perhour)
      
      #  show diagnostics here like how many blocks were found nearby? this slows it down
      if (test_getblocksnearby & getblocks_diagnostics_shown) {
        getblocks_diagnostics(mysites2blocks)
      }
      
    } # NEXT RADIUS 
    # cat("\nFinished analyzing all radius values for this set of", prettyNum(n, big.mark = ","),"points or sites.\n")
    if (saveoutput) { # slows it down so just for diagnostics or saving batches of results
      save(out, file = file.path(logfolder, paste0( "out n", n, "_rad", paste(radii,collapse = "-"), ".rda")))
      # save(out2, file= "out2.rda")
      x <- as.data.frame(do.call(rbind, speedtable))  # could fix this to be simpler
      x <- as.data.frame(sapply(x, unlist))  # could fix this to be simpler
      
      save(x, file = file.path(logfolder, paste0("speedtable_", n,"_rad", paste(radii,collapse = "-"), ".rda")))
    }
    
  } # NEXT LIST OF POINTS (facility list) ----------------------------------------------------------------- -
  
  endtime <- Sys.time()
  cat('Stopped timing.\n')
  
  cat("--------------------------------------------------------------------\n")
  
  CIRCLESDONE <- sum(nlist) * length(radii)
  cat("Finished with all sets of sites,", length(radii),"radius values, each for a total of", 
      prettyNum(sum(nlist), big.mark = ",") ,"sites =", prettyNum(CIRCLESDONE, big.mark = ","), "circles total.\n")
  speedreport(overall_start_time, endtime, CIRCLESDONE)
  
  speedtable <- as.data.frame(do.call(rbind, speedtable))  # could fix this to be simpler
  speedtable <- as.data.frame(sapply(speedtable, unlist))  # could fix this to be simpler
  
  speedtable <- speedtable_expand(speedtable)
  print(speedtable_summarize(speedtable))
  
  total_points_run <- sum(speedtable$points)
  total_hours <- sum(speedtable$points / speedtable$perhr)
  avg_perhr <- round(total_points_run / total_hours, 0)
  cat("\nAverage points/hour  =", prettyNum( avg_perhr,big.mark = ","), "\n")  
  
  cat("Size(s) of list(s) of points =", ntext, '\n')
  cat("Radius choice(s)     =", rtext, '\n')
  cat("test_ejamit =", test_ejamit, "\n")
  if (!test_ejamit) {
    cat("test_getblocksnearby =", test_getblocksnearby, "\n")
    cat("test_doaggregate     =", test_doaggregate, "\n")
    cat("test_batch.summarize =", test_batch.summarize, "\n")
  }
  if (logging) {sink(NULL)} # stop logging to file.
  if (plot) {speedtest_plot(speedtable, ...)}
  print(speedtable)
  
  if (interactive() && honk_when_ready && length(try(find.package("beepr", quiet = T))) != 0) {
    # using beepr::beep() since utils::alarm() may not work
    # using :: might create a dependency but prefer that pkg be only in Suggests in DESCRIPTION
    beepr::beep(8)
  }
  if (interactive()) { 
    rstudioapi::showDialog("", "FINISHED")
  }
  return(speedtable)
}
######################################################################### #


#' utility to plot output of speedtest(), rate of points analyzed per hour
#' 
#' @param x table from speedtest()
#' @param ltype optional type of line for plot
#' @param plotfile optional path and filename of .png image file to save
#' @return side effect is a plot. returns x but with seconds column added to it
#' @seealso [speedtest()]
#' 
#' @keywords internal
#'
speedtest_plot = function(x, ltype="b", plotfile=NULL, secondsperthousand=FALSE) {
  
  radii <- unique(x$miles)
  nlist  <- unique(x$points)
  mycolors <- runif(length(radii), 1, 600)
  if (secondsperthousand) {
    yvals = x$secondsper1000
    ylab = "Seconds per 1,000 sites"
  } else {
    yvals <- x$perhr/1000
    ylab = "Thousands of sites per hour"
  }
  yl <-  c(0,max(yvals)) # range(x$perhr)
  xl <- c(0,max(x$miles)) # range(x$miles)
  x$seconds <- x$points / x$perhr * 3600
  atmost = aggregate(x$seconds, by = list(n = x$points), FUN = max )
  maxseconds = atmost$x[match(nlist, atmost$n)] 
  if (!is.null(plotfile)) {
    png(filename = plotfile )
    on.exit(  dev.off())
  }
  
  plot(
    x$miles[x$points == nlist[1]], 
    yvals[x$points == nlist[1]] , 
    type = ltype, col = mycolors[1],
    xlim = xl, ylim = yl, ylab = ylab, xlab = "miles radius",
    main = "Speed of this analysis")
  if (length(nlist) > 1) {
    for (i in 2:length(nlist)) {
      points(
        x$miles[x$points == nlist[i]], 
        yvals[x$points == nlist[i]]  , 
        type = ltype, col = mycolors[i])
    }
    legwhere = ifelse(secondsperthousand, "topleft", "bottomleft")
    legend(legwhere, 
           legend = rev(paste0(prettyNum(nlist, big.mark = ","), " points take up to ",  round(maxseconds, 0), " seconds")),
           fill = rev(  mycolors[1:length(nlist)]))
  }
  return(x)
}
######################################################################### #


#' utility used by speedtest()
#' 
#' @param speedtable from speedtest(), with columns named points and perhr
#' @seealso [speedtest()]
#' 
#' @keywords internal
#'
speedtable_summarize <- function(speedtable) {
  
  # used by speedtest()
  runs <- sum(speedtable$points)
  total_hours <- sum(speedtable$points / speedtable$perhr)
  perhr <-  round(runs / total_hours ,0)
  mysummary <- data.frame(points = runs, miles = NA, perhr = perhr)
  return(speedtable_expand(mysummary))
}
######################################################################### #


#' Utility used by speedtest() and speedtable_summarize()
#' 
#' @param speedtable must have columns called  points, miles, and perhr 
#'
#' @keywords internal
#'
speedtable_expand <- function(speedtable) {
  # used by speedtest() and by speedtable_summarize()
  # input param speedtable must have columns called  points, miles, and perhr 
  speedtable$perminute <- round(speedtable$perhr /   60, 0)
  speedtable$persecond <- round(speedtable$perhr / 3600, 0)
  speedtable$minutes   <- round(speedtable$points / (speedtable$perhr / 60), 0)
  speedtable$seconds   <- round(speedtable$points / (speedtable$perhr / 3600), 0)
  speedtable$secondsper1000 <- round((1000/speedtable$points) * speedtable$points / (speedtable$perhr / 3600), 0)
  return(speedtable)
}
######################################################################### #




############################################################################### #
# older manual TESTING JUST getblocksnearby() not including doaggregate()

# olddir=getwd()
# setwd("~/../Downloads")
# t1_1000=system.time({  x1=getblocksnearby(testpoints_1000,1);  save(x1,file = 'x1_1000.rda');rm(x1)})
# t3_1000=system.time({  x3=getblocksnearby(testpoints_1000,3);  save(x3,file = 'x3_1000.rda');rm(x3)})
# t6_1000=system.time({  x6=getblocksnearby(testpoints_1000,6);  save(x6,file = 'x6_1000.rda');rm(x6)})
# 
# testpoints_10k <- testpoints_n(10000, weighting = "frs")
# 
# t1_10k=system.time({  x1=getblocksnearby(testpoints_10k,1);  save(x1,file = 'x1_10k.rda');rm(x1)})
# t3_10k=system.time({  x3=getblocksnearby(testpoints_10k,3);  save(x3,file = 'x3_10k.rda');rm(x3)})
# t6_10k=system.time({  x6=getblocksnearby(testpoints_10k,6);  save(x6,file = 'x6_10k.rda');rm(x6)})
# rm(testpoints_10k)
# setwd(olddir)
# 
# # points per hour: 
# 
# prettyNum(round(3600*1000/t1_1000[3],0),big.mark = ",")
# prettyNum(round(3600*1000/t3_1000[3],0),big.mark = ",")
# prettyNum(round(3600*1000/t6_1000[3],0),big.mark = ",")
# 
# prettyNum(round(3600*10000/t1_10k[3],0),big.mark = ",")
# prettyNum(round(3600*10000/t3_10k[3],0),big.mark = ",")
# prettyNum(round(3600*10000/t6_10k[3],0),big.mark = ",")
################################################################################

