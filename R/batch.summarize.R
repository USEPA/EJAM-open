# (batch.summarize.helpers functions are in a separate file) ####

# batch.summarize()  ####


#' @title Calculate more summary stats across all sites and all people
#' 
#' @details This is the function that takes the `doaggregate()` results and 
#'   calculates even more summary statistics, notably to characterize
#'   the distribution of indicator values across people and across sites, 
#'   as percentile of analyzed people and percentile of analyzed sites, etc.
#'   
#'   It can be expanded to provide other summary stats by adding those other formulas to this code.
#'   
#'   Note it can provide population-weighted summary stats only for the indicators found in popstats,
#'   which is fewer than those in sitestats, since doaggregate() or ejamit() did not
#'   provide various extra indicators in the very large results_bybg_people table.
#'   The parameter sitestats but not popstats has the ratios and pctiles (and US/State averages) of indicators,
#'   plus "bgid" "bgwt" and "valid" "invalid_msg"
#'   So this function cannot show, e.g., the median analyzed person's 
#'   ratio of local score to US average,
#'   and cannot say the Demog. Index US percentile was at least X% of people
#'   nationwide among the top 10% of people analyzed.
#'   
#' @param ejamitout Not typically used. If it is provided, 
#'   then parameters sitestats, popstats, and overall are ignored.
#'   A list that is the output of `ejamit()` -- the list of tables including 
#'   results_bysite, results_bybg_people, results_overall.
#'   ejamit() actually already does batch.summarize(), but this param might be
#'   useful interactively if you want to redo just the batch.summarize() step
#'   with new params like thresholds or probs.
#' 
#' @param sitestats A data.frame or data.table like `ejamit()`$results_bysite,
#'   with one row per site and one column per indicator.
#'   Ignored if ejamitout is provided.
#'   
#' @param popstats A data.frame or data.table like `ejamit()`$results_bysite,
#'   with one row per blockgroup at least partly in/at one or more of the sites,
#'   and one column per indicator. It provides blockgroup indicators, including
#'   total counts even for the blockgroups that are not entirely in/at a site.
#'   This is used to get stats on the distribution of each indicator across all 
#'   unique individuals (regardless of how many sites a resident is at).
#'   Ignored if ejamitout is provided.
#'   
#' @param overall A data.frame or data.table like `ejamit()`$results_bysite,
#'   with one column per indicator, and just one data row that has the overall
#'   average, sum, or other summary stat for the indicator across all sites.
#'   Ignored if ejamitout is provided.
#' 
#' @param probs Vector of numeric values, fractions, to use as probabilities used in finding quantiles.
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
#' 
#' @seealso [ejam2areafeatures()] [ejamit()]
#' 
#' @return output is a list with two named elements, rows and cols, where each is a matrix of summary stats.
#'   
#'   cols: Each element in a summary col summarizes 1 row (site) across all the RELEVANT cols of batch data 
#'     (e.g., all US Summary Index percentiles). This type of info is also saved in a tab of the output of `ejam2excel()`
#'   
#'   rows: Each element in a summary row summarizes 1 column (field) across all the rows of batch data.
#'     A subset of this is $keyindicators
#'   
#'   keystats: Key subset of the summary stats (for all indicators), for convenience. See `ejam2table_tall()` to view this.
#'   
#'   keyindicators: Key subset of the indicators from $row results 
#'     (for stats characterizing the distribution of each across people or sites,
#'      like avg, mean, and percentiles), for convenience
#'   
#'   flagged_areas: table of percentages of people or sites, here (people at analyzed sites) and nationwide,
#'     who reside in blockgroups that overlap at all with key types of areas like 
#'     [nonattainment for air standards](https://www.epa.gov/criteria-air-pollutants/process-determine-whether-areas-meet-naaqs-designations-process),
#'     and who have at least one school / hospital / church in their blockgroup,
#'     and with no broadband, and with no health insurance. 
#'     This can be viewed using `ejam2areafeatures()`
#'     
#'  In the flagged_areas table, summary stats mean the following:
#' 
#' - The "flag" or "yesno" indicators here are population weighted sums, so they show 
#'   how many people from the analysis live in blockgroups
#'   that overlap with the given special type of area, such as 
#'   non-attainment areas under the Clean Air Act.
#' 
#' - The "number" indicators are counts for each site in the 
#'   `ejamit()$results_overall` table, but here are summarized as 
#'   what percent of residents overall in the analysis have 
#'   AT LEAST ONE OR MORE of that type of site in
#'   the blockgroup they live in.
#'   
#' - The "pctno" or % indicators are summarized as what % of the 
#'   residents analyzed lack the critical service.
#'     
#' @export
#' 
batch.summarize <- function(ejamitout,
                            sitestats, popstats, overall, # if ejamitout is not provided
                            
                            wtscolname = 'pop', # note this wt is for working with sitestats, but popstats analysis needs pop and also must need "bgwt" found in popstats, from results_bybg_people
                            probs = c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1),
                            
                            thresholds = list(90), # only makes sense for the percentiles indicators though!
                            # thresholds = list(90, 90),
                            
                            threshnames = list(names(which(sapply(sitestats,  class) != 'character'))), # all the numeric columns 
                            # threshnames = list(c(names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile)),
                            # threshnames = list(c(names_ej_pctile, names_ej_state_pctile), c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
                            
                            threshgroups = list('variables'),
                            # threshgroups = list("EJ_or_Supp_EJ_US_or_State"),
                            # threshgroups = list("EJ-US-or-ST", "Supp-US-or-ST"),
                            
                            na.rm = TRUE, 
                            rowfun.picked = 'all', 
                            colfun.picked = 'all',
                            quiet = FALSE, testing = FALSE) {
  
  # ERROR CHECKING ####
  
  if (missing(ejamitout) || is.null(ejamitout)) {
    # confirm we have all 3: sitestats, popstats, overall
    stopifnot("if 1st parameter to batch.summarize(), ejamitout, is missing or null, then all 3 of these params must be specified and non-null data.frames: sitestats, popstats, overall" = all(
      !missing(sitestats), !is.null(sitestats), is.data.frame(sitestats),
      !missing(popstats), !is.null(popstats), is.data.frame(popstats),
      !missing(overall), !is.null(overall), is.data.frame(overall)
    )
    )
  } else {
    # confirm first param passed here is really ejamitout, the list expected, not something like sitestats.
    stopifnot("if the 1st parameter of batch.summarize(), ejamitout, is provided, it must be like the list that is the output of ejamit()" = all(
      is.list(ejamitout), "results_bysite" %in% names(ejamitout), "results_bybg_people" %in% names(ejamitout), "results_overall" %in% names(ejamitout)
    )
    )
    sitestats <- ejamitout$results_bysite      # details on each site, 1 per row, so it is used for stats across sites, not unique residents since some may be at 2+ sites.
    popstats  <- ejamitout$results_bybg_people # for analysis of all unique residents -- details on each blockgroup that has any portion of the bg at 1+ sites, with bgwt column, but data in for entire bg so counts are not scaled by bgwt yet 
    overall   <- ejamitout$results_overall     # already has the summary stats correctly calculated to represent weighted means
  }
  
  if (!is.atomic(probs) || !is.vector(probs) || !is.numeric(probs) || any(probs > 1) || any(probs < 0)) {
    stop('probs must be a numeric vector of fractions for quantile function')
    }
  
  if (1 != length(unique( length(thresholds), length(threshnames), length(threshgroups)))) {
    stop('lengths of thresholds list, threshnames list, threshgroups list must be identical')
  }
  if (is.null(threshnames) || !all(unlist(threshnames) %in% colnames(sitestats)) || is.null(thresholds) || is.null(threshgroups)) {
    warning("specified threshnames not all found in sitestats colnames or other problem with thresholds or threshgroups, so using defaults")
    # try to use some available set of pctiles, or just set count and max to NA.
    if (all(names_e_pctile %in% names(sitestats)) & all(names_e_state_pctile %in% names(sitestats))) {
      threshnames = list(names_e_pctile, names_e_state_pctile, 
                         names_e_pctile, names_e_state_pctile)
      threshgroups = list('Envt_US_pctiles', 'Envt_ST_pctiles', 
                          'Envt_US_pctiles', 'Envt_ST_pctiles')
      thresholds = list(80, 80, 
                        90, 90)
    } else {
      threshnames = NULL
      threshgroups = "none"
      thresholds = NA
    }
  }
  
  # _ ####
  {
    # _ ####
  }
  
  sitestats <- as.data.frame(sitestats) # in case it was a data.table, or tibble. this makes a copy unlike data.table::setDF(sitestats) but using "by reference" functions from data.table:: will alter the dt that was passed here as sitestats= param, in the calling envt!!
  popstats  <- as.data.frame(popstats)  # ditto
  overall   <- as.data.frame(overall)   # ditto
  
  bgwt_popstats <- popstats$bgwt # hardcoded here since it is by bg and not in sitestats and is needed for correct wts like for wtdmean by unique person
  
  
  ## INDICATORS FOUND in sitestats and in overall, vs in popstats, differ - popstats does NOT have all the same colnames.
  ## The only ones unique to sitestats are "valid","invalid_msg", and all the pctile, ratio, or averages, which is OK,
  #  The only ones unique to popstats are  "bgid" and "bgwt" which is OK.
  
  # Note it is OK that they have different counts of rows if sitestats is one per site (out$results_bysite) and popstats is one per unique blockgroup (out$results_bybg_people)
  
  ######################################## # 
  # 
  # *** WEIGHTING ISSUES !! *** ####
  
  # weighting is more complicated than one might think, here. The right weight depends on type of calculation.
  
  # see weighting in Quantiles calculations especially ! 
  # and in colfuns
  
  #  site-related stats will be based on unique sites, using sitestats table. No weights are needed.
  #  pop-related stats will be based on unique residents, using popstats table, 
  #     or can just use the already-calculated stats in overall table.
  
  # weights to use with the popstats table of blockgroups:
  
  # The right weight depends on type of calculation, and that needs to be reflected in the formulas for summary stats,
  #  but that means also using a new parameter that is kind of like calctype, 
  #  but flags which columns are to be calculated/weighted which way:
  #    - E and %D 
  #    - DEMOG COUNT indicators, and Feature counts?
  #    - Flag overlaps
  #    - (vs Others?) 
  
  #  These may need weights of at least 3 different kinds:
  
  #  a) pophere (maybe as share of total pop overall? the wtdmean formula handles that):  
  
  #  b) sharehere (NOT as share of total bgwts): a SUM of pop or DEMOG COUNT (not "feature" counts?) is already done by doaggregate(), in overall, but 
  #  would be summed using just bgwt_popstats as the weight, NOT pop*bgwt_popstats ! 
  #  since those counts are stored in bybg_people as total count in BG and bgwt_popstats is % of bg total that is at sites.
  #  
  #    and calculating percentiles over people for DEMOG COUNT should likewise use just bgwt_popstats in the weighted pctile.
  
  
  #  c) weightless (wt=1 in each blockgroup ie each row of popstats):  like min/max of E, %D
  
  
  ## WEIGHTS FOR AVERAGE PERSON's or POP-PERCENTILE of E or %D or %ile or Ratio indicator (NON COUNT indicators): 
  #    use weight of pophere = $pop * bgwt_popstats, so the wt is pop at 1+ of these sites from this bg, not total pop in bg.
  
  ## WEIGHTS FOR AVERAGE PERSON's or POP-PERCENTILE of *EntireBG-COUNT* Indicators: 
  # pophere is wt =(pop * bgwt) is the wt for wtdmean and for wtd pctile. (as we will count the entire BG for this summary)
  #
  #  Assume we want to summarize block-wide counts, not just scaled counts within the portions of blocks that are at the sites!?
  #  Not 100% clear that makes sense. It is conservative and useful if normally 0 or 1 in an entire BG - assumes the 1 is in your intersected part of BG.
  #  If often many schools per BG, probably you want to simply report the avg person analyzed has 3 in their entire bg not 3 in the analyzed parts of their bg.
  #  And avg bgcount of demog counts is not useful anyway.
  #   we dont really need avg count like how many hispanics are in the entire block group of the average resident...
  #   but we do want how many NPL sites or schools are in the (entire?) blockgroup of the average resident. 
  #  Like  npl.count (not npl.count*bgwt) is the count indicator of entire BG (even if only part of it is "at" sites) 
  #  pophere=(pop * bgwt) is the wt for wtdmean and for wtd pctile. so wtdmean npl.count = sum(npl.count * pop*bgwt) / sum(pop*bgwt)
  #
  #  FEATURE COUNTS like count of schools in the blockgroups, or nearby NPL count ***
  #   We will not use the number SCALED DOWN by the share of the bg that is "at" the sites, as with sums of demog counts.
  # Avg person's flag_ or yesno_ ???   see flagged_count_pop() flagged_pct_pop() 
  #
  #  FLAGS/OVERLAPS:  like 1 = overlapping with a Tribal or nonattainment or CJEST area?
  #   We want to report:   
  #  - if anybody/any bg overlapped such an area for overall set of sites analyzed (doaggregate() already provides that as max or sum of flags=1)
  # *** - how many of the sites overlap at all? sum of sitelevel flags, assuming 0 or 1 for each site. ?
  # *** - what # or % of people (and each group?) overlap with flagged areatype? 
  #     special formula needed... sum(Dcount[flagTribal == 1]) etc. using popstats table but that lacks the flags.
  #  Say we want to err on side of caution for these/ inclusiveness, and count it as "yes" (or even a count of 1?) 
  #   if ANY portion of the bg intersects the sites analyzed 
  #   since we don't know which part of bg does (if it is the part that is "at" a site).
  #   That would suggest unweighted max/min, ("max" would just tell you if any site intersected)
  #   but sum of flags over bgs, people just tells you how many block groups overlapped Tribal or whatever - not useful.
  
  #  What about # of people overall (and maybe by site??) overlapping with a Tribal or nonattainment or CJEST area?
  #  Want to know how many people have the flag, ie how many are in BGs that are in nonattainment areas.
  #   possibly even by pop subgroup, but at least for total unique residents at these sites. 
  # *** That would need a completely different type of formula -- not the sum of flags, but sum(pop[flagged] * bgwt_popstats[flagged])
  #  (also Useful to know how many sites intersect NA areas, or Tribal etc., and that is count of sites[flagged]... count of sites with flag>0.
  # Assuming each site has a flag of 0 or 1, only, (check that) then the sum of that flag indicator over sites gives the answer.
  
  
  ################### #
  
  ## WEIGHTS FOR MIN/MAX BG-SCORE CALCULATION:
  ##
  ## - for COUNTS"-TYPE INDICATORS, bgwt or sharehere (NOT as share of total bgwts) is the weighting for popstats table... min(bgwt * countvar, na.rm=T)
  ##   min (or max) COUNT at any bg should reflect counts that are already "weighted" by the bgwt (and pop if relevant)
  ##   so if only half the bg is "at" these sites, and bg has 100 people total, 28 hisp total, 4 NPL total,
  ##   then the actual counts at the site should be 50=pop, 28 * 0.5 = 14 hisp, (and 4 * 0.5 = 2 NPL??)
  ##   and then the minimum must take min of those weighted (reduced) counts 
  ##   reflecting what is "at" these sites from that bg, not of the total bg counts!!
  ##
  ## - for ALL OTHER INDICATORS (like %lowinc or npl.proximity or pctile or ratio if avail.), just use NO WTS... the unweighted BG number already in popstats.
  ##   since that score reflects each person in the BG regardless of how much of the BG is "at" the sites analyzed.
  #
  ##  "Min/max BLOCKGROUP score, out of all bgs (people) analyzed" is calculated from popstats table, but must use PROPER WEIGHTING !!
  ##  "Min/max BLOCKGROUP score, out of all bgs (people) JUST AT THIS 1 SITE" can be done only in doaggregate(), and only for selected Indicators like "Min Distance for anyone at this site" where the whole indicator is defined as a min or max.
  #
  ## WEIGHTS FOR MIN/MAX SITE-SCORE CALCULATION:  NO WTS AT ALL
  ##
  ## "Min/max SITEWIDE score, out of all sites analyzed" is calculated from sitestats table. It is distinct from what doaggregate() does for some min/max indicator types.
  
  
  ################### #
  
  
  bgwt_popstats[is.na(bgwt_popstats)] <- 0
  wts_popstats <- unlist(popstats[ , wtscolname]) * bgwt_popstats
  wts_popstats[is.na(wts_popstats)] <- 0
  
  # weights for sitestats table of sites are not needed probably - weight each site equally.
  #  vector length is NROW(sitestats) and 
  #  $pop is the weight to use (do not need $bgwt), since 
  #  in that table the pop was just the portion at that site.
  wts_sitestats <- unlist(sitestats[ , wtscolname]) 
  wts_sitestats[is.na(wts_sitestats)] <- 0
  {
    # ~----------------------------------------------------------------------------------- ####
  }
  #|################################################################################# ####
  
  # define rowfuns, to calc summary.cols ####
  
  ## (e.g. EACH SITE's "# of indexes >=80th" for a GROUP of INDICATORS) - Max or # of high scores at a site.
  ## ( 1 Site / row; 1 sumstat/ column ) ####
  ################################################################################# ##### #
  
  rowfuname <- vector()
  rowfun  <- list()
  rowargs <- list()
  i <- 0
  
  for (setnum in 1:length(threshgroups)) {
    
    # comparisons to thresholds(s), one set per element/vector in the list
    # one set of thresholds, which are recycled as needed to match length of this threshnames[[threshnum]] vector
    
    ############################################ #
    ## ...At site, what is max indicator? ####
    # i.e., FOR EACH SITE WHAT IS THE MAX VALUE OF A FEW INDICATORS (PERCENTILES) AT THAT SITE?
    ############################################ #
    i = i + 1
    rowfuname[i] = paste('Max of', threshgroups[[setnum]])
    rowfun[[i]] = function(x, varnames, na.rm) {
      if (is.null(varnames)) {
        return(rep(NA, NROW(x)))
      }
      rowMaxs.sofar <- suppressWarnings(rowMaxs2( x[ , varnames ], na.rm = na.rm))
      # replace negative infinity with NA, to handle cases where entire row was NA values so rowMaxs returned -Inf
      rowMaxs.sofar[is.infinite(rowMaxs.sofar)] <- NA
      return(rowMaxs.sofar)
    }
    rowargs[[i]] <- list(x = NULL, varnames = threshnames[[setnum]], na.rm = na.rm)
    
    ############################################ #
    ## ...At site, how many indicators are > x percentile? ####
    # FOR EACH SITE HOW MANY OF A FEW INDICATORS (PERCENTILES) ARE AT/ABOVE A SPECIFIED THRESHOLD?
    ############################################ #
    i = i + 1
    rowfuname[i] = paste('Number of', threshgroups[[setnum]], 'at/above threshold of', paste(thresholds[[setnum]], collapse = '/' ) )
    rowfun[[i]] = function(x, varnames, threshold, or.tied=TRUE, na.rm) {
      if (is.null(varnames)) {
        return(rep(NA, NROW(x)))
      }
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
    # if (is.null(varnames)) {
    #   return(rep(NA, NROW(x)))
    # }
    #       0 < colcounter( x[ ,  varnames], threshold=threshold, or.tied=or.tied, na.rm=na.rm ) 
    #     }
    #     rowargs[[i]] <- list(x=NULL, varnames=threshnames[[setnum]], threshold=thresholds[[setnum]], or.tied=TRUE, na.r = a.rm)
  }
  
  # ~ ----------------------------------------------------------------------------------- ####  
  #|################################################################################# ####
  
  # define colfuns, to calc summary.rows ####
  
  ## (e.g. EACH INDICATOR's mean across all sites or people) -- % LOW-INCOME OVERALL OR AVG TRAFFIC SCORE
  #- A SUMMARY OF EACH INDICATOR for THE WHOLE SET OF UNIQUE PEOPLE or over ALL SITES  
  # (e.g. overall percent lowincome, or avg person's traffic score, max, etc)
  ## ( 1 Sumstat / row; 1 Indicator/Column ) ####
  ###################################################################################### #
  
  # column SUMMARY FUNCTIONS RETURN 1 ROW EACH:
  
  colfuname <- vector()
  colfun <- list()
  bywhat <- vector() # specify if this function gets applied across people or across sites
  # n specifies the order in which summary stat rows will appear
  n = 0
  
  ## ...avg site *** already have this from doaggregate earlier steps!! ####
  
  n = n + 1
  colfuname[n] = 'Average site'
  colfun[[n]] = function(x, ...) {colMeans(x, na.rm = na.rm)}
  bywhat[n] <- 'site'   ## MUST CALC FROM sitestats -- NOT popstats
  
  ## ...avg person *** already have this from doaggregate earlier steps!!  ####
  # and was not done correctly here yet
  
  ### average person info WAS DONE CORRECTLY IN  results_overall which is not passed to batch.summarize() by server or ejamit()
  
  # #  + be CAREFUL - MUST USE ON popstats, like in  results_bybg_people. IF USED WITH sitestats, would be AVERAGING ACROSS ALL PEOPLE BUT WHILE DOUBLECOUNTING ANY AT 2 SITES. 
  n = n + 1
  colfuname[n] = 'Average person'
  colfun[[n]] = function(x, ...) {
    
    0 # replace later with correct values from  overall[ , ..vars]
    
    # collapse::fmean(x, w = wts, na.rm = na.rm)
  }
  bywhat[n] <- 'pop'  ## MUST CALC FROM popstats    -- NOT sitestats
  
  ## ...median site ####
  
  # n = n + 1
  # colfuname[n] = 'Median site'        # redundant if already getting 50th and other percentiles across sites, but may want "Median site" name used elsewhere
  # colfun[[n]] = function(x, ...) {
  #   sapply(x, FUN = function(y) {stats::median(y, na.rm = na.rm)})
  # }
  # bywhat[n] <- 'site'   ## MUST CALC FROM sitestats -- NOT popstats
  
  ## ...median person  *** CAREFUL - IF USED WITH sitestats, would be MEDIAN ACROSS ALL PEOPLE BUT WHILE DOUBLECOUNTING ANY AT 2 SITES. MUST USE ON popstats,####
  # and was not done correctly here yet
  
  # n = n + 1
  # colfuname[n] = 'Median person'   # redundant if already getting percentiles that include 50th
  # colfun[[n]] = function(x, ...) {
  #   collapse::fmedian(x = x, w = wts, na.rm = TRUE)
  #   # collapse::fquantile(x = y, w = wts, na.rm = TRUE, probs = 0.50)
  #   ## confirm this works as expected but in the test data the pop was the entire weight and didnt need bgwt
  #   # > x1 = sapply(testingdata,   Hmisc::wtd.quantile, weights = testingdata$pop, probs = 0.50)
  #   # > x2 = collapse::fmedian(x = testingdata, w = testingdata$pop, na.rm = TRUE)
  #   # > all.equal(as.vector(x1), as.vector(x2))
  #   #   TRUE !
  # }
  # bywhat[n] <- 'pop'   ## MUST CALC FROM popstats    -- NOT sitestats 
  
  
  ################### #
  
  ## ...min SITE ####
  
  n = n + 1
  colfuname[n] = 'Min site'  
  
  colfun[[n]] = function(x, ...) {
    # quiet warning about Inf or -Inf if no non-missing args and set those results to NA
    z = suppressWarnings( colMins2(x, na.rm = na.rm) )
    z[is.infinite(z)] <- NA
    return(z)
  }
  bywhat[n] <- 'site'
  
  ################### 
  #
  ## ...min BLOCKGROUP (any person's block group) score ####
  
  #  MIN/MAX BG-SCORE was not done correctly here yet
  
  # n = n + 1
  # colfuname[n] = 'Min BG'
  # colfun[[n]] = function(x, ...) {
  #   # quiet warning about Inf or -Inf if no non-missing args and set those results to NA
  #   z = suppressWarnings( collapse::fmin(x, na.rm = na.rm) )
  #   z[is.infinite(z)] <- NA
  #   return(z)
  # }
  # bywhat[n] <- 'pop'
  ################### #
  
  ## ...max SITE score ####
  
  n = n + 1
  colfuname[n] = 'Max site'  ## RENAMED IT 
  colfun[[n]] = function(x, ...) {
    # quiet warning about Inf or -Inf if no non-missing args and set those results to NA
    z = suppressWarnings( colMaxs2(x, na.rm = na.rm) )
    z[is.infinite(z)] <- NA
    return(z)
  }
  bywhat[n] <- 'site'
  
  ################### #
  
  ## ...max BLOCKGROUP (any person's block group) score ####
  
  #  MIN/MAX BG-SCORE was not done correctly here yet
  
  # n = n + 1
  # colfuname[n] = 'Max BG'
  # colfun[[n]] = function(x, ...) {
  #   # quiet warning about Inf or -Inf if no non-missing args and set those results to NA
  #   z = suppressWarnings( collapse::fmax(x, na.rm = na.rm) )
  #   z[is.infinite(z)] <- NA
  #   return(z)
  # }
  # bywhat[n] <- 'pop'
  # 
  # ################### #
  
  
  # already have SUMS from doaggregate earlier steps!! and was not done correctly here yet
  ## ...sum overall all unique people  *** CAREFUL - IF USED WITH sitestats, would be SUM ACROSS ALL SITES BUT WHILE DOUBLECOUNTING ANY AT 2 SITES ####
  ## sum over all people 
  # n = n + 1
  # colfuname[n] = 'Sum'
  # colfun[[n]] = function(x, ...) {colSums(x, na.rm = na.rm)}
  # bywhat[n] <- 'pop'
  
  
  # would you need a sum over site scores ever? (as distinct from sum over unique people, so avoiding overlaps) 
  
  #|########################################## ####
  
  #~ ----------------------------------------------------------------------------------- ####
  
  #|########################################## ####
  # (unused way) QUANTILES etc for EACH INDICATOR (ALL SITES/PEOPLE)   ####
  # VIA FUNCTIONS TO CREATE MULTIPLE SUMMARY STAT ROWS EACH:
  # THAT REQUIRES A DIFFERENT APPROACH TO POPULATING THE RESULTS VARIABLE
  # 1) could append the group of quantiles to summarycols (use probs) outside the loop over functions, using rbind instead of [i] <-  
  # 2) could write each quantile as a single function - time consuming to write out & hard to accomodate probs. 
  # 3) could have each function also have a value that specifies how many rows or cols it will output & account for that (seems complicated)
  
  # ...quantiles ####
  # while not yet working, use other approach via rbind, later in this code:
  if (1 == 0) {
    just.rbind.quantiles <- FALSE
    if (length(probs) > 0) {
      n = n + 1
      ########## # 
      
      ##? Percentile of sites  ####
      
      myfunlist <- list()
      
      for (z in 1:length(probs)) {
        # empty functions but will then create a func body for each percentile
        myfunlist[[z]] <- function(x, ...) { }
        f <- (parse( " function (x) ", as.symbol("{"), paste('quantile(x,probs = probs[',z,'], na.rm = na.rm)'), '}' )) 
        
        body(myfunlist[[z]]) <- f
      }
      colfuname[n:(n - 1 + length(probs))]    <- paste('Percentile of sites', 100 * probs)
      colfun[[  n:(n - 1 + length(probs))  ]] <- myfunlist
      ########## # 
      
      ##? Percentile of people  ####
      
      ## should do any quantiles here or not in this batch.sum function at all ? this whole batch sum function was not designed with vectorization across probs, etc., as an option
      # f <- parse( " function (x) ", as.symbol("{"), paste('collapse::fquantile(x, w = wts, probs = probs    , na.rm = na.rm)'), '}' )  
      
      myfunlist <- list()
      
      for (z in 1:length(probs)) {
        # empty functions but will then create a func body for each percentile
        myfunlist[[z]] <- function(x, ...) { }
        
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
      ########## #
      
      ## *** nextcol is defined but not used anywhere in this draft code
      # nextcol <- 1 + ((n - 1) + 2 * length(probs))
      # colfuname[ nextcol ]=' '
      # colfun[[ nextcol  ]]=function(x, na.rm=TRUE) {  }
    }
  } else {
    just.rbind.quantiles <- TRUE  
    # while not working
  }
  #~ ----------------------------------------------------------------------------------- ####
  
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
  ## but note numeric ones are only a subset of columns in sitestats, and columns in popstats is a subset of that subset.
  
  # rows (like extra rows for a results tables) with 
  # summary.rows = 1 row per Summary Stat, using colfuns ####
  # 1 col per results indicator to be summarized (e.g., traffic.score. numerics)
  summary.rows <- matrix(NA, nrow = colfuns.count.picked, ncol = ncol(sitestats)) # *** the size of sitestats but NOT popstats !
  
  # columns (like extra cols for a results table) with 
  # summary.cols = 1 col per Summary Stat, using rowfuns ####
  #  (summarizing groups of indicators, e.g., count of index percentiles >=80th),
  # 1 row per site.
  # Only works for sitestats not popstats. 
  summary.cols <- matrix(NA, nrow = nrow(sitestats), ncol = rowfuns.count.picked ) 
  
  summary.rows.names <- vector(length = colfuns.count.picked)
  summary.cols.names <- vector(length = rowfuns.count.picked)
  
  ############################################ #
  # ...* Make summary rows via colfun(s) ------------ ######
  # where each element in a summary row summarizes 1 column (field) across all the rows of batch data
  ############################################ #
  
  # keep track of the colnames to apply the functions to & carefully since sitestats and popstats have different sets of columns
  numcolnames_sitestats  <- colnames(sitestats)[sapply(sitestats, class) %in% c('numeric', 'integer')]
  numcolnames_popstats   <- colnames(popstats)[sapply(popstats, class) %in% c('numeric', 'integer')]
  
  if (!all(numcolnames_popstats %in% numcolnames_sitestats)) {
    # this would cause a problem if the results table is based on sitestats and has things like ratios, avg, pctile, etc.
    # but lacks column called bgwt that popstats has and popstats tries to calc avg etc. of that column and write it to results but that colname is not in results container
    # message("dropping summary stats for columns found in popstats but not in sitestats, so output colnames is based on sitestats, results_bysite, not results_bybg_people")
    numcolnames_popstats <- numcolnames_popstats[numcolnames_popstats %in% numcolnames_sitestats]
  }
  
  for (i in 1:colfuns.count.picked) {
    
    fnum <- which(colfun.picked)[i]
    
    if (bywhat[fnum] == 'site') {
      
      # *** wts are not passed from here to function since no wts are needed for for sitestats (& any weight would get ignored by those summary funcs anyway).
      
      summary.rows[i, match(numcolnames_sitestats, names(sitestats))] <- as.vector( colfun[[fnum]](sitestats[ , numcolnames_sitestats], na.rm = na.rm) ) #  wts = 1, 
      
    } else {
      # bywhat[fnum] == 'pop'
      
      ## Need bgwt * pop for some stats or indicators (e.g.,    ),
      ## just bgwt for some stats or indicators (e.g.,    ), 
      ## and no wts at all for some stats or indicators (e.g., max/min?) ***
      wts <- wts_popstats
      
      summary.rows[i, match(numcolnames_popstats, names(popstats))] <- as.vector( colfun[[fnum]](popstats[ , numcolnames_popstats], wts = wts, na.rm = na.rm) )
    }
    
    if (testing) {cat('now summary.rows[i, ] all cols is \n'); print(summary.rows[i, ])}
    
    summary.rows.names[i] <- colfuname[fnum]
    
    # could this have the wrong # of elements if na.rm=TRUE and it somehow doesn't return NA for one of the columns??
  }
  
  ############################################ #
  # ...* Make summary cols  via rowfun(s) ------------ ######
  # where each element in a summary col summarizes 1 row (site) across all the RELEVANT cols of batch data (e.g., all US Summary Index percentiles)
  ############################################ #
  
  for (i in 1:rowfuns.count.picked) {
    
    fnum <- which(rowfun.picked)[i]        # this line only matters if not all available functions were picked to be used
    myargs <- rowargs[[fnum]]
    
    # i think bywhat only is used for colfuns but rowfuns are always by-site stats
    myargs$x <- sitestats
    
    # if (bywhat[fnum] == 'site') { 
    #   myargs$x <- sitestats
    # } else {
    #   myargs$x <- popstats
    # }
    
    summary.cols[ , i] <- round(as.vector(
      do.call(rowfun[[fnum]], args = myargs)            ## ERROR HERE if only 1 indicator ?  if >1 group of indicators? 
    ), 2)
    
    summary.cols.names[ i] <- rowfuname[fnum]
  }
  
  ############################################ #
  # ...create useful rownames and colnames for the outputs ####
  ############################################ #
  
  rownames(summary.rows) <- summary.rows.names
  colnames(summary.rows) <- colnames(sitestats)
  
  colnames(summary.cols) <- summary.cols.names
  rownames(summary.cols) <- seq.int(length.out = NROW(sitestats))
  
  # For summary cols, maybe put a duplicate column of user's site names field first if it exists, so can freeze it when seeing summary stat columns view
  
  #~ ----------------------------------------------------------------------------------- ####
  
  # SIMPLER WAY TO DO QUANTILES  ######
  
  if (just.rbind.quantiles) {
    
    ## samples of data for testing:
    # library(collapse)
    # out = testoutput_ejamit_100pts_1miles
    # na.rm = TRUE
    # probs = c(50,80,90,95,100) / 100
    # sitestats = out$results_bysite
    # popstats  = out$results_bybg_people
    
    quantile.rows     <- matrix(NA, nrow = length(probs), ncol = NCOL(sitestats))
    wtd.quantile.rows <- quantile.rows # not  matrix(NA, nrow = length(probs), ncol = NCOL(popstats))
    # wtd.quantile.rows has all the extra columns that are not in popstats, just so it can be the same width as summary.rows and quantile.rows
    
    colnames(quantile.rows)     <- colnames(sitestats)
    colnames(wtd.quantile.rows) <- colnames(sitestats) # not  colnames(popstats)
    
    rownames(quantile.rows)     <- paste('Percentile of sites',  100 * probs)
    rownames(wtd.quantile.rows) <- paste('Percentile of people', 100 * probs)
    
    # quantiles of sites ####
    numcol = sapply(sitestats, class) %in%  c('numeric', 'integer')
    x <- sapply(
      sitestats[ , numcol], 
      function(y) {stats::quantile(y, probs = probs, na.rm = na.rm)} # no weights since each site is same weight
    )
    quantile.rows[ , numcol] <- as.matrix(x)
    
    # quantiles of people ####
    numcol = sapply(popstats, class) %in%  c('numeric', 'integer')
    setDT(popstats)
    popstats[is.na(pop), pop := 0] # crashes fquantile if pop is NA here
    x <-  popstats[ ,  lapply(.SD, function(x) {
      if (collapse::allNA(x)) {return(rep(NA, length(probs)))}
      collapse::fquantile(x, w = pop, na.rm = na.rm, probs = probs) # weighted quantiles -- see notes on selecting weights
    }),
    .SDcols = names(popstats)[numcol] ]
    
    setDF(x)
    setDF(popstats)
    # wtd.quantile.rows has all the extra columns that are not in popstats, just so it can be the same width as summary.rows and quantile.rows
    
    # setdiff(colnames(x), colnames(wtd.quantile.rows[, colnames(wtd.quantile.rows) %in% colnames(popstats)[numcol] ]))
    # [1] "bgid" "bgwt"
    x = x[ , colnames(x) %in% colnames(wtd.quantile.rows)]
    wtd.quantile.rows[, colnames(wtd.quantile.rows) %in% colnames(popstats)[numcol] ]  <-   as.matrix(x)
    
    summary.rows <- rbind(summary.rows, quantile.rows, wtd.quantile.rows)
  }
  
  #|################################################################################# ### #
  
  summary.cols <- data.frame(summary.cols, stringsAsFactors = FALSE)
  summary.rows <- data.frame(summary.rows, stringsAsFactors = FALSE)
  
  ## REPLACE PROBLEMATIC MEDIAN person w CORRECT VALUES from table of percentiles ####
  summary.rows['Median site', ]   <- summary.rows['Percentile of sites 50', ] 
  summary.rows['Median person', ] <- summary.rows['Percentile of people 50', ]
  
  ## REPLACE PROBLEMATIC AVERAGES w CORRECT VALUES from overall table ####
  cnames <- colnames(summary.rows)[colnames(summary.rows) %in% colnames(overall)] # just to be safe
  summary.rows["Average person", cnames] <- as.numeric(overall[ , cnames])
  
  #~ ----------------------------------------------------------------------------------- ####
  
  # RETURN A LIST OF 2 TABLES: summary rows, summary columns ############### 
  
  x <- list(rows = summary.rows, cols = summary.cols) 
  
  ################################################################################## ### #
  
  # provide some interesting subsets of outputs for convenience
  x$keystats <- round( t(x$rows[c('Average site', 'Average person'), ]), 2)
  x$keyindicators <- round(x$rows[ , c(names_wts, as.vector(names_d))] , 2)
  
  # flag/yesno: Overlaps w special areas, counts of features like schools, etc. ####
  junk = capture.output({
    
    zsites = flagged_pct_sites(sitestats)
    myrnames = names(zsites)
    names(zsites) <- gsub("num_school", "Any schools", names(zsites))
    names(zsites) <- gsub("num_hospital", "Any hospitals", names(zsites))
    names(zsites) <- gsub("num_church", "Any places of worship", names(zsites))
    longernames = fixcolnames(names(zsites), 'r', 'long')
    longernames = gsub("Flag for ", "", longernames)
    x$flagged_areas <- data.frame(
      
      data.frame(Indicator = longernames),
      data.frame(`Percent_of_these_Sites`  = t(zsites)),
      data.frame(`Percent_of_these_People` = t(flagged_pct_pop(popstats))),
      data.frame("Percent_of_all_People_Nationwide" = t(flagged_pct_pop_us()))
      # it would be some work to do this for the states analyzed wtd by analyzed people from each state (as done to get overall ratios to State averages)
    )
    rownames(x$flagged_areas) <- NULL
    x$flagged_areas$ratio <- round(x$flagged_areas$Percent_of_these_People / x$flagged_areas$Percent_of_all_People_Nationwide, 2)
    x$flagged_areas$rname = myrnames
  })
  
  if (interactive() && !quiet) {
    print(x$keystats) # print a useful subset of info to console (for interactive use)
    # shows Average Person and Average Site for the list of indicators analyzed
    print(x$keyindicators)
  }
  
  invisible(x)
}
