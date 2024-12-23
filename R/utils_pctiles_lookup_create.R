#' Utility to create lookup table of percentiles 0 to 100 and mean for each indicator by State or USA total
#'
#' @details EJScreen assigns each indicator in each block group a percentile value via python script, using
#' <https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.percentileofscore.html>
#'
#' \preformatted{
#'   The way the python function is used as of 2023 is that percentileofscore is 80% if
#'   80% of all indicator values (statewide or nationwide, depending on the type being calculated)
#'   are less than (NOT equal to) the indicator value
#'   in the specified block group (since kind="strict").
#'   The percentile recorded in the EJScreen dataset is the floor of that,
#'   meaning if the 81.9% of values are less than x, the percentile is reported as 81.
#'   The EJScreen python script used to create percentile lookup tables is in a file
#'   called cal_statepctile_0222.py and the key lines of code and functions it uses are
#'
#'   pctile = math.floor(stats.percentileofscore(barray, indicatorscore, kind="strict"))
#'
#'   binvalue = getBinvalue(pctile)
#'
#'   and
#'
#'   def getBinvalue(pct):
#'
#' if pct is None:
#'   return 0
#' else:
#'     if pct >= 95:
#'     return 11
#' elif pct >= 90 and pct < 95:
#'   return 10
#' elif pct >= 80 and pct < 90:
#'   return 9
#' elif pct >= 70 and pct < 80:
#'   return 8
#' elif pct >= 60 and pct < 70:
#'   return 7
#' elif pct >= 50 and pct < 60:
#'   return 6
#' elif pct >= 40 and pct < 50:
#'   return 5
#' elif pct >= 30 and pct < 40:
#'   return 4
#' elif pct >= 20 and pct < 30:
#'   return 3
#' elif pct >= 10 and pct < 20:
#'   return 2
#' else:
#'   return 1
#'   }
#' @param x data.frame with numeric data. Each column will be examined to calculate
#'   mean,   and percentiles, for each zone
#' @param zone.vector optional names of states or regions, for example. same length as wts, or rows in mydf
#' @param zoneOverallName optional. Default is USA.
#' @param wts leave as default since weighted percentiles of blockgroups are not used for EJScreen percentiles anymore
#' @param type DO NOT CHANGE - moot for EJScreen/EJAM - SEE SOURCE CODE - Hmisc pkg wtd.quantile type "1/n" was used here in the past and possibly by EJScreen
#'   (EJScreen no longer uses weighted percentiles so this is moot for the weighted case)
#'   but collapse pkg fquantile is now used here to avoid Hmisc dependency
#'   and fquantile type 4 seems to be the same as Hmisc type "1/n" but that has not been confirmed,
#'   and this function by default uses fquantile type 1, the inverse of the ECDF however,
#'   which seems simpler than using type 4 which does linear interpolation between points of the ECDF!
#'   ***  NEED TO CONFIRM IF THAT CREATES A TABLE DIFFERENT THAN WHAT EJSCREEN WOULD CREATE
#' @param usecollapse logical, whether to use collapse::fquantile()
#'   instead of Hmisc package wtd.quantile and stats pkg quantile,
#'   to test before fully removing dependency on Hmisc and also speed it up.
#'
#' @keywords internal
#'
pctiles_lookup_create <- function(x, zone.vector = NULL, zoneOverallName = 'USA',
                                  wts = NULL, usecollapse = TRUE, type = 7) {
  mydf <- x

  pctiles.exact <- function(x, usecollapse = FALSE, type = 7) {
    if (usecollapse) {
      cbind(collapse::fquantile(x, type = type, probs = (1:100)/100, na.rm = TRUE))
    } else {
      cbind(    stats::quantile(x, type = type, probs = (1:100)/100, na.rm = TRUE))
    }
    # actually this should be written to assign the floor of exact % of scores that are < given bg score (?) ***
    # floor(sum(x < thisx)/length(x))
  }

  wtd.pctiles.exact <-  function(x, wts = NULL, na.rm = TRUE, type = 7, probs = (1:100) / 100, usecollapse = FALSE) {
    #  PERCENTILES, WEIGHTED, SO DISTRIBUTION OVER PEOPLE NOT PLACES, in case you need that
    #    EJScreen does not use weights anymore for that, however.
    if (usecollapse) {
      cbind(collapse::fquantile(x, w = wts, probs = probs, na.rm = na.rm, type = type))
      # if you wanted to speed this up, use the o= parameter in fquantile() ....
      #  o	param is like order(x), an integer vector giving the order of elements in x, so identical(x[o], sort(x)). If available, it considerably speeds up the estimation.
    } else {
      warning("Hmisc package no longer used for wtd.quantile function. To avoid the dependency EJAM uses collapse package function fquantile that has a weighted option  ")
      # this used to use wtd.quantile function from the Hmisc package, which had a type = "1/n" option    avoiding needing Hmisc here is the point.
      #cbind(      wtd.quantile(x, weights = wts, probs = probs, na.rm = na.rm, type = type)) # this function was from the Hmisc package.  type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n')
      cbind(collapse::fquantile(x, w = wts, probs = probs, na.rm = na.rm, type = type))
      # Programmers version: no names, intelligent defaults, or checks would be collapse::.quantile
      # for collapse fquantile, type is an integer 5-9. See quantile. Hyndman and Fan (1996) recommended type 8. The default in fquantile is type 7. ... not sure if that gives different results than EJScreen version?! ***
    }
  }

  if (is.null(zone.vector)) {

    if (is.null(wts)) {
      r <- data.frame( sapply(mydf, function(x) pctiles.exact(x, usecollapse = usecollapse, type = type)))
      if (usecollapse) {
        r <- rbind(r, t(data.frame(   mean = sapply(mydf, function(x) collapse::fmean(x, na.rm = TRUE)))))
      } else {
        r <- rbind(r, t(data.frame(   mean = sapply(mydf, function(x) mean(x, na.rm = TRUE)))))
      }

    } else {
      r <- data.frame( sapply(mydf, function(x) wtd.pctiles.exact(x, wts = wts, na.rm = TRUE, usecollapse = usecollapse, type = type)))
      if (usecollapse) {
        r <- rbind(r, t(data.frame(   mean = sapply(mydf, function(x) collapse::fmean(x, na.rm = TRUE)))))
      } else {
        r <- rbind(r, t(data.frame(   mean = sapply(mydf, function(x) stats::weighted.mean(x, na.rm = TRUE)))))
        # this used to use wtd.mean function from the Hmisc package, which had a type = "1/n" option that stats:: version does not!  avoiding needing Hmisc here is the point.
        # for the stats::weighted.mean() function, no type can be specified, unlike Hmisc func... not sure if that gives different results than EJScreen version?! ***
        #   If w is missing then all elements of x are given the same weight, otherwise the weights are normalized to sum to one (if possible: if their sum is zero or infinite the value is likely to be NaN).
        #   Missing values in w are not handled specially and so give a missing value as the result. However, zero weights are handled specially and the corresponding x values are omitted from the sum.
      }
    }

    r$REGION <- "USA" # zoneOverallName
    r$PCTILE <- rownames(r) #   1:100, 'mean'

  } else {

    r <- list()
    for (i in 1:length(unique(zone.vector))) {
      z <- unique(zone.vector)[i]

      if (is.null(wts)) {

        # ZONES BUT NO WEIGHTS ####

        r[[i]] <- data.frame(sapply(mydf[zone.vector == z,  ], function(x) pctiles.exact(x, usecollapse = usecollapse, type = type)))
        if (usecollapse) {
          r[[i]] <- rbind(r[[i]], t(data.frame(mean = sapply(mydf[zone.vector == z,  ], function(x) collapse::fmean(x, na.rm = TRUE)))))
        } else {
          r[[i]] <- rbind(r[[i]], t(data.frame(mean = sapply(mydf[zone.vector == z,  ], function(x) mean(x, na.rm = TRUE)))))
        }
        r[[i]]$REGION <- z
        r[[i]]$PCTILE <- rownames(r[[i]]) # 1:100,'mean'
      } else {

        # ZONES AND WEIGHTS ####

        r[[i]] = data.frame(sapply(mydf[zone.vector == z, ], function(x) wtd.pctiles.exact(x, wts[zone.vector == z], usecollapse = usecollapse, type = type) ) )
        if (usecollapse) {
          r[[i]] = rbind(r[[i]], t(data.frame(mean = sapply(mydf[zone.vector == z,  ], function(x) collapse::fmean(x, w =  wts[zone.vector == z], na.rm = TRUE) ) ) ))
        } else {
          r[[i]] = rbind(r[[i]], t(data.frame(mean = sapply(mydf[zone.vector == z,  ], function(x) stats::weighted.mean(x, w = wts[zone.vector == z], na.rm = TRUE) ) ) ))
        }
        r[[i]]$REGION <- z
        r[[i]]$PCTILE <- rownames(r[[i]]) # 1:100, 'mean'
      }
    }
    # ZONE LOOP DONE
    r <- do.call(rbind, r)
  }

  r <- data.frame(
    OBJECTID = 1:NROW(r),
    REGION = r$REGION,
    PCTILE = r$PCTILE,
    r[ , !(colnames(r) %in% c('REGION', 'PCTILE'))],
    stringsAsFactors = FALSE
  )
  rownames(r) <- NULL
  invisible(r)
}
