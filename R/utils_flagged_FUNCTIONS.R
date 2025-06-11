
#  see table or plot, of ratios of flagged areas and features / US ####

################## #

#' barplot of summary stats on special areas and features at the sites
#' @description Summary of whether residents at the analyzed locations are more likely to have
#' certain types of features (schools) or special areas (Tribal, nonattainment, etc.)
#' @details  See 
#' `varinfo(c(names_featuresinarea, names_flag, names_criticalservice))[,c("longname", "varlist")]`
#' 
#' These are the indicator summary stats shown:
#' 
#'  - [1,] "Number of Schools"
#'  
#'  - [2,] "Number of Hospitals"
#'  
#'  - [3,] "Number of Worship Places"
#'  
#'  - [4,] "Flag for Overlapping with Tribes"
#'  
#'  - [5,] "Flag for Overlapping with Non-Attainment Areas"
#'  
#'  - [6,] "Flag for Overlapping with Impaired Waters"
#'  
#'  - [7,] "Flag for Overlapping with CJEST Disadvantaged Communities"
#'  
#'  - [8,] "Flag for Overlapping with EPA IRA Disadvantaged Communities"
#'  
#'  - [9,] "Flag for Overlapping with Housing Burden Communities"
#'  
#'  - [10,] "Flag for Overlapping with Transportation Disadvantaged Communities"
#'  
#'  - [11,] "Flag for Overlapping with Food Desert Areas"
#'  
#'  - [12,] "% Households without Broadband Internet"
#'  
#'  - [13,] "% Households without Health Insurance"
#'  
#' 
#' @param ejamitout output from ejamit()
#' @param main optional title for plot
#' @param ylab optional y axis label
#' @param shortlabels optional alternative labels for the bars
#' @param caption left blank to avoid default caption used in [plot_barplot_ratios()] 
#' @seealso [ejam2areafeatures()] [batch.summarize()]
#' @examples 
#' out <- testoutput_ejamit_1000pts_1miles
#' ejam2barplot_areafeatures(out)
#' 
#' shortlabels = EJAM:::flagged_areas_shortlabels_from_ejam(out)
#' ejam2barplot_areafeatures(out, shortlabels = shortlabels)
#' 
#' @returns ggplot2 plot
#' 
#' @export
#'
ejam2barplot_areafeatures <- function(ejamitout,
                                      main = "% of analyzed population that lives in blockgroups with given features or that overlap given area type",
                                      ylab = "Ratio of Indicator in Analyzed Locations / in US Overall",
                                      shortlabels = NULL) {
  
  ratios <- flagged_areas_ratios_from_ejam(ejamitout)
  plot_barplot_ratios(ratios, main = main, ylab = ylab,
                      shortlabels = shortlabels,
                      caption = "")
}
######################################################### # ######################################################### # 
######################################################### # ######################################################### # 

################## #

#' simple way to see the table of summary stats on special areas and features like schools
#'
#' @param ejamitout output from ejamit()
#' @details
#' In this table, summary stats mean the following:
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
#' @returns a data frame with the summary of flagged areas
#' @seealso [ejam2barplot_areafeatures()] [batch.summarize()]
#' 
#' @export
#'
ejam2areafeatures <- function(ejamitout) {
  
  ejamitout$results_summarized$flagged_areas
}
################## #
# same as ejam2areafeatures - just a more consistent but less friendly name, used by other internal functions

flagged_areas_from_ejam <- function(ejamitout) {
  ejamitout$results_summarized$flagged_areas
}
################## #

# count certain areas overlapped & if certain features are here ####

# Helper functions used by batch.summarize() to summarize info from these indicators:
#
#  c(names_featuresinarea, names_flag, names_criticalservice) # the varlists
#  
# > varinfo(c(names_featuresinarea, names_flag, names_criticalservice))[,c("longname", "varlist")]
#                                                                                longname               varlist
# num_school                                                            Number of Schools  names_featuresinarea
# num_hospital                                                        Number of Hospitals  names_featuresinarea
# num_church                                                     Number of Worship Places  names_featuresinarea
# yesno_tribal                                           Flag for Overlapping with Tribes            names_flag
# yesno_airnonatt                          Flag for Overlapping with Non-Attainment Areas            names_flag
# yesno_impwaters                               Flag for Overlapping with Impaired Waters            names_flag
# yesno_cejstdis                Flag for Overlapping with CJEST Disadvantaged Communities            names_flag
# yesno_iradis                Flag for Overlapping with EPA IRA Disadvantaged Communities            names_flag
# yesno_houseburden                  Flag for Overlapping with Housing Burden Communities names_criticalservice
# yesno_transdis       Flag for Overlapping with Transportation Disadvantaged Communities names_criticalservice
# yesno_fooddesert                            Flag for Overlapping with Food Desert Areas names_criticalservice
# pctnobroadband                                  % Households without Broadband Internet names_criticalservice
# pctnohealthinsurance                              % Households without Health Insurance names_criticalservice

######################################################### # 

flagged_count_sites <- function(bysite = testoutput_ejamit_1000pts_1miles$results_bysite, flagvarnames = c(names_featuresinarea, names_flag, names_criticalservice)) {
  
  if (!is.data.table(bysite)) {
    wasnt = TRUE
    setDT(bysite)
  } else {
    wasnt = FALSE
  }
  stopifnot(all(flagvarnames %in% colnames(bysite)))
  x <- bysite[, lapply(.SD, function(z) {sum(z > 0, na.rm = TRUE)}),
              .SDcols = flagvarnames]
  
  #  note these 2 indicators are %, not count or 1/0:  "pctnobroadband", "pctnohealthinsurance"
  pctvars = c( 'pctnobroadband' , 'pctnohealthinsurance')
  x[, pctvars] <- bysite[, lapply(.SD, function(z) {sum(z, na.rm = TRUE)}),
                         .SDcols = pctvars]  ## THIS SUM OF PERCENTAGES OF PEOPLE HERE DOES NOT MAKE SENSE AS IT DOES FOR POPULATION
  if (interactive()) {
    # message("Note that for ", paste0(pctvars, collapse = ","), " the SUM OVER SITES OF PERCENTAGES OF PEOPLE HERE DOES NOT MAKE SENSE AS IT WOULD FOR POPULATION")
  }
  if (wasnt) {
    setDF(bysite)
  }
  return(x)
}
######################################################### # 

flagged_pct_sites <- function(bysite = testoutput_ejamit_1000pts_1miles$results_bysite, flagvarnames = c(names_featuresinarea, names_flag, names_criticalservice), digits = 1) {
  
  if (!is.data.table(bysite)) {
    wasnt = TRUE
    setDT(bysite)
  } else {
    wasnt = FALSE
  }
  stopifnot(all(flagvarnames %in% colnames(bysite)))
  x <- flagged_count_sites(bysite = bysite, flagvarnames = flagvarnames)
  x <- round(100 * x / NROW(bysite), digits = digits)
  if (interactive()) {
    cat("Site count total: ", prettyNum(NROW(bysite), big.mark = ","), "\n")
  }
  if (wasnt) {
    setDF(bysite)
  }
  return(x)
}
######################################################### # 

flagged_count_pop <- function(bybg_people = testoutput_ejamit_1000pts_1miles$results_bybg_people, flagvarnames = c(names_featuresinarea, names_flag, names_criticalservice)) {
  
  if (!is.data.table(bybg_people)) {
    wasnt = TRUE
    setDT(bybg_people)
  } else {
    wasnt = FALSE
  }
  neededvars = c("pop", flagvarnames)
  stopifnot(all(neededvars %in% colnames(bybg_people)))
  x <- bybg_people[, lapply(.SD, function(z) {sum((z > 0) * pop * bgwt, na.rm = TRUE)}),
                   .SDcols = flagvarnames]
  pctvars = c( 'pctnobroadband' , 'pctnohealthinsurance')
  x[, pctvars] <- bybg_people[, lapply(.SD, function(z) {sum(z * pop * bgwt, na.rm = TRUE)}),
                              .SDcols = pctvars]
  if (interactive()) {
    print("POP COUNT that has overlap = yes, or has 1+ features, or is in the relevant pct, in their blockgroup:")
    print(x)
    cat("\n\n")
  }
  if (wasnt) {
    setDF(bybg_people)
  }
  return(x)
}
######################################################### # 

flagged_pct_pop <- function(bybg_people = testoutput_ejamit_1000pts_1miles$results_bybg_people, flagvarnames = c(names_featuresinarea, names_flag, names_criticalservice), digits = 1) {
  
  if (!is.data.table(bybg_people)) {
    wasnt = TRUE
    setDT(bybg_people)
  } else {
    wasnt = FALSE
  }
  neededvars = c("pop", flagvarnames)
  stopifnot(all(neededvars %in% colnames(bybg_people)))
  x <- flagged_count_pop(bybg_people = bybg_people, flagvarnames = flagvarnames)
  x <- round(100 * x / sum(bybg_people$pop * bybg_people$bgwt, na.rm = TRUE), digits = digits)
  if (interactive()) {
    cat("Population total: ", prettyNum(sum(bybg_people$pop * bybg_people$bgwt, na.rm = TRUE), big.mark = ","), "\n")
  }
  if (wasnt) {
    setDF(bybg_people)
  }
  return(x)
}
######################################################### # 
######################################################### # 

flagged_count_pop_us <- function(bybg_us = blockgroupstats, flagvarnames = c(names_featuresinarea, names_flag, names_criticalservice)) {
  
  neededvars = c("pop", flagvarnames)
  stopifnot(all(neededvars %in% colnames(bybg_us)))
  x <- bybg_us[, lapply(.SD, function(z) {sum((z > 0) * pop, na.rm = TRUE)}),
               .SDcols = flagvarnames]
  pctvars = c( 'pctnobroadband' , 'pctnohealthinsurance')
  x[, pctvars] <- bybg_us[, lapply(.SD, function(z) {sum(z * pop, na.rm = TRUE)}),
                          .SDcols = pctvars]
  if (interactive()) {
    print("POP COUNT that has overlap = yes, or has 1+ features, or is in the relevant pct, in their blockgroup:")
    print(x)
    cat("\n\n")
  }
  return(x)
}
######################################################### # 

flagged_pct_pop_us <- function(bybg_us = blockgroupstats, flagvarnames = c(names_featuresinarea, names_flag, names_criticalservice), digits = 1) {
  
  neededvars = c("pop", flagvarnames)
  stopifnot(all(neededvars %in% colnames(bybg_us)))
  x <- flagged_count_pop_us(bybg_us = bybg_us, flagvarnames = flagvarnames)
  x <- round(100 * x / sum(bybg_us$pop, na.rm = TRUE), digits = digits)
  if (interactive()) {
    cat("Population total: ", prettyNum(sum(bybg_us$pop, na.rm = TRUE), big.mark = ","), "\n")
  }
  # results_overall$pop  is a bit different as denominator than  sum(bybg_us$pop, na.rm = TRUE) 
  return(x)
}
######################################################### # 

flagged_count_pop_st <- function(ST = stateinfo$ST, bybg_st = blockgroupstats, flagvarnames = c(names_featuresinarea, names_flag, names_criticalservice)) {
  
  # e.g.,
  # flagged_count_pop_st(c('pr', 'dc'))
  # flagged_count_pop_st('dc') + flagged_count_pop_st('pr')
  
  neededvars = c("pop", "ST", flagvarnames)
  stopifnot(all(neededvars %in% colnames(bybg_st)))
  st <- tolower(ST)
  rm(ST)
  bybg_st <- bybg_st[tolower(ST) %in% st, ..neededvars]
  if (interactive()) {
    cat("Population total: ", prettyNum(sum(bybg_st$pop, na.rm = TRUE), big.mark = ","), "\n")
    cat("\n\n")
  }
  x <- bybg_st[, lapply(.SD, function(z) {sum(z * pop, na.rm = TRUE)}),
               .SDcols = flagvarnames]
  # if (interactive()) {
  #   print("POP COUNT x FEATURE COUNT, summed over blockgroups (makes sense for pctnobroadband or pctnohealthinsurance):")
  #   print(x)
  #   cat("\n\n")
  # }
  x <- bybg_st[, lapply(.SD, function(z) {sum((z > 0) * pop, na.rm = TRUE)}),
               .SDcols = flagvarnames]
  pctvars = c( 'pctnobroadband' , 'pctnohealthinsurance')
  x[, pctvars] <- bybg_st[, lapply(.SD, function(z) {sum(z * pop, na.rm = TRUE)}),
                          .SDcols = pctvars]
  if (interactive()) {
    print("POP COUNT that has overlap = yes, or has 1+ features, or is in the relevant pct, in their blockgroup:")
    print(x)
    cat("\n\n")
  }
  return(x)
}
######################################################### # 

flagged_pct_pop_st <- function(ST = stateinfo$ST, bybg_st = blockgroupstats, flagvarnames = c(names_featuresinarea, names_flag, names_criticalservice), digits = 1) {
  
  neededvars = c("pop", "ST", flagvarnames)
  stopifnot(all(neededvars %in% colnames(bybg_st)))
  st <- tolower(ST)
  rm(ST)
  bybg_st <- bybg_st[tolower(ST) %in% st, ..neededvars] 
  x <- flagged_count_pop_st(ST = st, bybg_st = bybg_st, flagvarnames = flagvarnames)
  x <- round(100 * x / sum(bybg_st$pop, na.rm = TRUE), digits = digits)
  # results_overall$pop  is a bit different as denominator than  sum(bybg_st$pop, na.rm = TRUE) 
  return(x)
}
######################################################### # ######################################################### # 
######################################################### # ######################################################### # 

# helpers to format these summary stats for barplot ####

flagged_areas_ratiosvector_from_flagged_areas <- function(flagged_areas) {
  # reformat the table  ejamit()$results_summarized$flagged_areas
  unlist(as.vector(
    tidyr::pivot_wider(
      data = flagged_areas[, c('Indicator', 'ratio')], 
      names_from = "Indicator",
      values_from = "ratio" 
    )
  ))
}
################## #

# this is mostly to make the barplot easier

flagged_areas_ratios_from_ejam <- function(ejamitout) {
  # after ejamitout <- ejamit()
  # reformat the table   ejamitout$results_summarized$flagged_areas
  # into a named vector of ratios
  # ready for plot_barplot_ratios(), etc.
  flagged_areas_ratiosvector_from_flagged_areas(
    flagged_areas_from_ejam(ejamitout)
  )
}
################## #
# these "shortlabels" functions are not much better than just letting it use the defaults

flagged_areas_shrinklabels <- function(rnames, n = 30, do_gsub = TRUE) {
  longlabels <- fixcolnames(rnames, 'r', 'short')
  if (do_gsub) {
    mediumlabels <- gsub("without", "no", gsub("Overlaps ", "", longlabels))
  } else {
    mediumlabels <- longlabels
  }
  shortlabels <- substr(mediumlabels, 1, n)
  return(shortlabels)
}
## test flagged_areas_shrinklabels
# flagged_areas_testnames = c("num_school", "num_hospital", "num_church", "yesno_tribal",
#                             "yesno_airnonatt", "yesno_impwaters", "yesno_cejstdis", "yesno_iradis",
#                             "yesno_houseburden", "yesno_transdis", "yesno_fooddesert", "pctnobroadband",
#                             "pctnohealthinsurance")
# all.equal(flagged_areas_shrinklabels("yesno_houseburden"),
# "Housing Burden Commu"
# )
# all.equal(flagged_areas_shrinklabels(flagged_areas_testnames), 
#           c("Schools", "Hospitals", "Worship Places", "Tribes", "Nonattainment Area", 
#             "Impaired Waters", "CJEST Disadvantaged", "EPA IRA Disadvantage", 
#             "Housing Burden Commu", "Transportation Disad", "Food Desert", 
#             "% hhlds no Broadband", "% hhlds no Health In")
#           )
################## #

flagged_areas_shortlabels_from_ejam <- function(ejamitout, n = 30, do_gsub = TRUE) {
  # after ejamitout <- ejamit()
  # get graph-friendy short labels for the indicators in ejamit()$results_summarized$flagged_areas
  flagged_areas_shrinklabels(
    flagged_areas_from_ejam(ejamitout)$rname,
    n = n,
    do_gsub = do_gsub
  )
}
################## #
