

# calc_ejam()  which uses  calc_byformula()  -  read the formulas and execute them
# formula_varname()  - utility to get the names of variables created by the formulas
# formulas_d  - a vector of formulas as text strings like "a = 1 + b" see data-raw/datacreate_formulas.R


######################################## #


################################################################ #


#' DRAFT utility to use formulas provided as text, to calculate indicators
#'
#' @details may be used by doaggregate_custom()
#' @param bg data.frame//table of indicators or variables to use
#' @param keep.old names of columns (variables) to retain from among those provided in bg
#' @param keep.new names of calculated variables to retain in output
#' @param formulas text strings of formulas
#'
#' @return data.frame of calculated variables one row per bg row
#' @examples
#'
#' ### example using just 10 block groups from 1 county in Delaware
#'
#'  c1 <- fips2countyname(fips_counties_from_state_abbrev('DE'), includestate = F)[1]
#'  bgdf = data.frame(EJAM::blockgroupstats[ST == "DE" & countyname == c1, ])[1:10, ]
#'
#'  newdf <- calc_ejam(bgdf, keep.old = "",
#'    formulas = c(
#'      "my_custom_recalc_demog <- (pctlowinc + pctmin)/2",
#'      "mystat2  = 100 * pctlowinc"))
#' cbind(Demog.Index = bgdf$Demog.Index, newdf, pctlowinc = bgdf$pctlowinc)
#'
#' newdf <- calc_ejam(bgdf, formulas = formulas_d)
#' newdf
#'
#'
#' ##  example of entire US
#' #
#' newdf1  <- calc_ejam(as.data.frame(bgdf), formulas = formulas_d)
#'   t(summary(newdf1))
#'
#' bgdf <- data.frame(blockgroupstats)
#' newdf <- calc_ejam(bgdf,
#'                    keep.old = c('bgid', 'pop', 'hisp'),
#'                    keep.new = "all",
#'                    formulas = formulas_d
#' )
#' round(t(newdf[1001:1002, ]), 3)
#' cbind(
#'   newdf[1001:1031, c('hisp', 'pop', 'pcthisp')],
#'   check = (newdf$hisp[1001:1031] / newdf$pop[1001:1031])
#'   )
#' ## note the 0-100 percentages in blockgroupstats versus the 0-1 calculated percentages
#' cbind(round(sapply(newdf, max, na.rm=T),2),
#' names(newdf) %in% names_pct_as_fraction_blockgroupstats)
#'
#' EJAM:::formula_varname(formulas_d)
#'
#' rm(bgdf)
#'
#' @export
#'
calc_ejam <- function(bg,
                      # folder = getwd(),
                      keep.old = c("bgid", "pop"),
                      keep.new = 'all',
                      # formulafile,
                      formulas) {

  if (is.null(formulas)) {
    if (exists("formulas_d")) {
      formulas <- formulas_d
    } else {
      warning("no formulas specified or found, so no calculation done")
      return(NULL)
    }
  }

  # if (!missing(formulafile) &
  #     !missing(formulas)) {
  #   stop('Cannot specify both formulafile and formulas.')
  # }

  # if (missing(formulafile) & missing(formulas)) {
  #   #both missing so use default built in formulas and fieldnames
  #   x <- ejscreen::ejscreenformulas  # lazy loads as data
  #   myformulas <- x$formula
  # }

  # if (!missing(formulafile) & missing(formulas)) {
  #   if (!file.exists(file.path(folder, formulafile))) {
  #     stop(paste(
  #       'formulafile not found at',
  #       file.path(folder, formulafile)
  #     ))
  #     # x <- ejscreen::ejscreenformulas  # or could lazy load as data the defaults here
  #   } else {
  #     x <-
  #       read.csv(file = file.path(folder, formulafile),
  #                stringsAsFactors = FALSE)
  #     myformulas <- x$formula
  #   }
  # }

  # if (missing(formulafile) & !missing(formulas)) {
  myformulas <- formulas
  ## could add error checking here
  # }

  if (keep.old[1] == 'all') {
    keep.old <- names(bg)
  }
  # don't try to keep fields not supplied in bg
  keep.old <- keep.old[keep.old %in% names(bg)]
  if (keep.new[1] == 'all') {
    keep.new <- formula_varname(myformulas) # tries to keep all of those formulas would try to create
  }
  bg <- calc_byformula(bg, formulas = myformulas, keep = c(keep.old, keep.new))
  # if any of these are not successfully created by calc_byformula(), they just won't be returned by that function.
  return(bg)
}
################################################################ #


#' DRAFT utility to use formulas provided as text, to calculate indicators
#'
#' @param mydf data.frame of indicators or variables to use
#' @param formulas text strings of formulas
#' @param keep useful if some of the formulas are just interim steps
#'   creating evanescent variables created only for use in later formulas 
#'   and not needed after that
#' @return data.frame of results, but 
#'   if mydf was a data.table, returns a data.table
#'
calc_byformula <- function(mydf, formulas = NULL, keep = formula_varname(formulas)) {


  # DRAFT WORK NOT COMPLETED
  
  if (is.data.table(mydf)) {
    wasdt = TRUE
    setDF(mydf)
    on.exit(setDT(mydf))
  } else {
    wasdt = FALSE
  }
    
  if (is.null(formulas)) {
    stop("no formulas specified or found, so no calculation done")
  }
  formulas <- trimws(formulas)
  formulas <- formulas[!is.na(formulas)]
  #  cat('\n formulas: ', formulas,'\n\n')
  #  cat('\n keep: ', keep,'\n\n')

  attach(mydf)
  on.exit(detach(mydf))

  for (thisformula in formulas) {

    # *** Need to handle cases where formula tries to use some variable that was not a colname in mydf
    #   but is in the search path, like in the calling envt.

    # May need to more carefully specify environment, as in

    #  as we probably do not want formulas to use variables defined globally or where this function was called from, right?
    # If not careful about the environment in which things are evaluated,
    # A problem arises if a formula relies on a variable that is not in mydf but is used by some loaded package,
    # for example if mydf does not contain the field called cancer but the survival package is attached and provides a dataset lazyloaded called cancer,
    #  the formula appears to work, but it uses the cancer from the survival package instead of the intended but missing mydf$cancer
    # So we could check the formula's inputs to see that they are all in mydf,

    #  or are the outputs of earlier formulas that would work!

    #    textform <- strsplit(gsub('.*<-', '', parse(text = thisformula)), split=' |[[:punct:]]| ' )[[1]]
    #    textform <- textform[textform != '']
    # that gets the words from the right side of the formula, but only if it is written as ....<-....
    # and it also gets functions like sum, min, max, mean, etc. so it can't really be used to check if each variable is in mydf since some are functions not variables in the formula.
    # and it splits up on _ etc.
    ## Also might consider package called  sourcetools that has a tokenize function to parse text into parts

    # y <- try(source_this_codetext(thisformula), silent = FALSE)

    y <- try(source_this_codetext(thisformula), silent = TRUE)
    # tries to evaluate formula in THIS environment (the one that called the function source_this... )
    suppressWarnings(
      if (class(y) == "try-error") {
        cat('Cannot use formula: '); cat(thisformula, '\n') # print(as.character(parse(text=thisformula)))
      } else {
        cat(paste0('Using ', thisformula, '\n'))
      }
    )
  }

  # RETURN ONLY THE ONES SUCCESSFULLY CREATED, OUT OF ALL REQUESTED TO BE KEPT

  keep.from.mydf <- keep[keep %in% names(mydf)]
  keep.other <- keep[!(keep %in% keep.from.mydf)]
  if (length(keep.other) > 0) {keep.other <- keep.other[keep.other %in% ls()] }

  # COULD ADD WARNINGS HERE ABOUT VARIABLES USER ASKED TO KEEP THAT DO NOT EXIST
  if (length(keep.from.mydf) > 0) {
    if (length(keep.other) > 0) {
      outdf <- data.frame( mydf[ , keep.from.mydf, drop = FALSE],
                           mget(keep.other),
                           stringsAsFactors = FALSE)
    } else {
      outdf <- data.frame( mydf[ , keep.from.mydf, drop = FALSE], stringsAsFactors = FALSE)
    }
  } else {
    if (length(keep.other) > 0) {
      outdf <- data.frame(mget(keep.other),
                          stringsAsFactors = FALSE)
    } else {
      outdf <- NULL
    }
  }
  if (wasdt) {
    setDT(outdf)
  }
  
  return(outdf)
}
################################################################ #


#' utility to check formulas and extract variable names they calculate values for
#'
#' @param myforms see calc_byformula()
#'
#'
formula_varname <- function(myforms) {

  return(
    gsub("^([^ <=]*)[ <=].*", "\\1", trimws(myforms))
  )

  # formula_varname(c("z=10", "b<- 1", "c <- 34", " h = 1+1", "   q=2+2"))
  # formula_varname(formulas_d)
  # > length(formulas_d)
  # [1] 40
  # > length(formula_varname(formulas_d))
  # [1] 40

}
################################################################ #
