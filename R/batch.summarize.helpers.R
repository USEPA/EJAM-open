
# batch.summarize() is in a separate file ####

# batch.summarize.helpers functions: ####

# rowMaxs2 ok
# colMins2 ok
# colMaxs2 ok
# change.fieldnames ok
# batch.clean ok
########################################################################### #

#' Get maximum of each row of data.frame
#' 
#' @param df data.frame or matrix.
#' @param na.rm default is TRUE. not tested for FALSE
#' @return max of each row
#' @seealso colMaxs2()
#' @keywords internal
#' 
rowMaxs2 <- function(df, na.rm = TRUE) {
  
  # had been doing this:
  # mymax <- do.call(pmax, c(bg[,names.ej.pctile], na.rm = TRUE)) 
  # now can do this:
  # mymax <- rowMaxs2(bg[,names.ej.pctile])

  if (is.matrix(df)) {df <- data.frame(df, stringsAsFactors = FALSE)}
  valid.cols <- sapply(df, function(x) { is.numeric(x) || is.logical(x) || is.character(x)})
  stopifnot(any(valid.cols)) 
  if (any(!valid.cols) ) {warning('using only numeric (double or integer) or logical or character columns -- ignoring other columns ')}
  
  result <- do.call(pmax, c(df[ , valid.cols], na.rm = na.rm))
  
  nononmissing <- rowSums(!is.na(df[ , valid.cols])) 
  result[nononmissing == 0] <- -Inf
  if (any(nononmissing)) {warning('where no non-missing arguments, returning -Inf')}
  return(result)
}
########################################################################### #


#' Get maximum of each column of data.frame
#' 
#' @param df data.frame or matrix.
#'   Can include numbers stored as character or as factors.
#' @param na.rm default is TRUE. not tested for FALSE
#' @return named vector of numbers
#' @examples 
#' df <- rbind(NA, data.frame(
#'   n1 = c(0, 0:8), n2 = c(0.1 + (0:9)), n3 = c(1:10), 
#'   allnas = c(rep(NA, 10)),
#'   logic = TRUE, 
#'   factortxt = factor('factor'), 
#'   txt = 'words',
#'   numberlike = as.character(6:15),
#'   numberlikefact = factor(as.character(6:15)),
#'   stringsAsFactors = FALSE))
#'   
#' df
#' colMaxs2(df)
#' @seealso [colMins2()]
#' @keywords internal
#' 
colMaxs2 <- function(df, na.rm = TRUE) {
  
  if (is.matrix(df)) {
    df <- as.data.frame(df)
  }
  
  chcol <- apply(df, 2, function(x) is.character(x) | is.factor(x))
  result <- rep(NA, NCOL(df))
  result[!chcol] <- collapse::fmax(df[, !chcol], na.rm = na.rm)
  result[chcol] <- suppressWarnings(
    collapse::fmax(apply(df[, chcol], 2, function(x) {
      # try to understand numbers that were stored as characters or factors
      as.numeric(as.character(x))
    }), na.rm = na.rm)
  )
  names(result) <- colnames(df)
  result <- unlist(result)
  return(result)
}
########################################################################### #

#' Get minimum of each column of data.frame
#' 
#' @param df data.frame or matrix.
#'   Can include numbers stored as character or as factors.
#' @param na.rm default is TRUE. not tested for FALSE
#' @return named vector of numbers
#' @seealso [colMaxs2()]
#' @examples 
#' df <- rbind(NA, data.frame(
#'   n1 = c(0, 0:8), n2 = c(0.1 + (0:9)), n3 = c(1:10), 
#'   allnas = c(rep(NA, 10)),
#'   logic = TRUE, 
#'   factortxt = factor('factor'), 
#'   txt = 'words',
#'   numberlike = as.character(6:15),
#'   numberlikefact = factor(as.character(6:15)),
#'   stringsAsFactors = FALSE))
#'   
#' df
#' colMins2(df)
#' 
#' @keywords internal
#' 
colMins2 <- function(df, na.rm = TRUE) {
  
  if (is.matrix(df)) {
    df <- as.data.frame(df)
  }
  
  chcol <- apply(df, 2, function(x) is.character(x) | is.factor(x))
  result <- rep(NA, NCOL(df))
  result[!chcol] <- collapse::fmin(df[, !chcol], na.rm = na.rm)
  result[chcol] <- suppressWarnings(
    collapse::fmin(apply(df[, chcol], 2, function(x) {
      # try to understand numbers that were stored as characters or factors
      as.numeric(as.character(x))
    }), na.rm = na.rm)
  )
  names(result) <- colnames(df)
  result <- unlist(result)
  return(result)
  
  ############################################### #   
  # Example: 
  # if (1 == 0) {
  # 
  df <- rbind(NA, data.frame(
    n1 = c(0, 0:8), n2 = c(0.1 + (0:9)), n3 = c(1:10), 
    allnas = c(rep(NA, 10)),
    logic = TRUE, 
    factortxt = factor('factor'), 
    txt = 'words',
    numberlike = as.character(6:15),
    numberlikefact = factor(as.character(6:15)),
    stringsAsFactors = FALSE))
  
  df
  
  colMins2(df)
  
  # not matrixStats package has a function called colMins and 
  
  # ** Note than max() and min() default to na.rm = FALSE, but this function defaults to na.rm = TRUE because that just seems more frequently useful.
  # Also,
  #  min() and max() would not handle text or factors etc. very well, even if you tried to use as.numeric()
  # apply(df, 2, function(x) min(as.numeric(x),na.rm = T))
  # min() and max() can be confusing -- e.g., note that 
  # min(c(8,10,'txt')) # returns '10' not '8' and max returns 'txt'
  # suppressWarnings(min(as.numeric(c(8,10,'txt', NA, "3", "20")) , na.rm = TRUE))  # gives a more useful answer
  # suppressWarnings(max(as.numeric(c(8,10,'txt', NA, "3", "20")) , na.rm = TRUE))  # gives a more useful answer
  
  # }
}
########################################################################### #


#' change.fieldnames
#' one way to convert variable names (colnames) to friendlier versions
#' @param allnames a vector of all the original fieldnames,
#' @param oldnames a vector of just the fieldnames to be changed, and
#' @param newnames a vector of what those should be change to
#' @param file path to a csv file with two columns: oldnames, newnames (instead of passing them to the function as parameters)
#' @param sort a logical (default is FALSE). If FALSE, return new fieldnames. If sort=TRUE, return vector of indexes giving new position of given field, based on sort order of oldnames.
#'
#' @return new versions of names as vector
#' 
#' @keywords internal
#' 
change.fieldnames <- function(
    allnames,
    oldnames,
    newnames,
    file = NA,
    sort = FALSE) {
  # THIS IS ALSO IN MY PACKAGE CALLED analyze.stuff
  # FUNCTION TO CHANGE SOME OR ALL FIELDNAMES
  
  # Input parameters are:
  # allnames   a vector of all the original fieldnames,
  # oldnames   a vector of just the fieldnames to be changed, and
  # newnames   a vector of what those should be change to
  # file       a path\filename csv file with two columns: oldnames, newnames (instead of passing them to the function as parameters)
  # sort       a logical (default is FALSE). If FALSE, return new fieldnames. If sort=TRUE, return vector of indexes giving new position of given field, based on sort order of oldnames.
  
  # Value it returns:  vector of character strings, the full set of fieldnames, with some or all updated if sort=FALSE (default).
  # If sort=TRUE, return vector of indexes giving new position of given field, based on sort order of oldnames.
  # If sort=TRUE, names in oldnames that are not in allnames are ignored with warning, & names in allnames that are left out of oldnames left out of new sort order indexes.
  
  if (!missing(file)) {
    if (!missing(oldnames) |
        !missing(newnames)) {
      stop('Cannot specify file and also oldnames or newnames')
    }
    changes <- readr::read_csv(file)
    if (!(('oldnames' %in% names(changes)) &
          ('newnames' %in% names(changes)))) {
      if (sort == FALSE) {
        stop('file must have columns named oldnames and newnames')
      }
    }
    
    if (sort == FALSE) {
      oldnames <- changes$oldnames
      newnames <- changes$newnames
    } else {
      # if just using file for sort of colnames, assume the first column of csv is the new sort order even if lacks colname
      warning('assuming first column in file contains fieldnames in the new sort order')
      oldnames <- changes[, 1]
    }
    if (missing(allnames)) {
      allnames <- oldnames
    }
    # ASSUME THAT IF ONLY file IS SPECIFIED, THE FIRST COLUMN HAS ALL THE NAMES, NOT JUST ONES TO CHANGE, BUT THEN FUNCTION SIMPLY RETURNS newnames FROM file
  }
  
  if (is.na(file) &
      missing(allnames)) {
    stop('Must specify allnames if file not specified')
  }  # specifying only file='blah.csv'  works
  if (!is.vector(allnames) |
      any(!is.character(allnames))) {
    stop('allnames must be a vector of character type fieldnames')
  }
  
  if (!sort) {
    if (is.na(file) & sum(missing(oldnames), missing(newnames)) == 1) {
      stop(
        'Must specify oldnames, newnames in 2-column csv file or as parameters, or specify none of the 3 for interactive editing of names (unless sort=TRUE)'
      )
    }
  }
  
  if (is.na(file) & missing(oldnames) & missing(newnames)) {
    changes <-
      edit(data.frame(
        oldnames = allnames,
        newnames = allnames,
        stringsAsFactors = FALSE
      ))
    write.csv(changes, file = 'saved fieldnames.csv', row.names = FALSE)
    cat('\n old and new names saved in csv file called:   "saved fieldnames.csv"\n')
    oldnames <- changes[, 1]
    newnames <- changes[, 2]
  }
  
  if (!is.vector(oldnames) |
      any(!is.character(oldnames))) {
    stop('oldnames must be a vector of character type fieldnames\n')
  }
  if (!sort) {
    if (!is.vector(newnames) |
        any(!is.character(newnames))) {
      stop('newnames must be a vector of character type fieldnames\n')
    }
  }
  if (!sort) {
    if (length(oldnames) != length(newnames)) {
      stop('oldnames and newnames must be the same length\n')
    }
  }
  if (!sort) {
    if (length(allnames) == 0 |
        length(oldnames) == 0 |
        length(newnames) == 0) {
      stop('no input can be length zero\n')
    }
  }
  
  # if (length(allnames) < length(oldnames)) {cat('Warning: length(allnames) generally should be >= length(oldnames)\n')}
  # This warning appears too often and does not really indicate a problem anyway
  # Done with error-checking or file-creation/editing.
  ############### #
  
  # Just replace the ones that match up, so
  #   if allnames has something not in the oldnames, newnames entries, that is just left unchanged in allnames.
  #   if oldnames has something that is not in allnames, that is ignored.
  
  if (!sort) {
    # return vector of new fieldnames in same order as they were passed to this function
    newnames <- newnames[oldnames %in% allnames]
    oldnames <- oldnames[oldnames %in% allnames]
    allnames[match(oldnames, allnames)] <- newnames
    return(allnames)
  } else {
    # return vector of new positions for the columns whose names were passed to this function, using sort order found in oldnames
    oldnames <- oldnames[oldnames %in% allnames]
    newposition <- match(oldnames, allnames)
    return(newposition)
  }
}
########################################################################### #


#' @title Clean raw output of doaggregate() from the EJAM package
#' @description Takes the raw output version of batch buffer results and cleans it up
#'   to make it ready for batch.summarize function
#'   Note this drops rows with no pop data - assumes those lack EJScreen batch results 
#' @param x Required. output of batch processor that runs EJScreen report once per site.
#' @param namesfile Optional but must specify either namesfile, or both oldcolnames and newcolnames. 
#'   A csv filename, of file that maps fieldnames from those in raw output of batch processor
#'   to more useful and friendly names that make more sense. 
#'   If function is called with the special value namesfile='keepnames' then 
#'   the names are unchanged from those in x.
#' @param oldcolnames Optional. The names to be found in x, ignored if namesfile specified.
#' @param newcolnames Optional. The corresponding names to change them to, 
#'   ignored if namesfile specified.
#'
#' @keywords internal
#' 
batch.clean <- function(x, namesfile = "keepnames", oldcolnames, newcolnames) {
  
  if (missing(namesfile) & 1 == sum(missing(oldcolnames) + missing(newcolnames) )) {
    stop('must specify either namesfile, or both oldcolnames and newcolnames')
  }
  
  x[x == 'NA'] <- NA  # it also fixes 'N/A' later 
  
  if (colnames(x)[1] == '') {colnames(x)[1] <- "hadnocolnameinheader"} # blah
  
  # FIRST, FIX THE ISSUE WHERE ARCGIS IS EXPORTING TO TXT FILE WITH 
  # COLUMN NAMES THAT HAVE AN UNDERSCORE AS THE FIRST CHARACTER
  # and R reads those and adds X before the underscore!!
  colnames(x) <- gsub(pattern = '^X_', replacement = '', x = colnames(x))
  # oldcolnames <- gsub(pattern = '^_', replacement = '', x = oldcolnames)
  
  # Problems? If rownames in file, no colname for 1st col so gets called X
  # FACID appearing twice in file, so 2d col named that gets called FACID.1
  x <- x[ , names(x) != "FACID.1"]
  x <- x[ , names(x) != "X"]
  
  if (missing(namesfile) & !missing(oldcolnames) & !missing(newcolnames) ) {
    # RENAME FIELDS TO FRIENDLIER NEW NAMES
    names(x) <- change.fieldnames(names(x), oldnames = oldcolnames, newnames = newcolnames)
  }
  
  if (!missing(namesfile)) {
    if (!missing(oldcolnames) | !missing(newcolnames)) {warning('ignoring oldcolnames and newcolnames because namesfile was specified')}
    if (namesfile == 'keepnames') {
      # that is how user can specify they want no changes made to the names
    } else {
      # RENAME FIELDS TO FRIENDLIER NEW NAMES
      names(x) <- change.fieldnames(names(x), file = namesfile)
    }
  }
  
  if (missing(namesfile) & missing(oldcolnames) & missing(newcolnames) ) {
    # use default fieldname changes if nothing is specified for column names
    # RENAME FIELDS TO FRIENDLIER NEW NAMES
    names(x) <- change.fieldnames(names(x), file = namesfile)
  }
  
  # try to convert fields from character to text by removing 
  # percent sign, comma, miles, and treat N/A as NA:
  makenum <- function(x) {as.data.frame( lapply(x, function(y) as.numeric(
    gsub('th', '', 
         gsub('<', '', 
              gsub(' miles', '', 
                   gsub('N/A','', 
                        gsub('%','', 
                             gsub(',','',y)) )))))),
    stringsAsFactors = FALSE)}
  
  charcol <- names(x) %in% c('OBJECTID', 'FACID', 'name', 'ST', 'statename', 'id', 'ID')
  x[ , !charcol] <- makenum(x[ , !charcol])
  if (!('OBJECTID' %in% names(x))) {x$OBJECTID <- 1:NROW(x)} # required by map and probably elsewhere
  
  # Remove rows where no results from ArcGIS since lat/lon in Guam, American Samoa, Mariana Islands, VI, etc. where EJScreen does not work.
  # It won't include those in any stats and also cannot map them since removed here, even though they had lat/lon data uploaded! 
  if (any(is.na(x$pop))) warning('Dropping the points where data were not available at all, such as in Guam, American Samoa, Mariana Islands, VI')
  if (all(is.na(x$pop))) warning('None of these points have batch buffer output results.')
  x <- x[!is.na(x$pop), ]  # it retains if pop = 0, but not if it is missing entirely, which shows up here as NA once cleaned above
  
  return(x)
}
########################################################################### #
