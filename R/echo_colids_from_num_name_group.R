
#' Interpret and check the list of requested columns to be asked of the ECHO API get_facility_info
#'
#' @param x vector of specifiers of variables needed to get from the ECHO facility query API,
#'   which can be the variable column id that the API needs (1 through about 316),
#'   or the actual ObjectName (variable name) found in varsinfo_ECHO_API, 
#'   or a word spefying a group of variables like critical or best 
#'   or others that are logical class columns in varsinfo_ECHO_API
#'   or the word all (for all available, about 316).
#'
#' @return Not the same sort order or length as input necessarily! A valid list of numbers that are the ColumnID numbers
#' @seealso [get_facility_info_via_ECHO()]
#'
#' @examples  
#'   x <- echo_colids_from_num_name_group(c('critical', 'NC', 'CensusBlockGroup' ))
#'   x
#'   varsinfo_ECHO_API$ObjectName[match( x, varsinfo_ECHO_API$ColumnID)]
#'   echo_colids_from_num_name_group(300:400)
#'   echo_colids_from_num_name_group(5:1)
#'   echo_colids_from_num_name_group(c(5:1,1:3))
#'
#' @keywords internal
#'    
echo_colids_from_num_name_group <- function(x=NULL) {
  
  qcolumns <- x
  if (is.null(qcolumns)) {
    qcolumns <- ''
  } else {
    if ('all' %in% qcolumns) {
      qcolumns <- varsinfo_ECHO_API$ColumnID
    } else {
      validnames <- varsinfo_ECHO_API$ObjectName
      validgroups <- names(varsinfo_ECHO_API)[sapply(varsinfo_ECHO_API, class) == 'logical']
      validnums <- 1:NROW(varsinfo_ECHO_API)
      valid_anything <- unique(c(validnames, validgroups, validnums, 'all'))
      
      if (any(!(qcolumns %in% valid_anything))) {warning('some invalid column specifiers were provided and ignored')}
      
      q_nums <- as.numeric(qcolumns)[!is.na(as.numeric(qcolumns))]
      q_nums <- q_nums[q_nums %in% validnums]
      
      q_groups <- qcolumns[qcolumns %in% validgroups]
      q_nums_by_group <- ''
      if (length(q_groups) == 1) {
        q_nums_by_group <- varsinfo_ECHO_API$ColumnID[ (varsinfo_ECHO_API[ , q_groups]) > 0] # USE THE variable that meets at least one of the criteria provided, like "critical"
      }
      if (length(q_groups) > 1) {
        q_nums_by_group <- varsinfo_ECHO_API$ColumnID[rowSums(varsinfo_ECHO_API[ , q_groups]) > 0] # USE THE variable that meets at least one of the criteria provided, like "critical"
      }
      
      q_nums_by_name <- varsinfo_ECHO_API$ColumnID[match( qcolumns, varsinfo_ECHO_API$ObjectName)]
      q_nums_by_name <- q_nums_by_name[!is.na(q_nums_by_name)]
      
      qcolumns <- unique(c(q_nums_by_group, q_nums, q_nums_by_name)) # DOES NOT RETAIN original or any USEFUL SEQUENCE !
         qcolumns <- qcolumns[qcolumns != '']
    }
    return(qcolumns)     
  }
}
