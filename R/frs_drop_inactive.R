#' Remove inactive sites from downloaded FRS data.table
#' @param frs Required, data.table from frs_get()
#' @param closedid Required, vector of codes to treat as inactive, 
#'   obtained from [frs_inactive_ids()] which downloads national dataset and
#'   uses assumed codes and returns ids of the inactive sites.
#' @md
#' @details  
#'  For the late 2023 version, 
#'  
#'  - Complete list of unique ids is 4,775,797 out of 7,558,760 rows of data.
#'  - Count of all REGISTRY_ID rows:   7,558,760
#'  - Count of unique REGISTRY_ID values: 4,775,797
#'  - Clearly inactive unique IDs:     1,511,111
#'  - Assumed active unique IDs:       3,264,686
#'  
#'  **The definitions of active/inactive here are not quite the same as used in ECHO, as of late 2023.**
#'  
#'  See <https://echo.epa.gov/help/facility-search/search-criteria-help#facchar>
#'  
#' Codes assumed to mean site is closed: 
#' 
#'  - CLOSED 
#'  - PERMANENTLY CLOSED 
#'  - PERMANENTLY SHUTDOWN 
#'  - INACTIVE 
#'  - TERMINATED 
#'  - N 
#'  - RETIRED 
#'  - OUT OF SERVICE – WILL NOT BE RETURNED 
#'  - CANCELED, POSTPONED, OR NO LONGER PLANNED 
#' 
#' @seealso  [frs_update_datasets()] which uses [frs_get()] and [frs_inactive_ids()] [frs_active_ids()]
#' @return Returns the full frs data.table but without the inactive ids
#'
#' @examples  
#'   # frs <- frs_get()
#'   # closedid <- frs_inactive_ids()
#'   # frs <- frs_drop_inactive(frs, closedid = closedid)
#'   # usethis::use_data(frs, overwrite = TRUE)
frs_drop_inactive <- function(frs, closedid) {
  if (missing(closedid)) {
    stop('Needs closedid, list of IDs that are inactive. Try (but very slow):  closedid <- frs_inactive_ids() ')
  }
  frs[!(as.numeric(frs$REGISTRY_ID) %in% as.numeric(closedid)), ]
}
