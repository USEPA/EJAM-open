
#' Get lat lon, Registry ID, and NAICS, for given FRS Program System CATEGORY
#'  
#' Find all FRS sites in a program like RCRAINFO, TRIS, or others
#' 
#' @details  For info on FRS program codes in general, see <https://www.epa.gov/frs/frs-program-crosswalks>
#'   
#'   Also see information at <https://echo.epa.gov/tools/data-downloads/frs-download-summary>
#'    about the file FRS_PROGRAM_LINKS.csv  
#'    
#'   For info on program codes ECHO uses, see <https://echo.epa.gov/resources/echo-data/about-the-data>
#'   
#'   including <https://www.epa.gov/frs/frs-environmental-interest-types>
#'   
#'   For a list of program acronyms, <https://www.epa.gov/frs/frs-rest-services#appendixa>
#'   
#'   The acronym is the abbreviated name that represents the name of an 
#'   information management system for an environmental program. 
#'   The Federal ones with at least 100k facilities each are 
#'   
#'   RCRAINFO (over 500k sites), NPDES, ICIS, AIR, FIS, EIS, and AIRS/AFS.
#'   
#' @param query like "RMP", "RCRAINFO", "TRIS", "RMP", or others.
#' 
#' @return data.table with lat  lon  REGISTRY_ID  program -- but not pgm_sys_id 
#'   since there could be duplicates where same REGISTRY_ID has 2 different pgm_sys_id values 
#'   in the same program, so results were sometimes longer than if using [frs_from_program()] 
#' @examples \dontrun{
#'  x = latlon_from_program("CAMDBS")
#'   mapfast(x)
#'  program <- c("EIS", "UST")
#'  x = latlon_from_program(program)
#'  # to get the facility name as well:
#'  x = frs[grepl("RCRAINFO", PGM_SYS_ACRNMS), ] # fast
#'  ## x = latlon_from_regid(latlon_from_program(program)[,REGISTRY_ID])  # slower!
#'  mapfast(x[sample(1:nrow(x), 1000), ])
#' }
#'
#' @export
#'
latlon_from_program <- function(query) {
  
  if (missing(query)) {return(NULL)} else if (all(is.na(query)) | is.null(query)) {return(NULL)}
  
  if (!exists("frs_by_programid")) dataload_from_pins("frs_by_programid")
  if (!exists("frs")) dataload_from_pins("frs")
  
  # NOW USE THIS VERSION THAT MAKES IT IDENTICAL TO frs_from_program() :
  frs[REGISTRY_ID %in% frs_by_programid[program %in% query, REGISTRY_ID], ]
  
  # old way:
  # frs_by_programid[program %in% query, ] # order does not matter since more than one match likely per input code
  # THAT VERSION RETURNED DUPLICATED REGIDS WHEN SAME SITE HAS TWO OR MORE PROGRAM IDS FOR SAME PROGRAM
  
  # none seems much faster than the others:
  # system.time( frs_by_programid[program ==     "RCRAINFO", ]  ) 
  # system.time( frs_by_programid[program %chin% "RCRAINFO", ]  ) 
  # system.time( frs_by_programid[program  %in%  "RCRAINFO", ]  )
  # user  system elapsed 
  # 0.03    0.00    0.03 
  # user  system elapsed 
  # 0.03    0.00    0.03 
  # user  system elapsed 
  # 0.03    0.00    0.03 
  #
  # BUT
  # system.time(frs[grepl("RCRAINFO", PGM_SYS_ACRNMS), ] )
  # user  system elapsed 
  # 1.02    0.00    1.02 
  
  # > system.time( frs[REGISTRY_ID %chin% frs_by_programid[program %chin% "RCRAINFO",REGISTRY_ID ]  ,])
  # user  system elapsed 
  # 3.83    0.21    3.86 
  # > system.time( frs[grepl("RCRAINFO", PGM_SYS_ACRNMS), ]  )
  # user  system elapsed 
  # 1.03    0.00    1.03 
}

