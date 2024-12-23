#' Reformat frs datatable to look up facilities by PGM_SYS_ACRNMS
#' @param x data.table frs from [frs_get()] 
#' @details More information including definitions of the programs (full names) can be found here: 
#' - https://www.epa.gov/frs/frs-data-sources
#' - [2021-05/frs_program_abbreviations_and_names.xlsx](https://www.epa.gov/sites/default/files/2021-05/frs_program_abbreviations_and_names.xlsx)
#' 
#' @import data.table
#' @return data.table with columns lat, lon, REGISTRY_ID, program, pgm_sys_id
#' @seealso [frs_update_datasets()]  
#'
frs_make_programid_lookup <- function(x) {
  
  # create the longer format lookup table for just matching on 
  # PGM_SYS_ACRNMS to get lat lon registry_id
  
  # input is frs, the output of frs_get, a data.table
  # 
  # Classes ‘data.table’ and 'data.frame':
  lat <- lon <- REGISTRY_ID <- PGM_SYS_ACRNMS <- pgm_sys_id <- program <- NULL # get rid of warnings for line below
  x <- x[ , .(lat, lon, REGISTRY_ID, PGM_SYS_ACRNMS)] # PRIMARY_NAME, REGISTRY_ID
  
  x <- tidyr::separate_rows(x, PGM_SYS_ACRNMS, sep = ',') # SLOW
  x$PGM_SYS_ACRNMS <- trimws(x$PGM_SYS_ACRNMS)
  
  x <- tidyr::separate(x, PGM_SYS_ACRNMS, sep = ':', into = c('program', 'pgm_sys_id'), remove = FALSE)
  # Warning message:
  # Expected 2 pieces. Missing pieces filled with `NA` in 1190 rows
  # [32787, 32789, 32822, 32823, 32853, 32855, 32888, 32975, 32997, 32999, 
  # 33012, 33109, 33135, 33162, 33272, 33326, 33327, 33329, 33348, 33349, ...]. 
  x  <- data.table::as.data.table(x)
  x <- x[ !is.na(pgm_sys_id), ] # dropped 1190 where is.na(pgm_sys_id) and program == "" 
  
  setkey(x, pgm_sys_id, program, REGISTRY_ID)
  attr(x, 'released') <- Sys.Date()
  # print('To use in package,  usethis::use_data(frs_by_programid, overwrite=TRUE)  ')
  
  invisible(x)
  return(x) # 
  # SFDW           46205
  # ACES           66031
  # NCDB           71290
  # OSHA-OIS       86069
  # AIRS/AFS      102698
  # TX-TCEQ ACR   108674
  # EIS           121677
  # FIS           122613
  # MN-TEMPO      127962
  # AIR           134121
  # ICIS          154646
  # CA-ENVIROVIEW 201571
  # NJ-NJEMS      244707
  # NPDES         384622
  # RCRAINFO      519339
  # 
  # sort(table(x$program))
  # 
  # NNEMS              TEST               ISD         SRPMICEMS             SWIPR          MERI-FIS              BRAC 
  # 3                 4                 6                12                25                49                50 
  # RADINFO             CNFRS            CASWIS BIA INDIAN SCHOOL          FFDOCKET               RFS              ECRM 
  # 53                84                85                93               109               115               142 
  # FFEP            REGION              FARR            CDAFLP             STATE               UST            CAMDBS 
  # 190               291               305               620               646               668               758 
  # LMOP          LA-TEMPO                               IDDEQ              RBLC   DTSC-ENVIROSTOR               EPS 
  # 856              1090              1229              1299              1591              1652              1720 
  # NH-DES          MD-TEMPO           ME-EFIS              UORS           MD-EPSC              CEDS             NV-FP 
  # 1798              2524              2753              2897              2901              3549              4362 
  # PERMIT TRACKING         LUST-ARRA            E-GGRT           MD-RCRA           OTAQREG                BR            HI-EHW 
  # 4808              5684              6428              6721              6785              6832              7251 
  # GEIMS               CIM          AIRS/AQS         RI-PLOVER          MD-PEMIS           EIA-860         MT-CEDARS 
  # 7452              7830              8321              8402              8646              8910              8983 
  # WDEQ               DEN          NM-TEMPO             CEDRI             EGRID              SEMS     HWTS-DATAMART 
  # 9002              9078              9243             10163             10680             10973             12659 
  # TSCA             ND-FP              SSTS               PDS             KS-FP            IN-FRS            WI-ESR 
  # 12758             13731             14824             20659             21770             21820             24435 
  # KY-TEMPO           SC-EFIS              NDEQ              SIMS           NC-FITS         MS-ENSITE            MO-DNR 
  # 24469             24574             25649             26094             26263             26706             27788 
  # PA-EFACTS           CA-CERS              TRIS          MA-EPICS           OH-CORE               FDM           AZURITE 
  # 30966             31792             32565             33995             34623             34774             35434 
  # WA-FSIS             ACRES          IN-TEMPO            OR-DEQ          IDNR_EFD              SFDW              ACES 
  # 35447             35529             40144             42411             46038             46205             66031 
  # NCDB          OSHA-OIS          AIRS/AFS       TX-TCEQ ACR               EIS               FIS          MN-TEMPO 
  # 71290             86069            102698            108674            121677            122613            127962 
  # AIR              ICIS     CA-ENVIROVIEW          NJ-NJEMS             NPDES          RCRAINFO 
  # 134121            154646            201571            244707            384622            519339 
  # 
}
