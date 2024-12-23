
# Note: compare frsprogramcodes, epa_programs, epa_programs_defined, etc.

# This must be redone/ rebuilt whenever frs and frs_by_programid  are updated to get counts updated !! ***
# This data object `epa_programs` is used in ui to offer a list of programs with a count of sites for each

# 1st must update frs_by_programid, via script in EJAM/datacreate_frs_.R

# get the frs_by_programid dataset (which should have been just updated via script in EJAM/datacreate_frs_.R ! )

if (!exists("frs_by_programid")) dataload_from_pins("frs_by_programid")

######################################################################################################## #

# COUNT FACILITIES PER PROGRAM, using frs_by_programid

# get counts by program type, like AIRS/AQS program has more than 8,000 facilities in FRS dataset, etc.

# epa_programs_counts <- dplyr::count(frs_by_programid, program, name = 'count')  # using dplyr
epa_programs_counts <- frs_by_programid[, .(count = .N), by = "program"]        # using data.table

###   and could be by increasing count, not alphabetical
epa_programs_counts <- epa_programs_counts[order(-count), ]

######################################################### ########################################################## #

#  GET THE PROGRAM NAME AND FULL DEFINITION

setDT(epa_programs_counts,  key = "program")        # was already a data.table
setDT(epa_programs_defined, key = "PGM_SYS_ACRNM")  # normally has been a data.frame 

epa_programs_counts <- epa_programs_defined[epa_programs_counts, .(program, count, PGM_SYS_NAME, PGM_SYS_DESC)]
setDF(epa_programs_defined)

# epa_programs_counts$PGM_SYS_NAME <- epa_programs_defined$PGM_SYS_NAME[match(epa_programs_counts$program, epa_programs_defined$PGM_SYS_ACRNM)]

# epa_programs_counts[!(epa_programs_counts$program %in% frsprogramcodes$code  ), 1:3]

######################################################################################################## #

#  WHICH PROGRAMS TO KEEP FOR THE PULLDOWN LIST? 

######################################################### #
## AT LEAST DROP THE CLEARLY STATE-SPECIFIC ONES/ PARTNERS  
#
epa_programs_counts$fed = !grepl("^[A-Z]{2}[-]", as.vector(epa_programs_counts$program), 1, 3)

# epa_programs_counts[fed == TRUE, .(program,count,PGM_SYS_NAME,fed)] #   PGM_SYS_DESC  is a very long text field
# epa_programs_counts[fed != TRUE, .(program,count,PGM_SYS_NAME,fed)] #   PGM_SYS_DESC  is a very long text field

# table((epa_programs_counts$program %in% epa_programs_defined$PGM_SYS_ACRNM), epa_programs_counts$fed)
## 3 were internal federal and another 63 were public federal and 32 appeared to be state programs.
# > epa_programs_defined[epa_programs_defined$PGM_SYS_ACRNM %in% epa_programs_counts$program[epa_programs_counts$fed == FALSE], 1:2]
# PGM_SYS_ACRNM                                                                       PGM_SYS_NAME
# 11        CA-CERS                                       CA-CALIFORNIA ENVIRONMENTAL REPORTING SYSTEM
# 12  CA-ENVIROVIEW                                                            CALIFORNIA - ENVIROVIEW
# 39         HI-EHW                                              HAWAII ENVIRONMENTAL HEALTH WAREHOUSE
# 44         IN-FRS                                                 INDIANA - FACILITY REGISTRY SYSTEM
# 45       IN-TEMPO            INDIANA-TOOLS FOR ENVIRONMENTAL MANAGEMENT AND PROTECTION ORGANIZATIONS
# 48          KS-FP                                                          KANSAS - FACILITY PROFILE
# 49       KY-TEMPO         KENTUCKY - TOOLS FOR ENVIRONMENTAL MANAGEMENT AND PROTECTION ORGANIZATIONS
# 51       LA-TEMPO        LOUISIANA - TOOLS FOR ENVIRONMENTAL MANAGEMENT AND PROTECTION ORGANIZATIONS
# 54       MA-EPICS                MASSACHUSETTS - ENVIRONMENTAL PROTECTION INTEGRATED COMPUTER SYSTEM
# 55        MD-EPSC                                     MARYLAND - ENVIRONMENTAL PERMIT SERVICE CENTER
# 56       MD-PEMIS                                                MARYLAND - PERMANENT (AIR) EMISSION
# 57        MD-RCRA                           MARYLAND-RESOURCE CONVERSATION AND RECOVERY ACT DATABASE
# 58       MD-TEMPO MARYLAND - TOOLS FOR ENVIRONMENTAL MANAGEMENT AND PROTECTION ORGANIZATIONS (TEMPO)
# 59        ME-EFIS                                  MAINE - ENVIRONMENTAL FACILITY INFORMATION SYSTEM
# 61       MN-TEMPO  MINNESOTA - PERMITTING, COMPLIANCE, AND ENFORCEMENT INFORMATION MANAGEMENT SYSTEM
# 62         MO-DNR                                           MISSOURI DEPARTMENT OF NATURAL RESOURCES
# 63      MS-ENSITE      MISSISSIPPI - TOOLS FOR ENVIRONMENTAL MANAGEMENT AND PROTECTION ORGANIZATIONS
# 64      MT-CEDARS         MONTANA - CONSOLIDATED ENVIRONMENTAL DATA ACQUISITION AND RETRIEVAL SYSTEM
# 67        NC-FITS                       NORTH CAROLINA - FACILITY IDENTIFICATION TEMPLATE FOR STATES
# 69          ND-FP                                                    NORTH DAKOTA - FACILITY PROFILE
# 71         NH-DES                               NEW HAMPSHIRE - DEPARTMENT OF ENVIRONMENTAL SERVICES
# 72       NJ-NJEMS                            NEW JERSEY - NEW JERSEY ENVIRONMENTAL MANAGEMENT SYSTEM
# 73       NM-TEMPO         NEW MEXICO-TOOLS FOR ENVIRONMENTAL MANAGEMENT AND PROTECTION ORGANIZATIONS
# 76          NV-FP                                                          NEVADA - FACILITY PROFILE
# 77        OH-CORE                                                   OHIO - CORE FACILITY INFORMATION
# 79         OR-DEQ                                       OREGON - DEPARTMENT OF ENVIRONMENTAL QUALITY
# 82      PA-EFACTS       PENNSYLVANIA - ENVIRONMENTAL FACILITY APPLICATION COMPLIANCE TRACKING SYSTEM
# 91      RI-PLOVER             RHODE ISLAND - PERMITS, LICENSES AND OTHER VITAL ENVIRONMENTAL RECORDS
# 93        SC-EFIS                         SOUTH CAROLINA - ENVIRONMENTAL FACILITY INFORMATION SYSTEM
# 104   TX-TCEQ ACR                TEXAS COMMISSION ON ENVIRONMENTAL QUALITY - AGENCY CENTRAL REGISTRY
# 107       WA-FSIS                                 WASHINGTON - FACILITY / SITE IDENTIFICATION SYSTEM
# 108        WI-ESR                                          WISCONSIN - ENVIRONMENTAL SYSTEM REGISTRY

# drop some others

othernonfedepa = c("ACES", "AZURITE", "BIA INDIAN SCHOOL",
                "CARB-TCH", "CASWIS", "CDAFLP", "CEDS", "CIM", "CNFRS", 
                "DEN", "DTSC-ENVIROSTOR",
                "FARR", "FDM", "FIS",
                "GEIMS", "HWTS-DATAMART",
                "IDDEQ", "IDNR_EFD", "ISD",
                "MERI-FIS", "NDEQ", "NNEMS", 
                "PDS", "PERMIT TRACKING", "REGION",
                "SIMS", "SRPMICEMS", "STATE", 
                "TEST", "UORS", "WDEQ")

epa_programs_counts$fed[epa_programs_counts$program %in% othernonfedepa] <- FALSE

# what is fed?

epa_programs_counts[epa_programs_counts$fed,c(1:3,5)]

# what is fed but not on the very very short list (from ECHO search tool )

   epa_programs_counts[!(epa_programs_counts$program %in% frsprogramcodes$code  ) & fed, 1:3]
   # program  count                                                                            PGM_SYS_NAME
   # <char>  <int>                                                                                  <char>
   #   1:  AIRS/AFS 100111                                                                     AIR FACILITY SYSTEM
   # 2:  AIRS/AQS   8327                                                                      AIR QUALITY SYSTEM
   # 3:      BRAC     48                                                            BASE REALIGNMENT AND CLOSURE
   # 4:     CEDRI  14162                                       COMPLIANCE AND EMISSIONS DATA REPORTING INTERFACE
   # 5:      ECRM    140                                                 ENFORCEMENT CRIMINAL RECORDS MANAGEMENT
   # 6:     EGRID  10471                                     EMISSIONS & GENERATION RESOURCE INTEGRATED DATABASE
   # 7:   EIA-860   8776                                ENERGY INFORMATION ADMINISTRATION-860 (EIA-860) DATABASE
   # 8:       EPS   2111                                                                ELECTRONIC PERMIT SYSTEM
   # 9:  FFDOCKET    107                                      FEDERAL FACILITY HAZARDOUS WASTE COMPLIANCE DOCKET
   # 10:      FFEP    186                                            FEDERAL FACILITIES RECOVERY AND REUSE OFFICE
   # 11:      ICIS 160386                                                INTEGRATED COMPLIANCE INFORMATION SYSTEM
   # 12:      LMOP    821                                                       LANDFILL METHANE OUTREACH PROGRAM
   # 13: LUST-ARRA   5683 LEAKING UNDERGROUND STORAGE TANK (LUST) ¿ AMERICAN RECOVERY AND REINVESTMENT ACT (ARRA)
   # 14:      NCDB  70688                                                            NATIONAL COMPLIANCE DATABASE
   # 15:       OIL     41                                                                                    <NA>
   #   16:  OSHA-OIS  84643                        OCCUPATIONAL SAFETY AND HEALTH ADMINISTRATION INFORMATION SYSTEM
   # 17:   OTAQREG   7280                             OFFICE OF TRANSPORTATION AND AIR QUALITY FUELS REGISTRATION
   # 18:   RADINFO     51                                                            RADIATION INFORMATION SYSTEM
   # 19:      RBLC   1523                                                            RACT/BACT/LAER CLEARINGHOUSE
   # 20:       RFS     91                                                                 RENEWABLE FUEL STANDARD
   # 21:      SSTS  15945                                                           SECTION SEVEN TRACKING SYSTEM
   # 22:     SWIPR     26                                            SUBPART W IMPOUNDMENT PHOTOGRAPHIC REPORTING
   # 23:       UST    658                                                                UNDERGROUND STORAGE TANK
   # program  count                                                                            PGM_SYS_NAME                
 
# maybe keep some of those?

maybe_keep_not_in_frsprogramcodes_echo <- c(
#   # "ICIS", # 160K SITES
#   # "OSHA-OIS",
    "EGRID",
#   "LUST-ARRA",
#   "UST"
)
epa_programs_counts$fed[epa_programs_counts$program %in% maybe_keep_not_in_frsprogramcodes_echo] <- TRUE


######################################################### #

# DROP NON-EPA/ NON-FED ONES

epa_programs_counts <- epa_programs_counts[fed == TRUE, ]
epa_programs_counts$fed <- NULL
                    
# DROP INTERNAL-ONLY ONES

epa_programs_counts <- epa_programs_counts[ (epa_programs_counts$program %in% epa_programs_defined$PGM_SYS_ACRNM), ] # removes 3 internal-only ones

epa_programs_counts[,1:3]
# 34 now
# <char>  <int>                                                                                  <char>
#   1:     ACRES  35582                                   ASSESSMENT, CLEANUP AND REDEVELOPMENT EXCHANGE SYSTEM
# 2:       AIR 134295                                                                          ICIS-AIR (AIR)
# 3:  AIRS/AFS 100111                                                                     AIR FACILITY SYSTEM
# 4:  AIRS/AQS   8327                                                                      AIR QUALITY SYSTEM
# 5:        BR   5672                                                                      BIENNIAL REPORTERS
# 6:      BRAC     48                                                            BASE REALIGNMENT AND CLOSURE
# 7:    CAMDBS    726                                      CLEAN AIR MARKETS DIVISION (CAMD) BUSINESS SYSTEMS
# 8:     CEDRI  14162                                       COMPLIANCE AND EMISSIONS DATA REPORTING INTERFACE
# 9:    E-GGRT   6084                                       ELECTRONIC GREENHOUSE GAS REPORTING TOOL (E-GGRT)
# 10:      ECRM    140                                                 ENFORCEMENT CRIMINAL RECORDS MANAGEMENT
# 11:     EGRID  10471                                     EMISSIONS & GENERATION RESOURCE INTEGRATED DATABASE
# 12:   EIA-860   8776                                ENERGY INFORMATION ADMINISTRATION-860 (EIA-860) DATABASE
# 13:       EIS 119772                                                         EMISSION INVENTORY SYSTEM (EIS)
# 14:       EPS   2111                                                                ELECTRONIC PERMIT SYSTEM
# 15:  FFDOCKET    107                                      FEDERAL FACILITY HAZARDOUS WASTE COMPLIANCE DOCKET
# 16:      FFEP    186                                            FEDERAL FACILITIES RECOVERY AND REUSE OFFICE
# 17:      ICIS 160386                                                INTEGRATED COMPLIANCE INFORMATION SYSTEM
# 18:      LMOP    821                                                       LANDFILL METHANE OUTREACH PROGRAM
# 19: LUST-ARRA   5683 LEAKING UNDERGROUND STORAGE TANK (LUST) ¿ AMERICAN RECOVERY AND REINVESTMENT ACT (ARRA)
# 20:      NCDB  70688                                                            NATIONAL COMPLIANCE DATABASE
# 21:     NPDES 409502                            NATIONAL POLLUTANT DISCHARGE ELIMINATION SYSTEM (ICIS-NPDES)
# 22:  OSHA-OIS  84643                        OCCUPATIONAL SAFETY AND HEALTH ADMINISTRATION INFORMATION SYSTEM
# 23:   OTAQREG   7280                             OFFICE OF TRANSPORTATION AND AIR QUALITY FUELS REGISTRATION
# 24:   RADINFO     51                                                            RADIATION INFORMATION SYSTEM
# 25:      RBLC   1523                                                            RACT/BACT/LAER CLEARINGHOUSE
# 26:  RCRAINFO 525179                               RESOURCE CONSERVATION AND RECOVERY ACT INFORMATION SYSTEM
# 27:       RFS     91                                                                 RENEWABLE FUEL STANDARD
# 28:      SEMS  11217                                                  SUPERFUND ENTERPRISE MANAGEMENT SYSTEM
# 29:      SFDW  40887                                                  SAFE DRINKING WATER INFORMATION SYSTEM
# 30:      SSTS  15945                                                           SECTION SEVEN TRACKING SYSTEM
# 31:     SWIPR     26                                            SUBPART W IMPOUNDMENT PHOTOGRAPHIC REPORTING
# 32:      TRIS  34147                                                         TOXICS RELEASE INVENTORY SYSTEM
# 33:      TSCA  13581                                                            TOXIC SUBSTANCES CONTROL ACT
# 34:       UST    658                                                                UNDERGROUND STORAGE TANK
# program  count                                                                            PGM_SYS_NAME
######################################################### ########################################################## #

#  OR JUST USE A SHORT LIST TO SIMPLIFY PUBLIC INTERFACE
#
# just use a very short list taken from ECHO's website search tool:

#   > frsprogramcodes
# description     code
# 1                     National Pollutant Discharge Elimination System (ICIS-NPDES)    NPDES
# 2                      Integrated Compliance Information System for Air (ICIS-Air)      AIR
# 3                 Resource Conservation and Recovery Act (RCRA) Information System RCRAINFO
# 4                                            Risk Management Plan (RMP) facilities      RMP
# 5                               The Safe Drinking Water Information System (SDWIS)     SFDW
# 6                                       The Superfund Enterprise Management System     SEMS
# 7                                       Clean Air Markets Division Business System   CAMDBS
# 8                                           Toxics Release Inventory Program (TRI)     TRIS
# 9                                                 Greenhouse Gas Reporting Program   E-GGRT
# 10                                                Emissions Inventory System (EIS)      EIS
# 11                                             Toxic Substances Control Act (TSCA)     TSCA
# 12   Assessment, Cleanup and Redevelopment Exchange System (ACRES) for Brownfields    ACRES
# 13 Biennial Reporters (BR) Hazardous Waste Treatment, Storage, Disposal Facilities       BR

setdiff(frsprogramcodes$code, epa_programs_counts$program)
# [1] "RMP"  # we lack RMP site info from the frs_by_program 

setdiff_yx(frsprogramcodes$code, epa_programs_counts$program)
# [1] "AIRS/AFS"  "AIRS/AQS"  "BRAC"      "CEDRI"     "ECRM"      "EGRID"     "EIA-860"   "EPS"       "FFDOCKET"  "FFEP"     
# [11] "ICIS"      "LMOP"      "LUST-ARRA" "NCDB"      "OSHA-OIS"  "OTAQREG"   "RADINFO"   "RBLC"      "RFS"       "SSTS"     
# [21] "SWIPR"     "UST" 

epa_programs_counts[epa_programs_counts$program %in% setdiff( epa_programs_counts$program, frsprogramcodes$code), 1:3]

#      program  count                                                                            PGM_SYS_NAME
#       <char>  <int>                                                                                  <char>
# 1:  AIRS/AFS 100,111                                                                     AIR FACILITY SYSTEM  - too big/general
# 2:  AIRS/AQS   8,327                                                                      AIR QUALITY SYSTEM
# 3:      BRAC     48                                                            BASE REALIGNMENT AND CLOSURE
# 4:     CEDRI  14,162                                       COMPLIANCE AND EMISSIONS DATA REPORTING INTERFACE
# 5:      ECRM    140                                                 ENFORCEMENT CRIMINAL RECORDS MANAGEMENT
# 6:     EGRID  10,471                                     EMISSIONS & GENERATION RESOURCE INTEGRATED DATABASE  -- keep this ***
# 7:   EIA-860   8,776                                ENERGY INFORMATION ADMINISTRATION-860 (EIA-860) DATABASE
# 8:       EPS   2,111                                                                ELECTRONIC PERMIT SYSTEM
# 9:  FFDOCKET    107                                      FEDERAL FACILITY HAZARDOUS WASTE COMPLIANCE DOCKET
# 10:      FFEP    186                                            FEDERAL FACILITIES RECOVERY AND REUSE OFFICE
# 11:      ICIS 160,386                                                INTEGRATED COMPLIANCE INFORMATION SYSTEM  - too big/general
# 12:      LMOP    821                                                       LANDFILL METHANE OUTREACH PROGRAM
# 13: LUST-ARRA   5,683 LEAKING UNDERGROUND STORAGE TANK (LUST) ¿ AMERICAN RECOVERY AND REINVESTMENT ACT (ARRA) - not sure it is clearly defined/useful
# 14:      NCDB  70,688                                                            NATIONAL COMPLIANCE DATABASE  - too big/general
# 15:  OSHA-OIS  84,643                        OCCUPATIONAL SAFETY AND HEALTH ADMINISTRATION INFORMATION SYSTEM  - too big/general
# 16:   OTAQREG   7,280                             OFFICE OF TRANSPORTATION AND AIR QUALITY FUELS REGISTRATION
# 17:   RADINFO     51                                                            RADIATION INFORMATION SYSTEM
# 18:      RBLC   1,523                                                            RACT/BACT/LAER CLEARINGHOUSE - not sure it is clearly defined/useful
# 19:       RFS     91                                                                 RENEWABLE FUEL STANDARD
# 20:      SSTS  15,945                                                           SECTION SEVEN TRACKING SYSTEM
# 21:     SWIPR     26                                            SUBPART W IMPOUNDMENT PHOTOGRAPHIC REPORTING
# 22:       UST    658                                                                UNDERGROUND STORAGE TANK - not sure it is clearly defined/useful
# program  count                                                                            PGM_SYS_NAME

# > names(epa_programs_counts)
# [1] "program"      "count"        "PGM_SYS_NAME" "PGM_SYS_DESC"


epa_programs_counts <- epa_programs_counts[epa_programs_counts$program %in% c("EGRID", frsprogramcodes$code), ]

epa_programs_counts$shortname <- frsprogramcodes$description[match(epa_programs_counts$program, frsprogramcodes$code)]

epa_programs_counts$shortname[epa_programs_counts$program == "EGRID"] <- "Emissions & Generation Resource Integrated Database"
epa_programs_counts$shortname <- gsub("Clean Air Markets Division Business System", "Clean Air Markets Division (CAMD) Business System", epa_programs_counts$shortname)
epa_programs_counts$shortname <- gsub("Greenhouse Gas Reporting Program", "Greenhouse Gas Reporting Program (E-GGRT)",  epa_programs_counts$shortname)
epa_programs_counts$shortname <- gsub("The Superfund Enterprise Management System", "The Superfund Enterprise Management System (SEMS)", epa_programs_counts$shortname)
epa_programs_counts$shortname <- gsub("Information", "Info", epa_programs_counts$shortname)

data.table::setcolorder(epa_programs_counts, c('count', 'program', 'shortname', 'PGM_SYS_NAME', 'PGM_SYS_DESC'))
# now is a data.table

######################################################### #

#  OR JUST USE A VERY, VERY SHORT LIST TO LIMIT IT TO ONLY GROUPS THAT CAN BE FINISHED QUICKLY, <5K OR 10K FACILITIES EACH

# ejamit() analyzes about 6,000 sites per minute, 
# so analyzing more than 3,000 seems too slow for a public tool.
# Those could be cached or later optimized for speed.
# but ALL of the interesting groups are >5k sites each except CAMDBS.
# only 3 groups have < 10k each
# 36k are in ACRES, Brownfields. 
# 192 MB file. Takes 8 minutes total. 4 minutes to do getblocks alone. too long.
#  48 MB file if delete the bybg table which took 3/4 of the total space!
## offer those as cached results, or not at all.

veryshort = c(
  "BR", "CAMDBS", "E-GGRT", "SEMS", "TSCA", "TRIS", 
  "ACRES", "EGRID"
)
setdiff(epa_programs_counts$program, veryshort)

# "AIR"  "EIS"   "NPDES"  "SFDW"    "RCRAINFO"   # all are huge groups 

epa_programs_counts <- epa_programs_counts[order(epa_programs_counts$count), ]  

epa_programs_counts[epa_programs_counts$program %in% veryshort, 1:3]
epa_programs_counts[ , 1:3]

# JUST USE A VERY, VERY SHORT LIST 
  epa_programs_counts <- epa_programs_counts[epa_programs_counts$program %in% veryshort, ]
 

######################################################### #

# manually fix some long names?
#
#epa_programs_counts$PGM_SYS_NAME[epa_programs_counts$program == "LUST-ARRA"] <- "LEAKING UNDERGROUND STORAGE TANK (LUST-ARRA)"

# cbind(frsprogramcodes, epa_programs_counts$PGM_SYS_NAME[match(frsprogramcodes$code, epa_programs_counts$program)])
#                                                                        description     code epa_programs_counts$name[match(frsprogramcodes$code, epa_programs_counts$program)]
# 1                     National Pollutant Discharge Elimination System (ICIS-NPDES)    NPDES                       NATIONAL POLLUTANT DISCHARGE ELIMINATION SYSTEM (ICIS-NPDES)
# 2                      Integrated Compliance Information System for Air (ICIS-Air)      AIR                                                                     ICIS-AIR (AIR)
# 3                 Resource Conservation and Recovery Act (RCRA) Information System RCRAINFO                          RESOURCE CONSERVATION AND RECOVERY ACT INFORMATION SYSTEM
# 4                                            Risk Management Plan (RMP) facilities      RMP                                                                               <NA>
# 5                               The Safe Drinking Water Information System (SDWIS)     SFDW                                             SAFE DRINKING WATER INFORMATION SYSTEM
# 6                                       The Superfund Enterprise Management System     SEMS                                             SUPERFUND ENTERPRISE MANAGEMENT SYSTEM
# 7                                       Clean Air Markets Division Business System   CAMDBS                                 CLEAN AIR MARKETS DIVISION (CAMD) BUSINESS SYSTEMS
# 8                                           Toxics Release Inventory Program (TRI)     TRIS                                                    TOXICS RELEASE INVENTORY SYSTEM
# 9                                                 Greenhouse Gas Reporting Program   E-GGRT                                  ELECTRONIC GREENHOUSE GAS REPORTING TOOL (E-GGRT)
# 10                                                Emissions Inventory System (EIS)      EIS                                                    EMISSION INVENTORY SYSTEM (EIS)
# 11                                             Toxic Substances Control Act (TSCA)     TSCA                                                       TOXIC SUBSTANCES CONTROL ACT
# 12   Assessment, Cleanup and Redevelopment Exchange System (ACRES) for Brownfields    ACRES                              ASSESSMENT, CLEANUP AND REDEVELOPMENT EXCHANGE SYSTEM
# 13 Biennial Reporters (BR) Hazardous Waste Treatment, Storage, Disposal Facilities       BR                                                                 BIENNIAL REPORTERS

# cbind(nchar(frsprogramcodes$description), nchar(epa_programs_counts$PGM_SYS_NAME[match(frsprogramcodes$code, epa_programs_counts$program)]))
#  
# [1,]   60   60
# [2,]   59   14  
# [3,]   64   57
# [4,]   37   NA   RMP is not in epa_program_counts ?
# [5,]   50   38
# [6,]   42   38
# [7,]   42   50
# [8,]   38   31
# [9,]   32   49
# [10,]   32   31
# [11,]   35   28
# [12,]   77   53
# [13,]   79   18


############################################################# #

## MAKE TEXT TO SHOW IN DROPDOWN SELECTION LIST FOR APP

epa_programs_counts$pgm_text_dropdown <- epa_programs_counts$shortname
epa_programs_counts$pgm_text_dropdown  <- paste0(
  epa_programs_counts$program,
  " (", prettyNum(epa_programs_counts$count, big.mark = ','), " sites) ",
  "",
  epa_programs_counts$pgm_text_dropdown
  )

## old format:
# epa_programs_counts$pgm_text_dropdown <- paste0(epa_programs_counts$pgm_text_dropdown, ' (',prettyNum(epa_programs_counts$count, big.mark = ','), ')')

# create a named vector where $program is the vector elements and $pgm_text_dropdown is the names:
epa_programs <- setNames(epa_programs_counts$program, epa_programs_counts$pgm_text_dropdown)


# > cbind(epa_programs)
#                                                                                                    epa_programs
# ACRES (35,582 sites) Assessment, Cleanup and Redevelopment Exchange System (ACRES) for Brownfields "ACRES"     
# AIR (134,295 sites) Integrated Compliance Info System for Air (ICIS-Air)                           "AIR"       
# BR (5,672 sites) Biennial Reporters (BR) Hazardous Waste Treatment, Storage, Disposal Facilities   "BR"        
# CAMDBS (726 sites) Clean Air Markets Division (CAMD) Business System                               "CAMDBS"    
# E-GGRT (6,084 sites) Greenhouse Gas Reporting Program (E-GGRT)                                     "E-GGRT"    
# EIS (119,772 sites) Emissions Inventory System (EIS)                                               "EIS"       
# NPDES (409,502 sites) National Pollutant Discharge Elimination System (ICIS-NPDES)                 "NPDES"     
# RCRAINFO (525,179 sites) Resource Conservation and Recovery Act (RCRA) Info System                 "RCRAINFO"  
# SEMS (11,217 sites) The Superfund Enterprise Management System (SEMS)                              "SEMS"      
# SFDW (40,887 sites) The Safe Drinking Water Info System (SDWIS)                                    "SFDW"      
# TRIS (34,147 sites) Toxics Release Inventory Program (TRI)                                         "TRIS"      
# TSCA (13,581 sites) Toxic Substances Control Act (TSCA)                                            "TSCA"    

############################################################# #

# SORT BY COUNT FROM LOW TO HIGH, for pulldown

epa_programs <- epa_programs[order(epa_programs_counts$count)]  

############################################################# #

# Finally, save it in the EJAM/data/ folder for use as a dataset loaded with the EJAM package:

epa_programs <- metadata_add(epa_programs)
usethis::use_data(epa_programs, overwrite = TRUE)

cat("UPDATED DOCUMENTATION OF THIS DATA SET MANUALLY - HELP DOC IS A BIT COMPLICATED\n")
if (rstudioapi::isAvailable()) {
    rstudioapi::documentOpen('./R/data_epa_programs.R')
}

############################################################################################ #

# ***  note `epa_programs` is a complete list and has counts, while 
#  `frsprogramcodes` has the full titles but only about a dozen key programs and no counts.

#   frsprogramcodes
#                                                               description     code
# 1    National Pollutant Discharge Elimination System (NPDES) (ICIS-NPDES)    NPDES
# 2  The Integrated Compliance Information System (ICIS) for Air (ICIS-Air)      AIR
# 3    The Resource Conservation and Recovery Act (RCRA) Information System RCRAINFO
# 4                                   Risk Management Plan (RMP) facilities      RMP
# 5                      The Safe Drinking Water Information System (SDWIS)     SFDW
# 6                              The Superfund Enterprise Management System     SEMS
# 7                              Clean Air Markets Division Business System   CAMDBS
# 8                                        Toxics Release Inventory Program     TRIS
# 9                                        Greenhouse Gas Reporting Program   E-GGRT
# 10                                             Emissions Inventory System      EIS
# 11                                           Toxic Substances Control Act     TSCA
# 12   Assessment, Cleanup and Redevelopment Exchange System (ACRES) for Brownfields    ACRES
# 13 Biennial Reporters (BR) Hazardous Waste Treatment, Storage, Disposal Facilities       BR

# (frsprogramcodes.rda is tiny and not a table so cannot use .arrow for it and not in pins board
#  - see EJAM/data-raw/datacreate_frsprogramcodes.R )
############################################################################################ #
