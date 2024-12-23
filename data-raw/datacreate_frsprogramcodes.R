## code to prepare `frsprogramcodes` dataset goes here

# Note: compare frsprogramcodes, epa_programs, epa_programs_defined, etc.

## note that this overlaps with drafted epa_programs_defined that downloads a much longer list of acryonyms and their definitions.

# Key Media programs ECHO uses to limit queries, and code found in frs dataset for that program:
#   
# 1 NPDES (ICIS-NPDES) = NPDES
# 2 The Integrated Compliance Information System (ICIS) for Air (ICIS-Air)  =  AIR ? OR AIRS/AFS??
# 3 Resource Conservation and Recovery Act (RCRA) = RCRAINFO ? https://www.epa.gov/enviro/rcrainfo-overview https://enviro.epa.gov/envirofacts/rcrainfo/search 
# 4 Risk Management Plan (RMP) Rule = ???
# 5 Safe Drinking Water Act = SFDW  
# 6 Superfund Enterprise Management System = SEMS
# 7 Clean Air Markets Division Business System = CAMDBS
# 8 Toxics Release Inventory Program = TRIS
# 9 Greenhouse Gas Reporting Program = E-GGRT
# 10 Emissions Inventory System = EIS
# 11 Toxic Substances Control Act = TSCA

# Consider adding these:
# ? AIRS/AFS	Air Facility System	National	 101,085 	0	0	add to short list	air		The Air Facility System (Afs) Contains Compliance And Permit Data For Stationary Sources Of Air Pollution Regulated By The Epa, State, And Local Air Pollution Agencies.
# ACRES	Assessment, Cleanup And Redevelopment Exchange System	National	 35,541 	0	0	add to short list	land	
# The Assessment, Cleanup And Redevelopment Exchange System (Acres) Stores Information Reported By Epa Brownfields Grant Recipients On Brownfields Properties Assessed Or Cleaned Up With Grant Funding, As Well As Information On Targeted Brownfields Assessments (Tba) Performed By Epa Regions.
# BR	Biennial Reporters	National	 5,776 	1	0	add to short list	tsdf/rcra		All Generators And Treatment, Storage, And Disposal (Tsd) Facilities Who Handle Hazardous Waste Are Required To Report To The Epa Administrator At Least Once Every Two Years. The Data Collected Is Used To Create The National Biennial Resource Conservation And Recovery Act (Rcra) Hazardous Waste Report. This Data Is Processed Within The Rcra Information (Rcrainfo) Database

frsprogramcodes <- data.frame(
  description = c(
    "National Pollutant Discharge Elimination System (ICIS-NPDES)",
    "Integrated Compliance Information System for Air (ICIS-Air)",
    "Resource Conservation and Recovery Act (RCRA) Information System",
    "Risk Management Plan (RMP) facilities",
    "The Safe Drinking Water Information System (SDWIS)",
    "The Superfund Enterprise Management System",
    "Clean Air Markets Division Business System",
   "Toxics Release Inventory Program (TRI)",
   "Greenhouse Gas Reporting Program",
   "Emissions Inventory System (EIS)",
   "Toxic Substances Control Act (TSCA)",
   
   "Assessment, Cleanup and Redevelopment Exchange System (ACRES) for Brownfields",
   "Biennial Reporters (BR) Hazardous Waste Treatment, Storage, Disposal Facilities"
  ),
  code = c(
    "NPDES", "AIR", "RCRAINFO", "RMP", "SFDW", "SEMS", "CAMDBS", "TRIS", "E-GGRT", "EIS", "TSCA", 
    "ACRES", "BR"
  )
)

frsprogramcodes <- metadata_add(frsprogramcodes) # here? or it might have obsolete metadata?

usethis::use_data(frsprogramcodes, overwrite = TRUE)
cat("UPDATE DOCUMENTATION MANUALLY IN .R FILE - it is a bit complicated to use dataset_documenter() for this one\n")

#  setorder(frs_by_programid[program %in% frsprogramcodes$code, .N, by=program], -N)[]

#      program      N
#       <char>  <int>
#  1: RCRAINFO 517725
#  2:    NPDES 394581
#  3:      AIR 134115
#  4:      EIS 119792
#  5:     SFDW  43309
#  6:    ACRES  35541
#  7:     TRIS  33178
#  8:     TSCA  12619
#  9:     SEMS  10986
# 10:   E-GGRT   6257
# 11:       BR   5776
# 12:   CAMDBS    724
