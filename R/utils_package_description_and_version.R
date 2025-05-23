

######################################################## #

## NOTES ON CONSOLIDATING AND CLARIFYING CODE THAT 
## CHECKS METADATA LIKE THE PACKAGE VERSION NUMBER AND ACS DATA VERSION
## FROM THE INSTALLED VERSION OF THE EJAM PACKAGE, AND/OR FROM THE CURRENT LOCAL SOURCE PACKAGE VERSION
## see global find of "desc::desc"
######################################################## #



### The function below MAY NOT BE NEEDED -- THERE ARE MANY WAYS TO GET THIS INFO - SEE NOTES BELOW

# one way is the EJAM:::description_file  object created by metadata_mapping.R when pkg is attached.
# That is the same as package_description() created below, unless you need the non-installed, local source version the function below can provide.

# e.g., simplest ways to get installed package version or acs version:
# EJAM:::description_file$get("Version")
# EJAM:::description_file$get("ACSVersion")



## EXAMPLE/DRAFT OF A FUNCTION THAT COULD REPLACE CODE WHERE desc::desc is used or description_file is used



# package_description <- function() {
# 
#   ## also see, as alternative to desc::desc(), packageDescription("EJAM")
#   ## as a way that uses the loaded or attached version,
#   ##    via getNamespaceInfo("EJAM", "path") if it is loaded already,
#   ##    or attr(as.environment("package:EJAM"), "path") if it is already on search() path
#   ## but then otherwise falls back on the installed version, even if you are in the working dir that is the root of the source pkg.
#   # packageDescription("EJAM")
# 
#   # EJAM:::description_file created by metadata_mapping.R
#   ## will already contain the info based on when the pkg was last installed,
#   ## unless you have just used load_all() in which case it should have the current source version?
#   ## e.g.,
#   # EJAM:::description_file$get("Version")
# 
#   # This first checks the current source version in the current working directory, not necessarily the same as the version installed
#   desc <- try(desc::desc(file = "DESCRIPTION"))
# 
#   # if not found, this then checks for an INSTALLED version (or maybe any version loaded via load_all() ?? )
#   if (inherits(desc, 'try-error')) {desc <- try(desc::desc(package = "EJAM"))}
# 
#   # if still not found, this also checks for an INSTALLED version (or maybe any version loaded via load_all() ?? )
#   if (inherits(desc, 'try-error')) {desc <- desc::desc(file = app_sys('DESCRIPTION'))}
# 
#   # this if done on the fly will look in current working dir
#   # description_file <- desc::description$new("DESCRIPTION")
# 
#   if (inherits(desc, 'try-error')) {
#     warning('cannot find DESCRIPTION file in working directory or in EJAM package')
#     desc <- NULL
#   }
#   return(desc)
# }
# ###################################################### #
# 
# 
# ejam_pkg_version_get = function() {
#   
#   ## simplest way:
#   # EJAM:::description_file$get("Version")
#   # EJAM:::description_file$get("ACSVersion")
#   
#   ##   various ways to check the version number of the package (note installed and source may differ during development)
#   ## 
#   ## look in current working directory (e.g., may be the current source version you are working on)
#   # desc::desc(file = "DESCRIPTION")$get("Version")
#   # ##
#   # ## look at installed (or maybe also attached/loaded version if that is different due to load_all ? )
#   # as.character(utils::packageVersion("EJAM"))
#   # desc::desc(package = "EJAM")$get("Version")
#   # EJAM:::description_file$get("Version")
#   # EJAM:::get_metadata_mapping("blockgroupstats")$ejam_package_version
#   # EJAM:::get_metadata_mapping()$ejam_package_version
#   # EJAM:::default_metadata$ejam_package_version
# 
#   # > EJAM:::metadata_mapping$blockgroupstats$acs_version
#   # ACSVersion 
#   # "2018-2022" 
#   # > EJAM:::get_metadata_mapping("blockgroupstats")$acs_version
#   # ACSVersion 
#   # "2018-2022" 
#   # > EJAM:::get_metadata_mapping()$acs_version
#   # ACSVersion 
#   # "2018-2022"
#   # 
#   
#   ## but also see, to get just the pkg version,
#   # as.character(utils::packageVersion("EJAM"))
#   
#   ## also see, to get just the pkg version,
#   # as.vector(metadata_mapping$blockgroupstats[['ejam_package_version']])
#   
#   pkg_info <- package_description() # more complicated, looks in a few places for latest description file info
#   ejam_app_version  <- ifelse(is.null(pkg_info), "", pkg_info$get("Version"))
# 
#   
#   ## trim version number to Major.Minor
#   ejam_app_version <- substr(ejam_app_version, start = 1, stop = gregexpr('\\.',ejam_app_version)[[1]][2] - 1)
#   
#   return(ejam_app_version)
# }
# ######################################################## #
