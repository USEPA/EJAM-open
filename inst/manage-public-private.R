# These are the items that are toggled depending on whether the version is Public or Private
# if run_app(isPublic = TRUE), then it's Public
# Most items toggled in app_server.R, unless otherwise specified

# note that manage-public-private.R is sourced prior to global.R being source, by run_app()

# About tab
default_hide_about_tab <- isTRUE(golem_opts$isPublic)

# Histograms tab
default_hide_plot_histo_tab <- isTRUE(golem_opts$isPublic)

# Advanced settings
default_hide_advanced_settings <- isTRUE(golem_opts$isPublic)

# Written Report
default_hide_written_report <- TRUE

# Barplots - Plot Average Scores
default_hide_plot_barplots_tab <- FALSE

default_hide_ejscreenapi_tab <- isTRUE(golem_opts$isPublic)  # not yet used

choices_for_type_of_site_category = if (isTRUE(golem_opts$isPublic)) {
  c('by Industry (NAICS) Code' = 'NAICS')
} else {
  c(
    'by Industry (NAICS) Code' = 'NAICS',
    'by Industry (SIC) Code'   = 'SIC',
    'by EPA Program'           = 'EPA_PROGRAM',
    'by MACT subpart'          = 'MACT'
  )
}

choices_for_type_of_site_upload <- if (isTRUE(golem_opts$isPublic)) {
  c(
    'Latitude/Longitude file upload'                = 'latlon',
    'EPA Facility IDs (FRS Identifiers)'            = 'FRS',
    'Shapefile of polygons'                         = 'SHP'
  )
} else {
  c(
    'Latitude/Longitude file upload'               = 'latlon',
    'EPA Facility ID (FRS Identifiers)'            = 'FRS',
    'EPA Program IDs'                              = 'EPA_PROGRAM',
    'FIPS Codes'                                   = 'FIPS',
    'Shapefile of polygons'                        = 'SHP'
  )
}

## app title & version   ###########################################
# note that manage-public-private.R is sourced prior to global.R being source, by run_app()
# but global.R and manage-public-private.R both need to know version info so this is done in both:
desc <- try(desc::desc(file = "DESCRIPTION"))
if (inherits(desc, 'try-error')) {desc <- try(desc::desc(package = "EJAM"))}
if (inherits(desc, 'try-error')) {desc <- desc::desc(file = EJAM:::app_sys('DESCRIPTION'))}
if (inherits(desc, 'try-error')) {stop('cannot find DESCRIPTION file in working directory or in EJAM package')}
ejam_app_version  <- desc$get("Version")
## trim version number to Major.Minor
ejam_app_version <- substr(ejam_app_version, start = 1, stop = gregexpr('\\.',ejam_app_version)[[1]][2] - 1)

.app_title <-  ifelse(isTRUE(golem_opts$isPublic), 
                      "EJScreen Multisite Tool", 
                      "EJAM"
)
.app_version_headertext <- paste0("  (Version ", ejam_app_version, ")")
## keep title and version separate so we can use a different font size for each
# .app_title_and_version <-  ifelse(isTRUE(golem_opts$isPublic), 
#                                   paste0(.app_title, " (Version ", ejam_app_version, ")"),
#                                   paste0(.app_title, " (Version ", ejam_app_version, ")")
# )
.community_report_title <- ifelse(isTRUE(golem_opts$isPublic), 
                                  "EJScreen Multisite Report", 
                                  "EJAM Multisite Report"
)
