# These are the items that are toggled depending on whether the version is Public or Private
# if run_app(isPublic = TRUE), then it's Public
# Most items toggled in app_server.R, unless otherwise specified


# note that manage-public-private.R is sourced, prior to global.R being sourced, by run_app()
# it's also sourced by .onAttach, in case user is using only the package
# this next line is necessary because while most toggled items are UI/specific to the application,
# a few are variables used also by the package, like the report titles
# so we need to default the isPublic parameter
if (!exists("golem_opts")) golem_opts <- list(isPublic = TRUE)
################################################################### #


######################################################################################################## #

# GENERAL OPTIONS & Testing ####

## ------------------------ Title of App & Version ####

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
                      "Environmental and Residential Population Analysis Multisite tool", 
                      "Environmental and Residential Population Analysis Multisite tool"
)
.app_version_headertext <- paste0("  (Version ", ejam_app_version, ")")
## keep title and version separate so we can use a different font size for each
# .app_title_and_version <-  ifelse(isTRUE(golem_opts$isPublic), 
#                                   paste0(.app_title, " (Version ", ejam_app_version, ")"),
#                                   paste0(.app_title, " (Version ", ejam_app_version, ")")
# )

# ------------------------ Tabs shown ####

# About tab
default_hide_about_tab <- isTRUE(golem_opts$isPublic)

# Histograms tab
default_hide_plot_histo_tab <- isTRUE(golem_opts$isPublic)

# Advanced settings
default_hide_advanced_settings <- TRUE # isTRUE(golem_opts$isPublic)

# Written Report
default_hide_written_report <- TRUE

# Barplots - Plot Average Scores
default_hide_plot_barplots_tab <- FALSE

default_hide_ejscreenapi_tab <- isTRUE(golem_opts$isPublic)  # not yet used

######################################################################################################## #

# SITE SELECTION: CAPS ON UPLOADS, PTS, RADIUS, etc.   ####

## ------------------------ Limits on # of points etc. ####

# see global.R

## ------------------------ EPA Programs options  #####

# see global.R

## ------------------------ choices_for_type_of_site_category #####

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

## ------------------------ choices_for_type_of_site_upload  #####

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
######################################################################################################## #

# CALCULATIONS & what stats to return ####


######################################################################################################## #

# RESULTS VIEWS ####

## ------------------------ Short report options ####

# top level header
.community_report_title <- ifelse(isTRUE(golem_opts$isPublic), 
                                  "EJAM Multisite Report",
                                  "EJAM Multisite Report"  # "Summary Report" might be appropriate for single site barplots version via build_barplot_report.R
)

default_show_ratios_in_report <- ifelse(isTRUE(golem_opts$isPublic), 
                                                   FALSE, 
                                                   TRUE
)
default_extratable_show_ratios_in_report <- ifelse(isTRUE(golem_opts$isPublic), 
                                                   FALSE, 
                                                   TRUE
)
# if passed as parameter to run_app(), override the above settings
default_show_ratios_in_report = EJAM:::global_or_param('default_show_ratios_in_report')
default_extratable_show_ratios_in_report = EJAM:::global_or_param('default_extratable_show_ratios_in_report')

######################################################################################################## #
