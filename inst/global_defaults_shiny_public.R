
# The "global_defaults_*.R" files define most default settings.
#
# This file was intended to define
# just those settings that differ depending on whether the version is "Public" or "Private"
# "Public" here meant a more basic version intended for general public use,
# with fewer complicated or narrow-purpose/uncommonly-used tools and options,
# rather than more advanced/specialized/niche/expert/analyst use.
# "Private" here used to refer to a version hosted only internally for staff analysts, e.g.,
# but also referred to any version of the app run (or package used) locally by analysts.
#
# if (run_app(isPublic = TRUE))  it is "Public"
#
# This is sourced by run_app() and by .onAttach
# This next line is necessary because while most toggled items are UI/specific to the application,
# a few are variables used also by the package, like the report titles
# so we need to default the isPublic parameter
# but if the user specified in run_app, then take what they specified

# if user didn't specify isPublic, default to TRUE
isPublic <- ifelse(
  "isPublic" %in% global_defaults_or_user_options, # see EJAM:::get_global_defaults_or_user_options()
  global_defaults_or_user_options$isPublic,
  FALSE
)

use_fipspicker <- TRUE # Set here to use below in more than 1 place.

global_defaults_shiny_public <- list(

  # GENERAL OPTIONS & Testing ####
  ## ------------------------ Tabs shown ####

  # About tab
  default_hide_about_tab = isTRUE(isPublic),  # gives access to turning on the Advanced/Experimental tab, which is NOT ideal for public-facing version

  # Histograms tab
  default_hide_plot_histo_tab = isTRUE(isPublic),  # hidden because complicated and public may not want it anyway

  default_hide_ejscreenapi_tab = isTRUE(isPublic),  # not used by UI unless ejscreenapi module/tab is re-enabled

  ######################################################################################################## #

  # ~ ####

  # SITE SELECTION: CAPS ON UPLOADS, PTS, RADIUS, etc.   ####

  # 'by Census place name (Cities, Counties, States)' = 'FIPS_PLACE',  # but NOT all fips of one category (unlike for NAICS etc.)

  default_choices_for_type_of_site_category = if (isTRUE(isPublic)) {
    c(
      'by Census place name (Cities, Counties, States)' = ifelse(use_fipspicker, 'FIPS_PLACE', NULL),
      'by Industry (NAICS) Code' = 'NAICS'
    )
  } else {
    c(
      'by Census place name (Cities, Counties, States)' = ifelse(use_fipspicker, 'FIPS_PLACE', NULL),
      'by Industry (NAICS) Code' = 'NAICS',
      'by Industry (SIC) Code'   = 'SIC',
      'by EPA Program'           = 'EPA_PROGRAM',
      'by MACT subpart'          = 'MACT'
    )
  },

  ## ------------------------ default_choices_for_type_of_site_upload  #####

  default_choices_for_type_of_site_upload = if (isTRUE(isPublic)) {
    c(
      'Latitude/Longitude file upload'                = 'latlon',
      'EPA Facility IDs (FRS Identifiers)'            = 'FRS',
      'Shapefile of polygons'                         = 'SHP'
    )
  } else {
    c(
      'Latitude/Longitude file upload'                 = 'latlon',
      'EPA Facility ID (FRS Identifiers) file upload'  = 'FRS',
      'EPA Program IDs file upload'                    = 'EPA_PROGRAM', # <---
      'Shapefile of polygons file upload'              = 'SHP',
      'Census place FIPS Codes file upload'            = 'FIPS'         # <---
    )
  },
  ######################################################################################################## #
  # ~ ####

  # RESULTS VIEWS ####

  ## ------------------------ Short report options ####

  default_show_ratios_in_report = !isTRUE(isPublic), # used by app_ui to affect input$show_ratios_in_report which server uses in ejam2report(), etc.
  default_extratable_show_ratios_in_report = !isTRUE(isPublic) # same
)
rm(use_fipspicker)
rm(isPublic) # ok?