
# The "global_defaults_*.R" files define most default settings.

# This one sets
# shiny app defaults, options, and variables needed in the global environment,
# (mostly for the shiny app to work rather than for the RStudio-focused functions like ejamit() etc.)

# Note: Do not set defaults specific to a shiny module UNTIL INSIDE THE MODULE, typically, (though fipspicker options may get set here).
#   e.g., The EJAM ejscreenapi module used its own defaults file: source(system.file("global_defaults_ejscreenapi.R", package = "EJAM"))

################################################################### #
################################################################### #
library(shiny)
EJAM::indexblocks()

################################################################### #

# CHECK PACKAGE VERSION ####

ver <- EJAM:::description_file$get("Version") # object created in EJAM namespace by metadata_mapping.R when loaded/attached
ejam_app_version <- substr(ver, start = 1, stop = gregexpr('\\.',ver)[[1]][2] - 1) ## trim version number to Major.Minor
.app_version_headertext <- paste0("  (Version ", ejam_app_version, ")")
rm(ver, ejam_app_version)

################################################################## #

## ------------------------ Title of App ####

app_title <- "Environmental and Residential Population Analysis Multisite tool"

## ------------------------ logo ####

## logo for app header:
app_header_logo <- "www/favicon.png" # (small wordless hex)
app_header_logo_html <- paste0(' <img id="titleLogo" src=', app_header_logo, ' alt="logo" title="logo"
              style="margin: 0px; padding-bottom: 4px; padding-top: 4px; padding-left: 4px; padding-right: 4px; width: 40px; height: 40px">'
                               )
## old/alt one:
# app_header_logo_html <- '<img id="titleLogo" src="www/epa_logo_horizBlue.png" alt="EPA" title="EPA"
#             style="margin: 0px; padding-bottom: 4px; padding-top: 4px;   padding-left: 4px; padding-right: 4px">'

## logo for About page:
aboutpage_logo <- "www/ejamhex4.png" # (small wordless hex)
# the width and height are defined in app_ui.R not here, in this case.
################################################################## #
# ~ ####
# ------------------------ ____ SET APP DEFAULTS / OPTIONS ------------------------  ####
# ~ ####
# NOTE DEFAULTS HERE ARE UNRELATED TO DEFAULTS IN API module that has its own namespace and is kept separate, like default radius, etc.
# * Note each time a user session is started, the application-level option set is duplicated, for that session.
# * If the options are set from inside the server function, then they will be scoped to the session.
#     LET ADVANCED USERS ADJUST THESE, as INPUTS ON ADVANCED SETTINGS TAB

######################################################################################################## #

# GENERAL OPTIONS & Testing ####

use_shapefile_from_any <- TRUE # used below in list in more than one place so set it here. handles more file formats if TRUE.

global_defaults_shiny <- list(

  ## .app_title ####
  .app_title = app_title,
  
  ## logos ####
  aboutpage_logo = aboutpage_logo,
  ## .community_report_logo_path is not needed here since since it is defined elsewhere, in global_defaults_package.R
  ## app_header_logo_html is not needed here since it is simply used below in the html directly, not queried by app_ui.R
  
  default_testing        = FALSE,
  default_shiny.testmode = FALSE,  # If TRUE, then various features for testing Shiny applications are enabled.
  default_print_uploaded_points_to_log = TRUE,

  ######################################################################################################## #

  # ~ ####

  # SITE SELECTION: CAPS ON UPLOADS, PTS, RADIUS, etc.   ####

  ## ------------------------ Limits on # of points etc. ####

  ## Options in file upload size max
  minmax_mb_upload = 5, # MB
  default_max_mb_upload = 50, # MB (note shiny default is only 5 MB)
  maxmax_mb_upload = 350, # MB

  # input$max_pts_upload
  default_max_pts_upload  =   5 * 1000,
  maxmax_pts_upload  =  10 * 1000, #   cap uploaded points

  # input$max_pts_select
  default_max_pts_select  =   5 * 1000,
  maxmax_pts_select  =  10 * 1000, #   cap selected points

  # input$max_pts_map uses these as its starting value and max allowed value
  default_max_pts_map   = 5 * 1000,
  maxmax_pts_map       = 15 * 1000, # max we will show on map

  marker_cluster_cutoff  = 1 * 1000,  # max before showing points as clusters, for leaflet markerClusters

  # input$max_pts_showtable uses these as its starting value and max allowed value
  default_max_pts_showtable = 1000, # max to show in interactive viewer. It drops the rest.
  maxmax_pts_showtable  = 5 * 1000, # 10k is extremely slow. check server side vs client side

  # input$max_pts_run uses these as its starting value and max allowed value
  default_max_pts_run  = 10 * 1000, # initial cap but can adjust in advanced tab
  maxmax_pts_run       = 15 * 1000, # absolute max you can analyze here, even with advanced tab

  # input$max_shapes_map uses these as its starting value and max allowed value
  default_max_shapes_map = 159, # TX has 254 counties, but no other state exceeds 159. EJAM::blockgroupstats[ , data.table::uniqueN(substr(bgfips, 1,5)), by = ST][order(V1), ]
  maxmax_shapes_map      = 254, # TX has 254 counties

  use_shapefile_from_any = use_shapefile_from_any, # *** newer code - handles more spatial formats like .json etc.

  default_shp_oktypes_1 = if (use_shapefile_from_any) {
    c("zip", "gdb", "geojson", "json", "kml", "shp", "dbf", "sbn", "sbx", "shx", "prj", "cpg")
  } else {
    c("zip",        "geojson", "json",        "shp", "dbf", "sbn", "sbx", "shx", "prj")
  },

  ## ------------------------ Radius options  #####

  #   radius miles for slider input where user specifies radius. Note 5 km is 3.1 miles, 10 km is 6.2 miles ; and 10 miles is 16 kilometers (10 * meters_per_mile/1000). 50 km is too much/ too slow.
  minradius  = 0.50, # miles -- significant uncertainty as radius shrinks, at least if blockgroups are small such as if # of blockgroups in circle << 30.
  minradius_shapefile = 0,
  default_default_miles_shapefile = 0,
  stepradius = 0.05, # miles.  0.25 allows quarter miles. 0.10 allows tenths. 0.05 is awkwardly small but allows both quarter mile and tenth of mile.

  # input$default_miles
  default_default_miles = 1, # and can override this with run_app(radius=3.1), and also see effects of bookmarked advanced settings
  max_default_miles = 50 * 1000 / EJAM::meters_per_mile, # 50 km. EJAM::meters_per_mile is lazyloaded data constant.

  # input$max_miles
  default_max_miles  = 10, #
  maxmax_miles = 50 * 1000 / EJAM::meters_per_mile, # 50 km.

  ## ------------------------ Site Selection options  #####

  # upload or dropdown method of site selection
  default_upload_dropdown = "upload",

  # EPA Programs (to limit NAICS/ facilities query)
  # used by inputId 'ss_limit_fac1' and 'ss_limit_fac2'
  default_epa_program_selected = "CAMDBS", # has only about 739 sites
  # cbind(epa_programs)
  # sort(unique(frs_by_programid$program)) # similar  # EJAM :: frs_by_programid

  ##################################################################################### #

  ## ------------------------ fipspicker module ####

  # *** but perhaps these should be set only inside the module, just to avoid clutter, unneeded settings if module usused, and possible namespace conflicts. as had been noted for the ejscreenapi module settings
  fipspicker_fips_type2pick_default = "Counties",  #"Cities or Places",
  fipspicker_fips_type2pick_choices_default = c(
    # `EPA Regions` = "EPA Regions", # if we wanted to allow user to pick an entire EPA Region or two to compare. More useful for filtering mode, like if picking all states within Region 2.
    States = "States",
    Counties = "Counties",
    `Cities/Places` = "Cities or Places"
  ),
  # Limits height of pulldown list of possible choices, but does NOT limit the number of selections shown in selectize box!
  fipspicker_maxOptions_default_states_picked   = 255,
  fipspicker_maxOptions_default_counties_picked = 255,
  fipspicker_maxOptions_default_cities_picked   = 255,

  fipspicker_all_regions_button_defaultchecked  = TRUE,
  fipspicker_all_states_button_defaultchecked   = TRUE,
  fipspicker_all_counties_button_defaultchecked = FALSE,

  fipspicker_all_regions_button_defaultshow  = FALSE,
  fipspicker_all_states_button_defaultshow   = FALSE,
  fipspicker_all_counties_button_defaultshow = FALSE,
  ##################################################################################### #

  ######################################################################################################## #


  # ~ ####
  # CALCULATIONS & what stats to return ####

  default_include_averages = TRUE,         # not implemented and is not a param of a function
  default_include_extraindicators = TRUE,  # not implemented and is not a param of a function


  ## ------------------------ ejamit() params ####


  ### params whose defaults could be included here and in advanced tab:

  # > cbind(formals(ejamit))
  #
  # sitepoints               NULL         na
  # radius                   3         for getblocksnearby(), shiny default is set in global
  # radius_donut_lower_edge  0         na
  # maxradius                31.07     for getblocksnearby(), shiny default is set in global
  # avoidorphans             FALSE     for getblocksnearby(), shiny default is set in global
  # quadtree                 NULL      na
  # fips                     NULL      na
  # shapefile                NULL      na
  # countcols                NULL      for doaggregate() ** shiny default NOT specified here
  # popmeancols              NULL      for doaggregate() ** shiny default NOT specified here
  # calculatedcols           NULL      for doaggregate() ** shiny default NOT specified here
  # calctype_maxbg         NULL      for doaggregate() ** shiny default NOT specified here
  # calctype_minbg         NULL      for doaggregate() ** shiny default NOT specified here
  # subgroups_type           "nh"      for doaggregate(), shiny default is set in global
  # include_ejindexes        TRUE      for doaggregate(), shiny default is set in global
  # calculate_ratios         TRUE      for doaggregate(), shiny default is set in global
  # extra_demog              TRUE      for doaggregate(), shiny default is set in global
  # need_proximityscore      FALSE     for doaggregate(), shiny default is set in global
  # infer_sitepoints         FALSE     for doaggregate() ** shiny default NOT specified here
  # need_blockwt             TRUE      for getblocksnearby_from_fips(), shiny default not specified here, but by function defaults
  # thresholds               expression  for batch.summarize()
  # threshnames              expression  for batch.summarize()
  # threshgroups             expression  for batch.summarize()
  # updateProgress           NULL      ** shiny default NOT specified here
  # updateProgress_getblocks NULL      ** shiny default NOT specified here
  # in_shiny                 FALSE     for ejamit(), build_community_report(), and related functions
  # quiet                    TRUE      for getblocksnearby() and batch.summarize(), na

  # silentinteractive        FALSE    for doaggregate() etc. na
  # called_by_ejamit         TRUE     for doaggregate(), na
  # testing                  FALSE    for doaggregate(), shiny default is set in global
  # showdrinkingwater        TRUE     for doaggregate(), na
  # showpctowned             TRUE     for doaggregate(), na
  #   download_fips_bounds_to_calc_areas FALSE
  # ...                      ?         for getblocksnearby(), like use_unadjusted_distance ** shiny default NOT specified here


  ## ------------------------ getblocksnearby() params ####

  # > cbind(formals(getblocksnearby))
  #
  # sitepoints              ?       na
  # radius                  3       shiny default is set in global
  # maxradius               31.07   shiny default is set in global
  # radius_donut_lower_edge 0       na
  # avoidorphans            FALSE   shiny default is set in global
  # quadtree                NULL    na
  # quaddatatable           NULL
  # quiet                   FALSE
  # parallel                FALSE
  # use_unadjusted_distance TRUE    ** shiny default NOT specified here
  # ...                     ?

  # > cbind( formals(getblocksnearbyviaQuadTree)[setdiff(names(formals(getblocksnearbyviaQuadTree)), names(formals(getblocksnearby))) ])
  #
  # report_progress_every_n    500    ** shiny default NOT specified here
  # retain_unadjusted_distance TRUE   ** shiny default NOT specified here
  # updateProgress             NULL   ** shiny default NOT specified here

  default_avoidorphans        = FALSE, # seems like EJScreen itself essentially uses FALSE? not quite clear
  default_maxradius =  31.06856,  # max search dist if no block within radius # 50000 / meters_per_mile #, # 31.06856 miles !!
  # also used as the maxmax allowed

  ## ------------------------ doaggregate() params ####

  default_download_fips_bounds_to_calc_areas = FALSE,

  # > cbind(formals(doaggregate))
  #
  # sites2blocks           ?       na
  # sites2states_or_latlon NA      na
  # radius                 NULL    shiny default is set in global
  # countcols              NULL  ** shiny default NOT specified here
  # wtdmeancols            NULL  ** shiny default NOT specified here
  # calculatedcols         NULL  ** shiny default NOT specified here
   # calctype_maxbg         NULL
   # calctype_minbg         NULL
  # subgroups_type         "nh"     shiny default is set in global
  # include_ejindexes      FALSE    shiny default is set in global
  # calculate_ratios       TRUE     shiny default is set in global
  # extra_demog            TRUE     shiny default is set in global
  # need_proximityscore    FALSE    shiny default is set in global
  # infer_sitepoints       FALSE ** shiny default NOT specified here
  # called_by_ejamit       FALSE    na
  # updateProgress         NULL  ** shiny default NOT specified here
  # silentinteractive      TRUE     na
  # testing                FALSE    shiny default is set in global
  # showdrinkingwater      TRUE     na
  # showpctowned           TRUE     na
  # ...                    ?

  default_include_ejindexes = TRUE, # include_ejindexes is a param in doaggregate() or ejamit()
  default_calculate_ratios = TRUE,   # and see default_show_ratios_in_report;  probably need to calculate even if not shown in excel download, since plots and short summary report rely on them/
  default_extra_demog = TRUE, # extra_demog is a param in  doaggregate() or ejamit(),
  # label = "Need extra indicators, on language, age groups, sex, percent with disability, poverty, etc.",
  default_need_proximityscore = FALSE, # need_proximityscore is a param in doaggregate() or ejamit()

  default_subgroups_type = 'nh',
  # this sets the default in the web app only, not in functions doaggregate() and ejamit() and plot_distance_mean_by_group() etc.,
  # if used outside web app app_server and app_ui code, as in using datacreate_testpoints_testoutputs.R
  # "nh" for non-hispanic race subgroups as in Non-Hispanic White Alone, nhwa and others in names_d_subgroups_nh;
  # "alone" for EJScreen v2.2 style race subgroups as in    White Alone, wa and others in names_d_subgroups_alone;
  # "both" for both versions.


  ## ------------------------ batch.summarize() params, Threshold comparisons etc ####

  # > cbind(formals(batch.summarize))
  #
  # sitestats     ?            na
  # popstats      ?            na
  # cols          "all"        ** shiny default NOT specified here
  # wtscolname    "pop"        see doaggregate()
  # probs         expression   ** shiny default NOT specified here. probs = c(0, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99, 1)
  # thresholds    expression   shiny default is set in global
  # threshnames   expressions  shiny default is set in global
  # threshgroups  expression   shiny default is set in global
  # na.rm         TRUE         na
  # rowfun.picked "all"     ** shiny default NOT specified here
  # colfun.picked "all"     ** shiny default NOT specified here
  # quiet         FALSE        na
  # testing       FALSE        shiny default is set in global

  ### ---------- threshold comparisons ----------- ####

  # stats summarizing EJ percentiles to count how many are at/above threshold percentile(s)

  # label for each group of indicators

  default.an_threshgroup1 = "EJ-US-or-ST",
  default.an_threshgroup2 = "Supp-US-or-ST",
  ### threshgroups = list("EJ-US-or-ST", "Supp-US-or-ST"), # list(c("EJ US", "EJ State", "Suppl EJ US", "Suppl EJ State")), # list("EJ US", "EJ State", "Suppl EJ US", "Suppl EJ State"), # list("variables"),
  ### threshgroups = list(input$an_threshgroup1, input$an_threshgroup2),

  # variable names of indicators compared to threshold

  default.an_threshnames1 = c(EJAM::names_ej_pctile, EJAM::names_ej_state_pctile), # regular in US or ST
  default.an_threshnames2 = c(EJAM::names_ej_supp_pctile, EJAM::names_ej_supp_state_pctile), # supplemental in US or ST
  ### threshnames = list(input$an_threshnames1, input$an_threshnames2)
  ### threshnames = list(c(names_ej_pctile, names_ej_state_pctile), c(names_ej_supp_pctile, names_ej_supp_state_pctile)), # list(c(names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile)),  #list(names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile),  # list(names(which(sapply(sitestats, class) != "character")))

  # what threshold to compare to

  default.an_thresh_comp1 = 80, # regular
  default.an_thresh_comp2 = 80, # supplemental
  ### thresholds   = list(input$an_thresh_comp1, input$an_thresh_comp2)
  ### thresholds   = list(90, 90) # percentile threshold(s) to compare to like to 90th

  ### ---------- quantiles (probs) ----------- ####
  #
  # unused, but could be used by batch.summarize( probs = as.numeric(input$an_list_pctiles) )
  #
  # probs.default.selected <- c(   0.25,            0.80,     0.95)
  # probs.default.values   <- c(0, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99, 1)
  # probs.default.names <- formatC(probs.default.values, digits = 2, format = 'f', zero.print = '0')

  ######################################################################################################## #


  # ~ ####
  # RESULTS VIEWS ####



  ## ------------------------ Map formatting options ####


  ##  Map colors, weights, opacity (for ejscreenapi module?)

  ### in ejscreenapi global_defaults_ejscreenapi.R:
  default_circleweight = 4,
  # opacitymin   <- 0
  # opacitymax   <- 0.5
  # opacitystep  <- 0.025
  # opacitystart <- 0.5
  # opacityratio <- 2 / 5
  # base_color_default      <- "blue"  ;
  # cluster_color_default   <- "red"   ;
  # highlight_color_default <- 'orange';

  # (predict time to complete for ejscreenapi module)
  # perhourslow  <- 3000  # to give an estimate of how long it will take
  # perhourguess <- 6000  # seeing 8k if 1 mile, 4.7k if 5 miles, roughly. 207 ECHO run was 2 . 1  minutes, 5.9k/hr.
  # perhourfast <- 12000  # approx 12k RMP sites would take almost 2 hours (1 to 2 hours, or even 4?).
  # report_every_n_default <- 100

  ## (download as excel vs csv, for ejscreenapi module)
  # asExcel <- TRUE # WHETHER TO DOWNLOAD RESULTS AS EXCEL OR CSV


  ## ------------------------ Excel formatting options ####


  # > (cbind(formals(table_xls_format)))
  #
  # overall                      ?          na
  # eachsite                     ?          na
  # longnames                    NULL       na
  # formatted                    NULL       na
  # bybg                         NULL       na
  # plot_distance_by_group       FALSE      na
  # summary_plot                 NULL       na
  # plotlatest                   FALSE      na
  # plotfilename                 NULL       na
  # mapadd                       FALSE      ***
  # community_reportadd          FALSE      ***
  # report_map                   NULL       ***
  # community_image              NULL       na
  # ok2plot                      TRUE       ***
  # analysis_title               "EJAM analysis"       na
  # buffer_desc                  "Selected Locations"  ***
  # radius_or_buffer_in_miles    NULL        na
  # radius_or_buffer_description "Miles radius of circular buffer (or distance used if buffering around polygons)" ***
  # notes                        NULL        ***
  # custom_tab                   NULL        ***
  # custom_tab_name              "other"     ***
  # heatmap_colnames             NULL            ***
  # heatmap_cuts                 expression      ***
  # heatmap_colors               expression      ***
  # heatmap2_colnames            NULL            ***
  # heatmap2_cuts                expression      ***
  # heatmap2_colors              expression      ***
  # hyperlink_colnames           expression   ***
  # graycolnames                 NULL
  # narrowcolnames               NULL
  # graycolor                    "gray"
  # narrow6                      6
  # testing                      FALSE
  # updateProgress               NULL
  # launchexcel                  FALSE
  # saveas                       NULL
  # ejscreen_ejam_caveat         NULL           ***
  # ...                          ?


  # heatmap column names - defaults could be set here and made flexible in advanced tab


  # heatmap cutoffs for bins - defaults could be set here and made flexible in advanced tab


  # heatmap colors for bins - defaults could be set here and made flexible in advanced tab


  default_ok2plot = TRUE, # the plots to put in excel tabs via table_xls_from_ejam() and table_xls_format() and the plot functions


  ## ------------------------ Short report options ####


  default_standard_analysis_title = 'Summary of Analysis', # Default title to show on each short report
  default_plotkind_1pager = "bar",  #    Bar = "bar", Box = "box", Ridgeline = "ridgeline"

  default_extratable_title = '', # above the table, not in the upper left cell

  default_extratable_title_top_row = "ADDITIONAL INFORMATION", # upper left cell
  # default_extratable_title was 'Additional Indicators' above the table, but its redundant if
  # extratable_title_top_row is "ADDITIONAL INFORMATION" in upper left cell.

  ## ------------------------ default_show_full_header_footer (EPA header) ####

  ## constant to show/hide EPA HTML header and footer in app UI
  ## for public branch, want to hide so it can be legible when embedded as an iframe
  default_show_full_header_footer = FALSE,

  # Advanced settings
  default_hide_advanced_settings = TRUE, # isTRUE(isPublic)

  # Written Report
  default_hide_written_report = TRUE,

  # Barplots - Plot Average Scores
  default_hide_plot_barplot_tab = FALSE

)

######################################################################################################## #

# R and Shiny Options
## ------------------------ autoloading of .R files ####
options(shiny.autoload.r = FALSE)
# show generalized errors in the UI
options(shiny.sanitize.errors = TRUE)

## ------------------------ Loading/wait spinners (color, type) ####
## note: was set at type = 1, but this caused screen to "bounce"
options(spinner.color = "#005ea2", spinner.type = 4)

options(shiny.maxRequestSize = global_defaults_shiny$default_max_mb_upload * 1024^2)

# default_show_ratios_in_report <- FALSE # and see default_calculate_ratios, calculate_ratios
# if (!default_calculate_ratios) {default_show_ratios_in_report <- FALSE}  # or let it show NA values

#            default_show_ratios_in_report  is defined in manage-public-private
# default_extratable_show_ratios_in_report  is defined in manage-public-private

######################################################## #
### for a UI report builder it may be useful to have a named list of
#  Quoted R expressions!! but be careful since "names_d" can get evaluated and turned into a vector of colnames
#  using eval(parse(text = "names_d"))
#  but eval(parse(text = c("pcthisp", "pctnhba") ))  would fail.

## nice format for UI:
# default_extratable_list_of_sections_usinglists = list(
#   `Breakdown by Race/Ethnicity` = list("`Various population groups` = names_d_subgroups"),
#### etc

list_unattributed = function(mylist) {
  for (i in seq_along(mylist)) {attributes(mylist[[i]]) <- NULL}
  return(mylist)
}

default_extratable_list_of_sections_ui = list(

  # see ejam2report defaults and build_community_report defaults

  `Extra Indicators` = list(  # just a heading for UI
    "`Breakdown by Population Group` = names_d_subgroups", # note here this is a whole quoted line, not a vector of colnames, and names_d_subgroups is not quoted since it is a vector of colnames
    "`Language Spoken at Home` = names_d_language",
    "`Language in Limited English Speaking Households` = names_d_languageli",
    "`Breakdown by Sex` = c('pctmale','pctfemale')", # individual colnames each do get quoted, unlike a variable that is a vector of colnames
    "`Health`  = names_health",
    "`Age` = c('pctunder5', 'pctunder18', 'pctover64')",
    "`Community` = names_community[!(names_community %in% c( 'pctmale', 'pctfemale', 'pctownedunits_dupe'))]",
    "`Poverty` = names_d_extra",
    "`Features and Location Information` = c(
    names_e_other,
    names_sitesinarea,
    names_featuresinarea ,
    names_flag
  )",
    "`Climate` = names_climate",
    "`Critical Services` = names_criticalservice"
  ),
  `Other` = list(  # just a heading for UI
    "`Other` = names_d_other_count"
    # , "`Count above threshold` = names_countabove"  # need to fix map_headernames longname and calctype and weight and drop 2 of the 6
  )
)

result_as_code <- paste("list(\n",
                        paste0(as.vector(unlist(default_extratable_list_of_sections_ui)), collapse = ",\n "), "\n)")
result_as_code <- eval(parse(text = result_as_code))
default_extratable_list_of_sections <- list_unattributed(result_as_code)


### for a UI report builder it is useful to have them as above, quoted R expressions
### but now need it converted from quoted R expressions into evaluated results...
##  each element of list is now a vector of strings that are colnames:

# Get rid of the distracting attributes that got created
extratable_stuff <- list(
  default_extratable_list_of_sections = default_extratable_list_of_sections,

  ################################# #

  ## this is unquoted vector of colnames (e.g., all of the indicators in default_extratable_list_of_sections)
  ## see also defaults in ejam2report() and build_...
  default_extratable_hide_missing_rows_for = as.vector(unlist(default_extratable_list_of_sections)) # c(names_d_language, names_health)
)
rm(result_as_code, default_extratable_list_of_sections_ui)

################################# #
if (interactive()) {cat("Running shiny app in interactive() mode \n")}
if (FALSE) {
  ## These are just notes, to check this default is same as defaults in functions:
  ##   build_community_report() defaults, ejam2report() defaults, and the global_defaults_*.R  defaults
  ## drafted helper funcs to get default value of an argument from a function:
  ##   fun should be unquoted, arg param should be quoted:
  arg1 <- function(fun,arg) {formals(fun)[[arg]]}
  arg_as_code <- function(myarg) {
    if (class(myarg) == "call") {
      myarg <- paste0(capture.output(myarg), collapse = " ")
      eval(parse(text = myarg))
    } else {
      eval(parse(text = quote(myarg)))
    }
  }
  argdefault <- function(fun,arg) {
    cat(arg, "= ")
    print(arg1(fun,arg))
    arg_as_code(arg1(fun,arg))
  }
  # dput(default_extratable_list_of_sections) # messier view

  v1 = list_unattributed(default_extratable_list_of_sections)
  v2 = list_unattributed(argdefault(ejam2report,            "extratable_list_of_sections"))
  v3 = list_unattributed(argdefault(build_community_report, "extratable_list_of_sections"))
  if (!all.equal(v1, v2)) {warning("default_extratable_list_of_sections in global_defaults_*.R and ejam2report() defaults do not match")}
  if (!all.equal(v1, v3)) {warning("default_extratable_list_of_sections in global_defaults_*.R and build_community_report() defaults do not match")}

  rm(v1,v2,v3,arg1,arg_as_code,argdefault)
}
rm(list_unattributed)

######################################################## #


## ------------------------ Long report options ####


# relocate any here from the Full Report tab?? - defaults could be set here and made flexible elsewhere ***


################################################################# #
# END OF DEFAULTS / OPTIONS / SETUP
################################################################# #



# ~ ####
# ------------------------ ____ HELP TEXT ------------------------  ####
# ~ ####

## info text for "About EJAM" tab ####

docs_url            <- EJAM:::repo_from_desc("github.io", get_full_url = TRUE)
testdata_repo_url   <- EJAM:::repo_from_desc("github.com", get_full_url = TRUE)
testdata_owner_repo <- EJAM:::repo_from_desc("github.com", get_full_url = FALSE)
testdata_repo <-  gsub(".*/", "", testdata_owner_repo)

help_texts <- list(
  intro_text = tagList(
    # tags$p("For more information about EJAM:"),
    h2( a(href = paste0(docs_url, "/", "articles/0_whatis.html"),
          "What is EJAM?",
          target = "_blank", rel = "noreferrer noopener") ),
    p("The Environmental and Residential Population Analysis Multisite tool (EJAM) is a tool developed by the United States Environmental Protection Agency (US EPA) that makes it easy to see residential population and environmental information summarized in and across any list of places in the nation. Using this tool is like getting reports for hundreds or thousands of places, all at the same time."),
    p("This provides interactive results and a formatted, ready-to-share report with tables, graphics, and a map. The report can provide information about communities near any of the industrial facilities on a list, for example."),
    br(),
    br()
  ),

  ## 5/5/2025 UPDATE: NO LONGER USED
  ## help text for ECHO facility search: echo_message ####
  ## used by inputId 'ss_search_echo'
  # echo_message = shiny::HTML(paste0('To use the ECHO website to search for and specify a list of regulated facilities,
  #                                     <br>1) Go to ', '<a href="https://echo.epa.gov/facilities/facility-search", target=\"_blank\" rel=\"noreferrer noopener\">', echo_url,  '</a>', ' and <br>
  #                                     2) Navigate website and select categories to include in data, then <br>
  #                                     3) Under Search Criteria Selected-Facility Characteristics-Results View select <b>Data Table</b> and click <b>Search</b>, then <br>
  #                                     3) click Customize Columns, use checkboxes to include Latitude and Longitude, then <br>
  #                                     4) click Download Data, then <br>
  #                                     5) Return to this app to upload that ECHO site list.<br>')),

  ## help text for upload: latlon_help_msg ####
  latlon_help_msg = paste0('
<div class="row">
  <div class="col-sm-12">
  <div class="well">
  <div id="selectFrom1" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
  <label class="control-label" for="selectFrom1">

  <p>You may upload a list of location coordinates (latitudes and longitudes).</p>',

                           # example file

                           tags$a(href = paste0("https://github.com/",
                                                testdata_owner_repo,
                                  "/blob/master/inst/testdata/latlon/testpoints_10.xlsx?raw=true"), target = "_blank",
                                  "Example of lat lon file"),
'
<p>Allowed filetypes: .csv, .xls, or .xlsx</p>
<p>Required column names in first row as header: lat, lon (or aliases)</p>

  <p>It also will work with some alternative names (and case insensitive) like
  Latitude, Lat, latitude, long, longitude, Longitude, Long, LONG, LAT, etc.
  but to avoid any mixup of names it is suggested that the file use lat and lon. </p>

  <p>The file could be formatted as follows, for example: </p>
  </label>
  <br>
  ID,lat,lon<br>
  1,36.26333,-98.48083<br>
  2,41.01778,-80.36194<br>
  3,43.43772,-91.90365<br>
  4,29.69083,-91.34333<br>
  5,40.11389,-75.34806<br>
  6,35.97889,-78.88056<br>
  7,32.82556,-89.53472<br>
  8,30.11275,-83.59778<br>
  9,30.11667,-83.58333<br>
  10,38.06861,-88.75361<br><br>
  </div>
  </div>
  </div>
  </div>'
  ),
  ## help text for upload: shp_help_msg ####

  shp_help_msg = paste0('
<div class="row">
  <div class="col-sm-12">
  <div class="well">
  <div id="selectFrom1" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
  <label class="control-label" for="selectFrom1">

  <p>You may upload a set polygons in a shapefile.</p>',

                        # example file

                        tags$a(href = paste0("https://github.com/",
                                             testdata_owner_repo,
                        "/blob/master/inst/testdata/shapes/portland.gdb.zip?raw=true"), target = "_blank", "Example of Shapefile"),
'
  <p>The file can be in one of the following formats:</p>
  <p>', paste0(global_defaults_shiny$default_shp_oktypes_1, collapse = ", "), '</p>',
                        '
</div>
  </div>
  </div>
  </div>'
  ),
  ## help text for upload: frs_help_msg ####

  frs_help_msg = paste0('  <div class="row">
    <div class="col-sm-12">
      <div class="well">
        <div id="selectFrom1" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
          <label class="control-label" for="selectFrom1">

            <p>You may upload a list of FRS IDs. The FRS ID should be in the second column. It should be unique (no duplicates), and it should be titled REGISTRY_ID.</p>
',

                        # example file

                        tags$a(href = paste0("https://github.com/",
                                             testdata_owner_repo,
                                             "/blob/master/inst/testdata/registryid/frs_test_regid_8.xlsx?raw=true"), target = "_blank",
                               "Example of Registry IDs file"),

                        '
<p>Allowed filetypes: .csv, .xls, or .xlsx</p>
<p>Required column names in first row as header: REGISTRY_ID (or alias)</p>

            <h5>The file should be formatted as follows: </h5>
          </label>
					<br>num,REGISTRY_ID<br>
		      1,110000308006<br>
		      2,110000308015<br>
      		3,110000308024<br>
		      4,110000308202<br>
      		5,110000308211<br>
		      <br>
        </div>
      </div>
    </div>
  </div>'),

  ## help text for upload: epa_program_help_msg ####

  epa_program_help_msg = paste0('
<div class="row">
  <div class="col-sm-12">
  <div class="well">
  <div id="selectFrom1" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
  <label class="control-label" for="selectFrom1">

  <p>You may upload a list of EPA Programs and Program IDs.</p>',

                                # example file

                                tags$a(href = paste0("https://github.com/",
                                                     testdata_owner_repo,
                                       "/blob/master/inst/testdata/programid/program_test_data_10.xlsx?raw=true"), target =  "_blank" ,
                                       "Example of EPA program ID file"),

                                '  <p>Allowed filetypes: .csv, .xls, or .xlsx</p>
  <p>Required column names in first row as header:  program, pgm_sys_id (or aliases)</p>

  <p>The file should contain at least these two column names in the first row: program and pgm_sys_id.</p>
  <p>It also will work with additional optional columns such as Facility Registry ID (REGISTRY_ID), latitude (lat), and longitude (lon). </p>
  <p>The file could be formatted as follows, for example: </p>
  </label>
  <br>
  program,	pgm_sys_id<br>
NC-FITS,	28122<br>
AIR,	NY0000004432800019<br>
NPDES,	GAR38F1E2<br>
TRIS,	7495WCRHMR59SMC<br>
MN-TEMPO,	17295<br>
HWTS-DATAMART,	CAR000018374<br>
IN-FRS,	330015781585<br>
TX-TCEQ ACR,	RN104404751<br>
NJ-NJEMS,	353065<br>
AIR,	IL000031012ACJ<br>
  </div>
  </div>
  </div>
  </div>'
  ),
  ## help text for upload: fips_help_msg ####
  fips_help_msg = paste0('
<div class="row">
  <div class="col-sm-12">
  <div class="well">
  <div id="selectFrom1" class="form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline">
  <label class="control-label" for="selectFrom1">
  ',
                         '<p>You may upload a list of FIPS codes specified at the State (2-digit), County (5-digit),',
                         ' Census Designated Place (CDP) like city or township (6-digit or 7-digit),',
                         ' Tract (11-digit), or blockgroup (12 digit), or even block (15-digit fips).</p>',

                         # example file

                         tags$a(href = paste0("https://github.com/",
                                              testdata_owner_repo,
                                "/blob/master/inst/testdata/fips/counties_in_Delaware.xlsx?raw=true"), target = "_blank",
                                "Example of FIPS codes file"),

                         '
<p>Allowed filetypes: .csv, .xls, or .xlsx</p>
<p>Required column names in first row as header: FIPS (or alias)</p>

  <p>It will also work with the following aliases: ',
                         'fips, fips_code, fipscode, Fips, statefips, countyfips, ST_FIPS, st_fips
  ',
                         '</p>
  <p>The file could be formatted as follows, for example: </p>
  </label>
  <br>
 FIPS<br>
36001014002<br>
26163594300<br>
36029008600<br>
36061006100<br>
15003005300<br>
<br>
  </div>
  </div>
  </div>
  </div>'
  )
)

#################################################################################################################### #
# ~ ####
# ------------------------ ____ TEMPLATE ONE EPA SHINY APP WEBPAGE _______ ####
# ~ ####
html_fmts <- list(
  html_header_fmt = tagList(

    #################################################################################################################### #
    # WHERE TO FIND THIS template  #
    #  USEPA/webcms/blob/main/utilities/r/OneEPA_template.R
    # but also see
    # https://www.epa.gov/themes/epa_theme/pattern-lab/patterns/pages-standalone-template/pages-standalone-template.rendered.html
    # START OF ONEEPA SHINY APP WEB UI TEMPLATE to insert within your fluid page
    #################################################################################################################### #

    tags$html(class = "no-js", lang = "en"),

    # head ####
    ## Google tag manager? ####
    tags$head(
      HTML(
        "<!-- Google Tag Manager

  		  <script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
  		new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
  		j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
  		'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
  		})(window,document,'script','dataLayer','GTM-L8ZB');</script>

      End Google Tag Manager -->"
      ),

      ## meta tags (some EPA-specific)  ####

      tags$meta(charset="utf-8"),

      tags$meta(property="og:site_name", content="US EPA"),

      #tags$link(rel = "stylesheet", type = "text/css", href = "css/uswds.css"),
      tags$link(rel="stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/css/uswds.min.css", integrity="sha512-ZKvR1/R8Sgyx96aq5htbFKX84hN+zNXN73sG1dEHQTASpNA8Pc53vTbPsEKTXTZn9J4G7R5Il012VNsDEReqCA==", crossorigin="anonymous", referrerpolicy="no-referrer"),
      tags$link(rel="canonical", href="https://www.epa.gov/themes/epa_theme/pattern-lab/.markup-only.html"),
      tags$link(rel="shortlink", href="https://www.epa.gov/themes/epa_theme/pattern-lab/.markup-only.html"),

      tags$meta(property="og:url", content="https://www.epa.gov/themes/epa_theme/pattern-lab/.markup-only.html"),
      tags$meta(property="og:url", content="https://www.epa.gov/themes/epa_theme/pattern-lab/.markup-only.html"),
      tags$meta(property="og:image", content="https://www.epa.gov/sites/all/themes/epa/img/epa-standard-og.jpg"),
      tags$meta(property="og:image:width", content="1200"),
      tags$meta(property="og:image:height", content="630"),
      tags$meta(property="og:image:alt", content="U.S. Environmental Protection Agency"),
      tags$meta(name="twitter:card", content="summary_large_image"),
      tags$meta(name="twitter:image:alt", content="U.S. Environmental Protection Agency"),
      tags$meta(name="twitter:image:height", content="600"),
      tags$meta(name="twitter:image:width", content="1200"),
      tags$meta(name="twitter:image", content="https://www.epa.gov/sites/all/themes/epa/img/epa-standard-twitter.jpg"),
      tags$meta(name="MobileOptimized", content="width"),
      tags$meta(name="HandheldFriendly", content="true"),
      tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
      tags$meta(`http-equiv`="x-ua-compatible", content="ie=edge"),

      ## >> App NAME/TITLE could be defined here ####

      # AND in golem_add_external_resources() IN app_ui.R,
      # AND BELOW IN SHORT VERSION OF HEADER
      # tags$title('EJAM | US EPA'),

      tags$meta(name = "application-name", content = app_title),

      ## Favicons (some EPA-specific) - but can be specified in (and this would conflict with) golem_add_external_resources() within app_ui.R ####

      # try to let app_ui.R define the main favicon instead of using the EPA one....
      # tags$link(rel="icon",                      href="https://www.epa.gov/themes/epa_theme/images/favicon-32.png", sizes="32x32"),
      # tags$link(rel="icon", type="image/x-icon", href="https://www.epa.gov/themes/epa_theme/images/favicon.ico"),

      tags$meta(name="msapplication-TileColor", content="#FFFFFF"),
      tags$meta(name="msapplication-TileImage", content="https://www.epa.gov/themes/epa_theme/images/favicon-144.png"),
      tags$meta(name="msapplication-config",    content="https://www.epa.gov/themes/epa_theme/images/ieconfig.xml"),

      tags$link(rel="apple-touch-icon-precomposed", sizes="196x196", href="https://www.epa.gov/themes/epa_theme/images/favicon-196.png"),
      tags$link(rel="apple-touch-icon-precomposed", sizes="152x152", href="https://www.epa.gov/themes/epa_theme/images/favicon-152.png"),
      tags$link(rel="apple-touch-icon-precomposed", sizes="144x144", href="https://www.epa.gov/themes/epa_theme/images/favicon-144.png"),
      tags$link(rel="apple-touch-icon-precomposed", sizes="120x120", href="https://www.epa.gov/themes/epa_theme/images/favicon-120.png"),
      tags$link(rel="apple-touch-icon-precomposed", sizes="114x114", href="https://www.epa.gov/themes/epa_theme/images/favicon-114.png"),
      tags$link(rel="apple-touch-icon-precomposed", sizes="72x72",   href="https://www.epa.gov/themes/epa_theme/images/favicon-72.png"),
      tags$link(rel="apple-touch-icon-precomposed",                  href="https://www.epa.gov/themes/epa_theme/images/favicon-180.png"),

      ## Fonts/ Themes ####

      tags$link(rel="preload", href="https://www.epa.gov/themes/epa_theme/fonts/source-sans-pro/sourcesanspro-regular-webfont.woff2", as="font", crossorigin="anonymous"),
      tags$link(rel="preload", href="https://www.epa.gov/themes/epa_theme/fonts/source-sans-pro/sourcesanspro-bold-webfont.woff2", as="font", crossorigin="anonymous"),
      tags$link(rel="preload", href="https://www.epa.gov/themes/epa_theme/fonts/merriweather/Latin-Merriweather-Bold.woff2", as="font", crossorigin="anonymous"),
      tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/ajax-progress.module.css?r6lsex"),
      tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/autocomplete-loading.module.css?r6lsex" ),
      tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/js.module.css?r6lsex"),
      tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/sticky-header.module.css?r6lsex"),
      tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/system-status-counter.css?r6lsex"),
      tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/system-status-report-counters.css?r6lsex"),
      tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/system-status-report-general-info.css?r6lsex"),
      tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/tabledrag.module.css?r6lsex"),
      tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/tablesort.module.css?r6lsex"),
      tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/tree-child.module.css?r6lsex"),
      tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/themes/epa_theme/css/styles.css?r6lsex"),
      tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/themes/epa_theme/css-lib/colorbox.min.css?r6lsex"),

      ## cloudflare script and container-fluid ####

      tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/js/uswds-init.min.js'),

      #fix container-fluid that boostrap RShiny uses
      tags$style(HTML(
        '.container-fluid {
              padding-right: 0;
              padding-left: 0;
              padding-bottom: 0;
              padding-top: 0;
              margin-right: 0;
              margin-left: 0;
              margin-bottom: 0;
              margin-top: 0
              }
          .tab-content {
              margin-right: 30px;
              margin-left: 30px;
          }'
      ))
    ),  # end of head
    ######################################################################## #

    # body ####

    ## >> APP TITLE in Header/ Body tag ####

    ### cloudflare script ####
    tags$body(
      class = "path-themes not-front has-wide-template", id = "top",
      tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/js/uswds.min.js')
    ),

    ######################################################################## #
    ## THIN HEADER ROW/ TITLE ####

    if (!global_defaults_shiny$default_show_full_header_footer) {

      HTML(paste0('
     <div class="container-fluid" style="border-spacing: 0; margin: 0; padding-bottom: 0; border: 0;
     border-right-width: 0px; font-size:24px; ";>

  <div id="ejamheader">

    <table width="100%" style="margin-bottom: 0px; margin-top: 0px";><tbody>
      <tr style="font-size:24px margin-bottom: 0px; margin-top: 0px";>

        <td  valign="top" style=
          "border-bottom-color: #ffffff; border-top-color: #ffffff; border-left-color: #ffffff; border-right-color: #ffffff;
          margin-bottom: 0px; margin-top: 0px; margin-left: 0px; margin-right: 0px;
          padding-bottom: 0px; padding-top: 0px; padding-left: 0px; padding-right: 0px">

',
                  app_header_logo_html
                  ,'
        </td>

        <td valign="bottom" style="line-height:34px; padding: 0px;
          border-bottom-color: #ffffff; border-top-color: #ffffff; border-left-color: #ffffff; border-right-color: #ffffff";
          vertical-align: bottom;>

                <span style="font-size: 15pt; font-weight:700; font-family:Arial";>',   # larger font for app title

                  app_title,

                  '</span>',

                  '<span style="font-size: 10pt; font-weight:700; font-family:Arial";>',  # smaller font for version info

                  .app_version_headertext,

                  '</span>',
                  '
        </td>',

                  ### > links ####
                  # could adjust which of the links here get shown in the header, depending on  isTRUE(golem_opts$isPublic)
                  '
        <td valign="bottom" align="right";  style="line-height:34px; padding: 0px;
                border-bottom-color: #ffffff; border-top-color: #ffffff; border-left-color: #ffffff; border-right-color: #ffffff";>
          <span id="homelinks">

  <!--           <a href="https://www.epa.gov/ejscreen" alt="Go to EJScreen home page" title="Go to EJScreen home page" target="_blank">EJScreen Website</a> |   -->
  <!--           <a href="https://ejscreen.epa.gov/mapper/" alt="Go to EJScreen mapper"    title="Go to EJScreen mapper" target="_blank">Mapper</a> |   -->
              <a href="https://web.archive.org/web/20250118193121/https://www.epa.gov/ejscreen/overview-socioeconomic-indicators-ejscreen" alt="Go to glossary page" title="Go to EJScreen glossary page" target="_blank">Glossary</a> |
            <a href="www/user-guide-2025-02.pdf" alt="Go to help document" title="Go to help document" target="_blank">Help</a> |
  <!--           <a href="mailto:ENVIROMAIL_GROUP@epa.gov?subject=EJAM%20Multisite%20Tool%20Question" id="emailLink" alt="Contact Us" title="Contact Us">Contact Us</a>   -->
          </span>&nbsp;&nbsp;
        </td>
 ',
                  '
      </tr>
    </tbody></table>

  </div>

</div>
     ',
                  ########################################################################## #

                  #HTML(
                  '<div class="l-page  has-footer" style="padding-top:0">
        <div class="l-constrain">

 '
      ))
      ########################################################################## #
      ########################################################################## #

    } else {

      ## (OPTIONAL LARGER EPA HEADER) ####

      # To display the full header, html_header_fmt can be set to NULL or an empty tagList
      HTML(
        '<div class="skiplinks" role="navigation" aria-labelledby="skip-to-main">
            <a id="skip-to-main" href="#main" class="skiplinks__link visually-hidden focusable">Skip to main content</a>
         </div>

      	<!-- Google Tag Manager (noscript) -->
      	<noscript><iframe src=https://www.googletagmanager.com/ns.html?id=GTM-L8ZB
      	height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript>
      	<!-- End Google Tag Manager (noscript) -->

          <div class="dialog-off-canvas-main-canvas" data-off-canvas-main-canvas>

          <section class="usa-banner" aria-label="Official government website">

            <div class="usa-accordion">

              <header class="usa-banner__header">
                <div class="usa-banner__inner">
                  <div class="grid-col-auto">
                    <img class="usa-banner__header-flag" src="https://www.epa.gov/themes/epa_theme/images/us_flag_small.png" alt="U.S. flag" />
                  </div>
                  <div class="grid-col-fill tablet:grid-col-auto">
                    <p class="usa-banner__header-text">An official website of the United States government</p>
                    <p class="usa-banner__header-action" aria-hidden="true">Here’s how you know</p>
                  </div>
                  <button class="usa-accordion__button usa-banner__button" aria-expanded="false" aria-controls="gov-banner">
                    <span class="usa-banner__button-text">Here’s how you know</span>
                  </button>
                </div>
              </header>

              <div class="usa-banner__content usa-accordion__content" id="gov-banner">
                <div class="grid-row grid-gap-lg">
                  <div class="usa-banner__guidance tablet:grid-col-6">
                    <img class="usa-banner__icon usa-media-block__img" src="https://www.epa.gov/themes/epa_theme/images/icon-dot-gov.svg" alt="Dot gov">
                    <div class="usa-media-block__body">
                      <p>
                        <strong>Official websites use .gov</strong>
                        <br> A <strong>.gov</strong> website belongs to an official government organization in the United States.
                      </p>
                    </div>
                  </div>
                  <div class="usa-banner__guidance tablet:grid-col-6">
                    <img class="usa-banner__icon usa-media-block__img" src="https://www.epa.gov/themes/epa_theme/images/icon-https.svg" alt="HTTPS">
                    <div class="usa-media-block__body">
                      <p>
                        <strong>Secure .gov websites use HTTPS</strong>
                        <br> A <strong>lock</strong> (<span class="icon-lock"><svg xmlns="http://www.w3.org/2000/svg" width="52" height="64" viewBox="0 0 52 64" class="usa-banner__lock-image" role="img" aria-labelledby="banner-lock-title banner-lock-description"><title id="banner-lock-title">Lock</title><desc id="banner-lock-description">A locked padlock</desc><path fill="#000000" fill-rule="evenodd" d="M26 0c10.493 0 19 8.507 19 19v9h3a4 4 0 0 1 4 4v28a4 4 0 0 1-4 4H4a4 4 0 0 1-4-4V32a4 4 0 0 1 4-4h3v-9C7 8.507 15.507 0 26 0zm0 8c-5.979 0-10.843 4.77-10.996 10.712L15 19v9h22v-9c0-6.075-4.925-11-11-11z"/></svg></span>) or <strong>https://</strong> means you’ve safely connected to the .gov website. Share sensitive information only on official, secure websites.
                      </p>
                    </div>
                  </div>
                </div>
              </div>

            </div>

          </section>

          <div>
            <div class="js-view-dom-id-epa-alerts--public">
              <noscript>
                <div class="usa-site-alert usa-site-alert--info">
                  <div class="usa-alert">
                    <div class="usa-alert__body">
                      <div class="usa-alert__text">
                        <p>JavaScript appears to be disabled on this computer. Please <a href="/alerts">click here to see any active alerts</a>.</p>
                      </div>
                    </div>
                  </div>
                </div>
              </noscript>
            </div>
          </div>

          <header class="l-header">

            <div class="usa-overlay"></div>
            <div class="l-constrain">

              <div class="l-header__navbar">

                <div class="l-header__branding">
                  <a class="site-logo" href="/" aria-label="Home" title="Home" rel="home">
                    <span class="site-logo__image">
                      <svg class="site-logo__svg" viewBox="0 0 1061 147" aria-hidden="true" xmlns="http://www.w3.org/2000/svg">
                        <path d="M112.8 53.5C108 72.1 89.9 86.8 69.9 86.8c-20.1 0-38-14.7-42.9-33.4h.2s9.8 10.3-.2 0c3.1 3.1 6.2 4.4 10.7 4.4s7.7-1.3 10.7-4.4c3.1 3.1 6.3 4.5 10.9 4.4 4.5 0 7.6-1.3 10.7-4.4 3.1 3.1 6.2 4.4 10.7 4.4 4.5 0 7.7-1.3 10.7-4.4 3.1 3.1 6.3 4.5 10.9 4.4 4.3 0 7.4-1.2 10.5-4.3zM113.2 43.5c0-24-19.4-43.5-43.3-43.5-24 0-43.5 19.5-43.5 43.5h39.1c-4.8-1.8-8.1-6.3-8.1-11.6 0-7 5.7-12.5 12.5-12.5 7 0 12.7 5.5 12.7 12.5 0 5.2-3.1 9.6-7.6 11.6h38.2zM72.6 139.3c.7-36.9 29.7-68.8 66.9-70 0 37.2-30 68-66.9 70zM67.1 139.3c-.7-36.9-29.7-68.8-67.1-70 0 37.2 30.2 68 67.1 70zM240 3.1h-87.9v133.1H240v-20.4h-60.3v-36H240v-21h-60.3v-35H240V3.1zM272.8 58.8h27.1c9.1 0 15.2-8.6 15.1-17.7-.1-9-6.1-17.3-15.1-17.3h-25.3v112.4h-27.8V3.1h62.3c20.2 0 35 17.8 35.2 38 .2 20.4-14.8 38.7-35.2 38.7h-36.3v-21zM315.9 136.2h29.7l12.9-35h54.2l-8.1-21.9h-38.4l18.9-50.7 39.2 107.6H454L400.9 3.1h-33.7l-51.3 133.1zM473.3.8v22.4c0 1.9.2 3.3.5 4.3s.7 1.7 1 2.2c1.2 1.4 2.5 2.4 3.9 2.9 1.5.5 2.8.7 4.1.7 2.4 0 4.2-.4 5.5-1.3 1.3-.8 2.2-1.8 2.8-2.9.6-1.1.9-2.3 1-3.4.1-1.1.1-2 .1-2.6V.8h4.7v24c0 .7-.1 1.5-.4 2.4-.3 1.8-1.2 3.6-2.5 5.4-1.8 2.1-3.8 3.5-6 4.2-2.2.6-4 .9-5.3.9-1.8 0-3.8-.3-6.2-1.1-2.4-.8-4.5-2.3-6.2-4.7-.5-.8-1-1.8-1.4-3.2-.4-1.3-.6-3.3-.6-5.9V.8h5zM507.5 14.5v-2.9l4.6.1-.1 4.1c.2-.3.4-.7.8-1.2.3-.5.8-.9 1.4-1.4.6-.5 1.4-.9 2.3-1.3.9-.3 2.1-.5 3.4-.4.6 0 1.4.1 2.4.3.9.2 1.9.6 2.9 1.2s1.8 1.5 2.4 2.6c.6 1.2.9 2.8.9 4.7l-.4 17-4.6-.1.4-16c0-.9 0-1.7-.2-2.4-.1-.7-.5-1.3-1.1-1.9-1.2-1.2-2.6-1.8-4.3-1.8-1.7 0-3.1.5-4.4 1.7-1.3 1.2-2 3.1-2.1 5.7l-.3 14.5-4.5-.1.5-22.4zM537.2.9h5.5V6h-5.5V.9m.5 10.9h4.6v25.1h-4.6V11.8zM547.8 11.7h4.3V6.4l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V37c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V15.1h-4.3v-3.4zM570.9 25.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.9-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.6h13zM612.9.9h4.6V33c0 1 .1 2.3.2 4h-4.6l-.1-4c-.2.3-.4.7-.7 1.2-.3.5-.8 1-1.4 1.5-1 .7-2 1.2-3.1 1.4l-1.5.3c-.5.1-.9.1-1.4.1-.4 0-.8 0-1.3-.1s-1.1-.2-1.7-.3c-1.1-.3-2.3-.9-3.4-1.8s-2.1-2.2-2.9-3.8c-.8-1.7-1.2-3.9-1.2-6.6.1-4.8 1.2-8.3 3.4-10.5 2.1-2.1 4.7-3.2 7.6-3.2 1.3 0 2.4.2 3.4.5.9.3 1.6.7 2.2 1.2.6.4 1 .9 1.3 1.4.3.5.6.8.7 1.1V.9m0 23.1c0-1.9-.2-3.3-.5-4.4-.4-1.1-.8-2-1.4-2.6-.5-.7-1.2-1.3-2-1.8-.9-.5-2-.7-3.3-.7-1.7 0-2.9.5-3.8 1.3-.9.8-1.6 1.9-2 3.1-.4 1.2-.7 2.3-.7 3.4-.1 1.1-.2 1.9-.1 2.4 0 1.1.1 2.2.3 3.4.2 1.1.5 2.2 1 3.1.5 1 1.2 1.7 2 2.3.9.6 2 .9 3.3.9 1.8 0 3.2-.5 4.2-1.4 1-.8 1.7-1.8 2.1-3 .4-1.2.7-2.4.8-3.4.1-1.4.1-2.1.1-2.6zM643.9 26.4c0 .6.1 1.3.3 2.1.1.8.5 1.6 1 2.3.5.8 1.4 1.4 2.5 1.9s2.7.8 4.7.8c1.8 0 3.3-.3 4.4-.8 1.1-.5 1.9-1.1 2.5-1.8.6-.7 1-1.5 1.1-2.2.1-.7.2-1.2.2-1.7 0-1-.2-1.9-.5-2.6-.4-.6-.9-1.2-1.6-1.6-1.4-.8-3.4-1.4-5.9-2-4.9-1.1-8.1-2.2-9.5-3.2-1.4-1-2.3-2.2-2.9-3.5-.6-1.2-.8-2.4-.8-3.6.1-3.7 1.5-6.4 4.2-8.1 2.6-1.7 5.7-2.5 9.1-2.5 1.3 0 2.9.2 4.8.5 1.9.4 3.6 1.4 5 3 .5.5.9 1.1 1.2 1.7.3.5.5 1.1.6 1.6.2 1.1.3 2.1.3 2.9h-5c-.2-2.2-1-3.7-2.4-4.5-1.5-.7-3.1-1.1-4.9-1.1-5.1.1-7.7 2-7.8 5.8 0 1.5.5 2.7 1.6 3.5 1 .8 2.6 1.4 4.7 1.9 4 1 6.7 1.8 8.1 2.2.8.2 1.4.5 1.8.7.5.2 1 .5 1.4.9.8.5 1.4 1.1 1.9 1.8s.8 1.4 1.1 2.1c.3 1.4.5 2.5.5 3.4 0 3.3-1.2 6-3.5 8-2.3 2.1-5.8 3.2-10.3 3.3-1.4 0-3.2-.3-5.4-.8-1-.3-2-.7-3-1.2-.9-.5-1.8-1.2-2.5-2.1-.9-1.4-1.5-2.7-1.7-4.1-.3-1.3-.4-2.4-.3-3.2h5zM670 11.7h4.3V6.4l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V37c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V15.1H670v-3.4zM705.3 36.9c-.3-1.2-.5-2.5-.4-3.7-.5 1-1.1 1.8-1.7 2.4-.7.6-1.4 1.1-2 1.4-1.4.5-2.7.8-3.7.8-2.8 0-4.9-.8-6.4-2.2-1.5-1.4-2.2-3.1-2.2-5.2 0-1 .2-2.3.8-3.7.6-1.4 1.7-2.6 3.5-3.7 1.4-.7 2.9-1.2 4.5-1.5 1.6-.1 2.9-.2 3.9-.2s2.1 0 3.3.1c.1-2.9-.2-4.8-.9-5.6-.5-.6-1.1-1.1-1.9-1.3-.8-.2-1.6-.4-2.3-.4-1.1 0-2 .2-2.6.5-.7.3-1.2.7-1.5 1.2-.3.5-.5.9-.6 1.4-.1.5-.2.9-.2 1.2h-4.6c.1-.7.2-1.4.4-2.3.2-.8.6-1.6 1.3-2.5.5-.6 1-1 1.7-1.3.6-.3 1.3-.6 2-.8 1.5-.4 2.8-.6 4.2-.6 1.8 0 3.6.3 5.2.9 1.6.6 2.8 1.6 3.4 2.9.4.7.6 1.4.7 2 .1.6.1 1.2.1 1.8l-.2 12c0 1 .1 3.1.4 6.3h-4.2m-.5-12.1c-.7-.1-1.6-.1-2.6-.1h-2.1c-1 .1-2 .3-3 .6s-1.9.8-2.6 1.5c-.8.7-1.2 1.7-1.2 3 0 .4.1.8.2 1.3s.4 1 .8 1.5.9.8 1.6 1.1c.7.3 1.5.5 2.5.5 2.3 0 4.1-.9 5.2-2.7.5-.8.8-1.7 1-2.7.1-.9.2-2.2.2-4zM714.5 11.7h4.3V6.4l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V37c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V15.1h-4.3v-3.4zM737.6 25.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.9-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.6h13zM765.3 29.5c0 .5.1 1 .2 1.4.1.5.4 1 .8 1.5s.9.8 1.6 1.1c.7.3 1.6.5 2.7.5 1 0 1.8-.1 2.5-.3.7-.2 1.3-.6 1.7-1.2.5-.7.8-1.5.8-2.4 0-1.2-.4-2-1.3-2.5s-2.2-.9-4.1-1.2c-1.3-.3-2.4-.6-3.6-1-1.1-.3-2.1-.8-3-1.3-.9-.5-1.5-1.2-2-2.1-.5-.8-.8-1.9-.8-3.2 0-2.4.9-4.2 2.6-5.6 1.7-1.3 4-2 6.8-2.1 1.6 0 3.3.3 5 .8 1.7.6 2.9 1.6 3.7 3.1.4 1.4.6 2.6.6 3.7h-4.6c0-1.8-.6-3-1.7-3.5-1.1-.4-2.1-.6-3.1-.6h-1c-.5 0-1.1.2-1.7.4-.6.2-1.1.5-1.5 1.1-.5.5-.7 1.2-.7 2.1 0 1.1.5 1.9 1.3 2.3.7.4 1.5.7 2.1.9 3.3.7 5.6 1.3 6.9 1.8 1.3.4 2.2 1 2.8 1.7.7.7 1.1 1.4 1.4 2.2.3.8.4 1.6.4 2.5 0 1.4-.3 2.7-.9 3.8-.6 1.1-1.4 2-2.4 2.6-1.1.6-2.2 1-3.4 1.3-1.2.3-2.5.4-3.8.4-2.5 0-4.7-.6-6.6-1.8-1.8-1.2-2.8-3.3-2.9-6.3h5.2zM467.7 50.8h21.9V55h-17.1v11.3h16.3v4.2h-16.3v12.1H490v4.3h-22.3zM499 64.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V87h-4.6V71c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V87H499V64.7zM524.6 61.8h5.1l7.7 19.9 7.6-19.9h5l-10.6 25.1h-4.6zM555.7 50.9h5.5V56h-5.5v-5.1m.5 10.9h4.6v25.1h-4.6V61.8zM570.3 67c0-1.8-.1-3.5-.3-5.1h4.6l.1 4.9c.5-1.8 1.4-3 2.5-3.7 1.1-.7 2.2-1.2 3.3-1.3 1.4-.2 2.4-.2 3.1-.1v4.6c-.2-.1-.5-.2-.9-.2h-1.3c-1.3 0-2.4.2-3.3.5-.9.4-1.5.9-2 1.6-.9 1.4-1.4 3.2-1.3 5.4v13.3h-4.6V67zM587.6 74.7c0-1.6.2-3.2.6-4.8.4-1.6 1.1-3 2-4.4 1-1.3 2.2-2.4 3.8-3.2 1.6-.8 3.6-1.2 5.9-1.2 2.4 0 4.5.4 6.1 1.3 1.5.9 2.7 2 3.6 3.3.9 1.3 1.5 2.8 1.8 4.3.2.8.3 1.5.4 2.2v2.2c0 3.7-1 6.9-3 9.5-2 2.6-5.1 4-9.3 4-4-.1-7-1.4-9-3.9-1.9-2.5-2.9-5.6-2.9-9.3m4.8-.3c0 2.7.6 5 1.8 6.9 1.2 2 3 3 5.6 3.1.9 0 1.8-.2 2.7-.5.8-.3 1.6-.9 2.3-1.7.7-.8 1.3-1.9 1.8-3.2.4-1.3.6-2.9.6-4.7-.1-6.4-2.5-9.6-7.1-9.6-.7 0-1.5.1-2.4.3-.8.3-1.7.8-2.5 1.6-.8.7-1.4 1.7-1.9 3-.6 1.1-.9 2.8-.9 4.8zM620.2 64.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V87h-4.6V71c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V87h-4.6V64.7zM650 65.1l-.1-3.3h4.6v3.6c1.2-1.9 2.6-3.2 4.1-3.7 1.5-.4 2.7-.6 3.8-.6 1.4 0 2.6.2 3.6.5.9.3 1.7.7 2.3 1.1 1.1 1 1.9 2 2.3 3.1.2-.4.5-.8 1-1.3.4-.5.9-1 1.5-1.6.6-.5 1.5-.9 2.5-1.3 1-.3 2.2-.5 3.5-.5.9 0 1.9.1 3 .3 1 .2 2 .7 3 1.3 1 .6 1.7 1.5 2.3 2.7.6 1.2.9 2.7.9 4.6v16.9h-4.6V70.7c0-1.1-.1-2-.2-2.5-.1-.6-.3-1-.6-1.3-.4-.6-1-1.2-1.8-1.6-.8-.4-1.8-.6-3.1-.6-1.5 0-2.7.4-3.6 1-.4.3-.8.5-1.1.9l-.8.8c-.5.8-.8 1.8-1 2.8-.1 1.1-.2 2-.1 2.6v14.1h-4.6V70.2c0-1.6-.5-2.9-1.4-4-.9-1-2.3-1.5-4.2-1.5-1.6 0-2.9.4-3.8 1.1-.9.7-1.5 1.2-1.8 1.7-.5.7-.8 1.5-.9 2.5-.1.9-.2 1.8-.2 2.6v14.3H650V65.1zM700.5 75.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.8-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.6h13zM725.7 64.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V87h-4.6V71c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V87h-4.6V64.7zM752.3 61.7h4.3v-5.2l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V87c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V65.1h-4.3v-3.4zM787.6 86.9c-.3-1.2-.5-2.5-.4-3.7-.5 1-1.1 1.8-1.7 2.4-.7.6-1.4 1.1-2 1.4-1.4.5-2.7.8-3.7.8-2.8 0-4.9-.8-6.4-2.2-1.5-1.4-2.2-3.1-2.2-5.2 0-1 .2-2.3.8-3.7.6-1.4 1.7-2.6 3.5-3.7 1.4-.7 2.9-1.2 4.5-1.5 1.6-.1 2.9-.2 3.9-.2s2.1 0 3.3.1c.1-2.9-.2-4.8-.9-5.6-.5-.6-1.1-1.1-1.9-1.3-.8-.2-1.6-.4-2.3-.4-1.1 0-2 .2-2.6.5-.7.3-1.2.7-1.5 1.2-.3.5-.5.9-.6 1.4-.1.5-.2.9-.2 1.2h-4.6c.1-.7.2-1.4.4-2.3.2-.8.6-1.6 1.3-2.5.5-.6 1-1 1.7-1.3.6-.3 1.3-.6 2-.8 1.5-.4 2.8-.6 4.2-.6 1.8 0 3.6.3 5.2.9 1.6.6 2.8 1.6 3.4 2.9.4.7.6 1.4.7 2 .1.6.1 1.2.1 1.8l-.2 12c0 1 .1 3.1.4 6.3h-4.2m-.5-12.1c-.7-.1-1.6-.1-2.6-.1h-2.1c-1 .1-2 .3-3 .6s-1.9.8-2.6 1.5c-.8.7-1.2 1.7-1.2 3 0 .4.1.8.2 1.3s.4 1 .8 1.5.9.8 1.6 1.1c.7.3 1.5.5 2.5.5 2.3 0 4.1-.9 5.2-2.7.5-.8.8-1.7 1-2.7.1-.9.2-2.2.2-4zM800.7 50.9h4.6V87h-4.6zM828.4 50.8h11.7c2.1 0 3.9.1 5.5.4.8.2 1.5.4 2.2.9.7.4 1.3.9 1.8 1.6 1.7 1.9 2.6 4.2 2.6 7 0 2.7-.9 5.1-2.8 7.1-.8.9-2 1.7-3.6 2.2-1.6.6-3.9.9-6.9.9h-5.7V87h-4.8V50.8m4.8 15.9h5.8c.8 0 1.7-.1 2.6-.2.9-.1 1.8-.3 2.6-.7.8-.4 1.5-1 2-1.9.5-.8.8-2 .8-3.4s-.2-2.5-.7-3.3c-.5-.8-1.1-1.3-1.9-1.7-1.6-.5-3.1-.8-4.5-.7h-6.8v11.9zM858.1 67c0-1.8-.1-3.5-.3-5.1h4.6l.1 4.9c.5-1.8 1.4-3 2.5-3.7 1.1-.7 2.2-1.2 3.3-1.3 1.4-.2 2.4-.2 3.1-.1v4.6c-.2-.1-.5-.2-.9-.2h-1.3c-1.3 0-2.4.2-3.3.5-.9.4-1.5.9-2 1.6-.9 1.4-1.4 3.2-1.3 5.4v13.3H858V67zM875.5 74.7c0-1.6.2-3.2.6-4.8.4-1.6 1.1-3 2-4.4 1-1.3 2.2-2.4 3.8-3.2 1.6-.8 3.6-1.2 5.9-1.2 2.4 0 4.5.4 6.1 1.3 1.5.9 2.7 2 3.6 3.3.9 1.3 1.5 2.8 1.8 4.3.2.8.3 1.5.4 2.2v2.2c0 3.7-1 6.9-3 9.5-2 2.6-5.1 4-9.3 4-4-.1-7-1.4-9-3.9-1.9-2.5-2.9-5.6-2.9-9.3m4.8-.3c0 2.7.6 5 1.8 6.9 1.2 2 3 3 5.6 3.1.9 0 1.8-.2 2.7-.5.8-.3 1.6-.9 2.3-1.7.7-.8 1.3-1.9 1.8-3.2.4-1.3.6-2.9.6-4.7-.1-6.4-2.5-9.6-7.1-9.6-.7 0-1.5.1-2.4.3-.8.3-1.7.8-2.5 1.6-.8.7-1.4 1.7-1.9 3-.7 1.1-.9 2.8-.9 4.8zM904.1 61.7h4.3v-5.2l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V87c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V65.1h-4.3v-3.4zM927.2 75.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.9-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.6h13zM966.1 69.8c0-.3 0-.8-.1-1.4-.1-.6-.3-1.1-.6-1.8-.2-.6-.7-1.2-1.4-1.6-.7-.4-1.6-.6-2.7-.6-1.5 0-2.7.4-3.5 1.2-.9.8-1.5 1.7-1.9 2.8-.4 1.1-.6 2.2-.7 3.2-.1 1.1-.2 1.8-.1 2.4 0 1.3.1 2.5.3 3.7.2 1.2.5 2.3.9 3.3.8 2 2.4 3 4.8 3.1 1.9 0 3.3-.7 4.1-1.9.8-1.1 1.2-2.3 1.2-3.6h4.6c-.2 2.5-1.1 4.6-2.7 6.3-1.7 1.8-4.1 2.7-7.1 2.7-.9 0-2.1-.2-3.6-.6-.7-.2-1.4-.6-2.2-1-.8-.4-1.5-1-2.2-1.7-.7-.9-1.4-2.1-2-3.6-.6-1.5-.9-3.5-.9-6.1 0-2.6.4-4.8 1.1-6.6.7-1.7 1.6-3.1 2.7-4.2 1.1-1 2.3-1.8 3.6-2.2 1.3-.4 2.5-.6 3.7-.6h1.6c.6.1 1.3.2 1.9.4.7.2 1.4.5 2.1 1 .7.4 1.3 1 1.8 1.7.9 1.1 1.4 2.1 1.7 3.1.2 1 .3 1.8.3 2.6h-4.7zM973.6 61.7h4.3v-5.2l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V87c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V65.1h-4.3v-3.4zM993.5 50.9h5.5V56h-5.5v-5.1m.5 10.9h4.6v25.1H994V61.8zM1006.1 74.7c0-1.6.2-3.2.6-4.8.4-1.6 1.1-3 2-4.4 1-1.3 2.2-2.4 3.8-3.2 1.6-.8 3.6-1.2 5.9-1.2 2.4 0 4.5.4 6.1 1.3 1.5.9 2.7 2 3.6 3.3.9 1.3 1.5 2.8 1.8 4.3.2.8.3 1.5.4 2.2v2.2c0 3.7-1 6.9-3 9.5-2 2.6-5.1 4-9.3 4-4-.1-7-1.4-9-3.9-1.9-2.5-2.9-5.6-2.9-9.3m4.7-.3c0 2.7.6 5 1.8 6.9 1.2 2 3 3 5.6 3.1.9 0 1.8-.2 2.7-.5.8-.3 1.6-.9 2.3-1.7.7-.8 1.3-1.9 1.8-3.2.4-1.3.6-2.9.6-4.7-.1-6.4-2.5-9.6-7.1-9.6-.7 0-1.5.1-2.4.3-.8.3-1.7.8-2.5 1.6-.8.7-1.4 1.7-1.9 3-.6 1.1-.9 2.8-.9 4.8zM1038.6 64.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V87h-4.6V71c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V87h-4.6V64.7zM479.1 100.8h5.2l14.1 36.1h-5.3l-3.8-9.4h-16.2l-3.8 9.4h-5l14.8-36.1m-4.4 22.7H488l-6.5-17.8-6.8 17.8zM508.7 138.8c.1.7.2 1.4.4 1.9.2.6.5 1.1.9 1.6.8.9 2.3 1.4 4.4 1.5 1.6 0 2.8-.3 3.7-.9.9-.6 1.5-1.4 1.9-2.4.4-1.1.6-2.3.7-3.7.1-1.4.1-2.9.1-4.6-.5.9-1.1 1.7-1.8 2.3-.7.6-1.5 1-2.3 1.3-1.7.4-3 .6-3.9.6-1.2 0-2.4-.2-3.8-.6-1.4-.4-2.6-1.2-3.7-2.5-1-1.3-1.7-2.8-2.1-4.4-.4-1.6-.6-3.2-.6-4.8 0-4.3 1.1-7.4 3.2-9.5 2-2.1 4.6-3.1 7.6-3.1 1.3 0 2.3.1 3.2.4.9.3 1.6.6 2.1 1 .6.4 1.1.8 1.5 1.2l.9 1.2v-3.4h4.4l-.1 4.5v15.7c0 2.9-.1 5.2-.2 6.7-.2 1.6-.5 2.8-1 3.7-1.1 1.9-2.6 3.2-4.6 3.7-1.9.6-3.8.8-5.6.8-2.4 0-4.3-.3-5.6-.8-1.4-.5-2.4-1.2-3-2-.6-.8-1-1.7-1.2-2.7-.2-.9-.3-1.8-.4-2.7h4.9m5.3-5.8c1.4 0 2.5-.2 3.3-.7.8-.5 1.5-1.1 2-1.8.5-.6.9-1.4 1.2-2.5.3-1 .4-2.6.4-4.8 0-1.6-.2-2.9-.4-3.9-.3-1-.8-1.8-1.4-2.4-1.3-1.4-3-2.2-5.2-2.2-1.4 0-2.5.3-3.4 1-.9.7-1.6 1.5-2 2.4-.4 1-.7 2-.9 3-.2 1-.2 2-.2 2.8 0 1 .1 1.9.3 2.9.2 1.1.5 2.1 1 3 .5.9 1.2 1.6 2 2.2.8.7 1.9 1 3.3 1zM537.6 125.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.9-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2.1c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.7h13zM562.9 114.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V137h-4.6v-16c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V137h-4.6v-22.3zM607 119.8c0-.3 0-.8-.1-1.4-.1-.6-.3-1.1-.6-1.8-.2-.6-.7-1.2-1.4-1.6-.7-.4-1.6-.6-2.7-.6-1.5 0-2.7.4-3.5 1.2-.9.8-1.5 1.7-1.9 2.8-.4 1.1-.6 2.2-.7 3.2-.1 1.1-.2 1.8-.1 2.4 0 1.3.1 2.5.3 3.7.2 1.2.5 2.3.9 3.3.8 2 2.4 3 4.8 3.1 1.9 0 3.3-.7 4.1-1.9.8-1.1 1.2-2.3 1.2-3.6h4.6c-.2 2.5-1.1 4.6-2.7 6.3-1.7 1.8-4.1 2.7-7.1 2.7-.9 0-2.1-.2-3.6-.6-.7-.2-1.4-.6-2.2-1-.8-.4-1.5-1-2.2-1.7-.7-.9-1.4-2.1-2-3.6-.6-1.5-.9-3.5-.9-6.1 0-2.6.4-4.8 1.1-6.6.7-1.7 1.6-3.1 2.7-4.2 1.1-1 2.3-1.8 3.6-2.2 1.3-.4 2.5-.6 3.7-.6h1.6c.6.1 1.3.2 1.9.4.7.2 1.4.5 2.1 1 .7.4 1.3 1 1.8 1.7.9 1.1 1.4 2.1 1.7 3.1.2 1 .3 1.8.3 2.6H607zM629.1 137.1l-3.4 9.3H621l3.8-9.6-10.3-25h5.2l7.6 19.8 7.7-19.8h5z"/>
                      </svg>
                    </span>
                  </a>
                  <button class="usa-menu-btn usa-button l-header__menu-button">Menu</button>
                </div>

                <div class="l-header__search">
                  <form class="usa-search usa-search--small usa-search--epa" method="get" action="https://search.epa.gov/epasearch">
                    <div role="search">
                      <label class="usa-sr-only" for="search-box">Search</label>
                      <input class="usa-input" id="search-box" type="search" name="querytext" placeholder="Search EPA.gov">

                      <button class="usa-button usa-search__submit" style="height:2rem;margin:0;padding:0;padding-left:1rem;padding-right:1rem;border-top-left-radius: 0;border-bottom-left-radius: 0;">
                        <span class="usa-sr-only">Search</span>
                      </button>
                      <input type="hidden" name="areaname" value="">
                      <input type="hidden" name="areacontacts" value="">
                      <input type="hidden" name="areasearchurl" value="">
                      <input type="hidden" name="typeofsearch" value="epa">
                      <input type="hidden" name="result_template" value="">
                    </div>
                  </form>
                </div>

              </div>

            </div>
            <div class="l-header__nav">
              <nav class="usa-nav usa-nav--epa" role="navigation" aria-label="EPA header navigation">
                <div class="usa-nav__inner">
                  <button class="usa-nav__close" aria-label="Close">
                    <svg class="icon icon--nav-close" aria-hidden="true" role="img">
                      <title>Primary navigation</title>
                      <use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#close"></use>
                    </svg> </button>
                  <div class="usa-nav__menu">
                     <ul class="menu menu--main">
                      <li class="menu__item"><a href="https://www.epa.gov/environmental-topics" class="menu__link">Environmental Topics</a></li>
                      <li class="menu__item"><a href="https://www.epa.gov/laws-regulations" class="menu__link" >Laws &amp; Regulations</a></li>
                      <li class="menu__item"><a href="https://www.epa.gov/report-violation" class="menu__link" >Report a Violation</a></li>
                      <li class="menu__item"><a href="https://www.epa.gov/aboutepa" class="menu__link" >About EPA</a></li>
                    </ul>
                  </div>
                </div>
              </nav>
            </div>
          </header>

          <main id="main" class="main" role="main" tabindex="-1">'

        #)    ,   #   comment  out when excluding html below

      ) # END OF   html_header_fmt()
      ########################################################################## #
      # end of large header
    }
  ), # end of header tag list
  # footer ####
  html_footer_fmt = tagList(
    if (!global_defaults_shiny$default_show_full_header_footer) {
      ## SMALL/NO FOOTER ####
      #
      HTML(
        '
      </div>

      <div class="l-page__footer">

      </div>

    </div>'
      )
    } else {
      ### (OPTIONAL LARGER FOOTER) ####
      HTML(
        '</main>
        <footer class="footer" role="contentinfo">
        <div class="l-constrain">
          <img class="footer__epa-seal" src="https://www.epa.gov/themes/epa_theme/images/epa-seal.svg" alt="United States Environmental Protection Agency" height="100" width="100">
          <div class="footer__content contextual-region">
            <div class="footer__column">
              <h2>Discover.</h2>
              <ul class="menu menu--footer">
                <li class="menu__item">
                  <a href="https://www.epa.gov/accessibility" class="menu__link">Accessibility</a>
                </li>
                <!--li class="menu__item"><a href="#" class="menu__link">EPA Administrator</a></li-->
                <li class="menu__item">
                  <a href="https://www.epa.gov/planandbudget" class="menu__link">Budget &amp; Performance</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/contracts" class="menu__link">Contracting</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/home/wwwepagov-snapshots" class="menu__link">EPA www Web Snapshot</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/grants" class="menu__link">Grants</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/ocr/whistleblower-protections-epa-and-how-they-relate-non-disclosure-agreements-signed-epa-employees" class="menu__link">No FEAR Act Data</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/web-policies-and-procedures/plain-writing" class="menu__link">Plain Writing</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/privacy" class="menu__link">Privacy</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/privacy/privacy-and-security-notice" class="menu__link">Privacy and Security Notice</a>
                </li>
              </ul>
            </div>
            <div class="footer__column">
              <h2>Connect.</h2>
              <ul class="menu menu--footer">
                <li class="menu__item">
                  <a href="https://www.data.gov/" class="menu__link">Data.gov</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/office-inspector-general/about-epas-office-inspector-general" class="menu__link">Inspector General</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/careers" class="menu__link">Jobs</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/newsroom" class="menu__link">Newsroom</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/data" class="menu__link">Open Government</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.regulations.gov/" class="menu__link">Regulations.gov</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/newsroom/email-subscriptions-epa-news-releases" class="menu__link">Subscribe</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.usa.gov/" class="menu__link">USA.gov</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.whitehouse.gov/" class="menu__link">White House</a>
                </li>
              </ul>
            </div>
            <div class="footer__column">
              <h2>Ask.</h2>
              <ul class="menu menu--footer">
                <li class="menu__item">
                  <a href="https://www.epa.gov/home/forms/contact-epa" class="menu__link">Contact EPA</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/web-policies-and-procedures/epa-disclaimers" class="menu__link">EPA Disclaimers</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/aboutepa/epa-hotlines" class="menu__link">Hotlines</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/foia" class="menu__link">FOIA Requests</a>
                </li>
                <li class="menu__item">
                  <a href="https://www.epa.gov/home/frequent-questions-specific-epa-programstopics" class="menu__link">Frequent Questions</a>
                </li>
              </ul>
              <h2>Follow.</h2>
              <ul class="menu menu--social">
                <li class="menu__item">
                  <a class="menu__link" aria-label="EPA’s Facebook" href="https://www.facebook.com/EPA">
                    <!-- svg class="icon icon--social" aria-hidden="true" -->
                    <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="facebook-square" xmlns="http://www.w3.org/2000/svg">
                      <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#facebook-square"></use-->
                      <path fill="currentcolor" d="M400 32H48A48 48 0 000 80v352a48 48 0 0048 48h137.25V327.69h-63V256h63v-54.64c0-62.15 37-96.48 93.67-96.48 27.14 0 55.52 4.84 55.52 4.84v61h-31.27c-30.81 0-40.42 19.12-40.42 38.73V256h68.78l-11 71.69h-57.78V480H400a48 48 0 0048-48V80a48 48 0 00-48-48z"></path>
                    </svg>
                    <span class="usa-tag external-link__tag" title="Exit EPA Website">
                      <span aria-hidden="true">Exit</span>
                      <span class="u-visually-hidden"> Exit EPA Website</span>
                    </span>
                  </a>
                </li>
                <li class="menu__item">
                  <a class="menu__link" aria-label="EPA’s Twitter" href="https://twitter.com/epa">
                    <!-- svg class="icon icon--social" aria-hidden="true" -->
                    <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="twitter-square" xmlns="http://www.w3.org/2000/svg">
                      <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#twitter-square"></use -->
                      <path fill="currentcolor" d="M400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48zm-48.9 158.8c.2 2.8.2 5.7.2 8.5 0 86.7-66 186.6-186.6 186.6-37.2 0-71.7-10.8-100.7-29.4 5.3.6 10.4.8 15.8.8 30.7 0 58.9-10.4 81.4-28-28.8-.6-53-19.5-61.3-45.5 10.1 1.5 19.2 1.5 29.6-1.2-30-6.1-52.5-32.5-52.5-64.4v-.8c8.7 4.9 18.9 7.9 29.6 8.3a65.447 65.447 0 01-29.2-54.6c0-12.2 3.2-23.4 8.9-33.1 32.3 39.8 80.8 65.8 135.2 68.6-9.3-44.5 24-80.6 64-80.6 18.9 0 35.9 7.9 47.9 20.7 14.8-2.8 29-8.3 41.6-15.8-4.9 15.2-15.2 28-28.8 36.1 13.2-1.4 26-5.1 37.8-10.2-8.9 13.1-20.1 24.7-32.9 34z"></path>
                    </svg>
                    <span class="usa-tag external-link__tag" title="Exit EPA Website">
                      <span aria-hidden="true">Exit</span>
                      <span class="u-visually-hidden"> Exit EPA Website</span>
                    </span>
                  </a>
                </li>
                <li class="menu__item">
                  <a class="menu__link" aria-label="EPA’s Youtube" href="https://www.youtube.com/user/USEPAgov">
                    <!-- svg class="icon icon--social" aria-hidden="true" -->
                    <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="youtube-square" xmlns="http://www.w3.org/2000/svg">
                      <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#youtube-square"></use -->
                      <path fill="currentcolor" d="M186.8 202.1l95.2 54.1-95.2 54.1V202.1zM448 80v352c0 26.5-21.5 48-48 48H48c-26.5 0-48-21.5-48-48V80c0-26.5 21.5-48 48-48h352c26.5 0 48 21.5 48 48zm-42 176.3s0-59.6-7.6-88.2c-4.2-15.8-16.5-28.2-32.2-32.4C337.9 128 224 128 224 128s-113.9 0-142.2 7.7c-15.7 4.2-28 16.6-32.2 32.4-7.6 28.5-7.6 88.2-7.6 88.2s0 59.6 7.6 88.2c4.2 15.8 16.5 27.7 32.2 31.9C110.1 384 224 384 224 384s113.9 0 142.2-7.7c15.7-4.2 28-16.1 32.2-31.9 7.6-28.5 7.6-88.1 7.6-88.1z"></path>
                    </svg>
                    <span class="usa-tag external-link__tag" title="Exit EPA Website">
                      <span aria-hidden="true">Exit</span>
                      <span class="u-visually-hidden"> Exit EPA Website</span>
                    </span>
                  </a>
                </li>
                <li class="menu__item">
                  <a class="menu__link" aria-label="EPA’s Flickr" href="https://www.flickr.com/photos/usepagov">
                    <!-- svg class="icon icon--social" aria-hidden="true" -->
                    <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="flickr-square" xmlns="http://www.w3.org/2000/svg">
                      <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#flickr-square"></use -->
                      <path fill="currentcolor" d="M400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48zM144.5 319c-35.1 0-63.5-28.4-63.5-63.5s28.4-63.5 63.5-63.5 63.5 28.4 63.5 63.5-28.4 63.5-63.5 63.5zm159 0c-35.1 0-63.5-28.4-63.5-63.5s28.4-63.5 63.5-63.5 63.5 28.4 63.5 63.5-28.4 63.5-63.5 63.5z"></path>
                    </svg>
                    <span class="usa-tag external-link__tag" title="Exit EPA Website">
                      <span aria-hidden="true">Exit</span>
                      <span class="u-visually-hidden"> Exit EPA Website</span>
                    </span>
                  </a>
                </li>
                <li class="menu__item">
                  <a class="menu__link" aria-label="EPA’s Instagram" href="https://www.instagram.com/epagov">
                    <!-- svg class="icon icon--social" aria-hidden="true" -->
                    <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="instagram-square" xmlns="http://www.w3.org/2000/svg">
                      <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#instagram-square"></use -->
                      <path fill="currentcolor" xmlns="http://www.w3.org/2000/svg" d="M224 202.66A53.34 53.34 0 10277.36 256 53.38 53.38 0 00224 202.66zm124.71-41a54 54 0 00-30.41-30.41c-21-8.29-71-6.43-94.3-6.43s-73.25-1.93-94.31 6.43a54 54 0 00-30.41 30.41c-8.28 21-6.43 71.05-6.43 94.33s-1.85 73.27 6.47 94.34a54 54 0 0030.41 30.41c21 8.29 71 6.43 94.31 6.43s73.24 1.93 94.3-6.43a54 54 0 0030.41-30.41c8.35-21 6.43-71.05 6.43-94.33s1.92-73.26-6.43-94.33zM224 338a82 82 0 1182-82 81.9 81.9 0 01-82 82zm85.38-148.3a19.14 19.14 0 1119.13-19.14 19.1 19.1 0 01-19.09 19.18zM400 32H48A48 48 0 000 80v352a48 48 0 0048 48h352a48 48 0 0048-48V80a48 48 0 00-48-48zm-17.12 290c-1.29 25.63-7.14 48.34-25.85 67s-41.4 24.63-67 25.85c-26.41 1.49-105.59 1.49-132 0-25.63-1.29-48.26-7.15-67-25.85s-24.63-41.42-25.85-67c-1.49-26.42-1.49-105.61 0-132 1.29-25.63 7.07-48.34 25.85-67s41.47-24.56 67-25.78c26.41-1.49 105.59-1.49 132 0 25.63 1.29 48.33 7.15 67 25.85s24.63 41.42 25.85 67.05c1.49 26.32 1.49 105.44 0 131.88z"></path>
                    </svg>
                    <span class="usa-tag external-link__tag" title="Exit EPA Website">
                      <span aria-hidden="true">Exit</span>
                      <span class="u-visually-hidden"> Exit EPA Website</span>
                    </span>
                  </a>
                </li>
              </ul>
              <p class="footer__last-updated">
                Last updated on March 30, 2022
              </p>
            </div>
          </div>
        </div>
      </footer>
      <a href="#" class="back-to-top" title="" aria-label="back-to-top">
        <svg class="back-to-top__icon" aria-label="">
        <svg class="back-to-top__icon" aria-label="" viewBox="0 0 19 12" id="arrow" xmlns="http://www.w3.org/2000/svg">
          <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#arrow"></use -->
          <path fill="currentColor" d="M2.3 12l7.5-7.5 7.5 7.5 2.3-2.3L9.9 0 .2 9.7 2.5 12z"></path>
        </svg>
      </a>'
      ) # end of large footer
    }
  )# end of footer tag list
)
# ------------------------ ____   _______ ####
# ~ ####

sanitize_functions <- list(
  # Sanitize functions ####
  sanitize_text = function(text) {
    gsub("[^a-zA-Z0-9 .-]", "", text)
  },

  sanitize_numeric = function(text) {
    cleaned_text <- gsub("[^0-9.-]", "", as.character(text))

    # Ensure only one decimal point
    cleaned_text <- sub("([0-9]*[.][0-9]*).*", "\\1", cleaned_text)

    cleaned_text <- sub("(.)-(.)", "\\1\\2", cleaned_text)
    cleaned_text <- sub("^(-?).*?(-?.*)$", "\\1\\2", cleaned_text)

    numeric_value <- as.numeric(cleaned_text)

    if (is.na(numeric_value)) {
      return(NA)
    } else {
      return(numeric_value)
    }
  },

  escape_html = function(text) {
    text <- gsub("&", "&amp;", text)
    text <- gsub("<", "&lt;", text)
    text <- gsub(">", "&gt;", text)
    text <- gsub("\"", "&quot;", text)
    text <- gsub("'", "&#39;", text)
    return(text)
  }
)
