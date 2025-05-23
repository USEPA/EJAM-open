# EJAM v2.32.3 (May 2025)

## Summary report and related improvements
- Added a long list of additional indicators in the summary report (in a subtable) and in outputs of `ejamit()`, etc.
  New indicators include counts of features (Superfund sites, schools, etc.), asthma and cancer rates,
  overlaps with certain types of areas (Tribal, C JEST disadv., air nonattainment areas, etc.), 
  flood risk, % with health insurance, more age groups (% under 18), and numerous other indicators.
  You can see the expanded report via `ejam2report()` or at `system.file("testdata/examples_of_output/testoutput_ejam2report_100pts_1miles.html", package = "EJAM")`
- Area in square miles (area_sqmi column) added to results, with calculation of size of each location (polygon or FIPS unit or circle around a point)
- More/better info on number of sites or site ID and lat/lon, now in header
- Enabled customization of summary table (for R users) to show fewer or new additional indicators 
  (as long as they are in the outputs of `doaggregate()` and `ejamit()` or at least are in the inputs to `ejam2report()` etc.).
  This is done via the `extratable_list_of_sections` parameter 
  in `ejam2report()`, in `build_community_report()`, in the community_report_template.Rmd, and 
  in global parameter `default_extratable_list_of_sections`. It may later be enabled as modifiable in the advanced tab.
- Easier to set which logo to show on summary report (EPA or EJAM or other logo), in global settings

## Other web app improvements 
- More types of shapefiles can be uploaded in the web app -- json, geojson, kml, zip (of gdb or other), and shp.
- Census units like States, Counties, and Cities/Towns/CDPs can now be selected from a menu or searched by typing part of the name,
  in a shiny module called fipspicker, and the feature will be enabled via default_choices_for_type_of_site_category,
  once it is fully tested. It works but current does not check or alert users if boundaries are not available, until after the Start Analysis button is clicked.
- Simpler UI for "More info" button about file types and formats allowed in upload.
- Preview maps can show FIPS now, along with shapefile polygons, or points
- `ejam2report()` and `ejam2map()` and `mapfast()` now better able to create maps of polygon data, FIPS, one site vs all sites, etc.
- progress bar added for doaggregate() in cases of fips and latlon

## RStudio user-related or internal improvements
- `ejamit()` in interactive mode (RStudio) now lets you select any type of file to upload if no sites specified by parameters
- extensive additions of and improvements in vignettes, including documentation of how to maintain repo, package, datasets. vignettes avoid hardcoded repo urls, and working on relative links within pkgdown site... repo_from_desc() added, avoids hardcoded repo url; download_latest_arrow_data avoids hardcoded repo url; links to testdata files on webapp UI avoid hardcoded repo url; simpler 0_whatis.Rmd
- clarified/explained 2025 status of api and urls in CONTRIBUTING and README, etc.
- Many options or starting values or settings for the shiny app (and in general) can now be set as 
  parameters passed to the `run_app()` function, which overrides the defaults.
  extensive changes to global defaults vs user parameters allowed: 
  replaced global.R; files renamed, put in 1 folder, etc.
  System for using user parameters passed to run_app(), global defaults otherwise, many can be changed in advanced tab; some may be bookmarkable.
  The default values are now set for the shiny app and in general in files called 
  `global_defaults_package.R`, `global_defaults_shiny_public.R`, and `global_defaults_shiny.R` 
  (rather than in the old files global.R or manage-public-private.R).
- acs_bybg examples added, on how to obtain and analyze new/custom indicators from the American Community Survey (ACS) data
- `testdata()` function improved, showing you examples of files that be used as inputs to `ejamit()`. testdata() files and data objects cleaned up/renamed consistently and new ones added for fips types, naics, sic, mact, etc.
- refactored names of plot functions made more consistent to use "plot" singular and "ratios" plural, as in `ejam2boxplot_ratios()`, `boxplot_ratios()`, etc.
- documentation fixed in some functions (e.g., ejam2map)
- large datasets managed via download_latest_arrow_data() and other new arrow-related functions and no longer on pins board or aws at all.  arrow datasets faster format used most places, other changes to handling downloads etc.
- shape_from_fips() checks if census API key available and tidycensus pkg now imported, uses alt method (arcgis services) to get boundaries if necessary.
- Continued towards refactoring/consolidating code in server vs in functions, related to creating summary report as HTML vs for download from shiny app vs from `ejam2report()`,
  in functions such as `report_residents_within_xyz()`, renamed generate_demog_header to generate_env_demog_header, etc.
- server uses ejamit() for SHP and latlon, and cleanup
- server uses ejam2excel() now not table_xls_format()
- server uses ejam2report() now not community_download() etc. 
- server uses shapefile_from_any now
- server: removed use of data_summarized reactive everywhere, use data_processed$...
- 2 new params doaggregate() has, to ejamit(), for calctype_maxbg and minbg
- bug fixes such as in ejamit() for wtdmeancols param, ejamit_compare_distances(), shapes_from_fips(), plot_ridgeline_ratios (), map_google(), in mapfast() for tracts vs blockgroups, many others
- unit tests added and others updated/fixed 
- misc helpers/utility added/updated/documented
- renamed map_headernames to new version and made edits/fixes
- reposissues and repoissues2() help record snapshot of gh issues
- DESCRIPTION file now has new field ejam_data_repo
- updated workflow action to use latest version of github-pages-deploy-action


# EJAM v2.32.2 (February 2025)

- Revised all language based on executive orders, to refer to environmental and residential population data analysis, rather than EJ/EJScreen/etc.
- Revised web links based on EJScreen website being offline
- Some edits made considering github repositories and gh pages may change location or go offline
- Updated FRS datasets, pulled on 2/12/25
- Remove screenshots from user guide document

# EJAM v2.32.1-EJAM (February 2025)

## Bug Fixes

- Fixed metadata warning shown during loading of arrow datasets
- Fixed typos in languages spoken indicators labels
- Improved labeling and legibility of barplot of ratios used in reports and downloads
- Fixed caps to \# of points selected, analyzed

## Enhancements

- Expanded tables of indicators shown in community report
- Languages spoken at home, health, community, age
- Added ratio columns to community report as advanced setting and heatmap highlighting optional
- Incorporated `shinytest2` tests for app-based functionality testing
- Implemented mapping for points in `ejam2excel()`

## Experimental enhancements

- Added draft plumber API for `ejam2excel()`
- Added widget to advanced settings
- proxistat() helps build proximity indicator
- Zipcodes vignette

## Other

- Refactored community report functions, `app_server.R` script

# EJAM v2.32-EJAM (January 2025)

## New Features + Improvements

- Enabled automatic download of latest arrow data from ejamdata repo
- Incorporated public-internal toggles to hide specific UI elements not yet applicable to the public version of EJAM
- Made improvements to maps of polgygons
- Added shapefile upload instructions

## Bug Fixes and Enhancements

- Added `leaflet.extras2` dependency to Imports, instead of Suggests, which is necessary for new installations

# EJAM v2.32.0

- The EJAM R package is available as an open source resource you can
    - clone from the [EJAM-open github repository](https://github.com/USEPA/EJAM-open) or
    - install using the [installation instructions](https://usepa.github.io/EJAM/articles/1_installing.html)
