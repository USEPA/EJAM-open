
# The "global_defaults_*.R" files define most default settings.
#
# This one defines things needed even when the shiny app is not being used.
# (also see the app_header_logo)

.community_report_logo_path = system.file('report/community_report/ejamhex4.png', package = "EJAM")

# .community_report_logo_path = 'www/EPA_logo_white_2.png'  # an earlier logo

global_defaults_package <- list(

  ### logo ####
  # top level header

  # Info used by generate_html_header(),
  #  as called from  build_barplot_report()  or
  #   build_community_report()
  #   as called from server and .Rmd template and ejam2report()

  .community_report_title = "EJAM Multisite Report",  # "Summary Report" might be appropriate for single site barplots version via build_barplot_report.R

  .community_report_logo_path =          .community_report_logo_path,
  .community_report_logo_file = basename(.community_report_logo_path),
  .community_report_logo_dir   = dirname(.community_report_logo_path)
)

rm(.community_report_logo_path)

################### #
