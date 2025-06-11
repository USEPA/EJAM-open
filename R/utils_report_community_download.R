
# OBSOLETE Functions to Render Summary Report (html etc.) ####

# NOTE on refactoring this code:
#
# if/when EJScreen API provided 1-site report, EJAM used that via that API,
# (instead of using the very similar EJAM version of 1-site report),
# AND provided in a separate button, this pair of barplots on 1 site,
# via this function.
#  BUT, while EJAM code will provide all 3 reports
# (overall, 1site normal summary, and 1site barplots pair)
# ...Consolidated ejam2report()  / server
# to use the same code (ejam2report or build_community_report and helpers)
# for both RStudio users and shiny app,
# for both web view and download of file,
# for both html and pdf format reports.

########################################################### #
# ~ ####

# ~ ####

# > OBSOLETE - rendered and saved html report ####

#' OBSOLETE - Generated Single-site or Multi-site Summary Report rendered as an HTML file for download in shiny app
#'
#' Saved a 2 page report (on overall results or for just one site), with residential population and environmental indicators, Summary Indexes if needed, etc.
#'
#' @details
#'
#' server now just uses `ejam2report()` and/or `build_community_report()`, not report_community_download()
#' and ejam2report() also uses `build_community_report()`.
#'
#' This function [report_community_download()] was very similar to [build_community_report()]
#' and was used by the shiny app to render the report as an HTML file for download
#' instead of returning HTML for display in a browser.
#'
#' report_community_download() relied on helpers [report_setup_temp_files()] to copy files,
#' [map_single_location()] to draw map, [v1_summary_plot_report()] to draw plot,
#' and [rmarkdown::render()] to do the parameterized render from .Rmd template into HTML
#'
#' @param file for download
#' @param row_index which row of results (which location), related to react_cur_button
#' @param react_cur_button like "button_2", event button asking for a 1-site report, a shiny app reactive
#' @param inshiny logical
#'
#' @param input_analysis_title a shiny app input$
#' @param input_include_ejindexes a shiny app input$
#' @param input_show_ratios_in_report a shiny app input$
#' @param input_extratable_show_ratios_in_report a shiny app input$
#' @param input_extratable_title a shiny app input$
#' @param input_extratable_title_top_row a shiny app input$
#' @param extratable_list_of_sections see [build_community_report()]
#' @param extratable_hide_missing_rows_for see [build_community_report()]
#'
#' @param input_plotkind_1pager a shiny app input$, like "bar", "box", "ridgeline"
#' @param input_Custom_title_for_bar_plot_of_indicators a shiny app input$
#' @param input_circleweight_in a shiny app input$
#'
#' @param react_sanitized_analysis_title a shiny app reactive
#' @param react_sanitized_bt_rad_buff a shiny app reactive
#' @param react_total_pop a shiny app reactive
#' @param react_submitted_upload_method a shiny app reactive
#'
#' @param react_data_uploaded a shiny app reactive, like input to ejamit()
#' @param react_data_processed a shiny app reactive, like output from ejamit()
#' @param react_ratio.to.us.d a shiny app reactive
#'
#' @param react_report_map a shiny app reactive, # map of all sites Overall
#'
#' @param react_v1_summary_plot a shiny app reactive
#' @param react_v1_summary_plot_state a shiny app reactive
#'
#' @seealso [build_community_report()]
#' @returns Renders HTML report
#'
#' @keywords internal
#' @noRd
#'
report_community_download <- function(file,
                                      row_index = NULL,
                                      react_cur_button, # event button asking for a 1-site report; was isolated in one line, not in another
                                      inshiny = FALSE,

                                      input_analysis_title,
                                      input_include_ejindexes,
                                      input_show_ratios_in_report,
                                      input_extratable_show_ratios_in_report,
                                      input_extratable_title,
                                      input_extratable_title_top_row,
                                        extratable_list_of_sections,
                                        extratable_hide_missing_rows_for,

                                      input_plotkind_1pager,
                                      input_Custom_title_for_bar_plot_of_indicators,
                                      input_circleweight_in,

                                      react_sanitized_analysis_title,
                                      react_sanitized_bt_rad_buff,    # radius
                                      react_total_pop,
                                      react_submitted_upload_method,  # points vs shapefile etc.

                                      react_data_uploaded,
                                      react_data_processed,
                                      react_ratio.to.us.d,

                                      react_report_map,         # map of all sites Overall
                                      # single_location_map( )  # map of 1 site; function inside here that had been a reactive

                                      # v1_summary_plot_report, #     # barplot   USA ratios for Overall; function inside here that had been a reactive
                                      react_v1_summary_plot,          # barplot   USA ratios for 1 site
                                      react_v1_summary_plot_state    # barplot State ratios for 1 site

) {

  if (inshiny) {
    # Create a progress object
    progress <- shiny::Progress$new( )
    progress$set(message = "Generating report", value = 0)
    # Ensure that the progress bar is closed when we exit this function
    on.exit(progress$close( ))
    progress$set(value = 0.1, detail = "Setting up temporary files...")
  }

  ## copy .Rmd (template), .png (logo), .css, via report_setup_temp_files() ####

  if (!is.null(row_index)) {

    # single-site report

    ## While ejscreen site was working, 1 link in report would go to ejscreen report and another to a barplot report.
    ## Since ejscreen site went down, and could provide the normal full summary report on a single site,
    ## we disabled the barplot report and instead just had EJAM provide
    ## a normal full summary on a single site (just like done for the summary of sites).
    ## BUT, we could add back the other button and provide both the summary report AND the barplot report. ***

    # copy files needed (css, logo, template)
    # tempReport <- report_setup_temp_files(Rmd_name = 'barplot_report_template.Rmd')
    tempReport <- report_setup_temp_files(Rmd_name = 'community_report_template.Rmd')

  } else {

    # overall summary multisite report

    # copy files needed (css, logo, template)
    tempReport <- report_setup_temp_files(Rmd_name = 'community_report_template.Rmd')
  }

  if (inshiny) {progress$set(value = 0.2, detail = "Defining parameters...")}

  # Define parameters for Rmd rendering

  ## get radius ####
  rad <- react_data_processed$results_overall$radius.miles

  if (inshiny) {progress$set(value = 0.3, detail = "Adjusting data...")}
  ################################################################### #

  # Adjust the data based on whether a specific row is selected

  if (!is.null(row_index)) {

    ## > for just 1 site ####

    # See notes on refactoring code on 1-site reports (normal 1-site report plus 2barplot 1-site report)

    output_df <- react_data_processed$results_bysite[row_index, ]
    popstr <- prettyNum(round(output_df$pop, 0), big.mark = ',')

    # Get the name of the selected location
    ### ...location info for header via report_residents_within_xyz() ####

    location_name <- output_df$statename

    # locationstr <- paste0("Residents within ", rad, " mile", ifelse(rad > 1, "s", ""),
    #                        " of this ",
    # ifelse(react_submitted_upload_method == 'SHP', "polygon",
    #        ifelse(react_submitted_upload_method == 'FIPS', "Census unit",
    #               "point"))
    # )
    nsites <- 1
    locationstr <- report_residents_within_xyz(
      sitetype = tolower(react_submitted_upload_method),
      radius = rad,
      nsites = nsites,
      area_in_square_miles = NULL # area_in_square_miles
    ) # ***

    # Create a filtered version of react_report_map for single location #####################  #

    if (inshiny) {progress$set(value = 0.4, detail = "Creating map...")}

    ### ...MAP via map_single_location() ####

    map_to_use <- map_single_location(row_index = row_index,
                                      inshiny = inshiny,
                                      input_circleweight_in,
                                      react_sanitized_bt_rad_buff,    # radius
                                      react_submitted_upload_method,  # points vs shapefile etc.
                                      react_data_uploaded,
                                      react_data_processed)
    # end of map code  #####################  #

    ### ...PLOT is param passed here ####
    ## make sure the barplots are for the 1 selected site

    # react_v1_summary_plot
    # react_v1_summary_plot_state

    ####################################### #
    #
    # parameters, some of which get passed to build_community_report() as used in community_report_template.Rmd

    params <- list(

      in_shiny = FALSE,
      # output_df = output_df,
      analysis_title = input_analysis_title,
      totalpop = popstr,
      locationstr = locationstr,
      # include_ejindexes = (input_include_ejindexes == 'TRUE'),
      extratable_title =         input_extratable_title,
      extratable_title_top_row = input_extratable_title_top_row,
      extratable_list_of_sections = extratable_list_of_sections,
      extratable_hide_missing_rows_for = extratable_hide_missing_rows_for,

      filename = NULL,

      map = map_to_use,
      summary_plot       = react_v1_summary_plot,
      summary_plot_state = react_v1_summary_plot_state,
      report_title = NULL,
      logo_path = NULL,
      logo_html = NULL
    )
    # end of report on 1 site

  } else {
    ################################################################### #

    ## > for all sites overall ####

    output_df <- react_data_processed$results_overall
    popstr <- prettyNum(round(react_total_pop, 0), big.mark = ',')
    nsites <- NROW(react_data_processed$results_bysite[react_data_processed$results_bysite$valid == T,])

    ### ...location info for header via report_residents_within_xyz() ####

    # locationstr <- paste0("Residents within ", rad, " mile", ifelse(rad > 1, "s", ""),
    #                       " of any of the ", nsites,
    #                       " selected ", ifelse(react_submitted_upload_method == 'SHP', "polygons",
    #                                            ifelse(react_submitted_upload_method == 'FIPS', "shapes", "points"))
    # )
    locationstr <- report_residents_within_xyz(
      sitetype = tolower(react_submitted_upload_method),
      radius = rad,
      nsites = nsites,
      area_in_square_miles = NULL # area_in_square_miles
    )

    ### ...MAP is param passed here ####

    map_to_use <- react_report_map

    ### ...PLOT via v1_summary_plot_report() ####

    plot_to_use <- v1_summary_plot_report(row_index,  # like sitenumber 1:N, not necessarily same as ejam_uniq_id. also see react_cur_button
                                          input_plotkind_1pager,  # like "bar", "box", or "ridgeline"
                                          input_Custom_title_for_bar_plot_of_indicators,
                                          react_cur_button, # also see row_index; like "button_1", event button asking for a 1-site report; was isolated in one line, not in another
                                          react_sanitized_bt_rad_buff,    # radius in miles
                                          react_data_processed,   # table like output from ejamit()
                                          react_ratio.to.us.d) + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))

    ####################################### #
    # parameters, some of which get passed to build_community_report() as used in community_report_template.Rmd

    params <- list(

      in_shiny = FALSE,
      output_df = output_df,
      analysis_title =  react_sanitized_analysis_title,
      totalpop = popstr,
      locationstr = locationstr,
      include_ejindexes = (input_include_ejindexes == 'TRUE'),

      show_ratios_in_report = input_show_ratios_in_report,
      extratable_show_ratios_in_report = input_extratable_show_ratios_in_report,
      extratable_title =         input_extratable_title,
      extratable_title_top_row = input_extratable_title_top_row,
      extratable_list_of_sections = extratable_list_of_sections,
      extratable_hide_missing_rows_for = extratable_hide_missing_rows_for,

      filename = NULL,
      map = map_to_use,
      summary_plot = plot_to_use,
      report_title = NULL,
      logo_path = NULL,
      logo_html = NULL
    )
  }

  ## Render Rmd to HTML (path of the output file is returned) ####

  ### ...uses build_community_report() via render of .Rmd template ####

  rmarkdown::render(tempReport,
                    output_format = 'html_document',
                    output_file = file,
                    params = params, # passed to build_community_report() as used in community_report_template.Rmd
                    envir = new.env(parent = globalenv( )),
                    intermediates_dir = tempdir( )
  )
}
########################################################### #
# ~ ####

# > OBSOLETE? - was used by report_community_download. leaflet MAP, of ONE SITE, for summary report ####

#' helper - OBSOLETE? - was used by report_community_download. creates leaflet map for summary report
#'
#' @param row_index which site in the table is the one of interest
#' @param inshiny logical
#' @param input_circleweight_in from shiny
#' @param react_sanitized_bt_rad_buff  from shiny
#' @param react_submitted_upload_method  from shiny
#' @param react_data_uploaded  from shiny, needed only if shapefile was input
#' @param react_data_processed  from shiny, results like output of ejamit(), but only 1 row of 1 table gets used
#'
#' @keywords internal
#' @noRd
#'
map_single_location <- function(row_index = NULL,
                                inshiny = FALSE,
                                input_circleweight_in,
                                react_sanitized_bt_rad_buff,    # radius
                                react_submitted_upload_method,  # points vs shapefile etc.
                                react_data_uploaded,
                                react_data_processed
) {
  if (inshiny) {
    # shiny::req(react_data_processed)
    shiny::validate(shiny::need(react_data_processed, 'Please run an analysis to see results.'))
  }
  filtered_data <- react_data_processed$results_bysite[row_index, ]

  ####################### #        ####################### #
  if (react_submitted_upload_method == "SHP") {
    # Handle shapefile case

    shp_valid <- react_data_uploaded[react_data_uploaded$ejam_uniq_id == filtered_data$ejam_uniq_id, ]
    d_up <- shp_valid
    d_up_geo <- d_up[,c("ejam_uniq_id","geometry")]
    d_merge = merge(d_up_geo, filtered_data, by = "ejam_uniq_id", all.x = FALSE, all.y = TRUE)

    popup_labels <- fixcolnames(namesnow = setdiff(names(d_merge),c('geometry', 'valid', 'invalid_msg')), oldtype = 'r', newtype = 'shortlabel')
    rad_buff <- react_sanitized_bt_rad_buff

    if (!is.na(rad_buff) && rad_buff > 0) {
      d_uploads <- sf::st_buffer(d_merge[d_merge$valid == T, ], dist = units::set_units(rad_buff, "mi"))
      map <- leaflet::leaflet(d_uploads) %>%
        leaflet::addTiles( ) %>%
        leaflet::addPolygons(data = d_uploads, color = '#000080',
                             popup = popup_from_df(d_uploads %>% sf::st_drop_geometry( ) %>% dplyr::select(-valid, -invalid_msg), labels = popup_labels),
                             popupOptions = leaflet::popupOptions(maxHeight = 200))
    } else {
      data_spatial_convert <- d_merge[d_merge$valid == T, ] %>%
        dplyr::select(-valid, -invalid_msg) %>%
        sf::st_zm( ) %>% sf::as_Spatial()
      map <- leaflet::leaflet(data_spatial_convert) %>%
        leaflet::addTiles( ) %>%
        leaflet::addPolygons(color = '#000080',
                             popup = popup_from_df(data_spatial_convert %>% sf::st_drop_geometry( ),
                                                   labels = popup_labels),
                             popupOptions = leaflet::popupOptions(maxHeight = 200))
    }

    # Get the bounding box of the shape
    bbox <- sf::st_bbox(d_merge)
    map %>% leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
      leaflet::setView(lng = mean(c(bbox[1], bbox[3])),
                       lat = mean(c(bbox[2], bbox[4])),
                       zoom = 14)

    ####################### #        ####################### #
  } else if (react_submitted_upload_method != "FIPS") {
    # Handle non-FIPS case

    popup_labels <- fixcolnames(namesnow = names(filtered_data), oldtype = 'r', newtype = 'shortlabel')
    popup_labels[is.na(popup_labels)] <- names(filtered_data)[is.na(popup_labels)]

    map <- leaflet::leaflet(filtered_data) %>%
      leaflet::addTiles( ) %>%
      leaflet::addCircles(
        radius = 1 * meters_per_mile,
        color = '#000080', fillColor = '#000080',
        fill = TRUE, weight = input_circleweight_in,
        popup = popup_from_df(
          filtered_data %>%
            dplyr::mutate(dplyr::across(
              dplyr::where(is.numeric), \(x) round(x, digits = 3))),
          labels = popup_labels),
        popupOptions = leaflet::popupOptions(maxHeight = 200)
      )

    # Set view with a slightly more zoomed out level
    map %>% leaflet::setView(lng = filtered_data$lon,
                             lat = filtered_data$lat,
                             zoom = 14)

  } else {
    ####################### #        ####################### #
    # FIPS case

    leaflet::leaflet( ) %>% leaflet::addTiles( ) %>% leaflet::fitBounds(-115, 37, -65, 48)
  }
} # end of map code  ########  #
########################################################### #
# ~ ####

# >  OBSOLETE? - was used by report_community_download. bar/box/ridgeline PLOT, OVERALL, for summary report ####

#'  OBSOLETE? - was used by report_community_download. helper - creates bar/box/ridgeline plot for summary report
#'
#' @param row_index  from shiny app, which row of table is the site of interest (should be related to react_cur_button)
#' @param input_plotkind_1pager from shiny app, from input$plotkind_1pager like "bar", "box", "ridgeline"
#' @param input_Custom_title_for_bar_plot_of_indicators  from shiny app
#' @param react_cur_button  from shiny app, such as "button_1" for sitenumber 1, or NULL (should be related to row_index)
#' @param react_sanitized_bt_rad_buff  from shiny app, radius in miles
#' @param react_data_processed  from shiny app, input table of places like input to ejamit()
#' @param react_ratio.to.us.d  from shiny app
#'
#' @keywords internal
#' @noRd
#'
v1_summary_plot_report <- function(row_index = NULL,
                                   input_plotkind_1pager,
                                   input_Custom_title_for_bar_plot_of_indicators = NULL,
                                   react_cur_button, # event button asking for a 1-site report; was isolated in one line, not in another
                                   react_sanitized_bt_rad_buff,    # radius
                                   react_data_processed,
                                   react_ratio.to.us.d
) {

  # shiny::req(react_data_processed) #  ***
  ## react_data_processed needed for ridgeline or boxplot, and react_ratio.to.us.d which is made from react_data_processed is needed for boxplots,
  ################################## #

  if (input_plotkind_1pager == 'bar') {

    ## barplot ####

    # (overall, ratio to avg)
    if (!is.null(react_cur_button)) {##################################### #           was a REACTIVE inside overall reactive
      # selected_row <- as.numeric(gsub('button_', '', isolate(react_cur_button )))##################################### #    ***       was a REACTIVE inside overall reactive
      selected_row <- as.numeric(gsub('button_', '',          (react_cur_button )))##################################### #          was a REACTIVE inside overall reactive
      plot_barplot_ratios_ez(
        out = react_data_processed,
        varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg),

        ## not allowing input_Custom_title_for_bar_plot_of_indicators to be used in this case (?)
        main = "Residential population groups at the Analyzed Location \n Compared to US Overall",

        single_location = TRUE,
        row_index = selected_row
      )
    } else {

      if (is.null(input_Custom_title_for_bar_plot_of_indicators) || '' %in% input_Custom_title_for_bar_plot_of_indicators) {

        #Default way
        plot_barplot_ratios_ez(
          out = react_data_processed,
          varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg),
          main = "Residential population groups at the Analyzed Location \n Compared to US Overall"
        )

      } else {
        #If there is a new title in advanced settings
        plot_barplot_ratios_ez(
          out = react_data_processed,
          varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg),
          main = input_Custom_title_for_bar_plot_of_indicators
        )
      }

    }
    ################################## #

  } else if (input_plotkind_1pager == 'ridgeline') {

    ## ridgeline plot ####
    ## distribution of ratios at all the sites  (demog each site / demog avg in US)
    ratio.to.us.d.bysite <- react_data_processed$results_bysite[ ,  c(
      ..names_d_ratio_to_avg,
      ..names_d_subgroups_ratio_to_avg
    )]
    plot_ridgeline_ratios(ratio.to.us.d.bysite,
                          main = input_Custom_title_for_bar_plot_of_indicators
    )

    ################################## #

  } else if (input_plotkind_1pager == "box") {

    ## boxplots ####
    ## distribution of ratios at all the sites
    ejam2boxplot_ratios(react_data_processed, react_sanitized_bt_rad_buff,
                        main = input_Custom_title_for_bar_plot_of_indicators,
                        varnames = c(names_d, names_d_subgroups))
  }# box
}
