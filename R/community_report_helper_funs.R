

#' Write a demog. or envt. indicator to an html table row
#'
#' @param output_df, single row of results table from doaggregate -
#'   either results_overall or one row of bysite
#' @param Rname, variable name of indicator to pull from results,
#'   such as 'pm', 'pctlowinc', 'Demog.Index'
#' @param longname, nicer name of indicator to use in table row;
#'   can include HTML sub/superscripts
#'
#' @keywords internal
#'
fill_tbl_row <- function(output_df, Rname, longname) {
  
  id_col <- "selected-variables"
  
  hdr_names <- c('value','state-average',
                 'percentile-in-state','usa average','percentile-in-usa')
  
  Rnames <- paste0(c('', 'state.avg.', 'state.pctile.', 'avg.', 'pctile.'),
                   Rname)
  
  cur_vals <- if ('data.table' %in% class(output_df)) {
    sapply(Rnames, function(v) output_df[, ..v])
  } else {
    sapply(Rnames, function(v) output_df[,v])
  }
  
  txt <- paste0(
    "<tr>",
    '\n','<td headers=\"data-indicators-table-',
    id_col,'\">',
    longname,'</td>',
    paste0('\n','<td headers=\"data-indicators-table-', hdr_names, '\">', cur_vals, '</td>',
           collapse = ""),'\n</tr>')
  
  return(txt)
}
################################################################### #


#' Write an EJ or EJ supp index to an html table row
#'
#'@param output_df, single row of results table from doaggregate -
#'  either results_overall or one row of bysite
#'@param Rname, variable name of indicator to pull from results,
#'  such as 'pm', 'pctlowinc', 'Demog.Index'
#'@param longname, nicer name of indicator to use in table row;
#'  can include HTML sub/superscripts
#'
#' @keywords internal
#'
fill_tbl_row_ej <- function(output_df, Rname, longname) {
  
  id_col <- 'selected-variables'
  
  hdr_names <- c('percentile-in-state','percentile-in-usa')
  
  Rnames <- paste0(c('state.pctile.','pctile.'), Rname)
  
  cur_vals <- if ('data.table' %in% class(output_df)) {
    sapply(Rnames, function(v) ifelse(v %in% names(output_df), output_df[,..v], NA))
  } else{
    sapply(Rnames, function(v) ifelse(v %in% names(output_df), output_df[, v], NA))
  }
  
  txt <- paste0(
    "<tr>",
    '\n','<td headers=\"data-indicators-table-',
    id_col,'\">',
    longname,'</td>',
    paste0('\n','<td headers=\"data-indicators-table-', hdr_names, '\">', cur_vals, '</td>',
           collapse = ""),'\n</tr>')
  
  return(txt)
}
################################################################### #


#' Create full demog. or envt. HTML table of indicator rows
#'
#' @param output_df, single row of results table from doaggregate -
#'   either results_overall or one row of bysite
#'
#' @keywords internal
#'
fill_tbl_full <- function(output_df) {
  
  full_html <- ''
  
  tbl_head <- '<table id=\"data-indicators-table\" class=\"color-alt-table\" style=\"margin-top: 0;\"  summary=\"EJScreen environmental and socioeconomic indicators data\">
  <thead id=\"data-indicators-table-header\" class=\"color-alt-table-header\">
  <tr>
  <th id=\"data-indicators-table-selected-variables\" scope=\"col\">SELECTED VARIABLES</th>
  <th id=\"data-indicators-table-value\" scope=\"col\">VALUE</th>
  <th id=\"data-indicators-table-state-average\" scope=\"col\">STATE<br> AVERAGE</th>
  <th id=\"data-indicators-table-percentile-in-state\" scope=\"col\">PERCENTILE<br> IN STATE</th>
  <th id=\"data-indicators-table-usa average\" scope=\"col\">USA AVERAGE</th>
  <th id=\"data-indicators-table-percentile-in-usa\" scope=\"col\">PERCENTILE<br> IN USA</th>
  </tr>
  </thead>
  <tbody>
  <tr class=\"color-alt-table-subheader\">
  <th colspan=\"7\">Pollution and Sources</th>
  </tr>'
  
  tbl_head2 <- '<tr class=\"color-alt-table-subheader\">
<th colspan=\"7\">Socioeconomic Indicators</th>
  </tr>'
  
  full_html <- paste(full_html, tbl_head, sep = '\n')
  
  ## reorder indicators to match report order
  
  
  Rnames_e <- namesbyvarlist(varlist = 'names_e')$rname
  
  longnames_e <- fixcolnames(Rnames_e, oldtype = 'r', newtype = 'long')
  
  tbl_rows_e <- sapply(seq_along(Rnames_e), function(x) {
    fill_tbl_row(output_df,
                 Rname = Rnames_e[x],
                 longname = longnames_e[x])
  })
  full_html <- paste(full_html,
                     paste(tbl_rows_e , collapse = '\n'),
                     sep = '', collapse = '\n')
  full_html <- paste(full_html, tbl_head2,collapse = '\n')
  
  
  ## reorder indicators to match report order
  
  Rnames_d <- namesbyvarlist(varlist = 'names_d')$rname
  
  longnames_d <- fixcolnames(Rnames_d, oldtype = 'r', newtype = 'long')
  
  tbl_rows_d <- sapply(seq_along(Rnames_d),
                       function(x) {
                         fill_tbl_row(output_df,
                                      Rname = Rnames_d[x],
                                      longname = longnames_d[x])})
  full_html <- paste(full_html,
                     paste(tbl_rows_d, collapse = '\n'),
                     sep = '', collapse = '\n')
  
  full_html <- paste(full_html, '</tbody>
</table>', collapse = '\n')
  
  return(full_html)
}
################################################################### #


#' Create full EJ + EJ supp index HTML table of indicator rows
#'
#'@param output_df, single row of results table from doaggregate - either results_overall or one row of bysite
#'
#' @keywords internal
#'
fill_tbl_full_ej <- function(output_df) {
  
  tbl_head <- '<table id=\"data-indicators-table\"        class=\"color-alt-table\"  summary=\"EJScreen environmental and socioeconomic indicators data\">
  <thead id=\"data-indicators-table-header\" class=\"color-alt-table-header\">
  <tr>
  <th id=\"data-indicators-table-selected-variables\" scope=\"col\">SELECTED VARIABLES</th>
  <th id=\"data-indicators-table-percentile-in-state\" scope=\"col\">PERCENTILE<br> IN STATE</th>
  <th id=\"data-indicators-table-percentile-in-usa\" scope=\"col\">PERCENTILE<br> IN USA</th>
  </tr>
  </thead>
  <tbody>
  <tr class=\"color-alt-table-subheader\">
  <th colspan=\"7\">EJ Indexes</th>
  </tr>'
  
  tbl_head2 <- '<tr class=\"color-alt-table-subheader\">
<th colspan=\"7\">Supplemental EJ Indexes</th>
  </tr>'
  
  full_html <- tbl_head #paste(full_html, tbl_head, sep='\n')
  
  ## reorder indicators to match report order
  
  Rnames_ej <-namesbyvarlist(varlist = 'names_ej')$rname
  
  longnames_ej <- fixcolnames(Rnames_ej, oldtype = 'r', newtype = 'long')
  longnames_ej <- gsub("US type of raw score for ", "", longnames_ej)
  # we combine values & percentiles on the same row
  # remove mention of raw score from longname so it applies to the whole row
  
  tbl_rows_ej <- sapply(seq_along(Rnames_ej),
                        function(x) {
                          fill_tbl_row_ej(output_df, 
                                          Rname = Rnames_ej[x],
                                          longname = longnames_ej[x])
                        })
  
  full_html <- paste(full_html,
                     paste(tbl_rows_ej, collapse = '\n'),
                     sep = '', collapse = '\n')
  full_html <- paste(full_html, tbl_head2, collapse = '\n')
  
  ## reorder indicators to match report order
  
  Rnames_ej_supp <- namesbyvarlist(varlist = 'names_ej_supp')$rname
  
  longnames_ej_supp <- fixcolnames(Rnames_ej_supp, oldtype = 'r', newtype = 'long')
  longnames_ej_supp <- gsub("US type of raw score for ", "", longnames_ej_supp)
  # we combine values & percentiles on the same row
  # remove mention of raw score from longname so it applies to the whole row
  
  tbl_rows_ej_supp <- sapply(seq_along(Rnames_ej_supp),
                             function(x) {
                               fill_tbl_row_ej(output_df, 
                                               Rname = Rnames_ej_supp[x],
                                               longname = longnames_ej_supp[x])
                             })
  
  full_html <- paste(full_html,
                     paste(tbl_rows_ej_supp, collapse = '\n'),
                     sep = '', collapse = '\n')
  
  full_html <- paste(full_html, '</tbody>
</table>', collapse = '\n')
  
  return(full_html)
}
################################################################### #


#' Write a demographic subgroup indicator to an html table row
#'
#'@param output_df, single row of results table from doaggregate - either results_overall or one row of bysite
#'@param Rname, variable name of indicator to pull from results, such as 'pm', 'pctlowinc', 'Demog.Index'
#'@param longname, nicer name of indicator to use in table row; can include HTML sub/superscripts
#'
#' @keywords internal

#' Write a demographic subgroup indicator to an html table row
#'
#'@param output_df, single row of results table from doaggregate - either results_overall or one row of bysite
#'@param Rname, variable name of indicator to pull from results, such as 'pm', 'pctlowinc', 'Demog.Index'
#'@param longname, nicer name of indicator to use in table row; can include HTML sub/superscripts
#'
#' @keywords internal
#'
fill_tbl_row_subgroups <- function(output_df, Rname, longname) {
  txt <- '<tr>'
  
  id_col <- 'selected-variables'
  txt <- paste0(txt, '\n','<td headers=\"data-indicators-table-',
                id_col,'\">',
                longname,'</td>')
  
  hdr_names <- 'value'
  
  Rnames <- paste0(c(''), Rname)
  
  cur_val <- if ('data.table' %in% class(output_df)){
    output_df[, ..Rnames]
  } else{
    output_df[, Rnames]
  }
  
  txt <-  paste0(txt, '\n', paste0('<td headers=\"data-indicators-table-',
                                   hdr_names,'\">',
                                   cur_val,'</td>'))
  
  
  txt <- paste0(txt, '\n','</tr>')
  return(txt)
}
################################################################### #


#' Create full demog subgroup HTML table of indicator rows
#'
#'@param output_df, single row of results table from doaggregate -
#'  either results_overall or one row of bysite
#'
#' @keywords internal
#'
fill_tbl_full_subgroups <- function(output_df) {
  
  # css_head <-'
  #    <link href="https://fonts.googleapis.com/css2?family=Heebo:wght@500;600" rel="stylesheet">
  # <link href="https://fonts.googleapis.com/css2?family=Oswald:wght@300;400;500;700&amp;display=swap" rel="stylesheet">
  # <link href="https://fonts.googleapis.com/css2?family=Noto+Sans&amp;display=swap" rel="stylesheet">
  # '
  
  full_html <- ''
  
  tbl_head <- '<table id=\"data-indicators-table\"   style=\"width: 60%\"     class=\"color-alt-table\"  summary=\"EJScreen environmental and socioeconomic indicators data\">
  <thead id=\"data-indicators-table-header\" class=\"color-alt-table-header\">
  <tr>
  <th id=\"data-indicators-table-selected-variables\" width=\"65%\" scope=\"col\">SELECTED VARIABLES</th>
  <th id=\"data-indicators-table-value\" width=\"35%\" scope=\"col\">VALUE</th>
  </tr>
  </thead>
  <tbody>
  <tr class=\"color-alt-table-subheader\">
  <th colspan=\"7\">Breakdown by Race</th>
  </tr>'
  
  tbl_head2 <- '<tr class=\"color-alt-table-subheader\">
<th colspan=\"7\">Breakdown by Gender</th>
  </tr>'
  
  tbl_head3 <- '<tr class=\"color-alt-table-subheader\">
<th colspan=\"7\">Limited English Speaking Breakdown</th>
  </tr>'
  
  full_html <- paste(full_html, tbl_head, sep = '\n')
  
  ## reorder indicators to match report order
  
  
  Rnames_d_race <- namesbyvarlist(varlist = 'names_d_subgroups')$rname
  
  longnames_d_race <- fixcolnames(Rnames_d_race, oldtype = 'r', newtype = 'long')
  
  tbl_rows_d_race <- sapply(seq_along(Rnames_d_race), function(x) {
    fill_tbl_row_subgroups(output_df,
                           Rname = Rnames_d_race[x],
                           longname = longnames_d_race[x])
  })
  
  full_html <- paste(full_html,
                     paste(tbl_rows_d_race , collapse = '\n'),
                     
                     sep = '', collapse = '\n')
  full_html <- paste(full_html, tbl_head2,collapse = '\n')
  
  
  Rnames_d_gender <- c('pctmale','pctfemale')
  
  longnames_d_gender <- c('% Male', '% Female')
  
  tbl_rows_d_gender <- sapply(seq_along(Rnames_d_gender), function(x) {
    fill_tbl_row_subgroups(output_df,
                           Rname = Rnames_d_gender[x],
                           longname = longnames_d_gender[x])
  }
  )
  full_html <- paste(full_html,
                     paste(tbl_rows_d_gender, collapse = '\n'),
                     sep = '', collapse = '\n')
  
  full_html <- paste(full_html, tbl_head3, collapse = '\n')
  
  Rnames_d_lim <- namesbyvarlist('names_d_languageli')$rname#intersect(names_d_language, names(blockgroupstats))  
  # names_d_language_li
  longnames_d_lim <- fixcolnames(Rnames_d_lim, 'r', 'short')
  
  tbl_rows_d_lim <- sapply(seq_along(Rnames_d_lim), function(x) {
    fill_tbl_row_subgroups(output_df,
                           Rname = Rnames_d_lim[x],
                           longname = longnames_d_lim[x])
  }
  )
  full_html <- paste(full_html,
                     paste(tbl_rows_d_lim, collapse = '\n'),
                     sep = '', collapse = '\n')
  
  
  full_html <- paste(full_html, '</tbody>
</table>', collapse = '\n')
  
  return(full_html)
}
################################################################### #


generate_report_footnotes <- function(
    # ejscreen_vs_ejam_caveat = "Note: Some numbers as shown on the EJScreen report for a single location will in some cases appear very slightly different than in EJScreen's multisite reports. All numbers shown in both types of reports are estimates, and any differences are well within the range of uncertainty inherent in the American Community Survey data as used in EJScreen. Slight differences are inherent in very quickly calculating results for multiple locations.",
  diesel_caveat = paste0("Note: Diesel particulate matter index is from the EPA's Air Toxics Data Update, which is the Agency's ongoing, comprehensive evaluation of air toxics in the United States. This effort aims to prioritize air toxics, emission sources, and locations of interest for further study. It is important to remember that the air toxics data presented here provide broad estimates of health risks over geographic areas of the country, not definitive risks to specific individuals or locations. More information on the Air Toxics Data Update can be found at: ",
                         url_linkify("https://www.epa.gov/haps/air-toxics-data-update", "https://www.epa.gov/haps/air-toxics-data-update"))
) {
  
  
  # This function gets called by 
  # build_community_report() in 
  #  app_server, 
  #  ejam2report(), and
  #  community_report_template.Rmd 
  
  dieselnote = paste0("
  <span style= 'font-size: 9pt'>
  <p tabindex=\'13\' style='font-size: 9pt'><small>", diesel_caveat, "</small></p>
  </span>"
  )
  
  ejamnote = ""
  # ejamnote <- paste0("
  # <span style= 'font-size: 9pt'>
  # <p tabindex=\'14\' style='font-size: 9pt'>", ejscreen_vs_ejam_caveat, "</p>
  # </span>"
  # )
  
  footnotes <- paste(
    dieselnote, 
    ejamnote,
    sep = "   "
  )
  return(HTML(footnotes))
}
################################################################### #


#' Build HTML header for community report
#'
#' @param analysis_title, title to use in header of report
#' @param totalpop, total population included in location(s) analyzed
#' @param locationstr, description of the location(s) analyzed
#' @param in_shiny, whether the function is being called in or outside of shiny - affects location of header
#' @param report_title generic name of this type of report, to be shown at top, like "EJScreen-EJAM Multisite Report" or "EJAM Multisite Report"
#'
#' @keywords internal
#'
generate_html_header <- function(analysis_title, totalpop, locationstr, in_shiny = FALSE, 
                                 report_title = .community_report_title) {

  
  if (in_shiny) {
    shift_hsb <- 630
    shift_hpb <- 600
    shift_hbd <- 550
  } else {
    shift_hsb <- 70
    shift_hpb <- 40
    shift_hbd <- 0
  }
  
  
  # should add padding and adjust size so that the img_html object is a bit lower on the screen and does not get shrunk
  
  img_html <- paste0('<img src=\"', 'www/EPA_logo_white_2.png',
                     '\" alt=\"EPA logo\" width=\"220\" height=\"70\">')
  
  paste0(
    '
  <link href=\"https://fonts.googleapis.com/css2?family=Heebo:wght@500;600\" rel=\"stylesheet\">
  <link href=\"https://fonts.googleapis.com/css2?family=Oswald:wght@300;400;500;700&amp;display=swap\" rel=\"stylesheet\">
  <link href=\"https://fonts.googleapis.com/css2?family=Noto+Sans&amp;display=swap\" rel=\"stylesheet\">
  <link rel=\"stylesheet\"  type=\"text/css\" media=\"all\" href=\"communityreport.css\" />',
    
    '<div id="header-primary-background">',
    '<div id="header-primary-background-inner">',
    '<h1 id="title" tabindex="0">', report_title, '</h1>',
    '<p>This report summarizes environmental and socioeconomic information for user-defined areas,<br> and combines that data into environmental justice and supplemental indexes.</p>',
    '</div>',
    '<div id="header-primary-background-img-container">',
    img_html,
    '</div>',
    '</div>',
    
    
    '<div class="header">
    <div>
        <h2 id="placename">', analysis_title , '</h2>
    </div>
  <div>
  
  <h5>', locationstr, '<br>Population: <span id="TOTALPOP">', totalpop, '</span><br></h5>
  
  
 </div>
</div>', 
    
    sep = '', collapse = '')
  
  # Population: <span id=\"TOTALPOP\">',totalpop,'</span><br>',
}
################################################################### #


#' Build header for demog. + envt. tables in community report
#'
#' @keywords internal
#'
generate_demog_header <- function() {
  '<h3 tabindex=\"12\" style=\"font-size: 8px\">Environmental and Socioeconomic Indicators Data</h3>'
}
################################################################### #


#' Build header for EJ index table in community report
#'
#' @keywords internal
#'
generate_ej_header <- function() {
  '<br>
 <div id=\"page-2-header\" class=\"header\" style=\"background-color: #005ea2;  color: white; text-align: center; padding: 20px 32px 10px 32px;\">
            <h2 tabindex=\"8\" style=\"font-size: 18px; margin-bottom: 5px\">Environmental Justice & Supplemental Indexes</h2>
            <p style=\"font-family: Oswald, Arial, sans-serif; font-size: 15px; padding-left: 20px;\">The environmental justice and supplemental indexes are a combination of environmental and socioeconomic information. For each of the environmental indicators in EJScreen, there is an EJ Index and a Supplemental EJ Index. The indexes for a selected area are compared to those for all other locations in the state or nation. For more information and calculation details on the EJ and supplemental indexes, please visit the <a tabindex=\"9\" href=\"https://www.epa.gov/ejscreen\" style=\"color: white\">EJScreen website</a>. </p>
        </div>
        <div style=\"background-color: #71bf44; color: white; text-align: center; padding: 0 32px 7px 32px;\">
            <h3  tabindex=\"10\" style=\"padding-top: 10px; margin-bottom: -10px; font-family: Arial, sans-serif; font-size: 23px;\">EJ INDEXES</h3>
            <p style=\"font-family: Oswald, Arial, sans-serif; font-weight: 300; font-size: 16px; margin: 15px 15% -2px 15%\">The EJ indexes help users screen for potential EJ concerns. To do this, the EJ index combines data on low income and people of color populations with a single environmental indicator.</p>
        </div>'
}
################################################################### #


#' Build header for EJ supp indexes in community report
#'!
#' @keywords internal
#'
generate_ej_supp_header <- function() {
  '<div style=\"background-color: #71bf44; color: white; text-align: center; padding: 0 32px 7px 32px;\">
    <h3 tabindex=\"11\" style=\"padding-top: 10px; margin-bottom: -10px; font-family: Arial, sans-serif; font-size: 23px;\">SUPPLEMENTAL INDEXES</h3>
      <p style=\"font-family: Oswald, Arial, sans-serif; font-weight: 300; font-size: 16px; margin-bottom: -2px; padding-left: 20px;\">The supplemental indexes offer a different perspective on community-level vulnerability. Each one combines the Demographic Indicator with a single environmental indicator.  </p>
  </div>'
}
################################################################### #

# helper for report_residents_within_xyz()

report_xmilesof <- function(radius, unitsingular = 'mile') {
  
  if (all(is.character(radius))) {
    # make sure it/each has a trailing space
    radius[substr(radius, nchar(radius) - 1, nchar(radius)) != " "] <- paste0(radius[substr(radius, nchar(radius) - 1, nchar(radius)) != " "], ' ')
    return(radius) # e.g., "1.3 km from "
    #  but if you provide custom text without ending with "of " then it will look odd
  }
  xmilesof <- ifelse(
    (is.null(radius) || radius == 0 || is.na(radius)), "",
    paste0(radius, " ", unitsingular, ifelse(radius > 1, "s", ""), " of ")
  )
  return(xmilesof)
}
################################################################### #

#' Build text for report: Residents within( X miles of)( any of) the (N) point(s)/polygon(s)/Census unit(s)
#' 
#' @param text1 text to start the phrase, like "Residents within "
#' @param radius The distance from each place, normally in miles (which can be 0), 
#'   or custom text like "seven kilometers from" in which case 
#'   it should end with words like "a safe distance from" or 
#'   "the vicinity of" or "proximity to" or "near" 
#'   -- but may need to specify custom text1 also.
#' @param unitsingular 'mile' by default, but can use 'kilometer' etc. 
#'   Ignored if radius is not a number.
#' @param nsites number of places or text in lieu of number
#' @param sitetype can be 'latlon', 'fips', 'shp', or 
#'   some singular custom text like "Georgia location"
#'   but should be something that can be made plural by just adding "s" so ending with "site" 
#'   works better than ending with "... facility" since that would print as "facilitys" here.
#' 
#' @keywords internal
#'
report_residents_within_xyz <- function(text1 = 'Residents within ', 
                                        radius = NULL, unitsingular = 'mile', 
                                        nsites = 1, 
                                        sitetype = c(
                                          
                                          # uploaded each site
                                          'latlon', 'fips', 'shp',
                                          'frs', 'epa_program_up',
                                          'echo',
                                          
                                          # selected pulldown category
                                          'naics', 'sic', 'mact', 
                                          'epa_program_sel'
                                          )[1]

) {
  
  xmilesof <- report_xmilesof(radius, unitsingular = unitsingular)
  
  sitetype_nullna <- " place"
  if (is.null(sitetype)) {sitetype <- sitetype_nullna}
  sitetype[is.na(sitetype)] <- sitetype_nullna
  
  # uploaded each site ---------------------------------- -
  
  if (sitetype == 'latlon') {
    location_type <- " specified point"
    
  } else if (sitetype == 'fips') {
    location_type <- " specified Census unit"
    
  } else if (sitetype == 'shp') {
    location_type <- " specified polygon"
    
  } else if (sitetype == 'frs') {
    location_type <- " FRS ID-specified site"
    
  } else if (sitetype == 'epa_program_up') {
    location_type <- " EPA Program ID-based site"
    
  } else if (sitetype == 'echo') {
    location_type <- " regulated facility"

    
    # selected pulldown category ---------------------------------- -
    
  } else if (sitetype == 'naics') {
    location_type <- " NAICS industry-specific site"
    
  } else if (sitetype == 'sic') {
    location_type <- " SIC industry-specific site"
    
  } else if (sitetype == 'mact') {
    location_type <- " MACT category site"
    
  } else if (sitetype == 'epa_program_sel') {
    location_type <- " EPA program-specific site"
    

    # misc / unknown ---------------------------------- -
    
  } else if (sitetype == sitetype_nullna) {
    # ok, use default filler
    location_type <- sitetype
  } else { 
    # an unknown sitetype was provided
    location_type <- sitetype
    # make sure it/each has a leading space
    if (substr(location_type,1,1) != " ") {location_type <- paste0(" ", location_type)}
  }
  
  if (is.null(nsites)) {nsites <- ''}
  nsites[is.na(nsites)] <- ""
  anyoftheplaces <- ifelse(nsites == 1, 
                           paste0('this', location_type),
                           paste0("any of the ", nsites, location_type, "s")
  )
  
  residents_within_xyz <- paste0(text1,
                                 xmilesof,
                                 anyoftheplaces)
  return(residents_within_xyz)
}
############################################################################ # 
############################################################################ # 
