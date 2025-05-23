

#' Create a gt-format table of results from EJAM
#' 
#' Uses the list of results of ejamit()
#' 
#' @details See the R package called gt. Also see code that creates html tables from html template
#'   and code that creates formatted spreadsheets like [table_xls_format()] 
#' @param ejamitoutput list of EJAM results formatted as in testoutput_ejamit_100pts_1miles, 
#'   as would be the output of ejamit()
#' @param type Must be "demog" or "envt" -- Creates one of these at a time
#' @return Provides table in gt format from the R package called gt 
#' @export
#' @examples  table_gt_from_ejamit(testoutput_ejamit_100pts_1miles)
#' 
table_gt_from_ejamit <- function(ejamitoutput = NULL, type = c("demog", "envt")[1]) {
  
  if (!is.list(ejamitoutput) | !("results_overall" %in% names(ejamitoutput))) {
    warning('RETURNING NA VALUES FOR ALL INDICATORS - Data must be a list of results exactly like the output of ejamit() such as testoutput_ejamit_10pts_1miles')
    return(
      table_gt_from_ejamit_overall(NULL, type = type)
    )
  } else {
    return(
      table_gt_from_ejamit_overall(ejamitoutput$results_overall, type = type)
    )
  } 
}
############################################################################# # 


#' Create a formatted table of results from EJAM overall summary stats
#' 
#' Uses the results_overall element of ejamit() output
#' @param ejamit_results_1row 1-row data.table like testoutput_ejamit_100pts_1miles$results_overall, 
#'   as would come from ejamit(testpoints_10)$results_overall 
#' @param type Must be "demog" or "envt" -- Creates one of these at a time
#' @return Provides table in gt format from the R package called gt 
#' @export
#' @examples 
#'  x <- table_gt_from_ejamit_overall(testoutput_ejamit_100pts_1miles$results_overall)
#'  
table_gt_from_ejamit_overall   <- function(ejamit_results_1row = NULL, type = c("demog", "envt")[1] ) {
  
  ## for example
  # ejamit_results_1row <-  testoutput_ejamit_100pts_1miles$results_overall
  
  x <- table_gt_format_step1(ejamit_results_1row = ejamit_results_1row, type = type) # returns a data.frame
  x <- table_gt_format_step2(x, type = type) # returns a gt table
  x
}
############################################################################# # 

#' Create a formatted table of results for 1 site from EJAM
#' 
#' Uses 1 row from the results_bysite part of ejamit() output
#' @param ... passed to [table_gt_from_ejamit_overall()]
#' @examples 
#'  table_gt_from_ejamit_1site(testoutput_ejamit_100pts_1miles$results_bysite[ 1, ])
#' @export
#' 
table_gt_from_ejamit_1site <- function(...) {
  
  table_gt_from_ejamit_overall(...)
  # alias. Example:  table_gt_from_ejamit_1site(testoutput_ejamit_100pts_1miles$results_bysite[ 1, ])
}
############################################################################# # 


#' Cleans/validates EJAM results for 1 place or overall
#' 
#' This is a first step in formatting results in nice tables
#' @param ejamit_results_1row 1-row data.table like testoutput_ejamit_100pts_1miles$results_overall, 
#'   
#'   as would come from ejamit(testpoints_10)$results_overall
#'    
#'   or a single row of testoutput_ejamit_100pts_1miles$results_bysite 
#' @return Returns the input as a 1-row data.table, indicators etc. in the columns. 
#'   If not a 1 row table, or colnames are not what is expected, it returns correct structure filled with NA values.
#' @export
#' @examples 
#'   x <- table_validated_ejamit_row(testoutput_ejamit_100pts_1miles$results_bysite[ 1, ])
#'   x <- table_validated_ejamit_row(testoutput_ejamit_100pts_1miles$results_overall)
#'   
table_validated_ejamit_row <- function(ejamit_results_1row = NULL) {
  
  if (!is.data.frame(ejamit_results_1row) | NROW(ejamit_results_1row) != 1) {
    warning('RETURNING NA VALUES FOR ALL INDICATORS - Data must be a 1-row table like testoutput_ejamit_10pts_1miles$results_bysite[1, ] or testoutput_ejamit_10pts_1miles$results_overall')
    ejamit_results_1row <- testoutput_ejamit_10pts_1miles$results_overall # used as a template here, but will be filled with NA values
    ejamit_results_1row[ , ] <- NA
    if (!data.table::is.data.table(ejamit_results_1row)) {data.table::setDT(ejamit_results_1row)}
    return(x)
  } else {
    if (!data.table::is.data.table(ejamit_results_1row)) {data.table::setDT(ejamit_results_1row)}
    if (!setequal(names(ejamit_results_1row), colnames(testoutput_ejamit_10pts_1miles$results_overall))) {
      # are just the ratios columns missing, because calculate_ratios = FALSE ?
      if (length(setdiff(setdiff(colnames(testoutput_ejamit_10pts_1miles$results_overall), names(ejamit_results_1row)  ),
                         c(names_these_ratio_to_avg, names_these_ratio_to_state_avg))) > 0) {
      warning('names(ejamit_results_1row) differs from colnames(testoutput_ejamit_10pts_1miles$results_overall) - THIS MAY CAUSE UNPREDICTABLE PROBLEMS')
      } else {
        message('names do not match the full set of typical names, but it seems to be because ratios were not calculated')
      }
      }
  }
  return(ejamit_results_1row)
  # *** if this slows things down at all, one could recode to make more efficient by not returning a copy of the input like this, 
  # and only modifying or not modifying by reference the data.table in calling envt, depending on whether it had problems or not. 
}
############################################################################# # 


#' Validate and reshape 1 row of ejamit results to prep for formatting as gt table/report
#' 
#' Reshapes a few columns of a 1 row data.table into a tall multirow data.frame.
#' @param ejamit_results_1row data.table (or data.frame) like testoutput_ejamit_100pts_1miles$results_overall
#'    from something like ejamit(testpoints_100, radius = 1)$results_overall
#' @param type demog or envt to specify which type of table
#' @seealso [table_gt_from_ejamit()] [table_gt_from_ejamit_overall()] [table_gt_from_ejamit_1site()] [table_validated_ejamit_row()] [table_gt_format_step1()] [table_gt_format_step2()]
#' @export
#'
table_gt_format_step1 <- function(ejamit_results_1row = NULL, type = "demog") {
  
  x <- table_validated_ejamit_row(ejamit_results_1row)
  
  # ignore input$calculate_ratios ? *** 
  ################################### # 
  
  if (type == "envt") {
    
    if (all(names_e_ratio_to_state_avg %in% names(x))) {
      state_ratio <- t(x[, ..names_e_ratio_to_state_avg])
    } else {
      state_ratio <- NULL
    }
    if (all(names_e_ratio_to_avg %in% names(x))) {
      usa_ratio <- t(x[, ..names_e_ratio_to_avg])
    } else {
      usa_ratio <- NULL
    }
    
    table4gt <- table4gt_from_scorevectors(
      varnames_r     = names_e, 
      varnames_shown = fixcolnames(names_e, 'r', 'long'), 
      value        = t(x[, ..names_e]), 
      state_avg    = t(x[, ..names_e_state_avg]),
      state_pctile = t(x[, ..names_e_state_pctile]),
      usa_avg      = t(x[, ..names_e_avg]), 
      usa_pctile   = t(x[, ..names_e_pctile]),  
      state_ratio  = state_ratio, 
      usa_ratio    = usa_ratio,
      ST = NULL
    )
  } else { # assume  type == "demog" 
    
    if (all(names_d_ratio_to_state_avg %in% names(x))) {
      state_ratio <- t(x[, ..names_d_ratio_to_state_avg])
    } else {
      state_ratio <- NULL
    }
    if (all(names_d_ratio_to_avg %in% names(x))) {
      usa_ratio <- t(x[, ..names_d_ratio_to_avg]) 
    } else {
      usa_ratio <- NULL
    }
    
    table4gt <- table4gt_from_scorevectors(
      varnames_r     = names_d, 
      varnames_shown = fixcolnames(names_d, 'r', 'long'), 
      value        = t(x[, ..names_d]), 
      state_avg    = t(x[, ..names_d_state_avg]),
      state_pctile = t(x[, ..names_d_state_pctile]),
      usa_avg      = t(x[, ..names_d_avg]), 
      usa_pctile   = t(x[, ..names_d_pctile]),  
      state_ratio  = state_ratio, 
      usa_ratio    = usa_ratio,
      ST = NULL
    )
    ################################### # 
  }
  return(table4gt)
}
############################################################################# # 


#' Format a table of demog or envt scores, percentiles, etc. to look similar to EJScreen report tables
#' 
#' @param df A data frame from table_gt_format_step1
#'   
#'   which is just a specific format of key EJAM results.
#' 
#'   It has these columns (but it still works if the first two are omitted 
#'   and user-provided indicators are used - it just names them indicator 1, indicator 2, etc.): 
#'   
#'   varnames_r, varnames_shown, value, state_avg, state_pctile, usa_avg, usa_pctile 
#'   
#'    and one row per indicator, where varnames_shown are longer indicator names for use in report.
#'    
#'    The sort order in this df is ignored!  Instead, the variables are shown in the same order as
#'    shown in EJScreen reports, as recorded in map_headernames and checked here via varinfo(varnames_r, "reportsort"), etc.
#'    
#'    Uses gt R package for formatting.
#'    
#' @param type string - must be demog or envt 
#' @param my_cell_color   color for table cell fill backgrounds,  can be given as string ('blue') or hex code ('#0070c0')
#' @param my_border_color color for table borders and boundaries, can be given as string ('blue') or hex code ('#0070c0')
#' @param digits_default number of digits to round to if not specified for a given indicator
#'   (rounding info is drawn from map_headernames$decimals)
#' 
#' @seealso [table_gt_from_ejamit()]
#' @return a gt-style table with formatting to closely match EJScreen standard report formatting
#' @export
#'
table_gt_format_step2 <- function(df, type = c("demog", "envt")[1], my_cell_color =  '#dce6f0', my_border_color = '#aaaaaa',
                                  digits_default = 2) {
  
  # nearRcolor('#dce6f0') is "aliceblue"
  # nearRcolor('#aaaaaa') is "darkgray"
  
  ############################# #
  
  # HANDLE MISSING INFO ####
  #  and make it possible to use this function even without providing all the columns:
  # *** might want to suppress even displaying columns with missing data? e.g., ratios? but be careful is so, as a given location might truly have NA values as result and you want to see that is the case not hide the column.
  
  if (missing(df)) { # show shell
    if (type == "demog") {rnames <- c(names_d, names_d_subgroups)} else {rnames <- names_e} 
    df <- data.frame(varnames_r = rnames, value = NA, stringsAsFactors = FALSE)
  }
  if (!(type[1] %in% c('demog', 'envt'))) {warning("report type must be demog or envt - using demog by default")}
  if (!("varnames_r"     %in% names(df))) {
    if ("varnames_shown" %in% names(df)) {
      df$varnames_r <- fixcolnames(df$varnames_shown, 'long', 'r') # try infer name
    } else {
      warning("Cannot show standard report without variable names - using placeholders") # maybe the user supplied their own indicators
      df$varnames_shown <- paste0("Indicator ", 1:NROW(df))
      df$varnames_r <- df$varnames_shown
    }
  }
  if (!("varnames_shown" %in% names(df))) {df$varnames_shown <- fixcolnames(df$varnames_r, 'r', 'long')}
  if (!("state_avg"      %in% names(df))) {df$state_avg    <- NA}
  if (!("state_pctile"   %in% names(df))) {df$state_pctile <- NA}
  if (!("usa_avg"        %in% names(df))) {
    df$usa_avg <- NA
    df$usa_avg[df$varnames_r %in% names(usastats)] <- usastats[usastats$PCTILE == "mean", match( df$varnames_r, names(usastats), nomatch = 0)] # fill in any that we can
  }  
  if (!("usa_pctile"      %in% names(df))) {df$usa_pctile <- NA}
  if (!("state_ratio"     %in% names(df))) {df$state_ratio <- NA}
  if (!("usa_ratio"       %in% names(df))) {df$usa_ratio <- NA}
  ############################# #
  
  # SORT TABLE ####
  #  to show in the same sort order as EJScreen reports use, acc to metadata in map_headernames
  df <- df[ order(as.numeric(
    as.vector(
      unlist(varinfo(df$varnames_r, "reportsort"))))), ] # items not found are just "" which is ok
  
  # ROUNDING ####
  
  # *** Note there is also the option to specify sigfigs via n_sigfig param, instead of decimals, when using the gt::fmt_number() function!
  
  # digits_rawcols rounding is row-specific, in df$digits_raw for df$value with df$varnames_r
  # digits_percentcols <- 1
  # digits_ratiocols   <- 2  # *** but this is specified in map_headernames also, for each indicator
  # digits_pctilecols  <- 0
  #
  # rawcols     names depend on whether type is demog or envt
  # percentcols names depend on whether type is demog or envt
  # ratiocols  <- c(         'state_ratio',  'usa_ratio')
  pctilecols <- c(         'state_pctile', 'usa_pctile')
  
  # new rounding code rounds each indicator based on digits specified in metadata table:
  
  # rounding for RAW values - get number of decimal places to use as in EJScreen reports, acc to metadata in map_headernames
  df$digits_raw <- table_rounding_info(df$varnames_r) # returns NA for nonmatches
  df$digits_raw[is.na(df$digits_raw)] <- digits_default # default if no match
  digits_raw <- df$digits_raw
    
  varnames_usa_ratio <- fixcolnames(
    df$varnames_r, 'topic_root_term', 'r', 
    mapping_for_names = map_headernames[map_headernames$vartype == 'usratio', ]) # paste0('ratio.to.us.avg.', df$varnames_r)
  varnames_state_ratio <- fixcolnames(
    df$varnames_r, 'topic_root_term', 'r', 
    mapping_for_names = map_headernames[map_headernames$vartype == 'stateratio', ])
  varnames_usa_pctile <- fixcolnames(
    df$varnames_r, 'topic_root_term', 'r', 
    mapping_for_names = map_headernames[map_headernames$vartype == 'uspctile', ])
  varnames_state_pctile <- fixcolnames(
    df$varnames_r, 'topic_root_term', 'r', 
    mapping_for_names = map_headernames[map_headernames$vartype == 'statepctile', ])
  
  digits_usa_ratio <- table_rounding_info(varnames_usa_ratio) # returns NA for nonmatches
  digits_usa_ratio[is.na(digits_usa_ratio)] <- digits_default
  
  digits_state_ratio <- table_rounding_info(varnames_state_ratio) # returns NA for nonmatches
  digits_state_ratio[is.na(digits_state_ratio)] <- digits_default
  
  digits_usa_pctile <- table_rounding_info(varnames_usa_pctile) # returns NA for nonmatches
  digits_usa_pctile[is.na(digits_usa_pctile)] <- digits_default
  
  digits_state_pctile <- table_rounding_info(varnames_state_pctile) # returns NA for nonmatches
  digits_state_pctile[is.na(digits_state_pctile)] <- digits_default
  
  # ENVT or DEMOG ####
  if (type == "demog") {
    ENVT_OR_DEMOG_SECTION_NAME <- '**Socioeconomic Indicators**'
    percentcols <- c("value", "state_avg",    "usa_avg")
    rawcols    <- c("value", "state_avg",    "usa_avg")
    subgroup_rows <- which(df$varnames_r %in% names_d_subgroups )
  } else {
    percentcols <- NULL
    subgroup_rows <- NULL
  }
  if (type == "envt") {
    ENVT_OR_DEMOG_SECTION_NAME <- '**Pollution and Sources**'
    rawcols    <- c("value", "state_avg",    "usa_avg")
  } else {
    rawcols    <- c("value", "state_avg",    "usa_avg")
    }
  ###################################################################################################### # 
  
  # CREATE gt TABLE ####
  
  nice_table <- gt::gt(df[ , c('varnames_shown',
                               'value',
                               'state_avg', 'state_pctile',
                               'usa_avg',     'usa_pctile', 
                               'state_ratio', 'usa_ratio')]) |> 
    ## COLUMN LABELS
    gt::cols_label(
      
      varnames_shown = gt::md(ENVT_OR_DEMOG_SECTION_NAME),   
      value          = gt::md('**Value**'),
      
      state_avg    = gt::md(   '**Average<br> in State**'),
      state_pctile = gt::md('**Percentile<br> in State**'),
      
      usa_avg    = gt::md(   '**Average<br> in USA**'),
      usa_pctile = gt::md('**Percentile<br> in USA**'),
      
      state_ratio = gt::md('**Ratio to<br> State Avg.**'),
      usa_ratio =   gt::md('**Ratio to<br> USA Avg.**')
    )  |>
    
    # RATIO COLUMNS
    
    # old way
    # gt::fmt_number(columns = ratiocols, rows = dplyr::everything(), decimals = digits_ratiocols) |>
    
    # new way (default is all rows, and each row's rounding is defined by value in the new hidden column)
    gt::cols_add(digits_state_ratio = digits_state_ratio) |>
    gt::fmt_number(columns = 'state_ratio', decimals = gt::from_column(column = "digits_state_ratio")) |>
    gt::cols_hide(columns = digits_state_ratio) |>
    
    gt::cols_add(digits_usa_ratio = digits_usa_ratio) |>
    gt::fmt_number(columns = 'usa_ratio', decimals = gt::from_column(column = "digits_usa_ratio")) |>
    gt::cols_hide(columns = digits_usa_ratio) |>
    
    # PERCENTILE COLUMNS
    # old way
    # gt::fmt_number(columns = pctilecols, rows = dplyr::everything(), decimals = digits_pctilecols) 
  
    # new way (default is all rows, and each row's rounding is defined by value in the new hidden column)
    gt::cols_add(digits_state_pctile = digits_state_pctile) |>
    gt::fmt_number(columns = 'state_pctile', decimals = gt::from_column(column = "digits_state_pctile")) |>
    gt::cols_hide(columns = digits_state_pctile) |>
    
    gt::cols_add(digits_usa_pctile = digits_usa_pctile) |>
    gt::fmt_number(columns = 'usa_pctile', decimals = gt::from_column(column = "digits_usa_pctile")) |>
    gt::cols_hide(columns = digits_usa_pctile)
    
  ###################################################################################################### # 
  
  # FORMATTING SPECIFIC TO ENVT VS DEMOG TABLE
  
  if (type == "demog") {
    
    ## add subgroup header?
    # if (length(subgroup_rows) > 0) {
    #       nice_table <- nice_table |> 
    #   gt::tab_row_group(label = gt::md('**Race/ethnic subgroups**'),
    #                     rows = subgroup_rows  # (2+nrow(df) - length(names_d_subgroups)):(nrow(df))
    #   ) |>
    #   # add all/main group header
    #   gt::tab_row_group(label = gt::md('**Socioeconomic Indicators**'),
    #                     rows = 1:nrow(df)   # 1:(1+nrow(df) - length(names_d_subgroups))
    #   )
    # }
    
    # RAW DEMOG (PERCENT) COLUMNS
    nice_table <- nice_table |> 
      # gt::fmt_percent(columns = percentcols, rows = dplyr::everything(),  decimals = digits_percentcols)
      gt::cols_add(digits_raw = digits_raw) |>
      gt::fmt_percent(columns = rawcols, decimals = gt::from_column(column = "digits_raw")) |>
      gt::cols_hide(columns = digits_raw)

  }
  
  if (type == "envt") {
    
    # RAW ENVT (SCORE) COLUMNS       
    # new way, handles a flexible number of rows instead of hard coding it for 13 (default is all rows, and each row's rounding is defined by value in the new hidden column)
    nice_table <- nice_table |> 
      gt::cols_add(digits_raw = digits_raw) |>
      gt::fmt_number(columns = rawcols, decimals = gt::from_column(column = "digits_raw") ) |>
      gt::cols_hide(columns = digits_raw)
      # gt::fmt_number(columns = rawcols, rows = 1,  decimals = df$digits[1]) |>              
      # gt::fmt_number(columns = rawcols, rows = 2,  decimals = df$digits[2]) |> 
      # gt::fmt_number(columns = rawcols, rows = 3,  decimals = df$digits[3]) |> 
      # gt::fmt_number(columns = rawcols, rows = 4,  decimals = df$digits[4]) |> 
      # gt::fmt_number(columns = rawcols, rows = 5,  decimals = df$digits[5]) |> 
      # gt::fmt_number(columns = rawcols, rows = 6,  decimals = df$digits[6]) |> 
      # gt::fmt_number(columns = rawcols, rows = 7,  decimals = df$digits[7]) |> 
      # gt::fmt_number(columns = rawcols, rows = 8,  decimals = df$digits[8]) |> 
      # gt::fmt_number(columns = rawcols, rows = 9,  decimals = df$digits[9]) |> 
      # gt::fmt_number(columns = rawcols, rows = 10, decimals = df$digits[10]) |> 
      # gt::fmt_number(columns = rawcols, rows = 11, decimals = df$digits[11]) |> 
      # gt::fmt_number(columns = rawcols, rows = 12, decimals = df$digits[12]) |> 
      # gt::fmt_number(columns = rawcols, rows = 13, decimals = df$digits[13])   
  }
  ###################################################################################################### # 
  
  nice_table <- nice_table |> 
    
    # MISC FORMATTING ####
  
  ## replace NAs with -- ####
  gt::sub_missing(missing_text = '--') |> 
    
    ##  footnote ####
  gt::tab_footnote(
    footnote = "Avg. in state means the average indicator value, among all the residents at these sites, using the statewide value in each resident's state. Percentile in state means the same, but using the site-specific value (expressed as a percentile) where each resident lives.", 
    locations = gt::cells_column_labels(
      columns = c(state_avg, state_pctile)
    )
  )  |> 
    ## center values for %ile columns
    gt::cols_align(
      align = 'center', columns = pctilecols
    )
  
  nice_table |> 
    
    ## add outer lines
    gt::opt_table_outline(color = my_border_color) |> 
    gt::opt_table_lines() |> 
    
    ## add stripes for alternating rows
    gt::opt_row_striping() |>
    
    ## center column headers
    gt::tab_style(
      style = gt::cell_text(align = 'center', v_align = 'middle'),
      locations = gt::cells_column_labels(columns = dplyr::everything())
    )  |> 
    
    gt::tab_options(
      ## Fonts and Colors ####
      ## set font, font size, font color 
      table.font.names = 'Arial',
      table.font.size = '13.33px', 
      table.font.color = '#333333',
      
      ## change spacing around text
      data_row.padding = '1px',
      data_row.padding.horizontal = '10px',
      row_group.padding = '1px',
      row_group.padding.horizontal = '0px',
      footnotes.padding = '20px',
      
      ## change colors of cells
      row.striping.background_color = my_cell_color,
      row_group.background.color    = my_cell_color,
      
      ## change colors of lines/borders
      column_labels.vlines.color     = my_border_color,
      row_group.border.top.color     = my_border_color,
      row_group.border.bottom.color  = my_border_color,
      table_body.hlines.color        = my_border_color,
      table_body.vlines.color        = my_border_color,
      table_body.border.bottom.color = my_border_color
    ) # |> 
  
  # gtExtras::gt_color_rows( # https://jthomasmock.github.io/gtExtras/reference/gt_color_rows.html
  # 
  #   palette = "ggthemes::colorblind",
  #   # note that you can manually define range like c(4, 6, 8)
  #   domain = c(80,90,95),
  #   pal_type = "discrete"
  # ) |> 
  
  # gt::data_color(  # 
  #   columns = c(state_pctile, usa_pctile),
  #   method = "bin",
  #   palette = c("white", "yellow", "orange", "red"),
  #   domain = c(0, 80, 90, 95)
  # )
  
}
############################################################################# # 



#' DRAFT EXPERIMENTAL - attempt to make table more flexible / any indicators
#' 
#' Based on just indicator names and a value for each, it tries to fill in the rest of a summary table's data.
#'   and formats this as a data.frame ready for the next step
#' @param varnames_r vector of variable names like names_d
#' @param varnames_shown vector like fixcolnames(names_d,'r','short')
#' @param value indicator values for a place or overall
#' @param state_avg indicator values average in State
#' @param state_pctile indicator values as State percentiles
#' @param usa_avg indicator values US average
#' @param usa_pctile indicator values as US percentiles
#' @param state_ratio indicator values as ratio to State average
#' @param usa_ratio indicator values as ratio to US average
#' @param ST State abbreviation like "NY"
#' @seealso [table_gt_from_ejamit()] [table_gt_from_ejamit_overall()] [table_gt_from_ejamit_1site()] [table_validated_ejamit_row()] [table_gt_format_step1()] [table_gt_format_step2()]
#' @return data.frame ready for table_gt_format_step2 ???
#' @keywords internal
#' 
table4gt_from_scorevectors <- function(varnames_r = names_e,
                                       varnames_shown = fixcolnames(varnames_r, 'r', 'long'),
                                       value = as.vector(usastats_means(varnames_r)), # placeholder
                                       state_avg    = NULL,
                                       state_pctile = NULL,
                                       usa_avg      = NULL, 
                                       usa_pctile   = NULL, 
                                       state_ratio  = NULL, 
                                       usa_ratio    = NULL, 
                                       ST = "NY") {
  if (is.null(usa_avg)) {
    usa_avg <-   as.vector(unlist(usastats_means(varnames = varnames_r, dig = 6)))  
  }
  if (is.null(state_avg)) {
    state_avg <-   as.vector(unlist(statestats_query(ST = ST, varnames = varnames_r, PCTILES = "mean",  dig = 6)[ , varnames_r]))
  }
  
  if (is.null(usa_pctile)) {
    usa_pctile <- lookup_pctile(myvector = value, varname.in.lookup.table = varnames_r, zone = "USA")
  }
  if (is.null(state_pctile)) {
    state_pctile <- lookup_pctile(myvector = value, varname.in.lookup.table = varnames_r, zone = ST, lookup = statestats)
  }
  
  if (is.null(usa_ratio)) {
    usa_ratio <- value / usa_avg
    usa_ratio[usa_avg == 0] <- NA
  }
  if (is.null(state_ratio)) {
    state_ratio <- value / state_avg
    state_ratio[state_avg == 0] <- NA
  }
  
  data.frame(
    varnames_r = varnames_r, 
    varnames_shown = varnames_shown, 
    value = value,
    state_avg = state_avg,
    state_pctile = state_pctile,
    usa_avg = usa_avg, 
    usa_pctile = usa_pctile, 
    state_ratio = state_ratio,  
    usa_ratio = usa_ratio
  )
  
}
############################################################################# # 
