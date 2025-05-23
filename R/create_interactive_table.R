
#' create_interactive_table - function that creates interactive html version of site by site table in app
#' @description Builds site by site table after an analysis in EJAM app. Pulls in uploaded and analyzed data to create table
#'
#' @param data_processed processed data from app_server
#' @param testing variable to indicate if analysis is being tested
#'
#' @keywords internal
#'
create_interactive_table <- function(data_processed, testing) {

  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(id, ...))
    }
    inputs
  }

  if (testing) {
    cat('interactive_table - preparing (most columns of the) site by site table for DT view \n')
  }

  # --------------------------------------------------- #

  ### disable ejscreen report links while the site is down
  if ("ejscreen_is_down" == "ejscreen_is_down") {
    hyperlink_columns <- 'ECHO Report'  # 'ACS Report'
  } else {
    hyperlink_columns <- c('EJScreen Report', 'EJScreen Map', 'ACS Report','ECHO Report')
  }

  cols_to_select <- c('ejam_uniq_id', 'invalid_msg',
                      'pop', 'Individual Report',
                      hyperlink_columns,
                      names_d, names_d_subgroups,
                      names_e #,
                      # no names here corresponding to number above x threshold, state, region ??
  )
  tableheadnames <- c('Site ID', 'Invalid Reason','Est. Population', 'Individual Report',  # should confirm that Barplot/Community Report belongs here
                      hyperlink_columns,
                      fixcolnames(c(names_d, names_d_subgroups, names_e), 'r', 'shortlabel'))
  ejcols          <- c(names_ej_pctile, names_ej_state_pctile, names_ej_supp_pctile, names_ej_supp_state_pctile)
  ejcols_short <- fixcolnames(ejcols, 'r', 'shortlabel')
  which_ejcols_here <- which(ejcols %in% names(data_processed$results_bysite))
  cols_to_select <- c(cols_to_select, ejcols[which_ejcols_here])
  tableheadnames <- c(tableheadnames, ejcols_short[which_ejcols_here])
  tableheadnames <- c(tableheadnames,
                      names(data_processed$results_summarized$cols),
                      # 'Max of selected indicators',  ###
                      # '# of indicators above threshold',   # will be made more flexible
                      'State', 'EPA Region')
  # --------------------------------------------------- #

  # use data_processed()
  dt <- data_processed$results_bysite %>%
    as.data.frame() %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), .fns = function(x) {round(x, digits = 2)})
                  # *** This should not be hard coded to 2 digits - instead should follow rounding rules provided via table_round() and table_rounding_info() that use map_headernames$decimals  !
                  #
    ) %>%
    dplyr::mutate(index = row_number()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      pop = ifelse(valid == TRUE, pop, NA),
      `Individual Report` = ifelse(valid == TRUE,
                                shinyInput(FUN = actionButton, len = 1,
                                           id = paste0('button_', index),
                                           label = "Generate",
                                           onclick = paste0('Shiny.onInputChange(\"select_button', index,'\", this.id)' )
                                ),
                               '')
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(cols_to_select), ST)

  # use results_summarized that is from batch.summarize()
  batch.sum.cols <- data_processed$results_summarized$cols
  batch.sum.cols[is.na(batch.sum.cols$pop), ] <- NA

  dt_final <- dt %>%
    dplyr::bind_cols(batch.sum.cols) %>%
    ## hide summary rows from table
    #dplyr::bind_rows(dt_avg) %>%
    #dplyr::bind_rows(dt_overall) %>%
    ## sort by Site ID - as numeric index
    #dplyr::arrange(ejam_uniq_id) %>%
    #dplyr::arrange(dplyr::desc(pop)) %>%
    # dplyr::mutate(
    #   Number.of.variables.at.above.threshold.of.90 = ifelse(
    #   is.na(pop), NA,
    #   Number.of.variables.at.above.threshold.of.90)) %>%
    dplyr::mutate(pop = ifelse(is.na(pop), NA, pop)) %>% #prettyNum(round(pop), big.mark = ','))) %>%
    dplyr::left_join(stateinfo %>% dplyr::select(ST, statename, REGION), by = 'ST') %>%
    dplyr::mutate(
      REGION = factor(REGION, levels = 1:10),
      statename = factor(statename)
    ) %>%
    dplyr::select(-ST ) # , -Max.of.variables)    # should be made more flexible so column need not be Max.of.variables

  colnames(dt_final) <- tableheadnames

  dt_final <- dt_final %>%
    dplyr::relocate(dplyr::all_of(c('Invalid Reason', 'State', 'EPA Region')),
                    # , '# of indicators above threshold'),
                    .before = 2) # *** this cutoff should be dynamic, set by probs.default.values etc./ inputs

  ## set # of indicators above threshold to NA if population = 0
  # dt_final <- dt_final %>%
  #   dplyr::mutate(`# of indicators above threshold` = ifelse(`Est. Population` == 0, 'N/A',
  #                                                                `# of indicators above threshold`))

  ## drop indicator column until corrected
  # dt_final <- dt_final %>%
  #   select(-`# of indicators above threshold`)

  n_cols_freeze <- 1

  ## format data table of site by site table
  # see also  EJAM/inst/notes_MISC/DT_datatable_tips_options.R

  out_dt <- DT::datatable(dt_final,
                          rownames = FALSE,
                          ## add column filters (confirm that does work)
                          filter = 'top',
                          ## allow selection of one row at a time (remove to allow multiple)
                          #selection = 'single',
                          selection = 'none',
                          ## add-in for freezing columns
                          extensions = c('FixedColumns'),
                          options = list(
                            ## column width
                            autoWidth = TRUE,
                            ## remove global search box
                            dom = 'lrtip',
                            ## freeze header row when scrolling down
                            fixedHeader = TRUE,
                            fixedColumns = list(leftColumns = n_cols_freeze),
                            pageLength = 100,
                            ## allow scroll left-to-right
                            scrollX = TRUE,
                            ## set scroll height up and down
                            scrollY = '500px'
                          ),
                          ## set overall table height
                          height = 1500,
                          escape = FALSE  # *** escape = FALSE may add security issue but makes links clickable in table
  ) %>%
    DT::formatStyle(names(dt_final), 'white-space' = 'nowrap') %>%
    DT::formatRound('Est. Population', digits = 0, interval = 3, mark = ',')
  #DT::formatStyle(names(dt_final), lineHeight = '80%')
  ## code for bolding certain rows - not currently used
  #           ) %>%
  # DT::formatStyle(
  #   valueColumns = 'Site ID',
  #   target = 'row', columns = 'all',
  #   fontWeight = DT::styleEqual(c('All sites','Average person','Average site'), values = 'bold')
  # )

  return(out_dt)
}


