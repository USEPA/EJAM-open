

#' Create facetted barplots of groups of indicators
#'
#' @param ejamitout output from running an EJAM analysis, with ejamit or the EJAM shiny app
#' @param indicator_type group of indicators to display, such as 'Environmental', etc.
#' @param data_type form to display data in: 'raw' or 'ratio'
#'
#' @returns ggplot object with facets for each indicator and 3 bars
#'
#' @examples ejam2barplot_indicators(testoutput_ejamit_1000pts_1miles)
#' 
ejam2barplot_indicators <- function(ejamitout, indicator_type = 'Demographic', data_type = 'raw') {
  
  ## set indicator group column names
  mybarvars <- switch(indicator_type,
                      'Demographic'   = c(names_d, names_d_subgroups),
                      'Environmental' = names_e,
                      'EJ Index'            = names_ej, # aka Summary Index
                      'Supplementary Index' = names_ej_supp # aka Suppl. Summary Index
  )
  
  ## set indicator group friendly names - use shortlabel
  mybarvars.friendly <- fixcolnames(mybarvars, oldtype = 'r', newtype = 'shortlabel')
  
  ## only using average for now
  mybarvars.stat <- 'avg' #"med"
  
  ## defaulting to average only in this version of EJAM
  mybarvars.sumstat <- c('Average site',
                         'Average person at these sites')
  
  ## if adding median ('med') back in future, can use this
  #mybarvars.stat <- input$summ_bar_stat
  # mybarvars.sumstat <- switch(input$summ_bar_stat,
  #                             'med' =  c('Median site', 'Median person'),
  #                             'avg' = c('Average site','Average person')
  # )
  
  ## filter to necessary parts of batch.summarize output
  
    barplot_data <- ejamitout$results_summarized$rows %>%
      tibble::rownames_to_column(var = 'Summary') %>%
      dplyr::mutate(Summary = gsub('Average person',
                                   'Average person at these sites',
                                   .data$Summary)) %>%
      dplyr::filter(.data$Summary %in% mybarvars.sumstat)
  
  ## set ggplot theme elements for all versions of barplot
  ggplot_theme_bar <- ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom',
                   axis.text = ggplot2::element_text(size = 16),
                   axis.title = ggplot2::element_text(size = 16),
                   legend.title = ggplot2::element_text(size = 16),
                   legend.text = ggplot2::element_text(size = 16),
                   strip.text = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank()
    )
  
  ## raw data
  if (data_type == 'raw') {
    
    myBarVarsDataRaw <- if (indicator_type == 'EJ Index') {
      names_ej_pctile
    } else if (indicator_type == 'Supplementary Index') {
      names_ej_supp_pctile
    } else {
      mybarvars
    }
    
    ## pivot from wide to long, 1 row per indicator
    barplot_data_raw <- barplot_data %>%
      dplyr::select(Summary, dplyr::all_of(myBarVarsDataRaw)) %>%
      tidyr::pivot_longer(cols = -1, names_to = 'indicator') %>%
      dplyr::mutate(type = 'raw') %>%
      dplyr::mutate(indicator = gsub("^pctile\\.", "", .data$indicator))
    
    ## median - not currently displayed
    if (mybarvars.stat == 'med') {
      barplot_usa_med <- usastats %>%
        dplyr::filter(.data$REGION == 'USA', .data$PCTILE == 50) %>% # for median
        dplyr::mutate(Summary = 'Median person in US') %>%
        dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
        tidyr::pivot_longer(-Summary, names_to = 'indicator')
      
      ## NOTE: Median Person calculations are all 0s for now!
      barplot_input <- dplyr::bind_rows(barplot_data_raw, barplot_usa_med)
      
      ## average
    } else {
      barplot_usa_avg <- usastats %>%
        dplyr::filter(.data$REGION == 'USA', .data$PCTILE == 'mean') %>%
        dplyr::mutate(Summary = 'Average person in US') %>%
        dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
        tidyr::pivot_longer(-Summary, names_to = 'indicator')
      
      barplot_input <- dplyr::bind_rows(barplot_data_raw, barplot_usa_avg)
    }
    
    ## set # of characters to wrap labels
    n_chars_wrap <- 15
    
    barplot_input$Summary <- factor(barplot_input$Summary, 
                                    levels = c('Average person in US',
                                               'Average site',
                                               'Average person at these sites'))
    
    ## merge with friendly names and plot
    p_out <- barplot_input %>%
      dplyr::left_join( data.frame(indicator = mybarvars, indicator_label = gsub(' \\(.*', '', mybarvars.friendly))) %>%
      ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = indicator_label, y = value, fill = Summary), stat = 'identity', position = 'dodge') +
      ggplot2::scale_fill_manual(values = c('Average person in US' = 'lightgray', 
                                   'Average person at these sites' = '#62c342',
                                   'Average site' = '#0e6cb5')) +
      #scale_fill_brewer(palette = 'Dark2') +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, n_chars_wrap)) +
      ## set y axis limits to (0, max value) but allow 5% higher on upper end
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05)))
    
    ## let environmental raw values have their own y axis
    if (indicator_type == 'Environmental') {
      p_out <- p_out + ggplot2::facet_wrap(~indicator_label,
                                  #ncol = 4,
                                  scales = 'free')
    } else {
      p_out <- p_out + ggplot2::facet_wrap(~indicator_label,
                                  #ncol = 4,
                                  scales = 'free_x')
    }
    
    p_out +
      ggplot2::labs(x = NULL, y = 'Indicator Value', fill = 'Legend') +
      ggplot_theme_bar
    
    ## future: add % scaling and formatting for residential population indicators
    ## see ggplot2::scale_y_continuous and scales::label_percent
    
    ## ratio to us
  } else if (data_type == 'ratio') {
    
    barplot_data_raw <- barplot_data %>%
      dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
      tidyr::pivot_longer(cols = -1, names_to = 'indicator')
    
    ## average
    if (mybarvars.stat == 'avg') {
      ## pull US average values from usastats to compute ratios
      barplot_usa_avg <-  dplyr::bind_rows(
        usastats %>%
          dplyr::filter(.data$REGION == 'USA', .data$PCTILE == 'mean') %>%
          dplyr::mutate(Summary = 'Average person at these sites') %>%
          dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
          tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value'),
        usastats %>%
          dplyr::filter(.data$REGION == 'USA', .data$PCTILE == 'mean') %>%
          dplyr::mutate(Summary = 'Average site') %>%
          dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
          tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value')
      )
      
      ## combine raw data with US averages
      barplot_input <- dplyr::left_join(
        barplot_data_raw,
        barplot_usa_avg
      ) %>%
        ## divide to get ratios
        dplyr::mutate(ratio = .data$value / .data$usa_value) %>%
        ## add row of all 1s to represent US average ratio being constant at 1
        dplyr::bind_rows(
          data.frame(Summary = 'Average person in US', indicator = mybarvars, value = 1, usa_value = 1, ratio = 1)
        )
      
    } else {
      ## median - not currently displayed
      barplot_usa_med <-  dplyr::bind_rows(
        usastats %>%
          dplyr::filter(.data$REGION == 'USA', .data$PCTILE == 50) %>%
          dplyr::mutate(Summary = 'Median person') %>%
          dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
          tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value'),
        usastats %>%
          dplyr::filter(.data$REGION == 'USA', .data$PCTILE == 50) %>%
          dplyr::mutate(Summary = 'Median site') %>%
          dplyr::select(Summary, dplyr::all_of(mybarvars)) %>%
          tidyr::pivot_longer(-Summary, names_to = 'indicator', values_to = 'usa_value')
      )
      
      barplot_input <- dplyr::left_join(barplot_data_raw, barplot_usa_med) %>%
        ## calc ratio
        dplyr::mutate(ratio = .data$value / .data$usa_value) %>%
        dplyr::bind_rows(
          data.frame(Summary = 'Median person in US', indicator = mybarvars, value = 1, usa_value = 1, ratio = 1)
        )
    }
    
    ## set # of characters to wrap labels
    n_chars_wrap <- 15
    
    barplot_input$Summary <- factor(barplot_input$Summary, levels = c(
      'Average person in US',
      'Average site',
      'Average person at these sites'
    ))
    
    ## join and plot
    barplot_input %>%
      dplyr::left_join( data.frame(indicator = mybarvars, indicator_label =  gsub(' \\(.*', '', mybarvars.friendly))) %>%
      ggplot2::ggplot() +
      ## add bars - position = 'dodge' places the 3 categories next to each other
      ggplot2::geom_bar(ggplot2::aes(x = indicator_label, y = ratio, fill = Summary), stat = 'identity', position = 'dodge') +
      ## add horizontal line at 1
      ggplot2::geom_hline(ggplot2::aes(yintercept = 1)) +
      ## set color scheme
      ggplot2::scale_fill_manual(values = c(
        'Average person in US'          = 'lightgray',
        'Average person at these sites' = '#62c342',
        'Average site'                  = '#0e6cb5'
      )) +
      # scale_fill_brewer(palette = 'Dark2') +
      ## alternate color scheme
      ## wrap long indicator labels on x axis
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, n_chars_wrap)) +
      ## set y axis limits to (0, max value) but allow 5% higher on upper end
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
      ## set axis labels
      ggplot2::labs(x = '', y = 'Indicator Ratio', fill = 'Legend') +
      ## break plots into rows of 4
      ggplot2::facet_wrap(~indicator_label,
                 #ncol = 4,
                 scales = 'free_x') +
      ggplot_theme_bar
  }
}
