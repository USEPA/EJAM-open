
#' Histogram of single indicator from EJAM output

#' @param ejamitout output of an EJAM analysis, like from [ejamit()]
#' @param varname indicator name, such as 'Demog.Ind' or 'pctlowinc'
#' @param distn_type group to show distribution across, either 'Sites' or 'People'
#' @param data_type type of values to show for the indicator, either 'raw' or 'pctile' 
#' @param n_bins number of bins
#' @param sitetype what type of sites were analyzed, like [ejamit()] 'sitetype'. Examples are 'latlon', 'FIPS', 'SHP'
#' 
#' @examples
#' ejam2histogram(testoutput_ejamit_1000pts_1miles, 'Demog.Index', distn_type='Sites', data_type='raw')

ejam2histogram <- function(ejamitout, varname, distn_type = 'Sites', data_type = 'raw', n_bins = 30, sitetype = ejamitout$sitetype){
  
  ## set font sizes
  ggplot_theme_hist <- ggplot2::theme(
    plot.title = ggplot2::element_text(size = 18, hjust = 0.5),
    axis.text  = ggplot2::element_text(size = 16),
    axis.title = ggplot2::element_text(size = 16)
  )
  
  ## future settings: bin sizes, reference lines
  
  if (distn_type == 'Sites') {
    if (data_type == 'raw') {
      
      ## subset doaggregate results_bysite to selected indicator
      if (sitetype == 'SHP') {
        hist_input <- as.data.frame(ejamitout$results_bysite[, varname])#input$summ_hist_ind])
        
      } else {
        hist_input <- ejamitout$results_bysite[, varname, with=F]#input$summ_hist_ind, with = F]
        
      }
      names(hist_input)[1] <- 'indicator'
      
      ## plot histogram
      ggplot2::ggplot(hist_input) +
        ggplot2::geom_histogram(ggplot2::aes(x = indicator), fill = '#005ea2',
                       bins = n_bins) +
        ## set y axis limits to (0, max value) but allow 5% higher on upper end
        ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::labs(
          x = '',
          y = 'Number of sites',
          title = 'Histogram of Raw Indicator Values Across Sites'
        ) +
        ggplot2::theme_bw() +
        ggplot_theme_hist
      
    } else if (data_type == 'pctile') {
      
      ## subset doaggregate results_bysite to selected indicator
      if (sitetype == 'SHP') {
        
        hist_input <- as.data.frame(ejamitout$results_bysite[, paste0('pctile.',varname)])#input$summ_hist_ind])
        
      } else {
        hist_input <- ejamitout$results_bysite[, paste0('pctile.',varname), with=F]#input$summ_hist_ind, with = F]
        
      }
      names(hist_input)[1] <- 'indicator'
      
      ggplot2::ggplot(hist_input) +
        ggplot2::geom_histogram(ggplot2::aes(x = indicator), fill = '#005ea2',
                       #bins = n_bins,
                       breaks = seq(0,100, length.out = n_bins+1)
        ) +
        ggplot2::labs(
          x = '',
          y = 'Number of Sites',
          title = 'Histogram of US Percentile Indicator Values Across Sites'
        ) +
        ggplot2::theme_bw() +
        ggplot_theme_hist
    }
  } else if (distn_type == 'People') {
    if (data_type == 'raw') {
      
      ## subset doaggregate results_bysite to selected indicator
      if (sitetype == 'SHP') {
        
        hist_input <- as.data.frame(ejamitout$results_bysite[, c('pop',varname)])
        
      } else {
        hist_input <- ejamitout$results_bysite[, c('pop',varname), with=F]
        
      }
      names(hist_input)[2] <- 'indicator'
      
      ## plot population weighted histogram
      ggplot2::ggplot(hist_input) +
        ggplot2::geom_histogram(ggplot2::aes(x = indicator, y = ggplot2::after_stat(density), weight = pop), fill = '#005ea2',
                       bins = n_bins) +
        ## set y axis limits to (0, max value) but allow 5% higher on upper end
        ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::labs(
          x = '',
          y = 'Weighted Density',
          title = 'Population Weighted Histogram of Raw Indicator Values'
        ) +
        ggplot2::theme_bw() +
        ggplot_theme_hist
      
    } else if (data_type == 'pctile') {
      
      ## subset doaggregate results_bysite to selected indicator
      if (sitetype == 'SHP') {
        
        hist_input <- as.data.frame(ejamitout$results_bysite[, c('pop',paste0('pctile.',varname))])
        
      } else {
        hist_input <- ejamitout$results_bysite[, c('pop',paste0('pctile.',varname)), with=F]
        
      }
      names(hist_input)[2] <- 'indicator'
      
      ## plot population weighted histogram
      ggplot2::ggplot(hist_input) +
        ggplot2::geom_histogram(ggplot2::aes(x = indicator, y = ggplot2::after_stat(density), weight = pop), fill = '#005ea2',
                       #bins = n_bins,
                       breaks = seq(0,100, length.out = n_bins+1)
        ) +
        ## set y axis limits to (0, max value) but allow 5% higher on upper end
        ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::labs(
          x = '',
          y = 'Weighted Density',
          title = 'Population Weighted Histogram of US Percentile Values'
        ) +
        ggplot2::theme_bw() +
        ggplot_theme_hist
    }
  }
  
}