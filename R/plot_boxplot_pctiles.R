

## notes on names:
#
# ejam2barplot()   and ejam2boxplot() 
# plot_barplot_ratios()  but no  plot_boxplot_pctiles 
# boxplots_ratios()  but no  boxplot_pctiles  
# plot_vs_us() for a single indicator at a time. 
 


#' Boxplots comparing a few indicators showing how each varies across sites
#' Visualize mean median etc. for each of several percentile indicators
#' 
#' @param out The output from ejamit() such as 
#'   testoutput_ejamit_10pts_1miles
#' @param vars Typically would be one of these:
#'  names_d_pctile, names_d_state_pctile, 
#'  names_d_subgroups_pctile, names_d_subgroups_state_pctile, 
#'  names_e_pctile, names_e_state_pctile,
#'  and possibly ratios or others, but this is designed to plot pctiles.
#' @param ylab inferred from vars normally
#' @param ranked set FALSE to avoid sorting x axis on size of wtdmeans
#' @param ... passed to boxplot()
#' 
#' 
#' @examples
#' \dontrun{
#' bplot = plot_boxplot_pctiles
#' bplot(out, names_d_state_pctile)
#' bplot(out, names_d_subgroups_state_pctile)
#' bplot(out, names_e_state_pctile)
#' bplot(out, names_d_pctile)
#' bplot(out, names_d_subgroups_pctile)
#' bplot(out, names_e_pctile)
#' }
#' @return prints means etc. and plots
#' 
#' @export
#'
plot_boxplot_pctiles <- function(out, vars = names_d_state_pctile, ylab = "Percentile in State", ranked = TRUE, ...) {
  
  if (missing(ylab)) {
    if (all(grepl('state_pctile', unique(varinfo(vars, 'varlist')$varlist)))) {
      ylab = "Percentile in State"
    }
    if (all(grepl('pctile', unique(varinfo(vars, 'varlist')$varlist))) &
        all(!grepl('state_pctile', unique(varinfo(vars, 'varlist')$varlist)))) {
      ylab = "Percentile in US"
    }
    if (all(grepl('ratio_to_avg', unique(varinfo(vars, 'varlist')$varlist))) ) {
      ylab = "Ratio to US Average"
    }
    if (all(grepl('ratio_to_state_avg', unique(varinfo(vars, 'varlist')$varlist))) ) {
      ylab = "Ratio to State Average"
    }
  }
  
  # site mean does not really make sense for percentiles though
  sitemeans = colMeans(  out$results_bysite[, ..vars ], na.rm = T)
  # popmeans = sapply(  out$results_bysite[, ..vars ], FUN = function(z) weighted.mean(z, w = out$results_bysite$pop, na.rm = T))
  wtdmeans =  as.vector(unlist(out$results_overall[, ..vars]))
  # print(round(cbind(sitemeans = sitemeans, wtdmeans = wtdmeans), 0))
  print( t(round(out$results_summarized$rows[, vars][1:2,], 0)))
  cat("\n")
  
  if (ranked) {
    vars <- vars[order(wtdmeans, decreasing = TRUE)]
    # site mean does not really make sense for percentiles though
    sitemeans = colMeans(  out$results_bysite[, ..vars ], na.rm = T)
    # popmeans = sapply(  out$results_bysite[, ..vars ], FUN = function(z) weighted.mean(z, w = out$results_bysite$pop, na.rm = T))
    wtdmeans =  as.vector(unlist(out$results_overall[, ..vars]))
    # print(round(cbind(sitemeans = sitemeans, wtdmeans = wtdmeans), 0))
    print( t(round(out$results_summarized$rows[, vars][1:2,], 0)))
    cat("\n\n")
  }
  
  df = out$results_bysite[, ..vars]
  nam =  gsub( "State%ile ", "", fixcolnames(vars, 'r','short'))
  nam =  gsub( "US%ile ", "", nam)
  
  if (grepl("Percentile", ylab)) {
    df.ref = 1:100
  } else {
    df.ref = NA
  }
  nam.ref = "Reference"
  if (grepl("US", ylab)) {
    nam.ref = "Reference (US)"
  } else {
    if (grepl("state", ylab, ignore.case = T)) {
      nam.ref = "Reference (State)"
    }
  }
  nam = c(nam, nam.ref)
  suppressWarnings({ # df has N rows but df.ref may have 100 or 1 elements
    df = cbind(df, df.ref)
  })
  
  boxplot(df, 
          names = nam,
          ylab = ylab,
          xlab = "Indicators (filled circle = avg. resident overall, filled triangle = avg. of sites, open triangle = at one site)",
          main = paste0( "Ranges of Values Across ", NROW(out$results_bysite), " Analyzed Locations"),
          ...
  )
  
  points(
    1:length(vars),  
    sitemeans,
    pch = 17, cex = 1.5, col = 'blue'  # percentile at avg site (avg over sites, of the pctile for the avg resident at each site)
  )
  
  points(
    1:length(vars),  
    wtdmeans,
    pch = 19, cex = 2, col = 'black'  # percentile for avg resident overall (across all sites)
  )
  
  if (grepl("Percentile", ylab)) {
    abline(h = 50, col = 'gray'); abline(h = 80, col = 'yellow'); abline(h = 90, col = "orange"); abline(h = 95, col = "red")
  }
  if (grepl("ratio", ylab, ignore.case = T)) {
    abline(h = 1, col = 'gray'); abline(h = 2, col = 'yellow'); abline(h = 3, col = 'orange'); abline(h = 5, col = 'red')
  }
  
  # dot per site
  for (i in 1:length(vars)) {
    points(
      jitter(rep(i, NROW(out$results_bysite))), 
      as.vector(unlist(out$results_bysite[, vars[i], with = FALSE])),
      pch = 2, cex = 1,
      col = 'black'    # one dot per site, percentile for avg resident at that site
    )
  }
  
  # US/State overall as reference
  points(
    jitter(rep(1 + length(vars), length(df.ref))),
    1:length(df.ref), 
    pch = 2, cex = 1, 
    col = 'darkgray')
  
}
################################################################# # 
# bplot <- plot_boxplot_pctiles

################################################################# # 
