 
# NOTES ON PLOT FUNCTIONS 
 
  # See [plot_boxplot_pctiles()] for PERCENTILE indicators compared in one plot.
  #       and [boxplot_ratios()] for an older RATIO plot.
  # See [ejam2barplot()]  for RATIO indicators compared in one plot.
  # See [plot_boxplot_vs_ref()] for 1 RAW indicator vs. a reference distribution.
#
# maybe  plot_barplot_ratios_ez() for ??

# maybe 
# ejam2plot() could be a wrapper to provide 
#    a box/violin/ridgline (or just simpler bar), or scatter  or line(s) plots?
#    for raw, ratio, pctile for D,E,EJ  
#     in US or State,  
#   at 1overall or 1site, 
#      or comparing sites like full bysite distribution,  
#      or comparing distance / as a function of distance, etc. (overall or 1site)
#      or comparing type of site (groups of sites) (overall stats for each group)
#    for multiple indicators (or 1 vs reference distrib.)

# ejam2boxplot
# ejam2boxplot_pctiles = plot_boxplot_pctiles

########################################################## # 

# ejam2boxplot <- function(ejamitout, 
#                          vartype =  c("raw", "usratio", "stateratio",  "uspctile", "statepctile", "usavg", "stateavg", "usraw", "stateraw" ),
#                          varcategory = c("Demographic", "Environmental", "EJ Index"), 
#                          ...) {
#   # can wrap plot_boxplot_pctiles() for PCTILES,
#      and boxplot_ratios() for RATIOS, and 
#    plot_boxplot_vs_ref()  for 1 RAW  or   ???? for multiple Raw scores? not on same units so not so commonly needed.
#   
# } 
###   or   ####################################################### # 

# ejam2boxplot <- function(ejamitout = NULL, # ejamit()$results_bysite, ## name differs from that in plot_vs_us()
#                          varname = "pctlowinc", 
#                          type = 'ggplot', # "box",
#                          refarealabel = "All Blockgroups Nationwide",
#                          siteslabel = "At Sites Analyzed",
#                          siteidlabel = NULL,
#                          # could recode to allow multiple types of sites and/or reference zones more generally like for ejamit_compare_types_of_places()
#                          refdata = NULL, 
#                          nsample = 5000, fix_pctcols = TRUE,
#                          colorfills = c("lightblue",  "yellow"),
#                          box.cex.ref = 0.6, box.cex.here = 2.2,  
#                          box.pch.ref = 20, box.pch.here = 2,
#                          ...) {
#   
#   plot_vs_us(bysite = ejamitout, 
#              type = type, 
#              refarealabel,
#              siteslabel,
#              siteidlabel ,
#              # could recode to allow multiple types of sites and/or reference zones more generally like for ejamit_compare_types_of_places()
#              refdata, 
#              nsample , fix_pctcols,
#              colorfills ,
#              box.cex.ref, box.cex.here,  
#              box.pch.ref, box.pch.here,
#              ...)
# }
########################################################## # 

#' @export
#' 
plot_boxplot_vs_ref <- function(bysite = NULL, # ejamit()$results_bysite, 
                                varname = "pctlowinc", 
                                type = 'ggplot', # "box",
                                refarealabel = "All Blockgroups Nationwide",
                                siteslabel = "At Sites Analyzed",
                                siteidlabel = NULL,
                                # could recode to allow multiple types of sites and/or reference zones more generally like for ejamit_compare_types_of_places()
                                refdata = NULL, 
                                nsample = 5000, fix_pctcols = TRUE,
                                colorfills = c("lightblue",  "yellow"),
                                box.cex.ref = 0.6, box.cex.here = 2.2,  
                                box.pch.ref = 20, box.pch.here = 2,
                                ...) {
  
  plot_vs_us(bysite, 
             varname , 
             type, # "box",
             refarealabel,
             siteslabel,
             siteidlabel ,
             # could recode to allow multiple types of sites and/or reference zones more generally like for ejamit_compare_types_of_places()
             refdata, 
             nsample , fix_pctcols,
             colorfills ,
             box.cex.ref, box.cex.here,  
             box.pch.ref, box.pch.here,
             ...)
}
########################################################## # 


#' Plot distribution of data among residents at sites vs reference area (US, etc.)
#' @description Visualize indicator values (scores) as distributed across places
#' @aliases plot_boxplot_vs_ref 
#' 
#' @seealso 
#'   See [plot_boxplot_pctiles()] for Percentile indicators compared in one plot. 
#'   and [boxplot_ratios()] for an older Ratios plot.
#'   See [ejam2barplot()]  for Ratio indicators compared in one plot.
#'   See [plot_boxplot_vs_ref()] for a Raw indicator vs. a reference distribution.
#'   
#' @details
#' 
#'   Not population weighted, so it is the distribution across sites not residents.
#' 
#'   Could be edited to allow multiple types of sites and/or reference zones 
#'   more generally like for ejamit_compare_types_of_places()
#'   
#' @param bysite table of results from ejamit()$results_bysite, like testoutput_ejamit_1000pts_1miles$results_bysite
#' @param varname name of column in bysite, like  "Demog.Index"
#' @param type "box", "plotly", or "ggplot"
#' @param refarealabel e.g., "All blockgroups in this State"
#' @param siteslabel e.g., "At Avg. Site Analyzed"
#' @param siteidlabel vector of text one per site to show if type 'box'
#' @param refdata reference area dataset, like blockgroupstats, but must have columns
#'   named 'pop' and varname. e.g., refdata = blockgroupstats[ST %in% "DE", .(pop, pcthisp)]
#' @param nsample to limit dots on plot of ref area like all bg in US
#' @param colorfills two colors for boxplot
#' @param box.cex.ref use default
#' @param box.cex.here use default
#' @param box.pch.ref use default
#' @param box.pch.here description
#' @param ... passed to boxplot()
#' @examples 
#' \dontrun{
#'   out <- testoutput_ejamit_1000pts_1miles
#'   # ejam2boxplot(out)
#'   # plot_boxplot_vs_ref(out$results_bysite)
#'   plot_vs_us(out$results_bysite, type = 'box')
#'   plot_vs_us(out$results_bysite, varname = "pctlingiso", type =  'box', ylim=c(0, 20))
#'   plot_vs_us(out$results_bysite, varname = "pctlingiso", type =  'ggplot')
#'   plot_vs_us(out$results_bysite, varname = "pctnhaa", type =  'ggplot')
#'   plot_vs_us(out$results_bysite, varname = "pctnhaa", type = 'box', ylim = c(0, 20))
#'   
#'  # td = testoutput_ejamit_1000pts_1miles$results_bysite
#'  # plot_vs_us(, type = 'box')
#'  # plot_vs_us(td, varname = "pctlingiso", type =  'box', ylim=c(0,20))
#'  # plot_vs_us(td, varname = "pctlingiso", type =  'ggplot')
#'  # plot_vs_us(td, varname = "pctnhaa", type =  'ggplot')
#'  # plot_vs_us(td, varname = "pctnhaa", type = 'box', ylim = c(0,20))
#'  # plot_vs_us(td[td$ST %in% "DE", ], 'pcthisp', refdata = blockgroupstats[ST %in% "DE", .(pop, pcthisp)])
#'   }
#'   
#' @return plots
#' 
#' @export
#' @keywords internal
#'
plot_vs_us <- function(bysite = NULL, # ejamit()$results_bysite, 
                       varname = "pctlowinc", 
                       type = 'ggplot', # "box",
                       refarealabel = "All Blockgroups Nationwide",
                       siteslabel = "At Sites Analyzed",
                       siteidlabel = NULL,
                       # could recode to allow multiple types of sites and/or reference zones more generally like for ejamit_compare_types_of_places()
                       refdata = NULL, 
                       nsample = 5000, fix_pctcols = TRUE,
                       colorfills = c("lightblue",  "yellow"),
                       box.cex.ref = 0.6, box.cex.here = 2.2,  
                       box.pch.ref = 20, box.pch.here = 2,
                       ...) {
  
  warning("draft function")
  
  # bysite
  if (is.null(bysite)) {
    if (interactive()) {   # request a new analysis interactively
      bysite <- ejamit()$results_bysite
    } else {
      stop("bysite is required")
    }
  }
  
  if ("results_bysite" %in% names(bysite)) {
    # looks like full output of ejamit() was provided, not just the results_bysite data.table
    bysite <- copy(bysite$results_bysite)
  } else {
    bysite <- copy(bysite)
  }
  
  stopifnot(NCOL(varname) == 1, NROW(varname) == 1, "character" %in% class(varname), all(varname %in% colnames(bysite)))
  
  # correct for different 0-1 or 0-100 scaling in blockgroupstats and ejamit()$results_bysite
  bysite <- fix_pctcols_x100(bysite, cnames = names_pct_as_fraction_ejamit)
  data.table::setDT(bysite)
  sites <- cbind(bysite[ , c("pop", varname, 'ejam_uniq_id'), with = FALSE], Locations = siteslabel)
  rm(bysite)
  
  # refdata -- If no reference area is specified, use all US block groups
  if (is.null(refdata)) {
    if (!(varname %in% names(blockgroupstats))) {stop(varname, "must be a column name in refdata (which is blockgroupstats by default)")}
    if (is.data.table(blockgroupstats)) {
      setDF(blockgroupstats)
      refdata <- blockgroupstats[!is.na(blockgroupstats$pop) & !is.na(blockgroupstats[ , varname]), c("pop", varname)]
      setDT(blockgroupstats)
    } else {
      refdata <- blockgroupstats[!is.na(blockgroupstats$pop) & !is.na(blockgroupstats[ , varname]), c("pop", varname)]
    }
    if (!fix_pctcols) {warning("if using default refdata, blockgroupstats, fix_pctcols must be TRUE and ignored if set FALSE")}
    refdata <- fix_pctcols_x100(refdata, cnames = names_pct_as_fraction_blockgroupstats)
  } else {
    if (!(varname %in% names(refdata))) {stop(varname, "must be a column name in refdata")}
    if (!( 'pop' %in% names(refdata))) {stop( 'pop', "must be a column name in refdata")}
    if (fix_pctcols) {
      message("assuming refdata provided was a subset of blockgroupstats, so rescaling some indicators to ensure all percentages are scaled as 0-100 not 0-1")
      refdata <- fix_pctcols_x100(refdata, cnames = names_pct_as_fraction_blockgroupstats)
    }
  }
  data.table::setDT(refdata)
  refdata$ejam_uniq_id <- NA
  refdata$Locations <- refarealabel
  refdata <- refdata[!is.na(pop), ]
  
  # Combine reference area and specified locations (e.g. near these sites)
  # as full dataset but also a smaller sampling
  both.sample <- rbind(refdata[sample(1:NROW(refdata), nsample), ], sites)
  both        <- rbind(refdata, sites)
  
  setnames(both,        varname, 'literalvarname')
  setnames(both.sample, varname, 'literalvarname')
  
  bothmeansinfo <- both[ , .(
    mean = mean(literalvarname, na.rm = T) #,
    # both75 = quantile(literalvarname, na.rm = T, probs = 0.75, type = 1),
    # both25 = quantile(literalvarname, na.rm = T, probs = 0.25, type = 1)
  ), by = "Locations"]
  setorder(bothmeansinfo, Locations) # alphabetical to match how boxplot does it
  bothmeans = bothmeansinfo$mean
  
  varlabel <- fixcolnames(varname, 'r', 'shortlabel')
  maintitle <- paste0("Comparison of ", varlabel, " among Residents ", siteslabel, " versus ", refarealabel)
  
  # browser()
  #################################################################### #
  if (type == 'box') {
    
    # y axis says what the mean values are
    ylabel = paste0(varlabel, ': ', 
                    round(bothmeansinfo$mean[bothmeansinfo$Locations != siteslabel], 1), 
                    " in reference areas vs. ",
                    round(bothmeansinfo$mean[bothmeansinfo$Locations == siteslabel], 1),
                    ' at avg. site analyzed'  )
    
    here <- siteslabel == both.sample$Locations
    
    boxplot(literalvarname ~ Locations, data = both, ylab = ylabel, col = colorfills,
            xlab = "Locations (triangle size indicates population count at site & label is site ID)",
            main = paste0(maintitle, "\n(boxplot represents a distribution over ", sum(here)," sites, not population weighted quantiles)"), ...)
    
    # boxplot using ~ Locations seems to sort them alphabetically, so "All Blockgroups Nationwide" is before  "At Avg. Site Analyzed"
    
    # draw points, one per site, and a sampling of US(reference area) blocks
    
    #               site population determines size of dot in plot
    if (missing(box.cex.here)) {
      # scale it based on pop from 1 to 5 dotsize
      # dotsize is 1 if pop is <=100
      # dotsize is 2.5 if pop is popmean
      # dotsize is 5 if pop is popmax
      # dotsize is scaled proportionately from 1 to 2.5 for pop values of 100 to popmean
      # dotsize is scaled proportionately from 2.5 to 5 for pop values of popmean to popmax
      popvalues <- both.sample$pop[here]
      dotmin = 1 # 100 people ?
      dotmean = 2.5
      dotmax = 5 #
      popmin = 100 # fixed
      popmean = mean(popvalues, na.rm = T)
      popmax =  max(popvalues, na.rm = T)
      dotsize = rep(dotmin, length(popvalues)) # so NA pop uses that
      dotsize[!is.na(popvalues) & popvalues <= popmean] <- dotmin + ((popvalues[!is.na(popvalues) & popvalues <= popmean] - popmin) / (popmean - popmin)) * (dotmean - dotmin)
      dotsize[!is.na(popvalues) & popvalues > popmean] <- dotmean + ((popvalues[!is.na(popvalues) & popvalues > popmean] - popmean) / (popmax - popmean)) * (dotmax - dotmean)
    } else {
      # fixed
      dotsize = box.cex.here
    }
    
    # Draw reference area points (just a random sample of them)
    points(jitter(1 + (siteslabel == both.sample$Locations[!here])), both.sample$literalvarname[!here], 
           pch = box.pch.ref,  col = "darkgray", 
           cex = box.cex.ref) # pch = "."
    
    # Draw site points
    xv = jitter(1 + (siteslabel == both.sample$Locations[here]))
    yv = both.sample$literalvarname[here]
    points(xv,  yv,  
           pch = box.pch.here, col = "black",    
           cex = dotsize)
    
    # text label at each point with the site ID
    if (missing(siteidlabel)) {
      siteidlabel <- both.sample$ejam_uniq_id[here]
    } else {
      if (length(siteidlabel) != NROW(both.sample$ejam_uniq_id[here])) {
        warning('siteidlabel must be same length as list of sites')
        siteidlabel <- both.sample$ejam_uniq_id[here]
      }
    }
    text(x = xv, y = yv, labels = siteidlabel, pos = 4, cex = 0.6)
    
    # Averages: draw horizontal line and point at mean value for sites and for US(ref area)
    points(1:2, bothmeans, col = 'black', pch = 22, bg = "white", cex = 3)
    abline(h = bothmeans[1], col = colorfills[1] )
    abline(h = bothmeans[2], col = 'orange') # colorfills[2])
    
  } else {
    #################################################################### #
    if (type == 'plotly') {
      
      setnames(both.sample, "literalvarname", "Indicator")
      d <- plotly::highlight_key(both.sample)
      # scatt <- plot_ly(d, x = ~Locations, y = ~Indicator) %>%
      #   add_markers(color = I("black"))
      
      # 'statistical trace types'
      # hist <- plotly::plot_ly(d, x = ~factor(Locations)) %>% 
      #   plotly::add_histogram(color = I("black"))
      # box <- plotly::plot_ly(d, x = ~Locations, y = ~Indicator, color = I("black")) %>% 
      #   plotly::add_boxplot(name = " ")
      
      violin <- plotly::plot_ly(d, x = ~Locations, y = ~Indicator, color = I("blue")) %>%
        plotly::add_trace(type = "violin", name = " ")
      plotly::subplot(
        # scatt, box, 
        violin, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
        # subplot(hist, widths = c(.75, .25), titleX = TRUE, titleY = TRUE) %>%
        plotly::layout(
          barmode = "overlay", 
          title = maintitle,
          showlegend = FALSE
        ) %>%
        plotly::highlight("plotly_selected")
      
    } else {
      #################################################################### #
      if (type == 'ggplot') {
        
        # https://r-graph-gallery.com/violin.html
        
        ggplot2::ggplot(both.sample, aes(x = Locations, y = literalvarname, color = Locations, fill = Locations)) +
          
          scale_color_manual(values = c( "gray", "black", "lightblue"),  aesthetics = c("color")) +
          scale_color_manual(values = c(colorfills, "gray"),  aesthetics = c("fill")) +
          
          ggplot2::geom_violin(aes(fill = Locations, 
                                   weight = pop), 
                               alpha = 0.05) +
          ggplot2::geom_jitter(size = 1, width = 0.05) +
          geom_boxplot(aes(x = Locations, y = literalvarname, 
                           weight = pop, 
                           alpha = 0.03, col = "gray")) +
          
          theme_bw() +
          theme(panel.grid = element_blank()) +
          xlab("Locations") +
          ylab(varlabel) +
          # labs(fill = "Locations", color = "Locations") +
          ggtitle(maintitle, subtitle = "Population weighted distribution (quantiles of all residents not sites)")
      } # end ggplot
      #################################################################### #
    }
  }}

