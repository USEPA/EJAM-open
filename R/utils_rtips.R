
## see draft functions to summarize like notable_ratios etc. in progress
# and in or for long report executive summary etc.



## draft temporary print pop density function tips. 
##
rtips <- function(out = testoutput_ejamit_1000pts_1miles, radius = 3, topic= "pop density", andcat = TRUE) {

  if (topic == 'pop density') {

    txt <- paste0(

      paste0("\n"),

      paste0("Population Density: \n\n"),

      ## show some actual results on pop density, not how to get them

      # paste0("  ", popshare_p_lives_at_what_pct(out$results_bysite$pop, p = 0.50, astext = TRUE), "\n"),
      # paste0("  ", popshare_at_top_n(out$results_bysite$pop, c(1, 5, 10), astext = TRUE), "\n\n"),


      ## show how to get some results, like a plot, etc.

      paste0("Try this, for example, \n out <- ejamit(testpoints_1000, radius = ", radius, ") \n # or\n out <- testoutput_ejamit_1000pts_1miles \n\n"),

      paste0("popshare_p_lives_at_what_pct(out$results_bysite$pop, p = 0.50, astext = TRUE) \n\n"),
      paste0("popshare_at_top_n(out$results_bysite$pop, c(1, 5, 10), astext = TRUE) \n\n"),

      paste0("To see a histogram of population counts nearby: \n\n",

          '     hist(out$results_bysite$pop/1000, 100,
            xlab = "Residents nearby (in thousands)",
            ylab = "Number of sites",
            main =  "Population Counts within', radius, 'miles of Various Sites")',
          "\n\n"),

      paste0("To see a histogram of log scale population counts nearby: \n\n",

          '     hist(log10(testoutput_ejamit_100pts_1miles$results_bysite$pop), 100,
             xlab = "Residents nearby as log10(pop), so 3 means 10^3 or 1,000 residents, 4 means 10k residents",
             ylab = "Number of sites",
             main =  "Population Counts within ', radius, 'miles of Various Sites")',
          "\n\n"),

      paste0("To see cumulative distribution of population nearby:\n\n",

          '     plot(ecdf(out$results_bysite$pop/1000),
            ylab="Fraction of total population living any one or more of these sites",
            xlab="# of residents (in thousands) near a site, showing one dot for each site",
            main="A fraction of these sites are where most of the residents are located")',
          "\n\n")

    )
    if (andcat) {
      cat(txt)
      invisible(txt)
    } else {
      cat("\nUse cat(x) to view the output of this function.\n")
      return(txt)
    }


  # cat("Population Density: \n")
  # cat("  ", popshare_p_lives_at_what_pct(out$results_bysite$pop, p = 0.50, astext = TRUE), "\n")
  # cat("  ", popshare_at_top_n(out$results_bysite$pop, c(1, 5, 10), astext = TRUE), "\n\n")
  #
  # cat("For example, \n out <- ejamit(testpoints_1000, radius = 1) \n # or\n out <- testoutput_ejamit_1000pts_1miles \n\n")
  #
  # cat("To see a histogram of population counts nearby: \n\n",
  #     '     hist(out$results_bysite$pop/1000, 100,
  #       xlab = "Residents nearby (in thousands)",
  #       ylab = "Number of sites",
  #       main =  "Population Counts within', radius, 'miles of Various Sites")',
  #     "\n\n")
  #
  # cat("To see a histogram of log scale population counts nearby: \n\n",
  #     '     hist(log10(testoutput_ejamit_100pts_1miles$results_bysite$pop), 100,
  #        xlab = "Residents nearby as log10(pop), so 3 means 10^3 or 1,000 residents, 4 means 10k residents",
  #        ylab = "Number of sites",
  #        main =  "Population Counts within ', radius, 'miles of Various Sites")',
  #     "\n\n")
  #
  # cat("To see cumulative distribution of population nearby:\n\n",
  #     '     plot(ecdf(out$results_bysite$pop/1000),
  #       ylab="Fraction of total population living any one or more of these sites",
  #       xlab="# of residents (in thousands) near a site, showing one dot for each site",
  #       main="A fraction of these sites are where most of the residents are located")',
  #     "\n\n")

  }

}
