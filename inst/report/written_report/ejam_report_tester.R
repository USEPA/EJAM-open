if (!exists("blockgroupstats")) {library(EJAM)} # to use installed version only if not already attached
# library(EJAM)
library(dplyr)
library(data.table)
library(tidyr)

# packages for getting image from map
# install.packages("mapview")
library(mapview)
# webshot::install_phantomjs()
Sys.setenv(OPENSSL_CONF = "/dev/null")

options(scipen = 999)
# install.packages('xfun')

colcounter = EJAM:::colcounter
x <- ejamit(testpoints_100, radius = 1)
results1 <- x$results_bysite  
results2 <- x$results_overall 
total_pop <- sum(x$results_bysite$pop)

demog_table <- table_gt_from_ejamit(x, "demog")
envt_table <- table_gt_from_ejamit(x, "envt")

demog_barplot <- ejam2barplot(x)
envt_barplot <- ejam2barplot(x,  varnames = c(names_e_ratio_to_avg, names_e_ratio_to_state_avg),
                             main = "Environmental Indicators at Selected Sites Compared to Averages")


demog_barplot <-
demog_barplot +
# ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, 10)) +
ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.2, size=10),
      plot.subtitle = ggplot2::element_text(hjust=0.5, size=10),
      axis.title.y = ggplot2::element_text(size=10),
      axis.text = ggplot2::element_text(size=8)
      )

envt_barplot <-
envt_barplot +
  # ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, 10)) +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.2, size=10),
                 plot.subtitle = ggplot2::element_text(hjust=0.5, size=10),
                 axis.title.y = ggplot2::element_text(size=10),
                 axis.text = ggplot2::element_text(size=8)
  )

library(svglite)
# png("inst/report/written_report/demog_ridgeline_plot.png", width = 1000)
svglite("inst/report/written_report/demog_ridgeline_plot.svg", width = 15)
plot_ridgeline_ratios_ez(x, varnames = c(names_d_ratio_to_avg, names_d_subgroups_ratio_to_avg))
dev.off()

# png("inst/report/written_report/envt_ridgeline_plot.svg", width = 1000)
svglite("inst/report/written_report/envt_ridgeline_plot.svg", width = 15)
plot_ridgeline_ratios_ez(x, varnames = c(names_e_ratio_to_avg))
dev.off()

# png("inst/report/written_report/demog_resident_barplot.png")
svglite("inst/report/written_report/demog_resident_barplot.svg", height = 10, width = 15)
plot_distance_mean_by_group(x$results_bybg_people)
dev.off()

# create map and save map as an image
map <- ejam2map(x, radius=1)
mapshot(map, file="inst/report/written_report/map.png")

params_list <- list(
  testmode = FALSE,
  results_overall = results2,
  results_bysite = results1,
  sitecount = 100,
  analysis_title = 'Summary of Analysis',
  zonetype = 'General',
  where = 'within 2 miles of',
  distance = '2 miles',
  sectorname_short = 'Example facilities of example category',
  in_the_x_zone = 'in the analyzed locations',
  facilities_studied = 'analyzed sites',
  within_x_miles_of = 'within 2 miles of',
  in_areas_where = 'in areas with high demographic variability',
  risks_are_x = 'risk is above 1 per million',
  source_of_latlons = "EPA's Facility Registry Service (FRS)",
  demog_table = demog_table,
  envt_table = envt_table,
  demog_barplot = demog_barplot,
  envt_barplot = envt_barplot,
  demog_ridgeline = "demog_ridgeline_plot.svg",
  envt_ridgeline = "envt_ridgeline_plot.svg",
  demog_resident_barplot = "demog_resident_barplot.svg",
  map = "map.png",
  total_pop = total_pop)

rmarkdown::render("inst/report/written_report/EJAM_long_report_2-10-25.Rmd",
                  params = params_list,
                  output_format = "bookdown::word_document2", 
                  output_file = 'custom_report.docx')

# note: keep Rmd file in "Source" mode to keep figure/table numbering
# if you need to be in "Visual" mode, afterwards returen to "Source" and
# and add backslashes before the # symbol. i.e. <caption>(\#tab:demog-table) Demographic Indicators Table</caption>

