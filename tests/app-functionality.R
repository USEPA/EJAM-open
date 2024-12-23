main_shinytest <- function(data_type) {
  # SPECIFY SITES ####
  
  
  test_that("{shinytest2} recording: EJAM", {
    app <- AppDriver$new(
      variant = platform_variant(), 
      name = data_type, 
      seed=12345, 
      load_timeout=2e+05,
      options = list(
        shiny.reactlog = TRUE, 
        shiny.trace = TRUE, 
        shiny.testmode = TRUE
      )
    )
    
    ## UPLOAD PLACES ####
    
    app$set_inputs(ss_choose_method = "upload", wait_ = FALSE)
    if(data_type == "latlon") {
      print("About to upload latlon testpoints_100.xlsx")
      app$upload_file(ss_upload_latlon = EJAM:::app_sys("testdata/latlon/testpoints_100.xlsx"))  
    } else if(data_type == "FIPS") {
      print("About to upload FIPS")
      app$set_inputs(ss_choose_method_upload = "FIPS") 
      app$upload_file(ss_upload_fips = EJAM:::app_sys("testdata/fips/counties_in_Delaware.xlsx"))  
    } else if(data_type == "shapefile") {
      app$set_inputs(ss_choose_method_upload = "SHP") 
      app$upload_file(ss_upload_shp = EJAM:::app_sys("testdata/shapes/portland_shp.zip"))  
    } else {
      
      ## PULLDOWN NAICS/ETC. ####
      
      print("selectings NAICS")
      app$set_inputs(ss_choose_method = "dropdown", wait_ = FALSE)
      app$set_inputs(ss_select_naics = "1111", wait_ = FALSE)#, timeout_ = 10000)
    }
    
    # START ANALYSIS ####
    
    print("About to get results")
    app$wait_for_idle(timeout = 20000)
    app$click("bt_get_results", wait_ = TRUE, timeout_ = 20000)
    print("done pulling results")
    app$expect_values(name="first-analysis")
    
    ## Re-run the analysis with a modified radius change ####
    if(!(data_type %in% c("FIPS","NAICS"))) {
      print("going back to Site Selection tab")
      app$set_inputs(all_tabs = "Site Selection")
      app$set_inputs(bt_rad_buff = 1.5)
      ## title ####
      print("set analysis title to Summary of EJ Analysis2")
      app$set_inputs(analysis_title = "Summary of EJ Analysis2")
      ## repull ####
      print("repulling results")
      app$click("bt_get_results", wait_ = TRUE, timeout_ = 20000)
      app$expect_values(name="second-analysis-radius-1.5")
    }
    
    # RESULTS - SUMMARY REPORT DOWNLOAD ####
    
    print("about to do community download")
    app$wait_for_idle(timeout = 20000)
    app$expect_download("community_download_all")
    
    # RESULTS - DETAILS tab ####
    print("going to details tab")
    
    ## DETAILS > SITE by SITE xlsx DOWNLOAD #### 
    
    app$set_inputs(results_tabs = "Details")
    app$wait_for_idle(timeout = 20000)
    app$expect_values(name = "site-by-site")
    
    print("downloading results table from details tab")
    app$expect_download("download_results_table")
    
    ## DETAILS > PLOT AVG SCORES ####
    
    print("going to plot_average details subtab")
    app$set_inputs(details_subtabs = "Plot Average Scores")
    app$expect_values(name="plot_average")
    app$wait_for_idle(timeout = 20000)
    
    print("Demographic summ_bar-ind")
    app$set_inputs(summ_bar_ind_radio = "Demographic")
    app$expect_values(name="demographic")
    app$wait_for_idle(timeout = 20000)
    
    print("Environmental summ_bar_ind")
    app$set_inputs(summ_bar_ind_radio = "Environmental")
    app$expect_values(name="environmental")
    app$wait_for_idle(timeout = 20000)
    
    if(app$get_value(input="include_ejindexes") == "TRUE") {
      print("EJ summ_bar-ind")
      app$set_inputs(summ_bar_ind_radio = "EJ")
      app$expect_values(name="EJ")
      app$wait_for_idle(timeout = 20000)
      
      print("EJ supplemental")
      app$set_inputs(summ_bar_ind_radio = "EJ Supplemental")
      app$expect_values(name="EJ-Supplemental")
      app$wait_for_idle(timeout = 20000)
    }
    
    ## DETAILS > PLOT RANGES (HISTO) ####
    
    if (!isTRUE(golem_opts$isPublic)) {
      
      print("going to plot_range details subtab")
      app$set_inputs(details_subtabs = "Plot Full Range of Scores")
      app$expect_values(name="plot_range")
      app$wait_for_idle(timeout = 20000)
      
      print("messing with summ hist options")
      app$set_inputs(summ_hist_distn = "Sites")
      app$expect_values(name="Sites")
      app$set_inputs(summ_hist_data = "raw")
      app$expect_values(name="hist-data-raw")
      app$set_inputs(summ_hist_bins = 15)
      app$set_inputs(summ_hist_bins = 20)
      app$expect_values(name="hist_bins_20")
      app$set_inputs(summ_hist_distn = "People")
      app$expect_values(name="hist-distn-people")
      app$set_inputs(summ_hist_data = "pctile")
      app$expect_values(name="hist-data-pctile")
      app$set_inputs(summ_hist_data = "raw")
      app$expect_values(name="hist-data-raw2")
      app$set_inputs(summ_hist_ind = "Demog.Index.Supp")
      app$expect_values(name="Demog.Index.Supp")
      app$set_inputs(summ_hist_ind = "pctlowinc")
      app$expect_values(name="pctlowinc")
      app$set_inputs(results_tabs = "Community Report")
      app$expect_values(name="Community Report")
      
      print("finished test")
    }
    
  })
}
