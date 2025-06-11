
## THIS IS SOME NOTES ON WHAT WOULD ESSENTIALLY BE A VERY VERY BASIC REPORT-BUILDER THAT 
## LETS A USER SPECIFY WHICH SETS OF INDICATORS TO SHOW IN THE REPORT
## AND WHAT TO NAME THAT SUBSECTION


# notes on how to select groups of indicators but with a nice name for each group and name(s) defining members of each group, etc.
# example below shows you can have choices show up in groups,

# *** but still need a way to let the user rename each group ***

# AND maybe still need a way to let the user specify the members of each group or pick some not all etc.

# and then server can evaluate the input as R code for a named list 
# as is needed for the extratable_list_of_sections param of build_community_report()
# but it would be a reactive value not an input if done like this
#################################################################### #



# ## FIRST EXAMPLE:

if (FALSE) {

shinyApp(
  ui = fluidPage(
    
    textInput('t1', 'Header for Group 1', value = 'Age'),
    selectizeInput("v1", "Indicators Group 1", 
                   choices = list(
                     `Age-related` = c("pctunder5", "pctunder18", "pctover64"),
                     `Other indicators` = "names_d_extra",
                     `Sub Groups` = c("names_d_subgroups")
                   ),
                   selected = "", 
                   multiple = TRUE
    ),
    hr(),
    textInput('t2', 'Header for Group 2', value = 'Ages'),
    selectizeInput("v2", "Indicators Group 2", 
                   choices = list(
                     `Age-related` = c("pctunder5", "pctunder18", "pctover64"),
                     `Other indicators` = "names_d_extra",
                     `Sub Groups` = c("names_d_subgroups")
                   ),
                   selected = "", 
                   multiple = TRUE
    ),
    hr(),
    textInput('t3', 'Header for Group 3', value = 'Sub Groups'),
    selectizeInput("v3", "Indicators Group 3", 
                   choices = list(
                     `Age-related` = c("pctunder5", "pctunder18", "pctover64"),
                     `Other indicators` = "names_d_extra",
                     `Sub Groups` = c("names_d_subgroups")
                   ),
                   selected = "", 
                   multiple = TRUE
    ),
    
    hr(),
    
    verbatimTextOutput("result")
  ),
  server = function(input, output) {
    
    output$result <- renderPrint({
      
      t1 = input$t1
      v1 = input$v1
      if (length(v1) == 0) {t1 = NULL; v1 = NULL}
      
      t2 = input$t2
      v2 = input$v2
      if (length(v2) == 0) {t2 = NULL; v2 = NULL}
      
      t3 = input$t3
      v3 = input$v3
      if (length(v3) == 0) {t3 = NULL; v3 = NULL}
      mylist = list(v1, v2, v3)
      names(mylist) <- c(t1, t2, t3)
      
      cat("value of list is \n")
      print(mylist)
      
      
      # cat(paste0("mylist <- list(\n  ",
      #            paste0(c(t1,t2,t3), " = ",
      #                   
      #                   paste0("c('", 
      #                          paste0(
      #                            c(
      #                              paste0(paste0(v1, collapse = "', '"), "')",
      #                                     paste0(paste0(v2, collapse = "', '"), "')",
      #                                            paste0(v3, collapse = "', '"), "')"
      #                                            
      #                                            "\n) \n "
      #                                     ), collapse = ",\n  "),
      #                            )
      #                          )
      #                   )
      #            )
      #            
      #            # print(dput(mylist))
      #            
    })
    
    
  }
)
  }
#################################################################### #

if (TRUE) {
  
#   EXAMPLE - better UI?
  
#   # see global_defaults_*.R :

  
  ################################################################# # 
  
  shinyApp(
    ui = fluidPage(
      
      selectizeInput("extratable_list_of_sections_code",
                     "Choose Groups of Indicators:",
                     choices = EJAM:::global_or_param("default_extratable_list_of_sections_ui"),
                     multiple = TRUE
      ),
      
      
      h5("explanation of what the list should look like now:"),
      verbatimTextOutput("result"),
      br(),
      h5("PRINT OF THE VALUE OF THE LIST CREATED:"),
      verbatimTextOutput("printvalue")
    ),
    
    
    server = function(input, output) {
      
      output$printvalue <- renderPrint({
        
        req(input$extratable_list_of_sections_code)
        
        result_as_code <- paste("list(\n", 
                                paste0(input$extratable_list_of_sections_code, collapse = ",\n "), "\n)")
        
        result_as_value <- EJAM:::source_this_codetext(result_as_code)
        
        for (i in 1:length(result_as_value)) {  attributes(result_as_value[[i]]) <- NULL}
        
        print(result_as_value)
        
      })
      
      output$result <- renderPrint({
        req(input$extratable_list_of_sections_code)
        cat(
          paste("We can turn it into a named list: \n\n selectionslist = list(\n", 
                paste0(input$extratable_list_of_sections_code, collapse = ",\n "), "\n)" ))
      })
      
      
    }
  )
}
################################################################# # 




stop("")


x = setdiff(names_all_r, names(testoutput_ejamit_10pts_1miles$results_overall))
x = data.frame(name = x, list = varinfo(x)$varlist)
x <- x[order(x$list, x$name) , ]
# x

sort(unique(x$list))
# [1] "names_climate"                              "names_climate_avg"                         
# [3] "names_climate_pctile"                       "names_climate_state_avg"                   
# [5] "names_climate_state_pctile"                 "names_countabove"                          
# [7] "names_criticalservice"                      "names_criticalservice_avg"                 
# [9] "names_criticalservice_pctile"               "names_criticalservice_state_avg"           
# [11] "names_criticalservice_state_pctile"         "names_d_language"                          
# [13] "names_d_other_count"                        "names_d_subgroups_alone_avg"               
# [15] "names_d_subgroups_alone_count"              "names_d_subgroups_alone_pctile"            
# [17] "names_d_subgroups_alone_ratio_to_avg"       "names_d_subgroups_alone_ratio_to_state_avg"
# [19] "names_d_subgroups_alone_state_avg"          "names_d_subgroups_alone_state_pctile"      
# [21] "names_featuresinarea"                       "names_flag"                                
# [23] "names_geo"                                  "names_health"                              
# [25] "names_health_avg"                           "names_health_pctile"                       
# [27] "names_health_state_avg"                     "names_health_state_pctile"                 
# [29] "names_misc"                                 "names_sitesinarea"      

x <- x[!grepl("_avg|_pctile", x$list), ]
x
sort(unique(x$list))
# [1] "names_climate"                 "names_countabove"              "names_criticalservice"        
# [4] "names_d_language"              "names_d_other_count"           "names_d_subgroups_alone_count"
# [7] "names_featuresinarea"          "names_flag"                    "names_geo"                    
# [10] "names_health"                  "names_misc"                    "names_sitesinarea"     

dput(as.vector(na.omit( unique(x$list))))

c(
  "names_featuresinarea",
  "names_sitesinarea", 
  "names_flag",
  
  "names_health", 
  "names_climate", 
  "names_criticalservice", 
  
  "names_d_language", # pctlan_rus_pol_slav etc.
  "names_d_other_count", #  "pop"   "nonmins"   "age25up"   "hhlds""unemployedbase" "pre1960"   "builtunits" "povknownratio" 
  "names_countabove", #  "count.ej.80up" "count.ej.80up.supp" "count.ej.80up2.eo" "count.ej.80up2.supp" "state.count.ej.80up"      "state.count.ej.80up.supp"
  
  "names_d_subgroups_alone_count", 
  "names_geo",  # placename, areatytpe, statlevel, area, etc.
  "names_misc" # "lifexyears_synonym"  "state.pctile.lowlifex_synonym"
)

########################################### #

extra_varlist_names <- c(
  
  'names_sitesinarea',
  'names_featuresinarea',
  'names_flag',
  
  'names_health',
  'names_climate',
  'names_criticalservice',
  
  'names_d_language',
  'names_d_other_count',
  'names_countabove'
)

########################################### #
x = setdiff(names(blockgroupstats), names(testoutput_ejamit_10pts_1miles$results_overall))
x = data.frame(name = x, list = varinfo(x)$varlist)
x[order(x$list, x$name) , ]

#                    name                          list
# 25            pctfire30                 names_climate
# 24           pctflood30                 names_climate
# 8         count.ej.80up              names_countabove
# 9    count.ej.80up.supp              names_countabove
# 19       pctnobroadband         names_criticalservice
# 20 pctnohealthinsurance         names_criticalservice
# 40     yesno_fooddesert         names_criticalservice
# 38    yesno_houseburden         names_criticalservice
# 39       yesno_transdis         names_criticalservice
# 47        pctlan_arabic              names_d_language
# 41       pctlan_english              names_d_language
# 42        pctlan_french              names_d_language
# 46   pctlan_other_asian              names_d_language
# 44      pctlan_other_ie              names_d_language
# 43  pctlan_rus_pol_slav              names_d_language
# 45    pctlan_vietnamese              names_d_language
# 14                   aa names_d_subgroups_alone_count
# 15                aiana names_d_subgroups_alone_count
# 13                   ba names_d_subgroups_alone_count
# 18                multi names_d_subgroups_alone_count
# 16                nhpia names_d_subgroups_alone_count
# 17           otheralone names_d_subgroups_alone_count
# 12                   wa names_d_subgroups_alone_count
# 32           num_church          names_featuresinarea
# 31         num_hospital          names_featuresinarea
# 30           num_school          names_featuresinarea
# 36      yesno_airnonatt                    names_flag
# 34       yesno_cejstdis                    names_flag
# 37      yesno_impwaters                    names_flag
# 35         yesno_iradis                    names_flag
# 33         yesno_tribal                    names_flag
# 11                 area                     names_geo
# 4              arealand                     names_geo
# 5             areawater                     names_geo
# 3            countyname                     names_geo
# 10         Shape_Length                     names_geo
# 22           rateasthma                  names_health
# 23           ratecancer                  names_health
# 21     rateheartdisease                  names_health
# 6             count.NPL             names_sitesinarea
# 7            count.TSDF             names_sitesinarea
# 27          num_airpoll             names_sitesinarea
# 28       num_brownfield             names_sitesinarea
# 29              num_tri             names_sitesinarea
# 26         num_waterdis             names_sitesinarea
# 1                bgfips                          <NA>
# 2                  bgid                          <NA

