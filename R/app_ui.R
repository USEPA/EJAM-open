
#' app_ui - The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @importFrom shinyjs useShinyjs extendShinyjs
#'
#'
#'
app_ui  <- function(request) {
  
  
  
  tagList(
    # golem_add_external_resources() ####
    # Leave this function for adding external resources, specifying title of app, see end of this source file.
    golem_add_external_resources(),
    # . ### #
    # _____App UI fluidPage starts here _______ ####
    fluidPage(
      
      ### enable JavaScript, CSS   ####
      #   functionality (such as resetting inputs) etc.
      shinyjs::useShinyjs(),
      ## javascript function for jumping to top of screen
      shinyjs::extendShinyjs(text = "shinyjs.toTop = function() {window.scrollTo(0, 0);}", functions = "toTop"),
      # For info on using javascript in shiny apps,
      #   see  https://deanattali.com/shinyjs/ and https://connect.thinkr.fr/js4shinyfieldnotes/js-in-shiny.html
      ## change selected tab color - #005ea2 matches blue on rest of website
      
      # Since R will during installation move all source/EJAM/inst/xyz to installed/EJAM/xyz,
      #  we use golem app_sys() to ensure it points to the right folder,
      #  which in source pkg is EJAM/inst/app/www/ejam_styling.css
      # but in installed pkg is EJAM/app/www/ejam_styling.css
      includeCSS(EJAM:::app_sys('app/www/ejam_styling.css')), #
      
      # use friendlier message if user gets disconnected from server
      shinydisconnect::disconnectMessage(
        text = "Sorry ... This app has stopped because of an error or a timeout. The app is still being tested and debugged. Also, if the app is left open with no interaction, then once a time limit is reached the server will disconnect to save resources.",
        refresh = "Click to Restart the App",
        background = "#FFFFFF", colour = "#444444", refreshColour = "darkgreen", overlayColour = "#000000", overlayOpacity = 0.5,
        width = 450, top = 50, size = 20, css = ""
      ), 
      
      # actionButton("disconnect", "Disconnect the app"),
      
      ### html header inserted from global.R ####
      html_header_fmt,
      
      ### title is now in html in global.R (for app and browser tab) ####

      # TABSETPANEL ALL -  tabsetPanel(id = 'all_tabs',  ####
      tabsetPanel( # up to line 1101 or so
        id = 'all_tabs',
        
        selected = 'Site Selection',
        # ______ About ______ tabPanel(title = 'About' ####
        ## see  default_hide_about_tab
        tabPanel(title = 'About',
                 br(),
                 fluidRow(
                   column(8,
                          
                          ## html intro text from global.R
                          intro_text,
                          actionButton(inputId = 'back_to_site_sel2', label = div(icon('play', style = 'transform: rotate(180deg);'), HTML('&nbsp;'), 'Return to Site Selection'), class = 'usa-button'),
                          br(), br(),
                          actionButton('ui_show_advanced_settings','Show Advanced Settings Tab', class = 'usa-button'),
                          actionButton('ui_hide_advanced_settings','Hide Advanced Settings Tab', class = 'usa-button'),
                          br(),br(),
                   ),
                   column(4,
                          htmltools::img(id = "biglogo", src = 'www/ejamhex4.png')
                   )
                 )
        ), # end About EJAM tab
        
        ######################################################################################################### #
        # . ## ##
        #
        ######################################################################################################### #
        #. ####
        # ______ SELECT SITES ________ tabPanel(title = 'Site Selection'####
        #. ####
        
        tabPanel(
          title = 'Site Selection',
          #h3('Welcome to EJAM'),
          # div(
          #   p("EJScreen's multisite tool (EJAM) lets you explore the demographics and environmental conditions in any list of places, such as for everyone who lives within 1 mile of a certain type of EPA-regulated site. EJAM stands for the Environmental Justice Analysis Multisite tool that is part of EJScreen."),
          #   class = "about-EJAM-span"
          # ),
          # hr(), ## horizontal line # removed to look a bit more like ejscreen mapper page and have more space
          
          ## fluidRow container for upload method (left column) and map (right column) ####
          fluidRow( # through about line 441
            
            ## upload-methods column ####
            column(
              4,  # through about line 359
              
              ## input: use CATEGORIES of sites, or upload LOCATIONS ? ####
              div(style = 'border: 1px solid #005ea2; padding: 10px;',
                  radioButtons(inputId = 'ss_choose_method',
                               label = 'How would you like to identify locations?',
                               choiceNames = c('Select a category of locations',
                                               'Upload specific locations'),
                               choiceValues = c('dropdown',
                                                'upload'),
                               selected = 'upload'),
                  ### input: what CATEGORY type? (NAICS, SIC, MACT, Program) ####
                  # end conditional choose category type
                  conditionalPanel(
                    condition = 'input.ss_choose_method == "dropdown"',
                    selectInput(inputId = 'ss_choose_method_drop',
                                label = tags$span(
                                  'How would you like to select categories?'
                                ),
                                choices = choices_for_type_of_site_category
                    )
                  ),
                  
                  ### input: what LOCATIONS type to upload? (IDs, latlon, FIPS, Shapes) ####
                  # end conditional picking what type of IDs to upload
                  conditionalPanel(
                    condition = 'input.ss_choose_method == "upload"',
                    selectInput(inputId = 'ss_choose_method_upload',
                                #label = 'What type of data are you uploading?',
                                label = tags$span(
                                  'What type of data are you uploading?'
                                ),
                                choices = choices_for_type_of_site_upload
                    )
                  ),
              ),
              
              br(),
              ################################################################# #
              
              ## *UPLOADING  SITES*  input: choose among facility dropdown options, conditional panel ####
              
              # __wellPanel start ___----------------------------------------------------------------------
              
              wellPanel(
                style = 'background-color: #e5f2f5; min-height: 500px',
                
                fluidRow(
                  column(
                    12,
                    #offset=3,
                    ################################################################# #
                    br(),
                    
                    wellPanel(
                      style = 'background-color: #e5f2f5; min-height: 500px',
                      
                      fluidRow(
                        column(
                          12,
                          ################################################################# #
                          # uploads:                          
                          ## *Latitude Longitude* LOCATIONS Uploads  (conditional panel)  ------------------------------------- - ####
                          
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'upload' && input.ss_choose_method_upload == 'latlon'",
                            
                            ## input: Upload list of facility lat/longs
                            
                            fileInput(inputId = 'ss_upload_latlon',
                                      label = 'Upload a file with lat-long coordinates',
                                      multiple = FALSE,
                                      accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values,text/plain")
                                      # add hover tips here maybe, or even a button to view examples of valid formats and details on that.
                            ),
                            tags$span(
                              tags$ul(
                                tags$li('Required filetype: .csv, .xls, or .xlsx'),
                                tags$li('Required Columns: lat, lon'),
                                tags$li(tags$a(href = 'https://github.com/ejanalysis/ejscreendata/blob/master/testdata/latlon/testpoints_10.xlsx?raw=true', target = '_blank', 
                                               'Example of lat lon file')) #,
                                # tags$li(tags$a(href = 'https://www.latlong.net/convert-address-to-lat-long.html', target = '_blank', 
                                #                'Get lat/long from a street address'))
                              )
                            ),
                            actionButton('latlon_help', label = 'More Info', class = 'usa-button usa-button--outline'), # HTML(latlon_help_msg)
                            br()
                          ), # end latlong conditionalPanel
                          ################################################################# #
                          
                          ## *Shapefile* LOCATIONS Uploads (conditional panel)  ------------------------------------- - ####
                          
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'upload' && input.ss_choose_method_upload == 'SHP'",
                            ## input: Upload list of FRS identifiers
                            fileInput(inputId = 'ss_upload_shp',
                                      label = 'Upload a shapefile',
                                      accept = c(".shp",".dbf",".sbn",".sbx",".shx",".prj",
                                                 # ".json", ".geojson",  # ***  add json,kml etc when available
                                                 # ".gdb",               # ***  add json,kml etc when available
                                                 # ".kml",               # ***  add json,kml etc when available
                                                 ".zip"
                                      ),
                                      multiple = TRUE
                            ),
                            tags$ul(
                              #                                            ***  add json,kml etc when available
                              tags$li('Required files: .zip OR .shp, .shx, .dbf, .prj'),
                              tags$li('Required fields: geometry'),
                              tags$li(tags$a(href = 'https://github.com/ejanalysis/ejscreendata/blob/master/testdata/shapes/portland.gdb.zip?raw=true', target = '_blank', 
                                             'Example of Shapefile'))
                            ),
                            actionButton('shp_help', label = 'More Info', class = 'usa-button usa-button--outline')
                          ),  # end Shapefile conditionalPanel
                          ################################################################# #
                          
                          
                          ## *CATEGORIES OF SITES*  input: choose among facility dropdown options, conditional panel ####
                          
                          # uploads:  
                          
                          ## _FIPS - upload conditional panel ------------------------------------- - ####
                          
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'upload' && input.ss_choose_method_upload == 'FIPS'",
                            ## input: Upload list of facility lat/longs
                            fileInput(inputId = 'ss_upload_fips',
                                      label = 'Upload a list of FIPS codes',
                                      multiple = FALSE,
                                      accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values,text/plain")
                            ),
                            tags$ul(
                              tags$li('Required filetype: .csv, .xls, or .xlsx'),
                              tags$li('Required columns: FIPS (or alias)'),
                              tags$li(tags$a(href = 'https://github.com/ejanalysis/ejscreendata/blob/master/testdata/fips/counties_in_Delaware.xlsx?raw=true', target = '_blank', 
                                             'Example of FIPS codes file'))
                            ),
                            actionButton('fips_help', label = 'More Info', class = 'usa-button usa-button--outline')
                          ), # end FIPS conditionalPanel
                          ################################################################# #
                          
                          ## _FRS regid - upload  conditional panel ------------------------------------- - ####
                          
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'upload' && input.ss_choose_method_upload == 'FRS'",
                            ## input: Upload list of FRS identifiers
                            fileInput(inputId = 'ss_upload_frs',
                                      label = 'Upload a file with FRS identifiers',
                                      accept = c('.xls', '.xlsx', ".csv", "text/csv", "text/comma-separated-values, text/plain")
                            ), # xxx
                            tags$span(
                              tags$ul(
                                tags$li('Required filetype: .csv, .xls, or .xlsx'),
                                tags$li('Required Columns: REGISTRY_ID'),
                                tags$li(tags$a(href = 'https://github.com/ejanalysis/ejscreendata/blob/master/testdata/registryid/frs_test_regid_8.xlsx?raw=true', target = '_blank', 
                                               'Example of Registry IDs file'))
                              )
                            ),
                            actionButton('frs_help', label = 'More Info', class = 'usa-button usa-button--outline')
                          ), # end FRS conditionalPanel
                          ################################################################# #
                          
                          ## _EPA program ID - upload conditional panel ------------------------------------- - ####
                          
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'upload' && input.ss_choose_method_upload == 'EPA_PROGRAM'",
                            ## input: upload an EPA program ID file
                            fileInput(inputId = 'ss_upload_program',
                                      label = 'Upload a file with program IDs'),
                            tags$ul(
                              tags$li('Required filetype: .csv, .xls, or .xlsx'),
                              tags$li('Required columns: program, pgm_sys_id'),
                              tags$li(tags$a(href = 'https://github.com/ejanalysis/ejscreendata/blob/master/testdata/programid/program_test_data_10.xlsx?raw=true', target = '_blank', 
                                             'Example of EPA program ID file'))
                            ),
                            actionButton('epa_program_help', label = 'More Info', class = 'usa-button usa-button--outline')
                          ), #end EPA program upload conditional panel
                          ################################################################# #
                          # dropdowns:                    
                          ## _EPA program - dropdown  conditional panel ------------------------------------- - ####
                          
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'dropdown' && input.ss_choose_method_drop == 'EPA_PROGRAM'",
                            ## input: select an EPA program from list ------------------------------------- - ------------------------------------- -
                            selectizeInput(inputId = 'ss_select_program', label = 'Pick an EPA program',
                                           ## named vector in global.R - values are acronyms,
                                           ## names include # of rows corresponding to that program
                                           choices = epa_programs,
                                           selected = default_epa_program_selected, # not sure this is a good idea but trying it out
                                           ## add X to remove selected options from list
                                           options = list('plugins' = list('remove_button'))),
                            br(), 
                            span('More info about ', a('these EPA programs', href = 'https://www.epa.gov/frs/frs-data-sources', target = '_blank', rel = 'noreferrer noopener')),
                            br(), 
                            span('Search for regulated facilities in ', a('EPA Envirofacts', href = 'https://enviro.epa.gov/envirofacts/multisystem/search', target = '_blank', rel = 'noreferrer noopener')),
                            br(),
                            span('Search for regulated facilities in ', a('EPA ECHO data', href = 'https://echo.epa.gov/', target = '_blank', rel = 'noreferrer noopener')),
                            br()
                          ), # end conditional panel EPA programs
                          ################################################################# #
                          
                          ## _NAICS - dropdown conditional panel -------------------------------------  -####
                          
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'dropdown' && input.ss_choose_method_drop == 'NAICS'",
                            ## input: Upload list of NAICS identifiers
                            selectizeInput(
                              inputId = "ss_select_naics",
                              label = htmltools::h6("Select industry of interest"),
                              # choose from named numeric vector on server-side
                              ## number is NAICS like 31182, names are like "31182 - Cookie, Cracker, and Pasta Manufacturing"
                              choices = NULL,
                              selected = NULL,
                              width = 400,
                              multiple = TRUE,
                              ## add X to remove selected options from list
                              options = list('plugins' = list('remove_button'))
                            ),#, # xxx
                            br(), ## vertical space
                          ), # end NAICS conditionalPanel overall
                          
                          ## _NAICS details conditionalPanel
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'dropdown' && input.ss_choose_method_drop == 'NAICS'",
                            div(style = 'border: 1px solid #005ea2; padding: 10px; background-color: white',
                                
                                radioButtons('naics_digits_shown', "See all subcategories of NAICS?",
                                             inline = TRUE,
                                             choiceNames = c("Basic list", "Detailed list"),
                                             choiceValues = c('basic', 'detailed'),
                                             selected = 'basic'),
                                radioButtons('add_naics_subcategories', "Add all subcategories of NAICS?",
                                             inline = TRUE,
                                             choiceNames = c("Yes","No"),
                                             choiceValues = c(TRUE,FALSE),
                                             selected = TRUE)
                            ),
                            
                            br(),
                            tags$ul(
                              # tags$li('Required columns: program, pgm_sys_id'),
                              tags$li(tags$a(href = 'https://www.naics.com/search', target = '_blank', 
                                             'More about NAICS codes from naics.com'))
                            )
                          ), # end conditional panel
                          ################################################################# #
                          
                          ## _SIC - dropdown conditional panel ------------------------------------- - ####
                          
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'dropdown' && input.ss_choose_method_drop == 'SIC'",
                            
                            ## input: Select SIC from list
                            selectizeInput(
                              inputId = "ss_select_sic",
                              label = htmltools::h6("Select industry of interest"),
                              # choose from named numeric vector on server-side
                              ## number is NAICS like 31182, names are like "31182 - Cookie, Cracker, and Pasta Manufacturing"
                              choices = NULL,
                              selected = NULL,
                              width = 400,
                              multiple = TRUE,
                              ## add X to remove selected options from list
                              options = list('plugins' = list('remove_button'))
                            ),
                            br(), ## vertical space
                          ), # end SIC conditionalPanel
                          ################################################################# #
                          
                          ## _MACT - dropdown conditionalPanel ------------------------------------- - ####
                          
                          conditionalPanel(
                            condition = "input.ss_choose_method == 'dropdown' && input.ss_choose_method_drop == 'MACT'",
                            ## input: choose MACT subpart from dropdown list
                            selectInput(inputId = 'ss_select_mact',
                                        label = 'Choose a MACT subpart',
                                        choices = setNames(mact_table$subpart,
                                                           mact_table$dropdown_label),
                                        selected = 'AA'
                            )
                          )  # end MACT conditionalPanel
                          ################################################################# #
                          
                        ) # end column
                      ) # end fluidRow
                    ),
                    # __ wellPanel end ___------------------------------------------------------
                    # . ##  ##
                    br(), #br(),
                    
                    ## input: Button to return to previous results  ####
                    shinyjs::hidden(
                      actionButton(inputId = 'return_to_results',
                                   label = div(icon('play', style = 'transform: rotate(180deg);'), HTML('&nbsp;'),
                                               'Return to Previous Results'), class = 'usa-button')
                    )
                  )))
            ), # end of upload-methods column
            ################################################################# #
            #. ####
            # ______ VIEW SITES ________ tabPanel(title = 'Site Selection'####
            #. ####
            column(8,
                   
                   ## TABLE of uploaded points in Modal window via Button  ####
                   div(
                     style = "display: flex; flex-direction: column; margin-bottom: .5em;",
                     div(
                       style = "flex: 1; display: flex; flex-wrap: wrap; gap: 1em; align-items: center;",
                       div(
                         actionButton('show_data_preview', label = 'Review selected sites',
                                      class = 'usa-button usa-button--outline')
                       ),
                       div(
                         htmlOutput(outputId = 'an_map_text'),
                       )
                     ),
                   ), # end view data uploads
                   uiOutput('invalid_sites_alert2'),
                   
                   ## MAP of uploaded points ####
                   
                   
                   #helpText('Red circles indicate overlapping sites.'),
                   ## output: show leaflet map of uploaded points
                   shinycssloaders::withSpinner(
                     leaflet::leafletOutput(outputId = 'an_leaf_map',
                                            height = '500px',
                                            width = '100%'
                     )
                   ),
                   
                   # . ####
                   # ______ RUN ANALYSIS ________####
                   # (when button is pressed)
                   ## input: RADIUS SLIDER ####
                   fluidRow(
                     column(6,
                            ## separated label from slider to allow for line break - shiny does not support it
                            ## in update*Input: https://github.com/rstudio/shiny/issues/3678
                            htmlOutput('radius_label'),
                            
                            ## input: RADIUS SLIDER for circular buffer
                            
                            shiny::uiOutput("radius_slider_ui")
                     ),
                     column(6,
                            ## input: Name of analysis (goes in report header) ####
                            shiny::uiOutput("analysis_title_ui"),
                            
                            ## input: START Button     ####
                            shiny::actionButton(
                              inputId = 'bt_get_results',
                              label = div('Start Analysis', HTML('&nbsp; <i class="fas fa-play" aria-hidden="true"></i>')),
                              class = 'usa-button',
                              role = 'button'  
                            )
                     ),
                   ) # end fluidRow with radius slide and analysis title and start button
                   
            ) # end of column with map
          ), # end of fluidRow container for upload method (left column) and map (right column)
          
        ), # end Site Selection tab panel
        
        #############################################################################  #
        # . --------------------------------------------------------------- ####
        
        # ______ SEE RESULTS _________ ####
        #. ####
        # #############################################################################  #
        
        # See Results tabPanel(title = "See Results" ####
        tabPanel(title = "See Results",
                 ##br(),
                 ##actionButton('back_to_site_sel', label = div(icon('play', style = 'transform: rotate(180deg);'),
                 ## HTML('&nbsp;'), 'Return to Site Selection'), class = 'usa-button'),
                 # . ### #
                 ## tabsetPanel(id = 'results_tabs'  ####
                 #tags$div( class = 'results_tabs_theme',
                 tabsetPanel(id = 'results_tabs',
                             #type = 'pills',
                             
                             
                             ######################################################################################################### #
                             # COMMUNITY REPORT VIEW ####
                             tabPanel(
                               title = "Community Report",
                               
                               includeCSS(EJAM:::app_sys('report/community_report/communityreport.css')),
                               includeCSS(EJAM:::app_sys('report/community_report/main.css')),
                               #includeCSS('inst/report/community_report/communityreport.css'),
                               #includeCSS('inst/report/community_report/main.css'),
                               
                               ## build HTML for community report
                               uiOutput('comm_report_html'),
                               
                               br(),
                               
                               #### quick_view_map ####
                               shinycssloaders::withSpinner(
                                 leaflet::leafletOutput('quick_view_map')#, width = '1170px', height = '627px')
                               ),
                               br(),
                               fluidRow(
                                 column(
                                   12, align = 'center',
                                   br(),br(),
                                   shinycssloaders::withSpinner(
                                     plotOutput(outputId = 'view1_summary_plot', width = '100%', height = '400px')  # {{ demog_plot }} goes in .html template
                                   )
                                 )
                               ),
                               div(
                                 style = "background-color: #edeff0; color: black; width: 100%; padding: 10px 20px; text-align: right; margin: 10px 0;",
                                 uiOutput("report_version_date")
                               ),
                               br(),
                               tags$div(
                                 shiny::downloadButton(
                                   outputId = 'community_download_all',
                                   label = 'Download Community Report', class = 'usa-button'), style = 'text-align: center;'
                               ),
                             ),  # end report tab
                             
                             
                             ######################################################################################################### #
                             #. ####
                             # ______ DETAILED RESULTS  _________ ####
                             ## tabPanel(title = 'Details' ####
                             #. ## ##
                             ##
                             
                             tabPanel(title = 'Details',
                                      br(),
                                      div(class = 'navbar1',
                                          navbarPage(
                                            id = "details_subtabs",
                                            title = NULL,
                                            #   navlistPanel(
                                            #   "Results Pages",
                                            #   well = FALSE,
                                            fluid = FALSE,
                                            # widths = c(2,10),
                                            
                                            ######################################################################################################### #
                                            # . ##  ##
                                            ### _TABLE of Sites - tabPanel(title = 'Site-by-Site Table'  ####
                                            
                                            tabPanel(title = 'Site-by-Site Table',
                                                     ### _button: Excel Download ## ##
                                                     
                                                     h4('About this Table'),
                                                     helpText('This table shows results for each Location in the analysis. It includes location and EJScreen demographic, environmental, and EJ indicator information.\n
       The downloaded version includes more columns than are displayed here.'),
                                                     fluidRow(
                                                       column(6,
                                                              #h3(id = 'site_by_site', 'Site-by-Site Table'),
                                                       ),
                                                       column(6,
                                                              ## button to download excel Table of Sites/Results - uses table_xls_format
                                                              downloadButton('download_results_table', 'Download Results Table', class = 'usa-button')
                                                       )
                                                     ),
                                                     br(), ## vertical space
                                                     
                                                     ### _output: Interactive Table of Sites/Results ## ##
                                                     shinycssloaders::withSpinner(
                                                       DT::DTOutput(outputId = 'view3_table', width = '100%')
                                                     )#,
                                                     # ### _MAP of 1 site clicked in table ####
                                                     # shinycssloaders::withSpinner(
                                                     #   leaflet::leafletOutput(outputId = 'v3_sitemap')
                                                     # )
                                            ), # end site by site table tabPanel
                                            
                                            
                                            
                                            ### _BARPLOT (AVG SCORES) - tabPanel(title = 'Plot Average Scores' ####
                                            # .
                                            
                                            tabPanel(id = "plot_average", 
                                                     title = 'Plot Average Scores',
                                                     h4('About this Chart'),
                                                     helpText('These charts show how each demographic group and environmental stressor, in the analyzed locations, compares to its US average.'),
                                                     
                                                     wellPanel(
                                                       style = 'width: 100%;',
                                                       ### _BARPLOT
                                                       br(),
                                                       #h3(id = 'barplot','Compare Across Indicators'),
                                                       #fluidRow(
                                                       ## input: Barplot setting - indicator type
                                                       # column(2,
                                                       shinycssloaders::withSpinner(
                                                         ## output: display barplot
                                                         plotOutput(outputId = 'summ_display_bar', height = '600px')
                                                       ),
                                                       
                                                       fluidRow(
                                                         column(4,
                                                                uiOutput('summ_bar_ind'),
                                                         ),
                                                         column(4,
                                                                ## input: Barplot setting - data type
                                                                radioButtons(inputId = 'summ_bar_data', label = 'Data Type',
                                                                             choiceValues = c('ratio',      'raw'),      # no 'pctile' at this time
                                                                             choiceNames  = c('Ratio to US','Raw data'), # no 'Percentile of population' at this time
                                                                             selected = 'ratio'),
                                                         )
                                                       ),
                                                       
                                                       ## hiding this option for now - defaulting to Average
                                                       ## input: Barplot setting - statistic type
                                                       # radioButtons(inputId = 'summ_bar_stat', 'Statistic',
                                                       #              choiceValues = c('avg', 'med'),
                                                       #              choiceNames = c('Average', 'Median'))
                                                       #  ),
                                                       
                                                       h4('Definitions'),
                                                       HTML("<strong>Average site</strong> = the average site's average resident (the average resident's score is calculated at each site as the site-specific population-weighted mean, and then the arithmetic mean of those site-specific scores is calculated)
          <br><strong>Average person at these sites</strong> = the average person among all the residents who are at any one or more of the sites, counting each person only once even if they live near more than one site.")
                                                     ),
                                                     br(), br(),
                                                     ######################################################################################################### #
                                            ),  # end of tabPanel(title = 'Plot Average Scores',
                                            
                                            ### _HISTOPLOT (RANGE OF SCORES) - tabPanel(title = 'Plot Full Range of Scores' ####
                                            
                                            tabPanel(id = "plot_range", 
                                                     title = 'Plot Full Range of Scores',
                                                     ### _HISTOGRAM
                                                     #h3(id = 'histogram',"Explore Indicator Distributions"),
                                                     h4('About this Chart'),
                                                     helpText('This chart shows the spread of indicator values across all locations in your analysis.'),
                                                     
                                                     wellPanel(
                                                       style = 'width: 100%;',
                                                       ## row of histogram settings
                                                       
                                                       fluidRow(
                                                         
                                                         column(2,
                                                                ## input: Histogram settings - distribution across sites or people
                                                                radioButtons(inputId = 'summ_hist_distn',
                                                                             label = h5('Distribution across sites or people (pop.wtd.)'),
                                                                             choices = c('Sites', 'People'), selected = 'People' ),
                                                                
                                                                ## input; Histogram settings - data type
                                                                radioButtons(inputId = 'summ_hist_data', label = h5('Data type'),
                                                                             choiceNames = c('Percentile of US', 'Raw data'),
                                                                             choiceValues = c('pctile',          'raw')),
                                                                
                                                                ## input: Histogram settings - number of bins
                                                                sliderInput(inputId = 'summ_hist_bins', label = h5('Bins'),
                                                                            min = 5, max = 50, step = 5, value = 10),
                                                         ),
                                                         column(10, align = 'center',
                                                                ## output: display histogram
                                                                shinycssloaders::withSpinner(
                                                                  plotOutput(outputId = 'summ_display_hist')
                                                                ),
                                                                
                                                                fluidRow(
                                                                  column(6, offset = 3,
                                                                         ## input: indicator dropdown for histogram
                                                                         uiOutput('summ_hist_ind'),
                                                                  ) # column with indicator selections
                                                                ) # row with indicator selections
                                                         ) #end column with hist
                                                       ) #end fluidrow
                                                       
                                                     ) # end wellpanel
                                            ) # end tab panel for histograms
                                          ) # end navbarPage
                                      ) # end div(class = 'navbar1'
                                      
                             ), # end 'Details' results tab
                             
                             ######################################################################################################### #
                             #. ####
                             # ______ FULL REPORT (Word doc) - tabPanel(title = 'Written Report' _________ ####
                             #. ## ##
                             
                             
                             tabPanel(title = 'Written Report',

                                      #  MAKE SURE all parameter names are used (identical names, and all are there) in these 4 places:
                                      #  1. input$ ids in app_ui.R, from user, to customize the long report
                                      #  2. params$ list passed by app_server.R to render the Rmd doc
                                      #  3. params: accepted in  .Rmd yaml info header
                                      #  4. params$  as used within body of  .Rmd text inline and in r code blocks.

                                      br(), ## vertical space

                                      wellPanel(
                                        br(), ## vertical space

                                        ## arrange text and buttons
                                        fluidRow(
                                          column(6,
                                                 ## add text above report settings
                                                 p('Edit report settings below to tailor the full report to your specific analysis.')
                                          ),
                                          column(6,
                                                 ## output: button to download static report
                                                 shiny::downloadButton(outputId = 'rg_download',
                                                                       label = 'Download report',
                                                                       class = 'usa-button'),

                                          )
                                        ), ######################################################### #

                                        #------- WHERE was analyzed? (where/ what sector/zones/types of places)

                                        #?  # analysis_title =  input$analysis_title,
                                        # zonetype =  input$rg_zonetype,   ### names differ by   rg_
                                        # where = input$rg_enter_miles,   ############# names differ
                                        # distance = paste0(input$bt_rad_buff,' miles'), #input$radius_units),   #############  param derived from input
                                        # sectorname_short = input$rg_enter_sites,                 ############# names differ
                                        # ## allow for either or
                                        # in_the_x_zone = ifelse(nchar(input$in_the_x_zone_enter) > 0,     ######  _enter  and derived from inputs
                                        #                        input$in_the_x_zone_enter,
                                        #                        input$in_the_x_zone),
                                        # facilities_studied = ifelse(nchar(input$facilities_studied_enter) > 0,    ####   _enter and derived from inputs
                                        #                             input$facilities_studied_enter,
                                        #                             input$facilities_studied),
                                        # within_x_miles_of = paste0("within ", paste0(input$bt_rad_buff,' miles'), " of"),   ##### param derived from input
                                        #
                                        # in_areas_where = paste0(input$in_areas_where, ' ', input$in_areas_where_enter),   ######   _enter
                                        # risks_are_x = input$risks_are_x,                      ### names match
                                        # source_of_latlons = input$source_of_latlons,          ### names match
                                        # sitecount = nrow(data_processed()$results_bysite),      ### param derived from data

                                        # put input$analysis_title   here???

                                        fluidRow(          #    param is called  where
                                          column(4,
                                                 ## input: analysis location - uses current value of radius slider
                                                 uiOutput('rg_enter_miles')
                                          )),

                                        # param distance is based on input$bt_rad_buff

                                        fluidRow(
                                          column(4,
                                                 ## input:  - which sites analyzed  #    param is called   sectorname_short
                                                 textInput(inputId = "rg_enter_sites",
                                                           label = "Describe sites analyzed:",
                                                           value = "facilities in the _____ source category"),
                                          )
                                        ),

                                        fluidRow(
                                          column(4,
                                                 ## input:   # zonetype =  input$rg_zonetype
                                                 selectInput(inputId = 'rg_zonetype',
                                                             label = 'Zone Type (How are zones defined?)',
                                                             choices = c('General' = 'zone_is_named_x','Proximity' = 'zone_is_nearby',
                                                                         'Risk' = 'zone_is_risk_x'))
                                          ),
                                          column(4,
                                                 ## input:   #  based on  input$bt_rad_buff
                                                 selectInput(inputId = 'within_x_miles_of',
                                                             label = 'Near to',
                                                             choices = c('near the','nearby',''))
                                          )
                                        ),

                                        fluidRow(
                                          column(4,
                                                 ## input:    # in_areas_where calculated from input$in_areas_where, and input$in_areas_where_enter
                                                 selectInput(inputId = 'in_areas_where',
                                                             label = 'Describe the surrounding area',
                                                             choices = c('in areas with',
                                                                         'where','in block groups where')
                                                 )
                                          ),
                                          column(4,
                                                 ## input:
                                                 textInput(inputId = 'in_areas_where_enter',
                                                           label = 'Add area details',
                                                           value = '')
                                          )
                                        ),
                                        fluidRow(
                                          column(8,
                                                 ## input:
                                                 selectInput(inputId = 'risks_are_x',
                                                             label = 'Risk level',
                                                             choices = c("risk is at or above 1 per million (lifetime individual cancer risk due to inhalation of air toxics from this source category)",
                                                                         "risk is above 1 per million",
                                                                         "the area is in nonattainment",
                                                                         "PM2.5 levels are in the highest decile",
                                                                         "ozone concentrations are at least 70 ppb")
                                                 )
                                          )
                                        ),
                                        fluidRow(
                                          column(4,
                                                 ## input:
                                                 selectInput(inputId = 'in_the_x_zone',
                                                             label = 'General study location',
                                                             choices = c('in the study area' = 'area', 'in the analyzed locations' = 'locs',
                                                                         'in [State X] (specify)' = 'state',
                                                                         'in EPA Region [XX] (specify)' = 'region')
                                                 )
                                          ),
                                          column(4,
                                                 ## add free text box if certain values chosen from radio button
                                                 conditionalPanel(
                                                   condition = "input.in_the_x_zone == 'state' || input.in_the_x_zone == 'region'",
                                                   textInput(inputId = 'in_the_x_zone_enter',
                                                             label = 'Other - please specify',
                                                             value = 'in ')
                                                 )
                                          )
                                        ),

                                        fluidRow(
                                          column(4,
                                                 ## input:
                                                 selectInput(inputId = 'facilities_studied',
                                                             label = 'Facilities Studied',
                                                             choices = c('facilities subject to this proposed rule' = 'rule',
                                                                         'analyzed facilities' = 'fac','analyzed sites' = 'sites',
                                                                         'facilities in the ___ source category' = 'cat',
                                                                         'facilities in the ____ sector (NAICS code __)' = 'sector')
                                                 )
                                          ),
                                          column(4,
                                                 ## add free text box if certain values chosen
                                                 conditionalPanel(
                                                   condition = "input.facilities_studied == 'cat' || input.facilities_studied == 'sector' || input.facilities_studied == 'rule'",
                                                   textInput(inputId = 'facilities_studied_enter',
                                                             label = 'Other - please specify')
                                                 )
                                          )
                                        ),

                                        fluidRow(
                                          column(4,
                                                 ## input:
                                                 textInput(inputId = 'source_of_latlons',
                                                           label = 'Source of Points',
                                                           placeholder = "EPA's Facility Registry Service (FRS)"),
                                          )
                                        ),


                                        #------- METHODS, AUTHORS, ETC.

                                        fluidRow(
                                          column(2,
                                                 ## input:
                                                 textInput(inputId = "rg_author_name",
                                                           label = "Author Name(s):",
                                                           value = "FirstName LastName")
                                          ),
                                          column(2,
                                                 ## input:
                                                 textInput(inputId = "rg_author_email",
                                                           label = "Author Email(s):",
                                                           value = "author@email.org")
                                          ),
                                          column(2,
                                                 ## input: checkbox to add line for coauthor information
                                                 checkboxInput(inputId = 'rg_add_coauthors',
                                                               label = 'Add co-authors?',
                                                               value = FALSE)
                                          )
                                        ),
                                        ## if checkbox is checked, add textinputs for co-author name and email
                                        conditionalPanel(
                                          condition = 'input.rg_add_coauthors == 1',
                                          fluidRow(
                                            column(2,
                                                   ## input:
                                                   textInput(inputId = 'coauthor_names', 'Co-Author Name(s)')
                                            ),
                                            column(2,
                                                   ## input:
                                                   textInput(inputId = 'coauthor_emails', 'Co-Author Email(s)')
                                            )
                                          )
                                        ),
                                        fluidRow(
                                          ## input:
                                          textInput(inputId = 'fundingsource',
                                                    label = 'Funding Source',
                                                    placeholder = "The Inflation Reduction Act (for example)"),
                                          ## input:
                                          textInput(inputId = 'acs_version',
                                                    label = 'Version of ACS data (years)',
                                                    placeholder =  as.vector(metadata_mapping$blockgroupstats[['acs_version']])),
                                          ## input:
                                          textInput(inputId = 'ejscreen_version',
                                                    label = 'Version of EJScreen',
                                                    placeholder =  as.vector(metadata_mapping$blockgroupstats[['ejam_package_version']]))
                                        ),
                                        ############################ #

                                        #------- RESULTS (tables and map and plots)

                                        # total_pop: NA
                                        # results: NA
                                        # results_formatted: NA
                                        # map: NA
                                        # map_placeholder_png:                 "map_placeholder.png"
                                        # envt_table: NA
                                        # envt_table_placeholder_png:   "envt_table_placeholder.png"
                                        # envt_table_placeholder_rda:   "envt_table_placeholder.rda"
                                        # demog_table: NA
                                        # demog_table_placeholder_png: "demog_table_placeholder.png"
                                        # demog_table_placeholder_rda: "demog_table_placeholder.rda"
                                        # boxplot: NA
                                        # boxplot_placeholder_png:         "boxplot_placeholder.png"
                                        # barplot: NA
                                        # barplot_placeholder_png:         "barplot_placeholder.png"
                                        #


                                        #------- TEXT PHRASES DESCRIBING AND INTERPRETING RESULT

                                        # demog_how_elevated: NA
                                        # envt_how_elevated: NA
                                        # demog_high_at_what_share_of_sites: NA
                                        # envt_high_at_what_share_of_sites: NA
                                        # conclusion1: NA
                                        # conclusion2: NA
                                        # conclusion3: NA

                                        fluidRow(
                                          column(4,
                                                 ## input:
                                                 textInput(inputId = 'demog_how_elevated',
                                                           label = 'Elevation of Demographic Indicators',
                                                           placeholder = 'moderately elevated'),
                                          ),
                                          column(4,
                                                 ## input:
                                                 textInput(inputId = 'envt_how_elevated',
                                                           label = 'Elevation of Environmental Indicators',
                                                           placeholder = 'moderately elevated'),
                                          )
                                        ),
                                        fluidRow(
                                          column(4,
                                                 ## input:
                                                 selectInput(inputId = 'demog_high_at_what_share_of_sites',
                                                             label = 'Demographic indicators high at what share of sites?',
                                                             choices = c('a surprisingly large share of these sites',
                                                                         'some of these sites, just as it varies nationwide',
                                                                         'a relatively small share of these sites'),
                                                             selected = 'some of these sites, just as it varies nationwide'),
                                          ),
                                          column(4,
                                                 ## input:
                                                 selectInput(inputId = 'envt_high_at_what_share_of_sites',
                                                             label = 'Environmental indicators high at what share of sites?',
                                                             choices = c('a surprisingly large share of these sites',
                                                                         'some of these sites, just as it varies nationwide',
                                                                         'a relatively small share of these sites'),
                                                             selected = 'some of these sites, just as it varies nationwide'),
                                          )
                                        ),
                                        fluidRow(
                                          column(8,
                                                 ## input: conclusion 1 -
                                                 textAreaInput(inputId = 'conclusion1',
                                                               label = 'Conclusion 1',
                                                               placeholder = "The people living near these sites are 40% more likely to be in Limited-English Households than the average US resident. (for example)"
                                                 )
                                          )
                                        ),
                                        fluidRow(
                                          column(8,
                                                 ## input: conclusion 2-
                                                 textAreaInput(inputId = 'conclusion2',
                                                               label = 'Conclusion 2',
                                                               placeholder = "The % low income among these residents is 2.4 times the rate in the US overall. (for example)")
                                          )
                                        ),
                                        fluidRow(
                                          column(8,
                                                 ## input: conclusion 3 -
                                                 textAreaInput(inputId = 'conclusion3',
                                                               label = 'Conclusion 3',
                                                               placeholder = "The average resident near these sites is 1.5 times as likely to be Hispanic as the average person in their State overall. (for example)")
                                          )
                                        ),
                                      ) # end wellpanel
                             )  # end written report  tab
                             
                 ) ## end of tabset panel results_tabs ^^^^^^^^^^  ####
                 
        )      # end of tab panel See Results ^^^^^^^^^^  ####
        
        ,  # uncomment this comma if uncommenting the advanced tab AND/OR ejscreenapi module
        
        
        ######################################################## #
        #
        # . --------------------------------------------------------------- ####
        ## . ####
        # EJSCREEN API MODULE -  tabPanel   ####
        ## may move to another tab. or in a conditional UI panel.
        ## see default_hide_ejscreenapi_tab in global.R
        
        # tabPanel(title = 'EJScreen Batch Tool',  
        #
        #          h3("Access to EJScreen results via the API"),
        #          h4("(slow, fewer features, and cannot aggregate overall, but exactly replicates EJScreen web app)"),
        #          br(),
        #
        #          # notes  ## ##
        #          # If a module needs to use a reactive expression, the outer function should take the reactive expression as a parameter.
        #          # If a module needs to update a reactiveVal that is in the calling envt, it can take it as a param and then just modify it, right?
        #          # If a module wants to return reactive expressions to the calling app, then return a list of reactive expressions from the function.
        #          # If a module needs to access an input that isn’t part of the module, the
        #          #   containing app should pass the input value wrapped in a reactive expression (i.e. reactive(...)):
        #          #   myModule("myModule1", reactive(input$checkbox1))
        #
        #          mod_ejscreenapi_ui("x2",
        #
        #                             simpleradius_default_for_ui = 2
        #          )
        #
        #          # uiOutput("mod_ejscreenapi_ui_TO_SHOW_IN_APP_UI")  # this approach would use the module UI from the outer app server, not here
        #          # mod_ejscreenapi_ui_test("x1")
        #
        # )
        # , # uncomment if uncommenting BOTH ejscreenapi module tab and advanced tab
        
        ######################################################## #
        ## . ####
        # ADVANCED SETTINGS - tabPanel(title = "Advanced Settings"  ####
        ######################################################## #
        
        tabPanel(title = "Advanced Settings",
                 
                 h3("Advanced settings and experimental features not fully tested"),
                 
                 # SET DEFAULTS / OPTIONS
                 
                 # * Each time a user session is started, the application-level option set is duplicated, for that session.
                 # * If the options are set from inside the server function, then they will be scoped to the session.
                 # h5("Note: Some defaults and caps are defined in global.R"),
                 
                 ######################################################## #
                 ## Bookmarking button ####
                 h2("Bookmarking to save settings and inputs"),
                 
                 conditionalPanel(condition = bookmarking_allowed, {
                   bookmarkButton()  # https://mastering-shiny.org/action-bookmark.html
                 }),
                 ######################################################## #
                 ### ------------------------ app title ### #
                 # will not be editable here.
                 
                 ######################################################## #
                 ##  Uploading files/points/shapes ####
                 h2("Limits on uploads/points/shapes"),
                 
                 numericInput('max_pts_upload', label = "Cap on number of points one can UPLOAD, additional ones in uploaded table get dropped entirely",
                              min = 1000,  step = 500,
                              value = default_max_pts_upload,
                              max =        maxmax_pts_upload),
                 numericInput('max_pts_map', label = "Cap on number of points one can MAP",
                              min = 500,  step = 100,
                              value = default_max_pts_map,
                              max =        maxmax_pts_map),
                 numericInput('max_pts_showtable', label = "Cap on number of points to be rendered for display in DT interactive TABLE (uploads or results)",
                              min = 100, step = 100,
                              value = default_max_pts_showtable,
                              max =        maxmax_pts_showtable),
                 numericInput('max_pts_run', label = "Cap on number of points one can request RESULTS for in one batch",
                              min = 1000,  step = 100,
                              value = default_max_pts_run,
                              max =        maxmax_pts_run),
                 
                 numericInput('max_shapes_map', label = "Cap on number of shapes (polygons) one can MAP",
                              min = 10,  step = 10,
                              value = default_max_shapes_map,
                              max =        maxmax_shapes_map),
                 
                 numericInput(inputId = 'max_mb_upload', label = 'Cap on size of file(s) one can upload in MB (an issue for shapefiles, mainly)',
                              min = minmax_mb_upload,
                              value = global_or_param("default_max_mb_upload"),
                              max = maxmax_mb_upload,
                              step = minmax_mb_upload),
                 
                 ######################################################## #
                 ## *Radius* options ####
                 h2("Radius options"),
                 
                 # minradius  # (set via global.R)
                 # minradius_shapefile # (0 set via global.R)
                 # stepradius # (set via global.R)
                 
                 numericInput('default_miles', label = "Default miles radius",  # what is shown at app startup for all but shapefiles
                              ### Also note server code where radius can be modified via updateSliderInput,
                              ### and saved current value stored is specific to each upload type, returns to that when switch type back.
                              min = minradius,  # from global.R
                              value = global_or_param("default_default_miles"),
                              max   = global_or_param("default_max_miles")), # (set via global.R) highest allowed default (i.e. initial) value
                 
                 numericInput('default_miles_shapefile', label = "Default miles width of buffer around shapefile edges",
                              min = minradius_shapefile, # from global.R
                              global_or_param("default_default_miles_shapefile"),
                              max   =     max_default_miles), # (set via global.R) highest allowed default (i.e. initial) value
                 
                 numericInput('max_miles', label = "Maximum radius in miles",
                              value = global_or_param("default_max_miles"), # (set via global.R) initial cap that advanced tab lets you increase here
                              max        = maxmax_miles), # (set via global.R) i.e., even in the advanced tab one cannot exceed this cap
                 
                 ######################################################## #
                 ## Calculating and reporting extra metrics ####
                 h2("Calculating and reporting extra metrics"),
                 
                 checkboxInput('calculate_ratios',
                               label = "Results in Excel should include ratios to US and State averages",
                               value = default_calculate_ratios),
                 checkboxInput('include_averages',
                               label = "Results should include US and State Averages - *** not implemented yet",
                               value = default_include_averages),
                 checkboxInput('include_extraindicators',
                               label = 'Results should include extra indicators from Community Report - *** not implemented yet',
                               value = default_include_extraindicators),
                 
                 ######################################################## #
                 ## Viewing maps, saving results ####
                 h2("Viewing maps, saving results"),
                 
                 textInput('prefix_filenames', label = "Prefix to use in default file names when downloading [***NOT implemented yet]", value = ""),
                 
                 ## Map colors, weights, opacity ####
                 ### in ejscreenapi:
                 numericInput(inputId = "circleweight_in", label = "weight of circles in maps", value = default_circleweight),
                 
                 # opacitymin   <- 0
                 # opacitymax   <- 0.5
                 # opacitystep  <- 0.025
                 # opacitystart <- 0.5
                 # opacityratio <- 2 / 5
                 # base_color_default      <- "blue"  ;
                 # cluster_color_default   <- "red"   ;
                 # highlight_color_default <- 'orange';
                 
                 ######################################################## #
                 ## Spreadsheet formatting of results ####
                 h2("Spreadsheet formatting of results"),
                 
                 # heatmap column names
                 
                 
                 # heatmap cutoffs for bins
                 
                 
                 # heatmap colors for bins
                 
                 
                 checkboxInput("ok2plot",
                               label = "OK to try to plot graphics and include in Excel download",
                               value = default_ok2plot),
                 
                 ######################################################## #
                 ##  Finding distances: getblocksnearby() ####
                 h2("Finding distances to nearby blocks and residents"),
                 
                 radioButtons(inputId = "avoidorphans",
                              label =  "Avoid orphans (by searching for nearest one out to maxradius, instead of reporting NA when no block is within radius)",
                              choices = c(Yes = TRUE, No = FALSE),
                              inline = TRUE,
                              selected = default_avoidorphans),
                 
                 numericInput(inputId = 'maxradius', # THIS IS NOT THE MAX RADIUS USERS CAN PICK - THIS IS THE MAX TO WHICH IT COULD SEARCH IF avoidorphans=T
                              label = 'If avoid orphans=T, Max distance in miles to search for closest single block if site has none within normal radius',
                              value =  default_maxradius,  # 50000 / meters_per_mile, # 31.06856 miles !!
                              min = 0, max = default_maxradius, step = 1),
                 
                 ######################################################## #
                 ## Which indicators to include in outputs via doaggregate() ####
                 h2("Which indicators to include in outputs"),
                 
                 shiny::selectInput('subgroups_type',
                                    #    "nh" for non-hispanic race subgroups as in Non-Hispanic White Alone, nhwa and others in names_d_subgroups_nh;
                                    #    "alone" for EJScreen v2.2 style race subgroups as in    White Alone, wa and others in names_d_subgroups_alone;
                                    #    "both" for both versions.
                                    label = "Which definition of demographic race ethnicity subgroups to include?",
                                    choices = list(NonHispanicAlone = 'nh', Alone = 'alone', Both = 'both'),
                                    selected = default_subgroups_type),
                 
                 shiny::radioButtons(inputId = "need_proximityscore",
                                     label = "Results should include proximity score?",
                                     choices = list(Yes = TRUE, No = FALSE ),
                                     selected = default_need_proximityscore),
                 
                 shiny::radioButtons(inputId = "include_ejindexes",
                                     label = "Need EJ Indexes",
                                     choices = list(Yes = TRUE, No = FALSE ),
                                     selected = default_include_ejindexes),
                 
                 shiny::radioButtons(inputId = "extra_demog",
                                     label = "Need extra indicators from EJScreen v2.2 report, on language, age groups, gender, percent with disability, poverty, etc.",
                                     choices = list(Yes = TRUE, No = FALSE ),
                                     selected = default_extra_demog),
                 
                 ######################################################## #
                 ## Counting indicators reaching certain thresholds ####
                 h2("Counting indicators reaching certain thresholds"),
                 
                 ## input: GROUP NAME for 1st set of comparisons - where the table counts which scores are above certain cutoffs?
                 shiny::textInput(inputId = 'an_threshgroup1',
                                  label = 'Name for 1st set of comparisons',
                                  value = default.an_threshgroup1
                 ),
                 ## input: variable names for 1st set of comparisons
                 shiny::selectizeInput(inputId = 'an_threshnames1',
                                       multiple = TRUE,
                                       label = 'variable names for 1st set of comparisons',
                                       choices = names_all_r,
                                       selected = default.an_threshnames1
                 ),
                 ## input: Threshold VALUE(s) for 1st set of comparisons
                 numericInput(inputId = 'an_thresh_comp1',
                              label = 'Threshold value(s) for 1st set of comparisons (e.g. %ile 1-100):',
                              value = default.an_thresh_comp1
                 ),
                 ###### #
                 
                 ## input: GROUP NAME for 2d set of comparisons
                 shiny::textInput(inputId = 'an_threshgroup2',
                                  label = 'Name for 2nd set of comparisons',
                                  value = default.an_threshgroup2
                 ),
                 ## input: variable names for 2d set of comparisons
                 shiny::selectizeInput(inputId = 'an_threshnames2',
                                       multiple = TRUE,
                                       label = 'variable names for 2d set of comparisons',
                                       choices = names_all_r,
                                       selected = default.an_threshnames2
                 ),
                 ## input: Threshold VALUE(s) for 2nd set of comparisons
                 numericInput(inputId = 'an_thresh_comp2',
                              label = 'Threshold value(s) for 2nd set of comparisons (e.g. %ile 1-100):',
                              value = default.an_thresh_comp2
                 ),
                 
                 ######################################################## #
                 ## Short report options ####
                 h2("Short report"),
                 
                 shiny::textInput("standard_analysis_title",
                                  label = "Default title to show on each short report",
                                  value = default_standard_analysis_title),
                 
                 ## input: Type of plot for 1page report
                 shiny::radioButtons(inputId = "plotkind_1pager",
                                     label = "Type of plot for 1page report",
                                     choices = list(Bar = "bar", Box = "box", Ridgeline = "ridgeline"),
                                     selected = default_plotkind_1pager),
                 
                 ## _radio button on format of short report
                 #                  was DISABLED while PDF KNITTING DEBUGGED
                 radioButtons("format1pager", "Format", choices = c(html = "html", html = "pdf"), inline = TRUE),
                 
                 textInput(inputId = "Custom_title_for_bar_plot_of_indicators", label = "Enter title for bar plot of indicators", value = gsub("[^a-zA-Z0-9 ]", "", "") ),
                 
                 ######################################################## #
                 ## Long report options ####
                 h2("Long report"),
                 
                 # relocate any here from the Full Report tab??
                 
                 br(), ## vertical space
                 
                 shiny::radioButtons(inputId = "more3",
                                     label = "placeholder for options not yet implemented",
                                     choices = list(TBD = "a", etc = "b"),
                                     selected = "a"),
                 
                 # ),
                 
                 ##################################################### #
                 ## Testing modes ####
                 h2("Testing/ debugging modes!!!"),
                 
                 radioButtons("testing", "testing?", choices = c(Yes = TRUE, No = FALSE),
                              inline = TRUE,
                              selected = default_testing),
                 
                 radioButtons("shiny.testmode", "shiny.testmode?", choices = c(Yes = TRUE, No = FALSE),
                              inline = TRUE,
                              selected = default_shiny.testmode),
                 # If TRUE, then various features for testing Shiny applications are enabled.
                 # shiny.reactlog (defaults to FALSE)
                 #    If TRUE, enable logging of reactive events, which can be viewed later with the reactlogShow() function. This incurs a substantial performance penalty and should not be used in production.
                 #
                 # shiny.devmode (defaults to NULL)
                 # Option to enable Shiny Developer Mode. When set, different default getOption(key) values will be returned. See devmode() for more details.
                 #
                 # shiny.sanitize.errors (defaults to FALSE)
                 #    If TRUE, then normal errors (i.e. errors not wrapped in safeError) won't show up in the app; a simple generic error message is printed instead (the error and strack trace printed to the console remain unchanged). If you want to sanitize errors in general, but you DO want a particular error e to get displayed to the user, then set this option to TRUE and use stop(safeError(e)) for errors you want the user to see.
                 #
                 # shiny.suppressMissingContextError (defaults to FALSE)
                 #    Normally, invoking a reactive outside of a reactive context (or isolate()) results in an error. If this is TRUE, don't error in these cases. This should only be used for debugging or demonstrations of reactivity at the console.
                 #
                 # shiny.stacktraceoffset (defaults to TRUE)
                 #    If TRUE, then Shiny's printed stack traces will display srcrefs one line above their usual location. This is an arguably more intuitive arrangement for casual R users, as the name of a function appears next to the srcref where it is defined, rather than where it is currently being called from.
                 #
                 # shiny.trace (defaults to FALSE)
                 # Print messages sent between the R server and the web browser client to the R console. This is useful for debugging. Possible values are "send" (only print messages sent to the client), "recv" (only print messages received by the server), TRUE (print all messages), or FALSE (default; don't print any of these messages).
                 #
                 # shiny.autoload.r (defaults to TRUE)
                 # If TRUE, then the R/ of a shiny app will automatically be sourced.
                 
                 checkboxInput('print_uploaded_points_to_log', label = "Print each new uploaded lat lon table full contents to server log",
                               value = default_print_uploaded_points_to_log),
                 ## . ####
                 ############################################################### #
                 # ejscreen API tool link ####

                 span('tool for batch use of the EJScreen API: ',

                      a('ejscreenapi tool',
                        href = 'https://rstudio-connect.dmap-stage.aws.epa.gov/content/163e7ff5-1a1b-4db4-ad9e-e9aa5d764002/',
                        target = '_blank', rel = 'noreferrer noopener'))
                 
                 # end advanced features and settings subtab
                 ##################################################################### #
                 
        ) # end Advanced Settings + API tab ## ##
        
        ################################################################################ #
        ## . ####
        
      ), # end tabset panel from line 37 or so ^^^^^^^^^  ## ##
      html_footer_fmt  ## adds HTML footer - defined in global.R
      
    ) ## end fluidPage
  ) # end tag list
  
  ########################################################################### #
} # end app_ui
# ___________ App UI ends here ________ ####
########################################################################### #


#' Add external Resources to App (from golem package code)
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#'
#' @noRd
#'
golem_add_external_resources <- function() {
  
  golem::add_resource_path(
    "www",
    EJAM:::app_sys("app/www") #   points to  installed/EJAM/app/www which is same as   source/EJAM/inst/app/www
  )
  tags$head(
    
    # app title ####
    golem::bundle_resources(
      path = EJAM:::app_sys("app/www"),   #  points to  installed/EJAM/app/www which is same as   source/EJAM/inst/app/www
      
      app_title = .app_title # BUT SEE ALSO THE TITLE IN HTML IN global.R 
      
    ),
    
    # favorites icons ####
    ### inserted this in head of index.html (or use tags$link() as below) to make all favicon versions work
    ###   for various platforms/sizes
    #  favicon.png  is the only one set up by golem::favicon() and was .ico in the example notes but png is bigger higher res here
    
    golem::favicon(ext = 'png'),
    
    tags$head(tags$link(rel = "apple-touch-icon",                   sizes = "180x180", href = "apple-touch-icon.png")),
    tags$head(tags$link(rel = "icon",           type = "image/png", sizes = "32x32" ,  href = "favicon-32x32.png"   )),
    tags$head(tags$link(rel = "icon",           type = "image/png", sizes = "16x16" ,  href = "favicon-16x16.png"   )),
    tags$head(tags$link(rel = "manifest",                                              href = "site.webmanifest"    )),
    tags$head(tags$link(rel = "mask-icon" ,                                            href = "safari-pinned-tab.svg",  color = "#5bbad5")),
    
    tags$meta(name = "msapplication-TileColor",  content = "#2d89ef"),
    tags$meta(name = "msapplication-config",     content = "browserconfig.xml"),
    tags$meta(name = "theme-color",              content = "#ffffff")
    
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
########################################################################### #

